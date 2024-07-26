#![feature(generic_arg_infer)]
#![feature(char_indices_offset)]

use std::{
    collections::HashMap, fmt::Display, io::Write, mem::transmute, os::unix::fs::OpenOptionsExt,
    process::exit, str::CharIndices,
};

struct Addr<'a> {
    base: Option<u32>,
    index: Option<u32>,
    scale: u32,
    disp: i64,
    #[allow(dead_code)]
    label: Option<(LabelName<'a>, usize)>,
}

enum Arg<'a> {
    None,
    Eax,
    Reg8(u32),
    _Reg16(u32),
    Reg32(u32),
    Reg64(u32),
    Imm(i64),
    Mem(Addr<'a>),
    Mem8(Addr<'a>),
    Mem16(Addr<'a>),
    Mem32(Addr<'a>),
    Mem64(Addr<'a>),
    Rel32(i64, usize),
    Label((LabelName<'a>, usize)),
    AnonLabel(usize, usize),
}

impl Arg<'_> {
    fn imm(&self) -> i64 {
        match self {
            Arg::Imm(imm) => *imm,
            _ => unreachable!(),
        }
    }
    fn reg(&self) -> u32 {
        match self {
            Arg::Eax => 0,
            Arg::Reg8(reg) => *reg,
            Arg::Reg32(reg) => *reg,
            Arg::Reg64(reg) => *reg,
            _ => unreachable!(),
        }
    }
}

impl Display for Arg<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::None => write!(f, "<none>"),
            Arg::Eax => write!(f, "eax"),
            Arg::Reg8(reg) => write!(f, "{} (r8)", REGS8[*reg as usize]),
            Arg::_Reg16(_reg) => todo!(),
            Arg::Reg32(reg) => write!(f, "{} (r32)", REGS32[*reg as usize]),
            Arg::Reg64(reg) => write!(f, "{} (r64)", REGS64[*reg as usize]),
            Arg::Imm(imm) => write!(f, "{imm} (imm)"),
            Arg::Rel32(off, _) => write!(f, "{off} (rel32)"),
            Arg::AnonLabel(idx, _) => write!(f, "{idx} (anon label)"),
            Arg::Label((label, _)) => write!(f, "{} (rel32)", label),
            Arg::Mem(addr)
            | Arg::Mem8(addr)
            | Arg::Mem16(addr)
            | Arg::Mem32(addr)
            | Arg::Mem64(addr) => {
                let mut s = String::new();

                s.push_str(match self {
                    Arg::Mem8(_) => "m8",
                    Arg::Mem16(_) => "m16",
                    Arg::Mem32(_) => "m32",
                    Arg::Mem64(_) => "m64",
                    Arg::Mem(_) => "m",
                    _ => unreachable!(),
                });

                if let Some(base) = addr.base {
                    s.push_str(&format!(" base={}", REGS64[base as usize]));
                }

                if let Some(index) = addr.index {
                    s.push_str(&format!(" index={}", REGS64[index as usize]));
                    if addr.scale > 1 {
                        s.push_str(&format!(" scale={}", addr.scale));
                    }
                }

                if addr.disp != 0 {
                    s.push_str(&format!(" disp={}", addr.disp));
                }

                if let Some((label, _)) = addr.label {
                    s.push_str(&format!(" label={label}"));
                }

                write!(f, "{}", s)
            }
        }
    }
}

struct Context<'a> {
    patches: Vec<Patch<'a>>,
    labels: HashMap<&'a str, Label<'a>>,
    out: Output<'a>,
    curr_label: &'a str,
    anon_labels: Vec<i64>,
    anon_patches: Vec<AnonPatch>,
}

struct Output<'a> {
    data: &'a mut Vec<u8>,
    buf: [u8; 16],
    len: usize,
    rex: u32,
}

impl Output<'_> {
    fn pos(&self) -> i64 {
        (self.data.len() + self.len + if self.rex != 0 { 1 } else { 0 }) as i64
    }
}

#[derive(Clone, Copy)]
struct LabelName<'a> {
    global: &'a str,
    local: &'a str,
}

impl Display for LabelName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.local.is_empty() {
            write!(f, "{}", self.global)
        } else {
            write!(f, "{}.{}", self.global, self.local)
        }
    }
}

struct Label<'a> {
    off: i64,
    locals: HashMap<&'a str, i64>,
}

struct Cursor<'a> {
    char: char,
    off: usize,
    iter: CharIndices<'a>,
}

#[derive(Debug)]
struct Error {
    msg: &'static str,
    at: usize,
}

struct AnonPatch {
    idx: usize,
    at: usize,
    off: i64,
}

struct Patch<'a> {
    off: i64,
    label: LabelName<'a>,
    at: usize,
    disp: i64,
}

#[derive(Debug, PartialEq)]
enum Enc {
    ZO,
    OI,
    RM,
    MR,
    MI,
    M1,
    O,
    D,
    M,
    // immediate is the second operand (used in cases where the first operand is encoded as part of the opcode)
    I2,
    RMI,
}

struct Insn {
    mnemonic: &'static str,
    op1: Op,
    op2: Op,
    op3: Op,
    rex: u8,
    opcodes: &'static [u8],
    reg: u8,
    encoding: Enc,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Op {
    None,
    Eax,
    R8,
    _R16,
    R32,
    R64,
    RM8,
    RM16,
    RM32,
    RM64,
    M,
    M64,
    One,
    Imm8,
    Imm8sx,
    Imm32,
    Imm32sx,
    Rel32,
}

const REGS8: [&str; 12] = [
    "al", "cl", "dl", "bl", "", "", "", "", "r8b", "r9b", "r10b", "r11b",
];

const REGS32: [&str; 16] = [
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d",
    "r13d", "r14d", "r15d",
];

const REGS64: [&str; 16] = [
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "r13",
    "r14", "r15",
];

const REX_W: u8 = 0b0100_1000;
const REX_R: u8 = 0b0100_0100;
const REX_X: u8 = 0b0100_0010;
const REX_B: u8 = 0b0100_0001;

fn main() {
    let mut args = std::env::args().skip(1);
    if args.len() == 0 {
        print_usage();
    }

    let mut path = None;
    let mut verbose = false;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-v" => verbose = true,
            "-h" => {
                print_usage();
            }
            _ => {
                if path.is_none() {
                    path = Some(arg);
                    continue;
                }

                eprintln!("error: invalid option '{arg}'");
                exit(1);
            }
        }
    }

    let path = path.unwrap_or_else(|| {
        print_usage();
    });

    let input = std::fs::read_to_string(&path).unwrap_or_else(|err| {
        eprintln!("error: failed to read input file: {err}");
        exit(1);
    });

    #[rustfmt::skip]
    let mut out = vec![
        0x7f, b'E', b'L', b'F', 0x02, 0x01, 0x01, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x3e, 0x00, 0x01, 0x00, 0x00, 0x00,
        0x78, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, // e_entry
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_phoff
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // e_shoff
        0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x38, 0x00,
        0x01, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,

        0x01, 0x00, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_offset
        0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, // p_vaddr
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_filesz
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // p_memsz
        0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ];

    if let Err(err) = assemble(&input, &mut out, verbose) {
        eprintln!("{}", fmt_error(err, &input, &path));
        exit(1);
    }

    let size = (out.len() - 120) as u64;
    unsafe {
        (out.as_mut_ptr().offset(96) as *mut u64).write_unaligned(size); // update p_filesz field
        (out.as_mut_ptr().offset(104) as *mut u64).write_unaligned(size); // update p_memsz field
    }

    let out_path = path.strip_suffix(".asm").unwrap_or_else(|| {
        eprintln!("error: output path is the same as input path");
        exit(1);
    });

    std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .mode(0o755)
        .open(out_path)
        .and_then(|mut file| file.write_all(&out))
        .unwrap_or_else(|err| {
            eprintln!("error: failed to write output file: {err}");
            exit(1);
        });
}

fn print_usage() -> ! {
    eprintln!(
        r"usage: asm [OPTIONS] INPUT
  -h display this message
  -v verbose output"
    );
    exit(1);
}

fn fmt_error(err: Error, input: &str, path: &str) -> String {
    let mut beg = 0;
    let mut line = 1;
    let mut column = 0;
    let mut iter = input.char_indices();

    loop {
        if let Some((off, char)) = iter.next() {
            if off == err.at {
                break;
            }
            if char == '\n' {
                line += 1;
                column = 0;
                beg = iter.offset();
            } else {
                column += 1;
            }
        } else {
            break;
        }
    }

    for char in input[beg..err.at].chars() {
        if char == ' ' || char == '\t' {
            beg += 1;
        } else {
            break;
        }
    }

    let mut spaces = 0;
    for _ in input[beg..err.at].chars() {
        spaces += 1;
    }

    let mut len = input.len();
    for (off, char) in input[err.at..].char_indices() {
        if char == '\n' {
            len = off;
            break;
        }
    }

    let prefix = format!("{path}:{line}:{column}: ", column = column + 1);
    let end = (err.at + len).min(input.len());

    format!(
        "{prefix}{line}\n{spaces:<off$}^ {msg}",
        line = &input[beg..end],
        spaces = "",
        off = spaces + prefix.len(),
        msg = err.msg
    )
}

fn assemble(input: &str, out: &mut Vec<u8>, verbose: bool) -> Result<(), Error> {
    let mut cur = Cursor {
        char: '\0',
        off: 0,
        iter: input.char_indices(),
    };

    advance(&mut cur);

    let mut ctx = Context {
        labels: HashMap::new(),
        patches: Vec::new(),
        out: Output {
            data: out,
            buf: [0; 16],
            len: 0,
            rex: 0,
        },
        curr_label: "",
        anon_labels: Vec::new(),
        anon_patches: Vec::new(),
    };

    loop {
        skip_whitespace(&mut cur);

        if match_char('.', &mut cur) {
            let label_at = cur.off - 1;
            if let Some((ident, at)) = match_identifier(&mut cur, &input) {
                if match_char(':', &mut cur) {
                    match ctx.labels.get_mut(ctx.curr_label) {
                        Some(label) => {
                            let off = ctx.out.data.len() as i64;
                            if label.locals.insert(ident, off).is_some() {
                                return error_at(label_at, "label with this name already exists");
                            };
                        }
                        None => return error_at(label_at, "local label declared in global scope"),
                    }
                    continue;
                } else {
                    skip_whitespace(&mut cur);
                    asm_directive(ident, at, &mut cur, ctx.out.data)?;
                }
            } else {
                return error_at(cur.off, "expected directive name");
            }
        } else if let Some((ident, at)) = match_identifier(&mut cur, input) {
            if match_char(':', &mut cur) {
                apply_anon_patches(&mut ctx.anon_patches, &ctx.anon_labels, &mut ctx.out)?;

                let label = Label {
                    off: ctx.out.data.len() as i64,
                    locals: HashMap::new(),
                };

                if ctx.labels.insert(ident, label).is_some() {
                    return error_at(at, "label with this name already exists");
                }

                ctx.curr_label = ident;
                ctx.anon_labels.clear();
                skip_whitespace(&mut cur);
                continue;
            } else {
                skip_whitespace(&mut cur);
                asm_instruction((ident, at), &mut cur, input, &mut ctx, verbose)?;
            }
        } else if match_char(':', &mut cur) {
            if ctx.curr_label.is_empty() {
                return error_at(
                    cur.off - 1,
                    "anonymous label must be declared in local scope",
                );
            }
            ctx.anon_labels.push(ctx.out.data.len() as i64);
            continue;
        }

        skip_whitespace(&mut cur);

        // optionally skip comment at end of line
        if match_char(';', &mut cur) {
            while cur.char != '\n' && cur.off != input.len() {
                advance(&mut cur);
            }
        }

        if cur.char == '\n' {
            (cur.off, cur.char) = cur.iter.next().unwrap_or_default();
        } else {
            break;
        }
    }

    if cur.char != '\0' || cur.iter.as_str().len() != 0 {
        return error_at(cur.off, "invalid input");
    }

    apply_anon_patches(&mut ctx.anon_patches, &ctx.anon_labels, &mut ctx.out)?;
    apply_patches(&ctx.labels, &ctx.patches, &mut ctx.out)?;

    Ok(())
}

#[allow(dead_code)]
fn print_label(name: &str, labels: &HashMap<&str, Label>, anon_labels: &[i64]) {
    if !name.is_empty() {
        println!("[{name}]");
    }

    let parent = if let Some(label) = labels.get(name) {
        for (name, off) in &label.locals {
            println!("  [{name}] {off}", off = off - label.off);
        }
        label.off
    } else {
        0
    };

    for &off in anon_labels {
        println!("  <anon> +{off}", off = off - parent);
    }
}

fn resolve_label(name: &LabelName, labels: &HashMap<&str, Label>) -> Option<i64> {
    if let Some(label) = labels.get(name.global) {
        if name.local.is_empty() {
            return Some(label.off);
        } else if let Some(&off) = label.locals.get(name.local) {
            return Some(off);
        }
    }
    None
}

fn apply_patches(
    labels: &HashMap<&str, Label>,
    patches: &Vec<Patch>,
    out: &mut Output,
) -> Result<(), Error> {
    for patch in patches {
        let mut disp = patch.disp;
        if let Some(off) = resolve_label(&patch.label, labels) {
            disp += off as i64 - patch.off as i64; // TODO: check that it fits within i32

            // TODO: this assumes that the displacement is always 32-bits, for enabling disp8
            // the offset will need to be stored with the patch
            unsafe {
                (out.data.as_mut_ptr().add(patch.off as usize - 4) as *mut i32)
                    .write_unaligned(disp as i32);
            };
        } else {
            return error_at(patch.at, "missing label");
        }
    }

    Ok(())
}

fn apply_anon_patches(
    patches: &mut Vec<AnonPatch>,
    anon_labels: &[i64],
    out: &mut Output,
) -> Result<(), Error> {
    for patch in patches.iter() {
        if let Some(off) = anon_labels.get(patch.idx) {
            let disp = off - patch.off; // TODO: check that it fits within i32
            unsafe {
                (out.data.as_mut_ptr().add(patch.off as usize - 4) as *mut i32)
                    .write_unaligned(disp as i32);
            };
        } else {
            return error_at(patch.at, "missing anonymous label");
        }
    }

    patches.clear();

    Ok(())
}

#[allow(dead_code)]
#[allow(unused_variables)]
fn choose_insn(mnemonic: &str, arg1: &Arg, arg2: &Arg, arg3: &Arg) -> Option<&'static Insn> {
    use self::Op::*;

    #[rustfmt::skip]
    static INSNS: [Insn; 76] = [
        insn("add",     RM32,  R32,     None,    0,       &[0x01],       0, Enc::MR),
        insn("add",     RM64,  R64,     None,    REX_W,   &[0x01],       0, Enc::MR),
        insn("add",     RM32,  Imm8sx,  None,    0,       &[0x83],       0, Enc::MI),
        insn("add",     RM64,  Imm8sx,  None,    REX_W,   &[0x83],       0, Enc::MI),
        insn("add",     RM32,  Imm32,   None,    0,       &[0x81],       0, Enc::MI),
        insn("add",     RM64,  Imm32sx, None,    REX_W,   &[0x81],       0, Enc::MI),
        insn("and",     RM32,  Imm8sx,  None,    0,       &[0x83],       4, Enc::MI),
        insn("call",    Rel32, None,    None,    0,       &[0xe8],       0, Enc::D),
        insn("cmove",   R32,   RM32,    None,    0,       &[0x0f, 0x44], 0, Enc::RM),
        insn("cmovg",   R32,   RM32,    None,    0,       &[0x0f, 0x4f], 0, Enc::RM),
        insn("cmovg",   R64,   RM64,    None,    REX_W,   &[0x0f, 0x4f], 0, Enc::RM),
        insn("cmovnz",  R32,   RM32,    None,    0,       &[0x0f, 0x45], 0, Enc::RM),
        insn("cmovz",   R32,   RM32,    None,    0,       &[0x0f, 0x44], 0, Enc::RM),
        insn("cmp",     RM32,  R32,     None,    0,       &[0x39],       0, Enc::MR),
        insn("cmp",     RM64,  R64,     None,    REX_W,   &[0x39],       0, Enc::MR),
        insn("cmp",     R8,    RM8,     None,    0,       &[0x3a],       0, Enc::RM),
        insn("cmp",     RM8,   Imm8,    None,    0,       &[0x80],       7, Enc::MI),
        insn("cmp",     RM64,  Imm8sx,  None,    REX_W,   &[0x83],       7, Enc::MI),
        insn("cmp",     RM32,  Imm8sx,  None,    0,       &[0x83],       7, Enc::MI),
        insn("cmp",     RM32,  Imm32,   None,    0,       &[0x81],       7, Enc::MI),
        insn("dec",     RM32,  None,    None,    0,       &[0xff],       1, Enc::M),
        insn("dec",     RM64,  None,    None,    REX_W,   &[0xff],       1, Enc::M),
        insn("div",     RM32,  None,    None,    0,       &[0xf7],       6, Enc::M),
        insn("imul",    R32,   RM32,    None,    0,       &[0x0f, 0xaf], 0, Enc::RM),
        insn("imul",    R32,   RM32,    Imm8sx,  0,       &[0x6b],       0, Enc::RMI),
        insn("imul",    R32,   RM32,    Imm32,   0,       &[0x69],       0, Enc::RMI),
        insn("imul",    R64,   RM64,    Imm32sx, REX_W,   &[0x69],       0, Enc::RMI),
        insn("inc",     RM32,  None,    None,    0,       &[0xff],       0, Enc::M),
        insn("inc",     RM64,  None,    None,    REX_W,   &[0xff],       0, Enc::M),
        insn("int3",    None,  None,    None,    0,       &[0xcc],       0, Enc::ZO),
        insn("ja",      Rel32, None,    None,    0,       &[0x0f, 0x87], 0, Enc::D),
        insn("jae",     Rel32, None,    None,    0,       &[0x0f, 0x83], 0, Enc::D),
        insn("je",      Rel32, None,    None,    0,       &[0x0f, 0x84], 0, Enc::D),
        insn("jg",      Rel32, None,    None,    0,       &[0x0f, 0x8f], 0, Enc::D),
        insn("jge",     Rel32, None,    None,    0,       &[0x0f, 0x8d], 0, Enc::D),
        insn("jl",      Rel32, None,    None,    0,       &[0x0f, 0x8c], 0, Enc::D),
        insn("jle",     Rel32, None,    None,    0,       &[0x0f, 0x8e], 0, Enc::D),
        insn("jmp",     Rel32, None,    None,    0,       &[0xe9],       0, Enc::D),
        insn("jne",     Rel32, None,    None,    0,       &[0x0f, 0x85], 0, Enc::D),
        insn("jnz",     Rel32, None,    None,    0,       &[0x0f, 0x85], 0, Enc::D),
        insn("jz",      Rel32, None,    None,    0,       &[0x0f, 0x84], 0, Enc::D),
        insn("lea",     R64,   M,       None,    REX_W,   &[0x8d],       0, Enc::RM),
        insn("mov",     RM8,   R8,      None,    0,       &[0x88],       0, Enc::MR),
        insn("mov",     RM32,  R32,     None,    0,       &[0x89],       0, Enc::MR),
        insn("mov",     RM64,  R64,     None,    REX_W,   &[0x89],       0, Enc::MR),
        insn("mov",     R8,    RM8,     None,    0,       &[0x8a],       0, Enc::RM),
        insn("mov",     R32,   RM32,    None,    0,       &[0x8b],       0, Enc::RM),
        insn("mov",     R32,   Imm32,   None,    0,       &[0xb8],       0, Enc::OI),
        insn("mov",     R64,   M64,     None,    REX_W,   &[0x8b],       0, Enc::RM),
        insn("mov",     RM8,   Imm8,    None,    0,       &[0xc6],       0, Enc::MI),
        insn("mov",     RM32,  Imm32,   None,    0,       &[0xc7],       0, Enc::MI),
        insn("mov",     RM64,  Imm32sx, None,    REX_W,   &[0xc7],       0, Enc::MI),
        insn("movzx",   R32,   RM8,     None,    0,       &[0x0f, 0xb6], 0, Enc::RM),
        insn("movzx",   R32,   RM16,    None,    0,       &[0x0f, 0xb7], 0, Enc::RM),
        insn("neg",     RM32,  None,    None,    0,       &[0xf7],       3, Enc::M),
        insn("or",      RM8,   Imm8,    None,    0,       &[0x80],       1, Enc::MI),
        insn("or",      RM32,  Imm8sx,  None,    0,       &[0x83],       1, Enc::MI),
        insn("or",      RM32,  R32,     None,    0,       &[0x09],       0, Enc::MR),
        insn("pop",     R64,   None,    None,    0,       &[0x58],       0, Enc::O),
        insn("push",    R64,   None,    None,    0,       &[0x50],       0, Enc::O),
        insn("rdrand",  R32,   None,    None,    0,       &[0x0f, 0xc7], 6, Enc::M),
        insn("ret",     None,  None,    None,    0,       &[0xc3],       0, Enc::ZO),
        insn("xor",     RM32,  R32,     None,    0,       &[0x31],       0, Enc::MR),
        insn("setz",    RM8,   None,    None,    0,       &[0x0f, 0x94], 0, Enc::M),
        insn("shl",     RM32,  Imm8,    None,    0,       &[0xc1],       4, Enc::MI),
        insn("shr",     RM32,  One,     None,    0,       &[0xd1],       5, Enc::M1),
        insn("shr",     RM32,  Imm8,    None,    0,       &[0xc1],       5, Enc::MI),
        insn("sub",     RM32,  R32,     None,    0,       &[0x29],       0, Enc::MR),
        insn("sub",     RM64,  R64,     None,    REX_W,   &[0x29],       0, Enc::MR),
        insn("sub",     RM64,  Imm32sx, None,    REX_W,   &[0x81],       5, Enc::MI),
        insn("syscall", None,  None,    None,    0,       &[0x0f, 0x05], 0, Enc::ZO),
        insn("test",    RM8,   Imm8,    None,    0,       &[0xf6],       0, Enc::MI),
        insn("test",    RM32,  R32,     None,    0,       &[0x85],       0, Enc::MR),
        insn("test",    Eax,   Imm32,   None,    0,       &[0xa9],       0, Enc::I2),
        insn("xchg",    RM32,  R32,     None,    0,       &[0x87],       0, Enc::MR),
        insn("xchg",    RM64,  R64,     None,    REX_W,   &[0x87],       0, Enc::MR),
    ];

    for insn in &INSNS {
        if insn.mnemonic == mnemonic
            && operand_compatible(arg1, insn.op1, insn.op2, insn.op3)
            && operand_compatible(arg2, insn.op2, insn.op1, insn.op3)
            && operand_compatible(arg3, insn.op3, insn.op1, insn.op2)
        {
            return Some(insn);
        }
    }

    return Option::None;

    const fn insn(
        mnemonic: &'static str,
        op1: Op,
        op2: Op,
        op3: Op,
        rex: u8,
        opcodes: &'static [u8],
        reg: u8,
        encoding: Enc,
    ) -> Insn {
        Insn {
            mnemonic,
            op1,
            op2,
            op3,
            rex,
            opcodes,
            reg,
            encoding,
        }
    }

    fn operand_compatible(arg: &Arg, op: Op, other1: Op, other2: Op) -> bool {
        match op {
            None => matches!(arg, Arg::None),
            Eax => matches!(arg, Arg::Eax),
            R8 => matches!(arg, Arg::Reg8(_)),
            _R16 => matches!(arg, Arg::_Reg16(_)),
            R32 => matches!(arg, Arg::Eax | Arg::Reg32(_)),
            R64 => matches!(arg, Arg::Reg64(_)),
            RM8 => {
                if matches!(other1, Op::R8 | Op::None) && matches!(other2, Op::R8 | Op::None) {
                    matches!(arg, Arg::Reg8(_) | Arg::Mem8(_) | Arg::Mem(_))
                } else {
                    matches!(arg, Arg::Reg8(_) | Arg::Mem8(_))
                }
            }
            RM16 => {
                if matches!(other1, Op::_R16 | Op::None) && matches!(other2, Op::_R16 | Op::None) {
                    matches!(arg, Arg::_Reg16(_) | Arg::Mem16(_) | Arg::Mem(_))
                } else {
                    matches!(arg, Arg::_Reg16(_) | Arg::Mem16(_))
                }
            }
            RM32 => {
                if matches!(other1, Op::Eax | Op::R32 | Op::None)
                    && matches!(other2, Op::Eax | Op::R32 | Op::None)
                {
                    matches!(arg, Arg::Eax | Arg::Reg32(_) | Arg::Mem32(_) | Arg::Mem(_))
                } else {
                    matches!(arg, Arg::Eax | Arg::Reg32(_) | Arg::Mem32(_))
                }
            }
            RM64 => {
                if matches!(other1, Op::R64 | Op::None) && matches!(other2, Op::R64 | Op::None) {
                    matches!(arg, Arg::Reg64(_) | Arg::Mem64(_) | Arg::Mem(_))
                } else {
                    matches!(arg, Arg::Reg64(_) | Arg::Mem64(_))
                }
            }
            One => {
                if let &Arg::Imm(imm) = arg {
                    imm == 1
                } else {
                    false
                }
            }
            Imm8 => {
                if let &Arg::Imm(imm) = arg {
                    is_valid8(imm)
                } else {
                    false
                }
            }
            Imm8sx => {
                if let &Arg::Imm(imm) = arg {
                    imm >= i8::min_value() as i64 && imm <= i8::max_value() as i64
                } else {
                    false
                }
            }
            Imm32 => {
                if let &Arg::Imm(imm) = arg {
                    is_valid32(imm)
                } else {
                    false
                }
            }
            Imm32sx => {
                if let &Arg::Imm(imm) = arg {
                    imm >= i32::min_value() as i64 && imm <= i32::max_value() as i64
                } else {
                    false
                }
            }
            M => matches!(arg, Arg::Mem(_)),
            M64 => matches!(arg, Arg::Mem(_) | Arg::Mem64(_)),
            Rel32 => matches!(arg, Arg::Rel32(_, _) | Arg::Label(_) | Arg::AnonLabel(_, _)),
        }
    }
}

fn asm_instruction<'a>(
    (mnemonic, at): (&str, usize),
    cur: &mut Cursor,
    input: &'a str,
    ctx: &mut Context<'a>,
    verbose: bool,
) -> Result<(), Error> {
    let mut arg1 = Arg::None;
    let mut arg2 = Arg::None;
    let mut arg3 = Arg::None;

    if let Some(arg) = match_argument(cur, input, ctx.curr_label, &ctx.anon_labels)? {
        arg1 = arg;
        skip_whitespace(cur);

        if match_char(',', cur) {
            skip_whitespace(cur);
            if let Some(arg) = match_argument(cur, input, ctx.curr_label, &ctx.anon_labels)? {
                arg2 = arg;
            } else {
                return error_at(cur.off, "expected operand after ','");
            }

            if match_char(',', cur) {
                skip_whitespace(cur);
                if let Some(arg) = match_argument(cur, input, ctx.curr_label, &ctx.anon_labels)? {
                    arg3 = arg;
                } else {
                    return error_at(cur.off, "expected operand after ','");
                }
            }
        }
    }

    if let Some(insn) = choose_insn(mnemonic, &arg1, &arg2, &arg3) {
        let off = ctx.out.data.len();

        emit_insn(insn, &arg1, &arg2, &arg3, ctx)?;

        if verbose {
            let mut s = String::new();

            s.push_str(&format!("{off:08x} ", off = off - 120));

            for &byte in &ctx.out.data[off..] {
                s.push_str(&format!("{byte:02x} "));
            }

            let mut col = 45;
            if s.len() < col {
                for _ in 0..col - s.len() {
                    s.push(' ');
                }
            }

            s.push_str(&format!("{mnemonic}"));

            col += 16;
            if s.len() < col {
                for _ in 0..col - s.len() {
                    s.push(' ');
                }
            }

            for (idx, arg) in [arg1, arg2, arg3].iter().enumerate() {
                if let Arg::None = arg {
                    break;
                }
                if idx > 0 && idx <= 2 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{arg}"));
            }

            println!("{}", s);
        }
    } else {
        return error_at(at, "unknown instruction");
    }

    Ok(())
}

fn is_valid8(val: i64) -> bool {
    if val > 0 {
        val <= u8::max_value() as i64
    } else {
        val >= i8::min_value() as i64
    }
}

fn is_valid16(val: i64) -> bool {
    if val > 0 {
        val <= u16::max_value() as i64
    } else {
        val >= i16::min_value() as i64
    }
}

fn is_valid32(val: i64) -> bool {
    if val > 0 {
        val <= u32::max_value() as i64
    } else {
        val >= i32::min_value() as i64
    }
}

fn asm_directive(name: &str, at: usize, cur: &mut Cursor, out: &mut Vec<u8>) -> Result<(), Error> {
    match name {
        "i8" => {
            loop {
                if match_char('"', cur) {
                    loop {
                        if match_char('"', cur) {
                            break;
                        } else if match_char('\n', cur) {
                            return error_at(cur.off - 1, "unterminated string literal");
                        } else if cur.iter.as_str().len() == 0 {
                            return error_at(cur.off + 1, "unterminated string literal");
                        } else {
                            out.push(cur.char as u8); // TODO: enable utf8 chars longer than
                                                      // one byte
                            advance(cur);
                        }
                    }
                } else if let Some(mut int) = match_expression(cur)? {
                    loop {
                        if is_valid8(int) {
                            out.push(int as u8);
                        } else {
                            return error_at(cur.off, "value doesn't fit in 16 bits");
                        }
                        skip_whitespace(cur);
                        if let Some(x) = match_expression(cur)? {
                            int = x;
                        } else {
                            break;
                        }
                    }
                } else {
                    return error_at(cur.off, "expected integer literal or string");
                }

                skip_whitespace(cur);

                if at_stmt_terminator(cur) {
                    break;
                }
            }
        }
        "i16" | "i32" | "i64" => {
            if let Some(mut int) = match_expression(cur)? {
                loop {
                    match name {
                        "i16" => {
                            if is_valid16(int) {
                                emit16(int as u16, out)
                            } else {
                                return error_at(cur.off, "value doesn't fit in 16 bits");
                            }
                        }
                        "i32" => {
                            if is_valid32(int) {
                                emit32(int as u32, out)
                            } else {
                                return error_at(cur.off, "value doesn't fit in 32 bits");
                            }
                        }
                        "i64" => {
                            // should be valid as long as the value was checked for overflow
                            emit64(int as u64, out)
                        }
                        _ => unreachable!(),
                    }
                    skip_whitespace(cur);
                    if let Some(x) = match_expression(cur)? {
                        int = x;
                    } else {
                        break;
                    }
                }
            } else {
                return error_at(cur.off, "expected expression");
            }
        }
        "res" => {
            if let Some(int) = match_expression(cur)? {
                let len = out.len() + int as usize;
                out.resize(len, 0);
            } else {
                return error_at(cur.off, "expected number of bytes to reserve");
            }
        }
        _ => return error_at(at, "unknown directive"),
    }
    Ok(())
}

fn error_at<T>(at: usize, msg: &'static str) -> Result<T, Error> {
    Err(Error { at, msg })
}

fn advance(cur: &mut Cursor) {
    (cur.off, cur.char) = cur.iter.next().unwrap_or((cur.iter.offset(), '\0'))
}

// TODO: remove if unused
fn _peek(cur: &mut Cursor) -> char {
    cur.iter.clone().next().unwrap_or((0, '\0')).1
}

fn skip_whitespace(cur: &mut Cursor) {
    while cur.char == ' ' || cur.char == '\t' {
        advance(cur);
    }
}

fn at_stmt_terminator(cur: &Cursor) -> bool {
    cur.char == '\n' || cur.char == ';' || cur.iter.as_str().len() == 0
}

fn match_char(char: char, cur: &mut Cursor) -> bool {
    if cur.char == char {
        advance(cur);
        true
    } else {
        false
    }
}

fn match_identifier<'a>(cur: &mut Cursor, input: &'a str) -> Option<(&'a str, usize)> {
    if cur.char >= 'a' && cur.char <= 'z' {
        let beg = cur.off;
        advance(cur);
        while (cur.char >= 'a' && cur.char <= 'z')
            || (cur.char >= '0' && cur.char <= '9')
            || cur.char == '_'
        {
            advance(cur);
        }
        Some((&input[beg..cur.off], beg))
    } else {
        None
    }
}

fn match_integer(cur: &mut Cursor) -> Result<Option<(i64, usize)>, Error> {
    let at = cur.off;

    // TODO: handle overflows

    if cur.char == '0' {
        advance(cur);

        if cur.char == 'x' {
            advance(cur);

            fn parse_hexdigit(char: char) -> Option<u64> {
                if char >= '0' && char <= '9' {
                    Some(char as u64 - '0' as u64)
                } else if char >= 'a' && char <= 'f' {
                    Some(char as u64 - 'a' as u64 + 10)
                } else {
                    None
                }
            }

            if let Some(n) = parse_hexdigit(cur.char) {
                let mut int = n;
                advance(cur);
                while let Some(n) = parse_hexdigit(cur.char) {
                    int = int * 16 + n;
                    advance(cur);
                }
                Ok(Some((int as i64, at)))
            } else {
                error_at(cur.off, "expected hexadecimal digit")
            }
        } else {
            let mut int = 0;
            while cur.char >= '0' && cur.char <= '9' {
                int = int * 10 + (cur.char as u64 - '0' as u64);
                advance(cur);
            }
            Ok(Some((int as i64, at)))
        }
    } else if cur.char >= '1' && cur.char <= '9' {
        let mut int = cur.char as u64 - '0' as u64;
        advance(cur);
        while cur.char >= '0' && cur.char <= '9' {
            int = int * 10 + (cur.char as u64 - '0' as u64);
            advance(cur);
        }
        Ok(Some((int as i64, at)))
    } else {
        Ok(None)
    }
}

fn parse_label<'a>(
    (global, at): (&'a str, usize),
    cur: &mut Cursor,
    input: &'a str,
) -> Result<(LabelName<'a>, usize), Error> {
    if match_char('.', cur) {
        if let Some((local, _)) = match_identifier(cur, input) {
            Ok((LabelName { global, local }, at))
        } else {
            return error_at(cur.off, "expected local label name");
        }
    } else {
        Ok((LabelName { global, local: "" }, at))
    }
}

fn match_primary(cur: &mut Cursor) -> Result<Option<i64>, Error> {
    if let Some((int, _)) = match_integer(cur)? {
        return Ok(Some(int));
    } else if match_char('-', cur) {
        if let Some(int) = match_primary(cur)? {
            return Ok(Some(-int));
        }
    } else if match_char('(', cur) {
        if let Some(int) = match_expression(cur)? {
            if match_char(')', cur) {
                return Ok(Some(int));
            }
        }
    } else {
        return Ok(None);
    }

    error_at(cur.off, "unexpected end of expression")
}

fn match_operator(char: char) -> Option<i32> {
    match char {
        '+' => Some(0),
        '-' => Some(0),
        '*' => Some(1),
        '/' => Some(1),
        _ => None,
    }
}

fn parse_expression(mut lhs: i64, mut op: char, prec: i32, cur: &mut Cursor) -> Result<i64, Error> {
    loop {
        let mut rhs = if let Some(int) = match_primary(cur)? {
            int
        } else {
            return error_at(cur.off, "unexpected end of expression");
        };

        skip_whitespace(cur);

        if let Some(prec2) = match_operator(cur.char) {
            if prec2 > prec {
                let op = cur.char;
                advance(cur);
                skip_whitespace(cur);
                rhs = parse_expression(rhs, op, prec2, cur)?;
            }
        }

        match op {
            '+' => lhs += rhs,
            '-' => lhs -= rhs,
            '*' => lhs *= rhs,
            '/' => lhs /= rhs,
            _ => unreachable!(),
        };

        if let Some(prec2) = match_operator(cur.char) {
            if prec2 == prec {
                op = cur.char;
                advance(cur);
                skip_whitespace(cur);
                continue;
            }
        }

        break;
    }

    Ok(lhs)
}

fn match_expression(cur: &mut Cursor) -> Result<Option<i64>, Error> {
    let mut lhs = if let Some(int) = match_primary(cur)? {
        int
    } else {
        return Ok(None);
    };

    skip_whitespace(cur);

    while let Some(prec) = match_operator(cur.char) {
        let op = cur.char;
        advance(cur);
        skip_whitespace(cur);
        lhs = parse_expression(lhs, op, prec, cur)?;
    }

    Ok(Some(lhs))
}

fn match_argument<'a>(
    cur: &mut Cursor,
    input: &'a str,
    label: &'a str,
    anon_labels: &[i64],
) -> Result<Option<Arg<'a>>, Error> {
    if let Some((ident, at)) = match_identifier(cur, input) {
        match ident {
            "m8" | "m16" | "m32" | "m64" => {
                skip_whitespace(cur);
                if !match_char('[', cur) {
                    return error_at(cur.off, "expected address after operand size");
                } else {
                    let addr = expect_address(cur, input)?;
                    let arg = match ident {
                        "m8" => Arg::Mem8(addr),
                        "m16" => Arg::Mem16(addr),
                        "m32" => Arg::Mem32(addr),
                        "m64" => Arg::Mem64(addr),
                        _ => unreachable!(),
                    };
                    Ok(Some(arg))
                }
            }
            _ => {
                let arg = if let Some(reg) = parse_register(ident) {
                    reg
                } else {
                    Arg::Label(parse_label((ident, at), cur, input)?)
                };
                Ok(Some(arg))
            }
        }
    } else if match_char('.', cur) {
        let at = cur.off - 1;
        if let Some((ident, _)) = match_identifier(cur, input) {
            let label = LabelName {
                global: label,
                local: ident,
            };
            Ok(Some(Arg::Label((label, at))))
        } else {
            error_at(cur.off, "expected label name")
        }
    } else if let Some(imm) = match_expression(cur)? {
        Ok(Some(Arg::Imm(imm)))
    } else if match_char('[', cur) {
        Ok(Some(Arg::Mem(expect_address(cur, input)?)))
    } else if match_char('<', cur) {
        let at = cur.off - 1;
        let mut delta = 1;

        while match_char('<', cur) {
            delta += 1;
        }

        if delta <= anon_labels.len() {
            let off = unsafe { *anon_labels.get_unchecked(anon_labels.len() - delta) };
            Ok(Some(Arg::Rel32(off, at)))
        } else {
            error_at(at, "trying to jump to a non-existent anonymous label")
        }
    } else if match_char('>', cur) {
        let at = cur.off - 1;
        let mut delta = 0;

        while match_char('>', cur) {
            delta += 1;
        }

        Ok(Some(Arg::AnonLabel(anon_labels.len() + delta, at)))
    } else {
        Ok(None)
    }
}

fn expect_address<'a>(cur: &mut Cursor, input: &'a str) -> Result<Addr<'a>, Error> {
    let mut label = None;
    let mut base = None;
    let mut index = None;
    let mut scale: u32 = 1;
    let mut disp: i64 = 0;

    loop {
        skip_whitespace(cur);

        if let Some((ident, at)) = match_identifier(cur, &input) {
            if let Some(reg) = parse_register(ident) {
                let reg = if let Arg::Reg64(reg) = reg {
                    reg
                } else {
                    return error_at(at, "expected 64-bit register");
                };

                skip_whitespace(cur);

                if match_char('*', cur) {
                    skip_whitespace(cur);
                    if let Some((int, _)) = match_integer(cur)? {
                        if int != 1 && int != 2 && int != 4 && int != 8 {
                            return error_at(cur.off, "scale must be 1, 2, 4, or 8");
                        }

                        if index.is_some() {
                            return error_at(at, "index already set");
                        }

                        if reg == 4 {
                            return error_at(at, "register can't be used as index");
                        }
                        index = Some(reg);
                        scale = int as u32;
                    } else {
                        return error_at(cur.off, "expected scale");
                    }
                } else {
                    if base.is_some() {
                        if index.is_some() {
                            return error_at(at, "base and index already set");
                        } else {
                            if reg == 4 {
                                // swap rsp with the already set base register because rsp can't be
                                // used as index
                                index = base;
                                base = Some(reg);
                            } else {
                                index = Some(reg);
                            }
                        }
                    } else {
                        base = Some(reg);
                    }
                }
            } else {
                label = Some(parse_label((ident, at), cur, input)?);
            }
        } else if let Some(int) = match_expression(cur)? {
            disp += int as i64; // TODO: check for overflow
        } else {
            return error_at(cur.off, "expected address component");
        }

        skip_whitespace(cur);

        if match_char('-', cur) {
            skip_whitespace(cur);
            if let Some(int) = match_expression(cur)? {
                disp -= int as i64; // TODO: check for overflow
            } else {
                return error_at(cur.off, "expected expression");
            }
        }

        if !match_char('+', cur) {
            break;
        }
    }

    if !match_char(']', cur) {
        return error_at(cur.off, "expected closing ']'");
    }

    Ok(Addr {
        base,
        index,
        scale,
        disp,
        label,
    })
}

fn parse_register(name: &str) -> Option<Arg> {
    if let Some((idx, _)) = REGS32.iter().enumerate().find(|(_, &reg)| reg == name) {
        if idx == 0 {
            Some(Arg::Eax)
        } else {
            Some(Arg::Reg32(idx as u32))
        }
    } else if let Some((idx, _)) = REGS64.iter().enumerate().find(|(_, &reg)| reg == name) {
        Some(Arg::Reg64(idx as u32))
    } else if let Some((idx, _)) = REGS8.iter().enumerate().find(|(_, &reg)| reg == name) {
        Some(Arg::Reg8(idx as u32))
    } else {
        None
    }
}

fn emit_insn<'a>(
    insn: &Insn,
    arg1: &Arg<'a>,
    arg2: &Arg<'a>,
    arg3: &Arg<'a>,
    ctx: &mut Context<'a>,
) -> Result<(), Error> {
    ctx.out.len = insn.opcodes.len();
    ctx.out.rex = insn.rex as u32;

    unsafe {
        ctx.out
            .buf
            .as_mut_ptr()
            .copy_from_nonoverlapping(insn.opcodes.as_ptr(), insn.opcodes.len())
    };

    match insn.encoding {
        Enc::ZO => (),
        Enc::O | Enc::OI => {
            let mut reg = arg1.reg();
            if reg >= 8 {
                reg -= 8;
                ctx.out.rex |= REX_B as u32;
            }

            ctx.out.buf[ctx.out.len - 1] += reg as u8;
            if insn.encoding == Enc::OI {
                write_imm(arg2.imm(), insn.op2, &mut ctx.out);
            }
        }
        Enc::RM => {
            emit_modrm(arg1.reg(), arg2, ctx)?;
        }
        Enc::MR => {
            emit_modrm(arg2.reg(), arg1, ctx)?;
        }
        Enc::MI => {
            emit_modrm(insn.reg as u32, arg1, ctx)?;
            write_imm(arg2.imm(), insn.op2, &mut ctx.out);
        }
        Enc::M1 => emit_modrm(insn.reg as u32, arg1, ctx)?,
        Enc::D => match *arg1 {
            Arg::Label((label, at)) => emit_rel32_from_label((label, at), 0, ctx)?,
            Arg::Rel32(off, at) => emit_rel32(off, at, &mut ctx.out)?,
            Arg::AnonLabel(idx, at) => {
                if let Some(&off) = ctx.anon_labels.get(idx) {
                    emit_rel32(off, at, &mut ctx.out)?;
                } else {
                    ctx.out.len += 4; // make space for the immediate value
                    ctx.anon_patches.push(AnonPatch {
                        idx,
                        at,
                        off: ctx.out.pos(),
                    });
                }
            }
            _ => unreachable!(),
        },
        Enc::M => emit_modrm(insn.reg as u32, arg1, ctx)?,
        Enc::I2 => write_imm(arg2.imm(), insn.op2, &mut ctx.out),
        Enc::RMI => {
            emit_modrm(arg1.reg(), arg2, ctx)?;
            write_imm(arg3.imm(), insn.op3, &mut ctx.out);
        }
    }

    if ctx.out.rex != 0 {
        ctx.out.data.push(ctx.out.rex as u8);
    }

    ctx.out.data.extend_from_slice(&ctx.out.buf[..ctx.out.len]);

    Ok(())
}

fn emit_rel32(off: i64, at: usize, out: &mut Output) -> Result<(), Error> {
    let rel = out.pos() + 4 - off;
    if rel <= i32::max_value() as i64 {
        write32((-rel) as u32, out);
        Ok(())
    } else {
        error_at(at, "distance to target is too large")
    }
}

fn emit_rel32_from_label<'a>(
    (label, at): (LabelName<'a>, usize),
    disp: i64,
    ctx: &mut Context<'a>,
) -> Result<(), Error> {
    if let Some(off) = resolve_label(&label, &ctx.labels) {
        emit_rel32(off, at, &mut ctx.out)?;
    } else {
        ctx.out.len += 4; // make space for the immediate value
        ctx.patches.push(Patch {
            off: ctx.out.pos(),
            label,
            at,
            disp,
        });
    }

    Ok(())
}

fn write8(val: u8, out: &mut Output) {
    assert!(out.len + 1 <= out.buf.len());
    unsafe { out.buf.as_mut_ptr().add(out.len).write(val) };
    out.len += 1;
}

fn write32(val: u32, out: &mut Output) {
    assert!(out.len + 4 <= out.buf.len());
    unsafe { (out.buf.as_mut_ptr().add(out.len) as *mut u32).write(val) };
    out.len += 4;
}

fn write_imm(imm: i64, op: Op, out: &mut Output) {
    match op {
        Op::Imm8 | Op::Imm8sx => write8(imm as u8, out),
        Op::Imm32 | Op::Imm32sx => write32(imm as u32, out),
        _ => panic!(),
    }
}

fn emit16(val: u16, out: &mut Vec<u8>) {
    let val: [u8; 2] = unsafe { transmute(val) };
    out.extend(val);
}

fn emit32(val: u32, out: &mut Vec<u8>) {
    let val: [u8; 4] = unsafe { transmute(val) };
    out.extend(val);
}

#[allow(dead_code)]
fn emit64(val: u64, out: &mut Vec<u8>) {
    let val: [u8; 8] = unsafe { transmute(val) };
    out.extend(val);
}

fn emit_modrm<'a>(mut reg: u32, rm: &Arg<'a>, ctx: &mut Context<'a>) -> Result<(), Error> {
    let out = &mut ctx.out;

    if reg >= 8 {
        reg -= 8;
        out.rex |= REX_R as u32;
    }

    match rm {
        Arg::Mem(addr)
        | Arg::Mem8(addr)
        | Arg::Mem16(addr)
        | Arg::Mem32(addr)
        | Arg::Mem64(addr) => {
            if let Some((label, at)) = addr.label {
                // TODO: this should be caught by the parser
                assert!(addr.base.is_none());
                assert!(addr.index.is_none());

                write_modrm(0b00, reg, 0b101, out);
                emit_rel32_from_label((label, at), addr.disp, ctx)?;
            } else {
                assert!(addr.label.is_none()); // TODO: this should be caught by the parser

                let index = match addr.index {
                    Some(mut index) => {
                        if index >= 8 {
                            index -= 8;
                            out.rex |= REX_X as u32;
                        }
                        Some(index)
                    }
                    None => None,
                };

                if let Some(mut base) = addr.base {
                    if base >= 8 {
                        base -= 8;
                        out.rex |= REX_B as u32;
                    }

                    let r#mod = match addr.disp {
                        disp if disp == 0 => 0b00,
                        disp if disp <= i8::max_value() as i64 => 0b01,
                        disp if disp <= i32::max_value() as i64 => 0b10,
                        _ => unreachable!(),
                    };

                    if base == 4 {
                        write_modrm(r#mod, reg, 0b100, out);
                        if let Some(index) = index {
                            write_sib(base, index, addr.scale, out);
                        } else {
                            write_sib(base, 0b100, 1, out);
                        }
                    } else if base == 5 && addr.disp == 0 {
                        if let Some(index) = index {
                            write_modrm(0b01, reg, 0b100, out);
                            write_sib(base, index, addr.scale, out);
                        } else {
                            write_modrm(0b01, reg, base, out);
                        }
                        write8(0, out);
                    } else {
                        if let Some(index) = index {
                            write_modrm(r#mod, reg, 0b100, out);
                            write_sib(base, index, addr.scale, out);
                        } else {
                            write_modrm(r#mod, reg, base, out);
                        }
                    }

                    match addr.disp {
                        disp if disp == 0 => (),
                        disp if disp <= i8::max_value() as i64 => write8(addr.disp as u8, out),
                        disp if disp <= i32::max_value() as i64 => write32(addr.disp as u32, out),
                        _ => unreachable!(),
                    };
                } else if let Some(index) = index {
                    write_modrm(0b00, reg, 0b100, out);
                    write_sib(0b101, index, addr.scale, out);
                    if addr.base.is_none() {
                        write32(addr.disp as u32, out);
                    }
                } else {
                    // no base or index, just displacement
                    // TODO: this should be caught by the parser
                    unreachable!();
                }
            }
        }
        Arg::Eax => write_modrm(0b11, reg, 0, out),
        &Arg::Reg8(mut rm) | &Arg::_Reg16(mut rm) | &Arg::Reg32(mut rm) | &Arg::Reg64(mut rm) => {
            if rm >= 8 {
                rm -= 8;
                out.rex |= REX_B as u32;
            }
            write_modrm(0b11, reg, rm, out)
        }
        _ => unreachable!(),
    }

    Ok(())
}

fn write_modrm(r#mod: u32, reg: u32, rm: u32, out: &mut Output) {
    write8((r#mod << 6 | reg << 3 | rm) as u8, out);
}

fn write_sib(base: u32, index: u32, scale: u32, out: &mut Output) {
    let ss = match scale {
        1 => 0b00,
        2 => 0b01,
        4 => 0b10,
        8 => 0b11,
        _ => unreachable!(),
    };
    write8((ss << 6 | index << 3 | base) as u8, out);
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Runner {
        failed: bool,
    }

    impl Runner {
        fn new() -> Self {
            Runner { failed: false }
        }

        fn fail(&mut self, input: &str, at: usize) {
            let mut actual = Vec::new();

            match assemble(input, &mut actual, false) {
                Ok(_) => {
                    eprintln!("{input}");
                    eprintln!("  expected failure, but succeded");
                    eprint!("\n");
                    self.failed = true;
                }
                Err(err) => {
                    if err.at != at {
                        eprintln!("{input}");
                        eprintln!("expected error at {}, got error at {}", at, err.at);
                        eprint!("\n");
                        self.failed = true;
                    }
                }
            }
        }

        fn pass(&mut self, input: &str, expected: &[u8]) {
            let mut actual = Vec::new();

            if let Err(err) = assemble(input, &mut actual, false) {
                let s = fmt_error(err, input, "_");
                eprintln!("{}", s.as_str());
                eprint!("\n");
                self.failed = true;
                return;
            }

            if &actual != expected {
                let count = expected.len().max(actual.len());

                eprintln!("{input}");
                eprintln!("  EXP  ACT ??");

                for idx in 0..count {
                    let exp = expected.get(idx);
                    let act = actual.get(idx);

                    match (exp, act) {
                        (Some(exp), Some(act)) => eprintln!(
                            "  {exp:02x}   {act:02x} {hint}",
                            hint = if exp != act { "!!!" } else { "" }
                        ),
                        (Some(exp), None) => eprintln!("  {exp:02x}   -- !!!"),
                        (None, Some(act)) => eprintln!("  --   {act:02x} !!!"),
                        (None, None) => (),
                    };
                }
                eprint!("\n");
                self.failed = true;
            }
        }
    }

    #[test]
    #[rustfmt::skip]
    fn test_modrm() {
        let mut r = Runner::new();

        // no displacement
        r.pass("mov eax, [rbx]", &[0x8b, 0b00_000_011]);

        // disp8
        r.pass("mov eax, [rbx+127]", &[0x8b, 0b01_000_011, 127]);

        // disp32
        r.pass(
            "mov eax, [rbx+1234]",
            &[0x8b, 0b10_000_011, 0xd2, 0x04, 0x00, 0x00],
        );
        r.pass(
            "mov eax, [rbx+128]",
            &[0x8b, 0b10_000_011, 0x80, 0x00, 0x00, 0x00],
        );

        // base + scaled index
        r.pass(
            "mov rax, [rbx+rcx*4]",
            &[0b0100_1000, 0x8b, 0b00_000_100, 0b10_001_011],
        );
        r.pass(
            "mov rax, [rcx*2+rbx+1234]",
            &[
                0b0100_1000,
                0x8b,
                0b10_000_100,
                0b01_001_011,
                0xd2,
                0x04,
                0x00,
                0x00,
            ],
        );
        r.pass(
            "mov rax, [r10+rcx*4]",
            &[0b0100_1001, 0x8b, 0b00_000_100, 0b10_001_010],
        );
        r.pass(
            "mov rax, [r11*2+rbx+1234]",
            &[
                0b0100_1010,
                0x8b,
                0b10_000_100,
                0b01_011_011,
                0xd2,
                0x04,
                0x00,
                0x00,
            ],
        );

        // scaled index but no base
        r.pass(
            "mov rax, [rdx*2]",
            &[0b0100_1000, 0x8b, 0b00_000_100, 0b01_010_101, 0x00, 0x00, 0x00, 0x00],
        );
        r.pass(
            "mov rax, [rdx*4+123]",
            &[0b0100_1000, 0x8b, 0b00_000_100, 0b10_010_101, 123, 0x00, 0x00, 0x00],
        );

        // two register operands
        r.pass("mov ecx, edx", &[0x89, 0b11_010_001]);
        r.pass("mov eax, r13d", &[0b0100_0100, 0x89, 0b11_101_000]);
        r.pass("mov r9, r10", &[0b0100_1101, 0x89, 0b11_010_001]);
        r.pass("mov r11, rbx", &[0b0100_1001, 0x89, 0b11_011_011]);
        r.pass("mov r8, r9", &[0b0100_1101, 0x89, 0b11_001_000]);

        // SIB byte required when base is rsp/r12
        r.pass("mov eax, [rsp]", &[0x8b, 0b00_000_100, 0b00_100_100]);
        r.pass(
            "mov eax, [r12]",
            &[0b0100_0001, 0x8b, 0b00_000_100, 0b00_100_100],
        );

        // rbp/r13 as base must be encoded with displacement of 0 (mod=01)
        r.pass("mov rax, [rbp]", &[0b0100_1000, 0x8b, 0b01_000_101, 0]);
        r.pass("mov rax, [r13]", &[0b0100_1001, 0x8b, 0b01_000_101, 0]);
        r.pass("mov rax, [rbp+1234]", &[0b0100_1000, 0x8b, 0b10_000_101, 0xd2, 0x04, 0x00, 0x00]);

        // rsp can't be used as index register
        r.fail("mov rax, [rbx+rsp*4]", 14);
        // in case where base and index are ambiguous, treat rsp as base
        r.pass("mov rax, [rbx+rsp]", &[0b0100_1000, 0x8b, 0b00_000_100, 0b00_011_100]);
        // r12 can be used as index register (distinguished from rsp by expanded index field)
        r.pass(
            "mov rax, [rbx+r12*4+123]",
            &[0b0100_1010, 0x8b, 0b01_000_100, 0b10_100_011, 123],
        );

        // rbp/r13 as a base with index has to be encoded with explicit displacement
        r.pass(
            "mov rax, [rbp+r10*2+123]",
            &[0b0100_1010, 0x8b, 0b01_000_100, 0b01_010_101, 123],
        );
        r.pass(
            "mov rax, [r13+r10*2+123]",
            &[0b0100_1011, 0x8b, 0b01_000_100, 0b01_010_101, 123],
        );
        r.pass(
            "mov rax, [rbp+r14*8]",
            &[0b0100_1010, 0x8b, 0b01_000_100, 0b11_110_101, 0],
        );
        r.pass(
            "mov rax, [r13+r14*8]",
            &[0b0100_1011, 0x8b, 0b01_000_100, 0b11_110_101, 0],
        );

        // labels
        r.pass(
            "lea rsi, [label]\n\
            .i64 0xabcdef0011223344\n\
            label:\n\
            .i32 0xab",
            &[0b0100_1000, 0x8d, 0b00_110_101, 0x08, 0x00, 0x00, 0x00,
              0x44, 0x33, 0x22, 0x11, 0x00, 0xef, 0xcd, 0xab,
              0xab, 0x00, 0x00, 0x00,
            ],
        );
        r.pass(
            "lea rsi, [label + 4]\n\
            .i64 0xabcdef0011223344\n\
            label:\n\
            .i16 0xab 0xcd",
            &[0b0100_1000, 0x8d, 0b00_110_101, 0x0c, 0x00, 0x00, 0x00,
              0x44, 0x33, 0x22, 0x11, 0x00, 0xef, 0xcd, 0xab,
              0xab, 0x00, 0xcd, 0x00],
        );

        assert!(!r.failed);
    }

    #[test]
    fn test_immediates() {
        let mut r = Runner::new();

        r.pass("add eax, 127", &[0x83, 0b11_000_000, 127]);
        r.pass("add eax, -128", &[0x83, 0b11_000_000, 0x80]);
        r.pass("add eax, 128", &[0x81, 0b11_000_000, 128, 0, 0, 0]);
        r.pass(
            "add eax, -129",
            &[0x81, 0b11_000_000, 0x7f, 0xff, 0xff, 0xff],
        );
        r.pass(
            "add eax, 2147483647",
            &[0x81, 0b11_000_000, 0xff, 0xff, 0xff, 0x7f],
        );
        r.pass(
            "add eax, -2147483648",
            &[0x81, 0b11_000_000, 0x00, 0x00, 0x00, 0x80],
        );
        r.pass("add eax, 2147483648", &[0x81, 0xc0, 0x00, 0x00, 0x00, 0x80]);
        r.fail("add eax, -2147483649", 0);

        r.pass(
            "add eax, 0xff",
            &[0x81, 0b11_000_000, 0xff, 0x00, 0x00, 0x00],
        );
        r.pass(
            "add eax, 0x0100",
            &[0x81, 0b11_000_000, 0x00, 0x01, 0x00, 0x00],
        );
        r.pass(
            "add eax, 0xffffffff",
            &[0x81, 0b11_000_000, 0xff, 0xff, 0xff, 0xff],
        );
        r.fail("add eax, 0x100000000", 0);

        assert!(!r.failed);
    }

    #[test]
    fn test_local_labels() {
        let mut r = Runner::new();

        r.pass(
            "proc:\n\
                xor eax, eax\n\
                mov ebx, 7\n\
            .next:\n\
                inc eax\n\
                cmp eax, 5\n\
                je .done\n\
                dec ebx\n\
                jnz .next\n\
            .done:\n\
                ret",
            &[
                0x31, 0xc0, 0xbb, 0x07, 0x00, 0x00, 0x00, 0xff, 0xc0, 0x83, 0xf8, 0x05, 0x0f, 0x84,
                0x08, 0x00, 0x00, 0x00, 0xff, 0xcb, 0x0f, 0x85, 0xed, 0xff, 0xff, 0xff, 0xc3,
            ],
        );

        r.pass(
            "proc:\n\
              mov eax, 5\n\
            .next:\n\
              dec eax\n\
              jnz .next\n\
              ret\n\
            proc2:\n\
              mov eax, 3\n\
            .next:\n\
              dec eax\n\
              jnz .next\n\
              ret",
            &[
                0xb8, 0x05, 0x00, 0x00, 0x00, 0xff, 0xc8, 0x0f, 0x85, 0xf8, 0xff, 0xff, 0xff, 0xc3,
                0xb8, 0x03, 0x00, 0x00, 0x00, 0xff, 0xc8, 0x0f, 0x85, 0xf8, 0xff, 0xff, 0xff, 0xc3,
            ],
        );

        r.pass(
            "lea rax, [data.x]\n\
             lea rbx, [data.y]\n\
             data:\n\
                .x:\n.i8 10\n\
                .y:\n.i8 20",
            &[
                0x48, 0x8d, 0x05, 0x07, 0x00, 0x00, 0x00, 0x48, 0x8d, 0x1d, 0x01, 0x00, 0x00, 0x00,
                0x0a, 0x14,
            ],
        );

        // missing label
        r.fail("jmp .done", 4);
        r.fail("lea rax, [doesnt.exist]", 10);

        // label declared twice
        r.fail("proc:\n.done:\n.done:", 13);

        // label declared in different scope
        r.fail("proc:\njmp .done\nproc2:\n.done:", 10);

        // label declared in global scope
        r.fail(".done:", 0);

        // '.done' and 'done' are different labels
        r.fail("done:\njmp .done", 10);

        assert!(!r.failed);
    }

    #[test]
    fn test_anonymous_labels() {
        let mut r = Runner::new();

        r.pass(
            "proc:\n\
               jmp >\n\
               xor ebx, ebx\n\
             : cmp ebx, 5\n\
               je >>\n\
               mov eax, 10\n\
             : dec eax\n\
               jnz <\n\
               jmp <<\n\
             : ret",
            &[
                0xe9, 0x02, 0x00, 0x00, 0x00, 0x31, 0xdb, 0x83, 0xfb, 0x05, 0x0f, 0x84, 0x12, 0x00,
                0x00, 0x00, 0xb8, 0x0a, 0x00, 0x00, 0x00, 0xff, 0xc8, 0x0f, 0x85, 0xf8, 0xff, 0xff,
                0xff, 0xe9, 0xe5, 0xff, 0xff, 0xff, 0xc3,
            ],
        );

        r.fail("proc:\njmp >", 10);
        r.fail("proc:\njmp >>\n:", 10);
        r.fail("proc:\njmp <", 10);
        r.fail("proc:\n: jmp <<\n", 12);
        r.fail(": proc:", 0);

        assert!(!r.failed);
    }

    #[test]
    fn test_expressions() {
        let mut r = Runner::new();

        r.pass("mov eax, 123", &[0xb8, 0x7b, 0x00, 0x00, 0x00]);
        r.pass("mov eax, -100", &[0xb8, 0x9c, 0xff, 0xff, 0xff]);
        r.pass("mov eax, --100", &[0xb8, 0x64, 0x00, 0x00, 0x00]);
        r.pass("mov eax, ---100", &[0xb8, 0x9c, 0xff, 0xff, 0xff]);
        r.fail("mov eax, -", 10);

        r.pass("mov eax, 12 + 34", &[0xb8, 0x2e, 0x00, 0x00, 0x00]);
        r.fail("mov eax, 12 +", 13);
        r.pass("mov eax, 12 - 34", &[0xb8, 0xea, 0xff, 0xff, 0xff]);
        r.fail("mov eax, 12 -", 13);
        r.pass("mov eax, 12 + -34", &[0xb8, 0xea, 0xff, 0xff, 0xff]);
        r.pass("mov eax, 12 - -34", &[0xb8, 0x2e, 0x00, 0x00, 0x00]);

        r.pass("mov eax, 2 * 3 + 4", &[0xb8, 0x0a, 0x00, 0x00, 0x00]);
        r.pass("mov eax, 2 + 3 * 4", &[0xb8, 0x0e, 0x00, 0x00, 0x00]);
        r.pass("mov eax, 2 * 3 * 4", &[0xb8, 0x18, 0x00, 0x00, 0x00]);
        r.pass("mov eax, 2 + 3 * 4 - 6", &[0xb8, 0x08, 0x00, 0x00, 0x00]);
        r.pass("mov eax, 2 + 3 * 4 * 5 - 12 / (3 * 2)", &[0xb8, 0x3c, 0x00, 0x00, 0x00]);

        r.pass("mov eax, 2 * (3 + 4)", &[0xb8, 0x0e, 0x00, 0x00, 0x00]);
        r.pass("mov eax, (2 + 3) * 4", &[0xb8, 0x14, 0x00, 0x00, 0x00]);
        r.pass("mov eax, -(2 + 3)", &[0xb8, 0xfb, 0xff, 0xff, 0xff]);
        r.pass("mov eax, --(2 + 3)", &[0xb8, 0x05, 0x00, 0x00, 0x00]);

        r.fail("mov eax, (2 + 3 * 4", 19);
        r.fail("mov eax, 2 + 3 * 4)", 18);

        r.pass(
            "mov eax, (2 + 3) * -4 / -(5 - 6)",
            &[0xb8, 0xec, 0xff, 0xff, 0xff],
        );

        assert!(!r.failed);
    }
}
