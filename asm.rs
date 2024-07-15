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
    label: Option<(&'a str, usize)>,
}

enum Arg<'a> {
    None,
    Eax,
    Reg8(u32),
    _Reg16(u32),
    Reg32(u32),
    Reg64(u32),
    One, // immediate 1
    Imm8(u64),
    Imm32(u64),
    Mem(Addr<'a>),
    Mem8(Addr<'a>),
    Mem16(Addr<'a>),
    Mem32(Addr<'a>),
    Mem64(Addr<'a>),
    Rel32((&'a str, usize)),
}

impl Arg<'_> {
    fn imm(&self) -> u64 {
        match self {
            Arg::Imm8(imm) => *imm,
            Arg::Imm32(imm) => *imm,
            Arg::One => 1,
            _ => panic!("unhandled imm <{self}>"),
        }
    }
    fn reg(&self) -> u32 {
        match self {
            Arg::Eax => 0,
            Arg::Reg8(reg) => *reg,
            Arg::Reg32(reg) => *reg,
            Arg::Reg64(reg) => *reg,
            _ => panic!(),
        }
    }
}

impl Display for Arg<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::None => write!(f, "<none>"),
            Arg::Eax => write!(f, "eax"),
            Arg::Reg8(reg) => write!(f, "r8 ({})", REGS8[*reg as usize]),
            Arg::_Reg16(_reg) => todo!(),
            Arg::Reg32(reg) => write!(f, "r32 ({})", REGS32[*reg as usize]),
            Arg::Reg64(reg) => write!(f, "r64 ({})", REGS64[*reg as usize]),
            Arg::One => write!(f, "1 (exact)"),
            Arg::Imm8(imm) => write!(f, "imm8 {imm}"),
            Arg::Imm32(imm) => write!(f, "imm32 {imm}"),
            Arg::Rel32((label, _)) => write!(f, "rel: \"{label}\""),
            Arg::Mem(addr)
            | Arg::Mem8(addr)
            | Arg::Mem16(addr)
            | Arg::Mem32(addr)
            | Arg::Mem64(addr) => write!(
                f,
                "{size} [{base} + {index}*{scale} + {disp} + {label}]",
                size = match self {
                    Arg::Mem8(_) => "m8",
                    Arg::Mem16(_) => "m16",
                    Arg::Mem32(_) => "m32",
                    Arg::Mem64(_) => "m64",
                    Arg::Mem(_) => "m",
                    _ => panic!(),
                },
                base = if let Some(base) = addr.base {
                    REGS64[base as usize]
                } else {
                    "<none>"
                },
                index = if let Some(index) = addr.index {
                    REGS64[index as usize]
                } else {
                    "<none>"
                },
                scale = addr.scale,
                disp = addr.disp,
                label = if let Some((label, _)) = addr.label {
                    label
                } else {
                    "<none>"
                }
            ),
        }
    }
}

struct Cursor<'a> {
    char: char,
    off: usize,
    iter: CharIndices<'a>,
}

#[derive(Debug)]
pub struct Error {
    msg: &'static str,
    pub at: usize,
}

struct Patch<'a> {
    off: i64,
    label: &'a str,
    at: usize,
    disp: i64,
}

#[derive(Debug)]
enum Enc {
    ZO,
    OI,
    RM,
    MR,
    MI,
    M1,
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
    Imm32,
    Rel32,
}

const REGS8: [&str; 4] = ["al", "cl", "dl", "bl"];

const REGS32: [&str; 16] = [
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d",
    "r13d", "r14d", "r15d",
];

const REGS64: [&str; 8] = ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"];

const REX_W: u8 = 0b0100_1000;
const REX_R: u8 = 0b0100_0100;
const _REX_X: u8 = 0b0100_0010;
const REX_B: u8 = 0b0100_0001;

fn main() {
    let path = std::env::args().skip(1).next().unwrap_or_else(|| {
        eprintln!("usage: asm INPUT");
        exit(1);
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

    if let Err(err) = assemble(&input, &mut out) {
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

        eprintln!("{prefix}{line}", line = &input[beg..end]);
        eprintln!(
            "{:<off$}^ {msg}",
            "",
            off = spaces + prefix.len(),
            msg = err.msg
        );
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

pub fn assemble(input: &str, out: &mut Vec<u8>) -> Result<(), Error> {
    let mut cur = Cursor {
        char: '\0',
        off: 0,
        iter: input.char_indices(),
    };

    advance(&mut cur);

    let mut labels = HashMap::<&str, usize>::new();
    let mut patches = Vec::<Patch>::new();

    loop {
        skip_whitespace(&mut cur);

        if match_char('.', &mut cur) {
            if let Some((ident, at)) = match_identifier(&mut cur, &input) {
                skip_whitespace(&mut cur);
                asm_directive(ident, at, &mut cur, out)?;
            } else {
                return error_at(cur.off, "expected directive name");
            }
        } else if let Some((ident, at)) = match_identifier(&mut cur, &input) {
            if match_char(':', &mut cur) {
                skip_whitespace(&mut cur);
                if labels.insert(ident, out.len()).is_some() {
                    return error_at(at, "label with this name already exists");
                }
            } else {
                skip_whitespace(&mut cur);

                let mnemonic = ident;
                let mut arg1 = Arg::None;
                let mut arg2 = Arg::None;
                let mut arg3 = Arg::None;

                if let Some(arg) = match_argument(&mut cur, &input)? {
                    arg1 = arg;
                    skip_whitespace(&mut cur);

                    if match_char(',', &mut cur) {
                        skip_whitespace(&mut cur);
                        if let Some(arg) = match_argument(&mut cur, &input)? {
                            arg2 = arg;
                        } else {
                            return error_at(cur.off, "expected operand after ','");
                        }

                        if match_char(',', &mut cur) {
                            skip_whitespace(&mut cur);
                            if let Some(arg) = match_argument(&mut cur, &input)? {
                                arg3 = arg;
                            } else {
                                return error_at(cur.off, "expected operand after ','");
                            }
                        }
                    }
                }

                //eprintln!("{mnemonic} {op1}, {op2} {op3}");

                if let Some(insn) = choose_insn(mnemonic, &arg1, &arg2, &arg3) {
                    //eprintln!(
                    //    "  {op1:?} {op2:?} {op3:?} | {enc:?}",
                    //    op1 = insn.op1,
                    //    op2 = insn.op2,
                    //    op3 = insn.op3,
                    //    enc = insn.encoding,
                    //);
                    emit_insn(insn, &arg1, &arg2, &arg3, out, &mut patches, &labels)?;
                } else {
                    return error_at(at, "unknown instruction");
                }
            }
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

    if cur.iter.as_str().len() != 0 {
        return error_at(cur.off, "invalid input");
    }

    for patch in patches {
        let mut disp = patch.disp;
        if let Some(&addr) = labels.get(patch.label) {
            disp += addr as i64 - patch.off as i64; // TODO: check that it fits within i32

            // TODO: this assumes that the displacement is always 32-bits, for enabling disp8
            // the offset will need to be stored with the patch
            unsafe {
                (out.as_mut_ptr().add(patch.off as usize - 4) as *mut i32).write_unaligned(disp as i32);
            };
        } else {
            return error_at(patch.at, "missing label");
        }
    }

    Ok(())
}

#[allow(dead_code)]
#[allow(unused_variables)]
fn choose_insn(mnemonic: &str, arg1: &Arg, arg2: &Arg, arg3: &Arg) -> Option<&'static Insn> {
    use self::Op::*;

    #[rustfmt::skip]
    static INSNS: [Insn; 38] = [
        insn("add",     RM32,  R32,   None, 0,     &[0x01],       0, Enc::MR),
        insn("add",     RM64,  R64,   None, REX_W, &[0x01],       0, Enc::MR),
        insn("add",     RM64,  Imm8,  None, REX_W, &[0x83],       0, Enc::MI),
        insn("add",     RM64,  Imm32, None, REX_W, &[0x81],       0, Enc::MI),
        insn("call",    Rel32, None,  None, 0,     &[0xe8],       0, Enc::D),
        insn("cmp",     RM32,  R32,   None, 0,     &[0x39],       0, Enc::MR),
        insn("cmp",     R8,    RM8,   None, 0,     &[0x3a],       0, Enc::RM),
        insn("cmp",     RM8,   Imm8,  None, 0,     &[0x80],       7, Enc::MI),
        insn("cmp",     R32,   Imm8,  None, 0,     &[0x83],       7, Enc::MI),
        insn("dec",     RM32,  None,  None, 0,     &[0xff],       1, Enc::M),
        insn("imul",    R32,   RM32,  Imm8, 0,     &[0x6b],       0, Enc::RMI),
        insn("imul",    R32,   RM32,  Imm32, 0,    &[0x69],       0, Enc::RMI),
        insn("inc",     RM32,  None,  None, 0,     &[0xff],       0, Enc::M),
        insn("int3",    None,  None,  None, 0,     &[0xcc],       0, Enc::ZO),
        insn("jae",     Rel32, None,  None, 0,     &[0x0f, 0x83], 0, Enc::D),
        insn("je",      Rel32, None,  None, 0,     &[0x0f, 0x84], 0, Enc::D),
        insn("jmp",     Rel32, None,  None, 0,     &[0xe9],       0, Enc::D),
        insn("jne",     Rel32, None,  None, 0,     &[0x0f, 0x85], 0, Enc::D),
        insn("jnz",     Rel32, None,  None, 0,     &[0x0f, 0x85], 0, Enc::D),
        insn("jz",      Rel32, None,  None, 0,     &[0x0f, 0x84], 0, Enc::D),
        insn("lea",     R64,   M,     None, REX_W, &[0x8d],       0, Enc::RM),
        insn("mov",     RM8,   R8,    None, 0,     &[0x88],       0, Enc::MR),
        insn("mov",     RM32,  R32,   None, 0,     &[0x89],       0, Enc::MR),
        insn("mov",     RM64,  R64,   None, REX_W, &[0x89],       0, Enc::MR),
        insn("mov",     R8,    RM8,   None, 0,     &[0x8a],       0, Enc::RM),
        insn("mov",     R32,   RM32,  None, 0,     &[0x8b],       0, Enc::RM),
        insn("mov",     R32,   Imm32, None, 0,     &[0xb8],       0, Enc::OI),
        insn("mov",     R64,   M64,   None, REX_W, &[0x8b],       0, Enc::RM),
        insn("mov",     RM32,  Imm32, None, 0,     &[0xc7],       0, Enc::MI),
        insn("movzx",   R32,   RM8,   None, 0,     &[0x0f, 0xb6], 0, Enc::RM),
        insn("movzx",   R32,   RM16,  None, 0,     &[0x0f, 0xb7], 0, Enc::RM),
        insn("ret",     None,  None,  None, 0,     &[0xc3],       0, Enc::ZO),
        insn("xor",     RM32,  R32,   None, 0,     &[0x31],       0, Enc::MR),
        insn("shr",     RM32,  One,   None, 0,     &[0xd1],       5, Enc::M1),
        insn("sub",     RM64,  Imm32, None, REX_W, &[0x81],       5, Enc::MI),
        insn("syscall", None,  None,  None, 0,     &[0x0f, 0x05], 0, Enc::ZO),
        insn("test",    RM32,  R32,   None, 0,     &[0x85],       0, Enc::MR),
        insn("test",    Eax,   Imm32, None, 0,     &[0xa9],       0, Enc::I2),
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
                if matches!(other1, Op::_R16 | Op::None)
                    && matches!(other2, Op::_R16 | Op::None)
                {
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
                if matches!(other1, Op::R64 | Op::None) && matches!(other2, Op::R64 | Op::None)
                {
                    matches!(arg, Arg::Reg64(_) | Arg::Mem64(_) | Arg::Mem(_))
                } else {
                    matches!(arg, Arg::Reg64(_) | Arg::Mem64(_))
                }
            }
            One => matches!(arg, Arg::One),
            Imm8 => matches!(arg, Arg::One | Arg::Imm8(_)),
            Imm32 => matches!(arg, Arg::One | Arg::Imm8(_) | Arg::Imm32(_)),
            M => matches!(arg, Arg::Mem(_)),
            M64 => matches!(arg, Arg::Mem(_) | Arg::Mem64(_)),
            Rel32 => matches!(arg, Arg::Rel32(_)),
        }
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
                } else if let Some(int) = match_integer(cur)? {
                    // TODO: check that int fits into a byte
                    out.push(int as u8);
                    skip_whitespace(cur);
                    while let Some(int) = match_integer(cur)? {
                        out.push(int as u8);
                        skip_whitespace(cur);
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
        "i16" => {
            if let Some(int) = match_integer(cur)? {
                // TODO: check that int fits into a word
                emit16(int as u16, out);
                skip_whitespace(cur);
                while let Some(int) = match_integer(cur)? {
                    emit16(int as u16, out);
                    skip_whitespace(cur);
                }
            } else {
                return error_at(cur.off, "expected integer literal");
            }
        }
        "i32" => {
            if let Some(int) = match_integer(cur)? {
                // TODO: check that int fits into a word
                emit32s(int as i32, out);
                skip_whitespace(cur);
                while let Some(int) = match_integer(cur)? {
                    emit32s(int as i32, out);
                    skip_whitespace(cur);
                }
            } else {
                return error_at(cur.off, "expected integer literal");
            }
        }
        "i64" => {
            if let Some(int) = match_integer(cur)? {
                // TODO: check that int fits into a word
                emit64(int as u64, out);
                skip_whitespace(cur);
                while let Some(int) = match_integer(cur)? {
                    emit64(int as u64, out);
                    skip_whitespace(cur);
                }
            } else {
                return error_at(cur.off, "expected integer literal");
            }
        }
        "res" => {
            if let Some(int) = match_integer(cur)? {
                // TODO: check that int fits into a word
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

fn match_integer(cur: &mut Cursor) -> Result<Option<u64>, Error> {
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
                Ok(Some(int))
            } else {
                error_at(cur.off, "expected hexadecimal digit")
            }
        } else {
            let mut int = 0;
            while cur.char >= '0' && cur.char <= '9' {
                int = int * 10 + (cur.char as u64 - '0' as u64);
                advance(cur);
            }
            Ok(Some(int))
        }
    } else if cur.char >= '1' && cur.char <= '9' {
        let mut int = cur.char as u64 - '0' as u64;
        advance(cur);
        while cur.char >= '0' && cur.char <= '9' {
            int = int * 10 + (cur.char as u64 - '0' as u64);
            advance(cur);
        }
        Ok(Some(int))
    } else {
        Ok(None)
    }
}

fn match_argument<'a>(cur: &mut Cursor, input: &'a str) -> Result<Option<Arg<'a>>, Error> {
    if let Some((ident, at)) = match_identifier(cur, input) {
        // TODO: collapse m* cases
        if ident == "m8" {
            skip_whitespace(cur);
            if !match_char('[', cur) {
                error_at(cur.off, "expected address after operand size")
            } else {
                Ok(Some(Arg::Mem8(expect_address(cur, input)?)))
            }
        } else if ident == "m16" {
            skip_whitespace(cur);
            if !match_char('[', cur) {
                error_at(cur.off, "expected address after operand size")
            } else {
                Ok(Some(Arg::Mem16(expect_address(cur, input)?)))
            }
        } else if ident == "m32" {
            skip_whitespace(cur);
            if !match_char('[', cur) {
                error_at(cur.off, "expected address after operand size")
            } else {
                Ok(Some(Arg::Mem32(expect_address(cur, input)?)))
            }
        } else if ident == "m64" {
            skip_whitespace(cur);
            if !match_char('[', cur) {
                error_at(cur.off, "expected address after operand size")
            } else {
                Ok(Some(Arg::Mem64(expect_address(cur, input)?)))
            }
        } else if let Some(reg) = parse_register(ident) {
            Ok(Some(reg))
        } else {
            Ok(Some(Arg::Rel32((ident, at))))
        }
    } else if let Some(imm) = match_integer(cur)? {
        if imm == 1 {
            Ok(Some(Arg::One))
        } else if imm <= 0xff {
            Ok(Some(Arg::Imm8(imm)))
        } else {
            Ok(Some(Arg::Imm32(imm)))
        }
    } else if match_char('[', cur) {
        Ok(Some(Arg::Mem(expect_address(cur, input)?)))
    } else if match_char('-', cur) {
        if let Some(imm) = match_integer(cur)? {
            if imm <= 0xff {
                Ok(Some(Arg::Imm8(-(imm as i64) as u64)))
            } else {
                Ok(Some(Arg::Imm32(-(imm as i64) as u64)))
            }
        } else {
            error_at(cur.off, "expected integer literal")
        }
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
                    if let Some(int) = match_integer(cur)? {
                        if int != 1 && int != 2 && int != 4 && int != 8 {
                            return error_at(cur.off, "scale must be 1, 2, 4, or 8");
                        }

                        if index.is_some() {
                            return error_at(at, "index already set");
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
                            index = Some(reg);
                        }
                    } else {
                        base = Some(reg);
                    }
                }
            } else {
                label = Some((ident, at));
            }
        } else if let Some(int) = match_integer(cur)? {
            disp += int as i64; // TODO: check for overflow
        } else {
            return error_at(cur.off, "expected address component");
        }

        skip_whitespace(cur);

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
    out: &mut Vec<u8>,
    patches: &mut Vec<Patch<'a>>,
    labels: &HashMap<&str, usize>,
) -> Result<(), Error> {
    //let beg = out.len();

    let mut bytes = Vec::<u8>::new();

    let mut rex = insn.rex as u32;

    bytes.extend(insn.opcodes);

    match insn.encoding {
        Enc::ZO => (),
        Enc::OI => {
            let idx = bytes.len() - 1;
            let mut reg = arg1.reg();
            if reg >= 8 {
                reg -= 8;
                rex |= REX_B as u32;
            }

            bytes[idx] += reg as u8;
            emit_imm(arg2.imm(), insn.op2, &mut bytes);
        }
        Enc::RM => {
            emit_modrm(arg1.reg(), arg2, &mut bytes, patches, labels, out.len(), &mut rex)?;
        }
        Enc::MR => {
            emit_modrm(arg2.reg(), arg1, &mut bytes, patches, labels, out.len(), &mut rex)?;
        }
        Enc::MI => {
            emit_modrm(insn.reg as u32, arg1, &mut bytes, patches, labels, out.len(), &mut rex)?;
            emit_imm(arg2.imm(), insn.op2, &mut bytes);
        }
        Enc::M1 => emit_modrm(insn.reg as u32, arg1, &mut bytes, patches, labels, out.len(), &mut rex)?,
        Enc::D => {
            let Arg::Rel32((label, at)) = *arg1 else {
                panic!()
            };

            if let Some(&off) = labels.get(label) {
                let pos = out.len() + bytes.len() + 4;
                if let Some(rel) = rel32(off, pos) {
                    emit32s(-rel, &mut bytes);
                } else {
                    return error_at(at, "distance to target is too large");
                }
            } else {
                emit32(0, &mut bytes);
                let pos = out.len() + bytes.len();
                patches.push(Patch {
                    off: pos as i64,
                    label,
                    at,
                    disp: 0,
                });
            }
        }
        Enc::M => emit_modrm(insn.reg as u32, arg1, &mut bytes, patches, labels, out.len(), &mut rex)?,
        Enc::I2 => emit_imm(arg2.imm(), insn.op2, &mut bytes),
        Enc::RMI => {
            emit_modrm(arg1.reg(), arg2, &mut bytes, patches, labels, out.len(), &mut rex)?;
            emit_imm(arg3.imm(), insn.op3, &mut bytes);
        }
    }

    if rex != 0 {
        out.push(rex as u8);
    }

    out.extend_from_slice(&bytes);

    //eprintln!("  {:02x?}", &out[beg..]);

    Ok(())
}

fn rel32(p0: usize, p1: usize) -> Option<i32> {
    let diff = p1 - p0;
    if diff <= i32::max_value() as usize {
        Some(diff as i32)
    } else {
        None
    }
}

fn emit_imm(imm: u64, op: Op, out: &mut Vec<u8>) {
    match op {
        Op::Imm8 => out.push(imm as u8),
        Op::Imm32 => emit32(imm as u32, out),
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

fn emit32s(val: i32, out: &mut Vec<u8>) {
    let val: [u8; 4] = unsafe { transmute(val) };
    out.extend(val);
}

#[allow(dead_code)]
fn emit64(val: u64, out: &mut Vec<u8>) {
    let val: [u8; 8] = unsafe { transmute(val) };
    out.extend(val);
}

fn emit_modrm<'a>(
    mut reg: u32,
    rm: &Arg<'a>,
    out: &mut Vec<u8>,
    patches: &mut Vec<Patch<'a>>,
    labels: &HashMap<&str, usize>,
    mut pos: usize,
    rex: &mut u32
) -> Result<(), Error> {
    let modrm;

    if reg >= 8 {
        reg -= 8;
        *rex |= REX_R as u32;
    }

    match rm {
        Arg::Mem(addr) | Arg::Mem8(addr) | Arg::Mem16(addr) | Arg::Mem32(addr) | Arg::Mem64(addr) => {
            let r#mod;
            let rm;
            let mut has_disp = false;
            let mut has_imm = false;
            let mut sib = None;

            if let Some(base) = addr.base {
                if addr.disp > 0 {
                    r#mod = 0b10;
                    has_imm = true;
                    assert!(addr.label.is_none());
                } else {
                    r#mod = 0b00;
                }

                if base == 4 {
                    rm = 0b100;
                    sib = Some(generate_sib(base, addr.index, addr.scale));
                } else {
                    if addr.index.is_some() {
                        rm = 0b100;
                        sib = Some(generate_sib(base, addr.index, addr.scale));
                    } else {
                        rm = base;
                    }
                }
            } else {
                if addr.index.is_some() {
                    r#mod = if addr.disp > 0 || addr.label.is_some() {
                        0b10
                    } else {
                        0b00
                    };
                    rm = 0b100;
                    sib = Some(generate_sib(5, addr.index, addr.scale));
                } else {
                    assert!(addr.disp > 0 || addr.label.is_some());
                    r#mod = 0b00;
                    rm = 0b101;
                    has_disp = true;
                }
            }

            let modrm = (r#mod << 6 | reg << 3 | rm) as u8;

            out.push(modrm);
            if let Some(sib) = sib {
                out.push(sib as u8);
            }

            if has_imm {
                if addr.disp <= i32::max_value() as i64 {
                    emit32(addr.disp as u32, out);
                } else {
                    // TODO: error
                }
            }

            if has_disp {
                if *rex != 0 {
                    pos += 1;
                }

                if let Some((label, at)) = addr.label {
                    if let Some(&off) = labels.get(label) {
                        if let Some(rel) = rel32(off, pos + out.len() + 4) {
                            emit32s(-rel, out);
                        } else {
                            return error_at(at, "distance to target is too large");
                        }
                    } else {
                        emit32(0, out);
                        let patch = Patch {
                            off: (pos + out.len()) as i64,
                            label,
                            at,
                            disp: addr.disp,
                        };
                        patches.push(patch);
                    }
                }
            }
        }
        Arg::Eax => {
            modrm = (0b11 << 6 | reg << 3 | 0) as u8;
            out.push(modrm);
        }
        &Arg::Reg8(mut rm) | &Arg::_Reg16(mut rm) | &Arg::Reg32(mut rm) | &Arg::Reg64(mut rm) => {
            if rm >= 8 {
                rm -= 8;
                *rex |= REX_B as u32;
            }
            modrm = (0b11 << 6 | reg << 3 | rm) as u8;
            out.push(modrm);
        }
        _ => panic!(),
    }

    Ok(())
}

fn generate_sib(base: u32, index: Option<u32>, scale: u32) -> u32 {
    let ss = match scale {
        1 => 0b00,
        2 => 0b01,
        4 => 0b10,
        8 => 0b11,
        _ => panic!(),
    };

    let index = index.unwrap_or(0b100);

    return ss << 6 | index << 3 | base;
}
