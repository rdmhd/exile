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
    disp: u64,
    label: Option<(&'a str, usize)>,
}

enum Op<'a> {
    None,
    Reg8(u32),
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

impl Op<'_> {
    fn imm(self) -> u64 {
        match self {
            Op::Imm8(imm) => imm,
            Op::Imm32(imm) => imm,
            Op::One => 1,
            _ => panic!(),
        }
    }
}

impl Display for Op<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::None => write!(f, "<none>"),
            Op::Reg8(reg) => write!(f, "r8 {}", REGS8[*reg as usize]),
            Op::Reg32(reg) => write!(f, "r32 {}", REGS32[*reg as usize]),
            Op::Reg64(reg) => write!(f, "r64 {}", REGS64[*reg as usize]),
            Op::One => write!(f, "1 (exact)"),
            Op::Imm8(imm) => write!(f, "imm8 {imm}"),
            Op::Imm32(imm) => write!(f, "imm32 {imm}"),
            Op::Rel32((label, _)) => write!(f, "rel: \"{label}\""),
            Op::Mem(addr)
            | Op::Mem8(addr)
            | Op::Mem16(addr)
            | Op::Mem32(addr)
            | Op::Mem64(addr) => write!(
                f,
                "{size} [{base} + {index}*{scale} + {disp} + {label}]",
                size = match self {
                    Op::Mem8(_) => "m8",
                    Op::Mem16(_) => "m16",
                    Op::Mem32(_) => "m32",
                    Op::Mem64(_) => "m64",
                    Op::Mem(_) => "m",
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
    disp: i32,
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
    RMI,
}

#[derive(Debug)]
struct Insn {
    encoding: Enc,
    rex: u32, // 0 for no REX prefix
    reg: u32, // /r field for ModR/M byte
    len: u8,  // number of opcodes
    opcodes: [u8; 2],
}

const REGS8: [&str; 4] = ["al", "cl", "dl", "bl"];

const REGS32: [&str; 16] = [
    "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "r8d", "r9d", "r10d", "r11d", "r12d",
    "r13d", "r14d", "r15d",
];

const REGS64: [&str; 8] = ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"];

const REX_W: u32 = 0b0100_1000;
const REX_R: u32 = 0b0100_0100;
const _REX_X: u32 = 0b0100_0010;
const REX_B: u32 = 0b0100_0001;

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

    let mut labels = HashMap::<&str, u64>::new();
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
                if labels.insert(ident, out.len() as u64).is_some() {
                    return error_at(at, "label with this name already exists");
                }
            } else {
                skip_whitespace(&mut cur);

                let mnemonic = ident;
                let mut op1 = Op::None;
                let mut op2 = Op::None;
                let mut op3 = Op::None;

                if let Some(op) = match_operand(&mut cur, &input)? {
                    op1 = op;
                    skip_whitespace(&mut cur);

                    if match_char(',', &mut cur) {
                        skip_whitespace(&mut cur);
                        if let Some(op) = match_operand(&mut cur, &input)? {
                            op2 = op;
                        } else {
                            return error_at(cur.off, "expected operand after ','");
                        }

                        if match_char(',', &mut cur) {
                            skip_whitespace(&mut cur);
                            if let Some(op) = match_operand(&mut cur, &input)? {
                                op3 = op;
                            } else {
                                return error_at(cur.off, "expected operand after ','");
                            }
                        }
                    }
                }

                let insn = match choose_instruction(mnemonic, &op1, &op2, &op3) {
                    Some(insn) => insn,
                    None => {
                        println!("{mnemonic} {op1}, {op2}");
                        return error_at(at, "unknown instruction");
                    }
                };

                //let beg = out.len();
                //println!("{mnemonic} {op1}, {op2}");
                //println!("{insn:x?}");

                let mut rex = insn.rex;
                let opcodes = &insn.opcodes[..insn.len as usize];

                match insn.encoding {
                    Enc::ZO => out.extend(opcodes),
                    Enc::OI => {
                        let Op::Reg32(mut reg) = op1 else { panic!() };

                        if reg >= 8 {
                            reg -= 8;
                            rex |= REX_B;
                        }

                        if rex != 0 {
                            out.push(rex as u8);
                        }

                        out.extend(opcodes);
                        let idx = out.len() - 1;
                        *unsafe { out.get_unchecked_mut(idx) } += reg as u8;
                        emit32(op2.imm() as u32, out);
                    }
                    Enc::RM => {
                        let reg = match op1 {
                            Op::Reg8(reg) | Op::Reg32(reg) | Op::Reg64(reg) => {
                                if reg >= 8 {
                                    rex |= REX_R;
                                    reg - 8
                                } else {
                                    reg
                                }
                            }
                            _ => panic!(),
                        };

                        if let Op::Reg32(reg) = op2 {
                            if reg >= 8 {
                                op2 = Op::Reg32(reg - 8);
                                rex |= REX_B;
                            }
                        } else if let Op::Reg64(reg) = op2 {
                            if reg >= 8 {
                                op2 = Op::Reg64(reg - 8);
                                rex |= REX_B;
                            }
                        }

                        if rex != 0 {
                            out.push(rex as u8);
                        }
                        out.extend(opcodes);
                        emit_modrm(reg, op2, out, &mut patches);
                    }
                    Enc::RMI => {
                        let reg = match op1 {
                            Op::Reg8(reg) | Op::Reg32(reg) | Op::Reg64(reg) => {
                                if reg >= 8 {
                                    rex |= REX_R;
                                    reg - 8
                                } else {
                                    reg
                                }
                            }
                            _ => panic!(),
                        };

                        if let Op::Reg32(reg) = op2 {
                            if reg >= 8 {
                                op2 = Op::Reg32(reg - 8);
                                rex |= REX_B;
                            }
                        } else if let Op::Reg64(reg) = op2 {
                            if reg >= 8 {
                                op2 = Op::Reg64(reg - 8);
                                rex |= REX_B;
                            }
                        }

                        if rex != 0 {
                            out.push(rex as u8);
                        }
                        out.extend(opcodes);
                        emit_modrm(reg, op2, out, &mut patches);

                        match op3 {
                            Op::Imm8(imm) => out.push(imm as u8),
                            Op::Imm32(imm) => emit32(imm as u32, out),
                            _ => panic!(),
                        }
                    }
                    Enc::MR => {
                        if let Op::Reg32(reg) = op1 {
                            if reg >= 8 {
                                op1 = Op::Reg32(reg - 8);
                                rex |= REX_B;
                            }
                        } else if let Op::Reg64(reg) = op1 {
                            if reg >= 8 {
                                op1 = Op::Reg64(reg - 8);
                                rex |= REX_B;
                            }
                        }

                        let reg = match op2 {
                            Op::Reg8(reg) | Op::Reg32(reg) | Op::Reg64(reg) => {
                                if reg >= 8 {
                                    rex |= REX_R;
                                    reg - 8
                                } else {
                                    reg
                                }
                            }
                            _ => panic!(),
                        };

                        if rex != 0 {
                            out.push(rex as u8);
                        }
                        out.extend(opcodes);
                        emit_modrm(reg, op1, out, &mut patches);
                    }
                    Enc::MI => {
                        if rex != 0 {
                            out.push(rex as u8);
                        }
                        out.extend(opcodes);

                        match op1 {
                            Op::Reg32(rm) | Op::Reg64(rm) => {
                                let modrm = (0b11 << 6 | insn.reg << 3 | rm) as u8;
                                out.push(modrm);
                            }
                            Op::Mem8(_) | Op::Mem32(_) | Op::Mem64(_) => {
                                emit_modrm(insn.reg, op1, out, &mut patches);
                            }
                            _ => panic!(),
                        }

                        emit32(op2.imm() as u32, out);
                    }
                    Enc::M | Enc::M1 => {
                        if rex != 0 {
                            out.push(rex as u8);
                        }
                        out.extend(opcodes);

                        match op1 {
                            Op::Reg32(rm) | Op::Reg64(rm) => {
                                let modrm = (0b11 << 6 | insn.reg << 3 | rm) as u8;
                                out.push(modrm);
                            }
                            Op::Mem8(_) | Op::Mem32(_) | Op::Mem64(_) => {
                                emit_modrm(insn.reg, op1, out, &mut patches);
                            }
                            _ => panic!(),
                        }
                    }
                    Enc::D => {
                        if rex != 0 {
                            out.push(rex as u8);
                        }
                        out.extend(opcodes);

                        let Op::Rel32((label, _at)) = op1 else {
                            panic!()
                        };

                        if let Some(&off) = labels.get(label) {
                            let rel = off as isize - (out.len() + 4) as isize;
                            emit32s(rel as i32, out);
                        } else {
                            emit32(0, out);
                            patches.push(Patch {
                                off: out.len() as i64,
                                label,
                                at,
                                disp: 0,
                            });
                        }
                    }
                }

                //println!("  {:02x?}\n", &out[beg..]);
                //for (idx, &byte) in out[beg..].iter().enumerate() {
                //    println!("[{idx}] {byte:08b}");
                //}
                //println!("-----------------------------------");
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
            disp += (addr as i64 - patch.off as i64) as i32; // TODO: check that it fits within i32

            // TODO: this assumes that the displacement is always 32-bits, for enabling disp8
            // the offset will need to be stored with the patch
            unsafe {
                (out.as_mut_ptr().add(patch.off as usize - 4) as *mut i32).write_unaligned(disp);
            };
        } else {
            return error_at(patch.at, "missing label");
        }
    }

    Ok(())
}

fn choose_instruction(mnemonic: &str, op1: &Op, op2: &Op, op3: &Op) -> Option<Insn> {
    #[allow(unreachable_patterns)] // TODO: not sure why this gets triggered
    #[rustfmt::skip]
    let (encoding, rex, reg, opcodes): (Enc, u32, u32, &[u8]) = match (mnemonic, &op1, &op2, &op3) {
        ("inc", Op::Mem8{..}, Op::None, Op::None)
            => (Enc::M, 0, 0, &[0xfe]),
        ("dec", Op::Mem8{..}, Op::None, Op::None)
            => (Enc::M, 0, 1, &[0xfe]),
        ("mov", Op::Mem8{..}, Op::Reg8(_), Op::None)
            => (Enc::MR, 0, 0, &[0x88]),
        ("test", Op::Reg32(_), Op::Reg32(_), Op::None)
            => (Enc::MR, 0, 0, &[0x85]),
        ("cmp", Op::Reg32(_), Op::Reg32(_), Op::None)
            => (Enc::MR, 0, 0, &[0x39]),
        ("cmp", Op::Reg32(_), Op::Mem{..}, Op::None)
            => (Enc::RM, 0, 0, &[0x3b]),
        ("cmp", Op::Reg8(_), Op::Mem{..}, Op::None)
            => (Enc::RM, 0, 0, &[0x3a]),

        ("cmp", Op::Reg8(_) | Op::Mem8{..}, Op::Imm8(_), Op::None)
            => (Enc::MI, 0, 7, &[0x80]),

        ("and", Op::Reg32(_), Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::MI, 0, 4, &[0x81]),
        ("add", Op::Reg32(_), Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::MI, 0, 0, &[0x81]),
        ("add", Op::Reg64(_), Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::MI, REX_W, 0, &[0x81]),
        ("add", Op::Reg32(_), Op::Reg32(_), Op::None)
            => (Enc::MR, 0, 0, &[0x01]),
        ("add", Op::Reg64(_), Op::Reg64(_), Op::None)
            => (Enc::MR, REX_W, 0, &[0x01]),
        ("call", Op::Rel32(_), Op::None, Op::None)
            => (Enc::D, 0, 0, &[0xe8]),
        ("cmp", Op::Reg32(_), Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::MI, 0, 7, &[0x81]),
        ("dec", Op::Reg32(_), Op::None, Op::None)
            => (Enc::M, 0, 1, &[0xff]),
        ("imul",    Op::Reg32(_), Op::Reg32(_), Op::Imm8(_) | Op::One)
            => (Enc::RMI, 0, 0, &[0x6b]),
        ("imul",    Op::Reg32(_), Op::Reg32(_), Op::Imm32(_) | Op::Imm8(_) | Op::One)
            => (Enc::RMI, 0, 0, &[0x69]),
        ("inc", Op::Reg32(_), Op::None, Op::None)
            => (Enc::M, 0, 0, &[0xff]),
        ("inc", Op::Reg64(_), Op::None, Op::None)
            => (Enc::M, REX_W, 0, &[0xff]),
        ("int3", Op::None, Op::None, Op::None)
            => (Enc::ZO, 0, 0, &[0xcc]),
        ("jmp", Op::Rel32(_), Op::None, Op::None)
            => (Enc::D, 0, 0, &[0xe9]),
        ("jae", Op::Rel32(_), Op::None, Op::None)
            => (Enc::D, 0, 0, &[0x0f, 0x83]),
        ("je", Op::Rel32(_), Op::None, Op::None)
            => (Enc::D, 0, 0, &[0x0f, 0x84]),
        ("jz", Op::Rel32(_), Op::None, Op::None)
            => (Enc::D, 0, 0, &[0x0f, 0x84]),
        ("jne", Op::Rel32(_), Op::None, Op::None)
            => (Enc::D, 0, 0, &[0x0f, 0x85]),
        ("jnz", Op::Rel32(_), Op::None, Op::None)
            => (Enc::D, 0, 0, &[0x0f, 0x85]),
        ("lea", Op::Reg64(_), Op::Mem { .. }, Op::None)
            => (Enc::RM, REX_W, 0, &[0x8d]),
        ("mov", Op::Reg32(_), Op::Mem { .. }, Op::None)
            => (Enc::RM, 0, 0, &[0x8b]),
        ("mov", Op::Reg64(_), Op::Mem { .. }, Op::None)
            => (Enc::RM, REX_W, 0, &[0x8b]),
        ("mov", Op::Reg8(_) | Op::Mem { .. }, Op::Reg8(_), Op::None)
            => (Enc::MR, 0, 0, &[0x88]),
        ("mov", Op::Reg32(_) | Op::Mem(_), Op::Reg32(_), Op::None)
            => (Enc::MR, 0, 0, &[0x89]),
        ("mov", Op::Reg64(_), Op::Reg64(_), Op::None)
            => (Enc::MR, REX_W, 0, &[0x89]),
        ("mov", Op::Reg8(_), Op::Reg8(_) | Op::Mem { .. }, Op::None)
            => (Enc::RM, 0, 0, &[0x8a]),
        ("mov", Op::Reg32(_), Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::OI, 0, 0, &[0xb8]),
        ("mov", Op::Mem32 { .. }, Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::MI, 0, 0, &[0xc7]),
        ("mov", Op::Mem64 { .. }, Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::MI, REX_W, 0, &[0xc7]),
        ("movzx", Op::Reg32(_), Op::Mem8 { .. }, Op::None)
            => (Enc::RM, 0, 0, &[0x0f, 0xb6]),
        ("movzx", Op::Reg32(_), Op::Mem16 { .. }, Op::None)
            => (Enc::RM, 0, 0, &[0x0f, 0xb7]),
        ("ret", Op::None, Op::None, Op::None)
            => (Enc::ZO, 0, 0, &[0xc3]),
        ("shr", Op::Reg32(_), Op::One, Op::None)
            => (Enc::M1, 0, 5, &[0xd1]),
        ("sub", Op::Reg64(_), Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::MI, REX_W, 5, &[0x81]),
        ("syscall", Op::None, Op::None, Op::None)
            => (Enc::ZO, 0, 0, &[0x0f, 0x05]),
        ("test", Op::Reg32(_), Op::Imm32(_) | Op::Imm8(_) | Op::One, Op::None)
            => (Enc::MI, 0, 0, &[0xf7]),
        ("xor", Op::Reg32(_), Op::Reg32(_), Op::None)
            => (Enc::MR, 0, 0, &[0x31]),
        _ => {
            return None;
        }
    };

    // TODO: learn how to initialize array from a slice
    let len = opcodes.len() as u8;
    let opcodes: [u8; _] = {
        if opcodes.len() == 1 {
            [opcodes[0], 0]
        } else {
            [opcodes[0], opcodes[1]]
        }
    };

    Some(Insn {
        encoding,
        rex,
        reg,
        opcodes,
        len,
    })
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

fn match_operand<'a>(cur: &mut Cursor, input: &'a str) -> Result<Option<Op<'a>>, Error> {
    if let Some((ident, at)) = match_identifier(cur, input) {
        // TODO: collapse m* cases
        if ident == "m8" {
            skip_whitespace(cur);
            if !match_char('[', cur) {
                error_at(cur.off, "expected address after operand size")
            } else {
                Ok(Some(Op::Mem8(expect_address(cur, input)?)))
            }
        } else if ident == "m16" {
            skip_whitespace(cur);
            if !match_char('[', cur) {
                error_at(cur.off, "expected address after operand size")
            } else {
                Ok(Some(Op::Mem16(expect_address(cur, input)?)))
            }
        } else if ident == "m32" {
            skip_whitespace(cur);
            if !match_char('[', cur) {
                error_at(cur.off, "expected address after operand size")
            } else {
                Ok(Some(Op::Mem32(expect_address(cur, input)?)))
            }
        } else if ident == "m64" {
            skip_whitespace(cur);
            if !match_char('[', cur) {
                error_at(cur.off, "expected address after operand size")
            } else {
                Ok(Some(Op::Mem64(expect_address(cur, input)?)))
            }
        } else if let Some(reg) = parse_register(ident) {
            Ok(Some(reg))
        } else {
            Ok(Some(Op::Rel32((ident, at))))
        }
    } else if let Some(imm) = match_integer(cur)? {
        if imm == 1 {
            Ok(Some(Op::One))
        } else if imm <= 0xff {
            Ok(Some(Op::Imm8(imm)))
        } else {
            Ok(Some(Op::Imm32(imm)))
        }
    } else if match_char('[', cur) {
        Ok(Some(Op::Mem(expect_address(cur, input)?)))
    } else if match_char('-', cur) {
        if let Some(imm) = match_integer(cur)? {
            if imm <= 0xff {
                Ok(Some(Op::Imm8(-(imm as i64) as u64)))
            } else {
                Ok(Some(Op::Imm32(-(imm as i64) as u64)))
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
    let mut disp: u64 = 0;

    loop {
        skip_whitespace(cur);

        if let Some((ident, at)) = match_identifier(cur, &input) {
            if let Some(reg) = parse_register(ident) {
                let reg = if let Op::Reg64(reg) = reg {
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
            disp += int as u64; // TODO: check for overflow
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

fn parse_register(name: &str) -> Option<Op> {
    if let Some((idx, _)) = REGS32.iter().enumerate().find(|(_, &reg)| reg == name) {
        Some(Op::Reg32(idx as u32))
    } else if let Some((idx, _)) = REGS64.iter().enumerate().find(|(_, &reg)| reg == name) {
        Some(Op::Reg64(idx as u32))
    } else if let Some((idx, _)) = REGS8.iter().enumerate().find(|(_, &reg)| reg == name) {
        Some(Op::Reg8(idx as u32))
    } else {
        None
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

fn emit_modrm<'a>(reg: u32, rm: Op<'a>, out: &mut Vec<u8>, patches: &mut Vec<Patch<'a>>) {
    let modrm;

    match rm {
        Op::Mem(addr) | Op::Mem8(addr) | Op::Mem16(addr) | Op::Mem32(addr) | Op::Mem64(addr) => {
            let r#mod;
            let rm;
            let mut has_disp = false;
            let mut sib = None;

            if let Some(base) = addr.base {
                if addr.disp > 0 || addr.label.is_some() {
                    r#mod = 0b10;
                    has_disp = true;
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

            if has_disp {
                emit32(addr.disp as u32, out);

                if let Some((label, at)) = addr.label {
                    patches.push(Patch {
                        off: out.len() as i64,
                        label,
                        at,
                        disp: addr.disp as i32,
                    });
                }
            }
        }
        Op::Reg8(rm) | Op::Reg32(rm) | Op::Reg64(rm) => {
            modrm = (0b11 << 6 | reg << 3 | rm) as u8;
            out.push(modrm);
        }
        Op::None | Op::Imm8(_) | Op::Imm32(_) | Op::One | Op::Rel32(_) => panic!(),
    }
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
