#![feature(iter_advance_by)]

use indoc::indoc;
use logos::Logos;

pub trait SubsliceOffset {
    fn subslice_offset_stable(&self, inner: &Self) -> Option<usize>;
}

impl SubsliceOffset for str {
    fn subslice_offset_stable(&self, inner: &str) -> Option<usize> {
        let self_beg = self.as_ptr() as usize;
        let inner = inner.as_ptr() as usize;
        if inner < self_beg || inner > self_beg.wrapping_add(self.len()) {
            None
        } else {
            Some(inner.wrapping_sub(self_beg))
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Register {
    General(usize),
    ProgramCounter,
    FramePointer,
    StackPointer,
}

#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(subpattern decimal = r"[0-9][_0-9]*")]
#[logos(subpattern hex = r"[0-9a-fA-F][_0-9a-fA-F]*")]
#[logos(skip r"[ \t\n\f;,]+")]
enum Token {
    #[regex("(?&decimal)", |lex| lex.slice().parse().ok())]
    #[regex("0[xX](?&hex)", |lex| u32::from_str_radix(&lex.slice()[2..], 16).ok())]
    Immediate(u32),

    #[token("[")]
    LeftSquareBracket,
    #[token("]")]
    RightSquareBracket,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,

    #[regex("R[1-8]", |lex| Register::General((lex.slice()[1..].bytes().next().unwrap() - b'0') as usize))]
    #[token("PC", |_| Register::ProgramCounter)]
    #[token("FP", |_| Register::FramePointer)]
    #[token("SP", |_| Register::StackPointer)]
    Register(Register),

    #[token("ADD")]
    Add,
    #[token("AND")]
    And,
    #[token("CALL")]
    Call,
    #[token("JMP")]
    Jump,
    #[token("JNZ")]
    JumpNonZero,
    #[token("LT")]
    LessThan,
    #[token("LTE")]
    LessThanOrEqual,
    #[token("MOV")]
    Move,
    #[token("MUL")]
    Multiply,
    #[token("POP")]
    Pop,
    #[token("PUSH")]
    Push,
    #[token("RET")]
    Return,
    #[token("SHL")]
    ShiftLeft,
    #[token("SUB")]
    Subtract,
    #[token("SYSCALL")]
    SystemCall,
    #[token("XOR")]
    ExclusiveOr,
}

static SOURCE: &'static str = indoc! {"
JMP 0x400321;      
PUSH FP;
MOV FP, SP;
SUB SP, 4;
MOV R2, [FP+12];
MOV R4, [FP+8];
MOV R3, R4;
AND R3, R2;
MOV R1, R3;
SHL R1, 1;
MOV R5, R4;
XOR R5, R2;
MOV R3, R5;
ADD R3, R1;
MOV R1, R3;
MOV SP, FP;
POP FP;
RET;
PUSH FP;
MOV FP, SP;
SUB SP, 12;
MOV R5, [FP+8];
LTE R5, 2;
JNZ +9;
JMP +48;
MOV R3, R1;
MOV R1, 1;
MOV SP, FP;
POP FP;
RET;
MOV R6, R5;
SUB R6, 1;
MOV R4, R6;
PUSH R4;
MOV [FP+8], R5;
MOV [FP-8], R4;
CALL 0x4000d9;
ADD SP, 4;
MOV R4, 2;
MUL R4, R1;
MOV R1, [FP-8];
MOV R1, R4;
MOV R5, [FP+8];
MOV R4, R5;
SUB R4, 2;
MOV R2, R4;
PUSH R2;
MOV [FP+8], R5;
MOV [FP-8], R1;
MOV [FP-12], R2;
CALL 0x4000d9;
ADD SP, 4;
MOV R2, [FP-12];
MOV R2, R1;
PUSH R2;
MOV R1, [FP-8];
PUSH R1;
MOV [FP-8], R1;
MOV [FP-12], R2;
CALL 0x400014;
ADD SP, 8;
MOV R2, R1;
MOV R1, R2;
MOV SP, FP;
POP FP;
RET;
SUB SP, 104;
MOV R2, SP;
MOV SP, FP;
SUB SP, 0;
PUSH 0x8341013f;
PUSH 0x83391117;
PUSH 0xe35141cf;
PUSH 0xa3899167;
PUSH 0xc3e101df;
PUSH 0x43599137;
PUSH 0x23f1416f;
PUSH 0x63a91187;
PUSH 0x381017f;
PUSH 0x3791157;
PUSH 0x6391410f;
PUSH 0x23c991a7;
PUSH 0x3e1e602a;
PUSH 0xaac6fc18;
PUSH 0x940434cc;
PUSH 0xbcdd4ea9;
PUSH 0xb39e6f8f;
PUSH 0xea8e25ed;
PUSH 0xd2bc703b;
PUSH 0xd339ce89;
PUSH 0xa23e362a;
PUSH 0x73bba5e8;
PUSH 0x54412994;
PUSH 0x501b6575;
PUSH 0x66626a69;
MOV SP, R2;
MOV R5, 0;
PUSH R5;
MOV [FP-104], R5;
CALL 0x4000d9;
ADD SP, 4;
MOV R2, R1;
AND R2, 255;
MOV R4, R2;
MOV R1, FP;
SUB R1, 100;
MOV R2, R1;
MOV R5, [FP-104];
ADD R2, R5;
XOR [R2], R4;
MOV R1, FP;
SUB R1, 100;
MOV R2, R1;
ADD R2, R5;
XOR R2, R1;
XOR R1, R2;
XOR R2, R1;
MOV R6, R2;
MOV R2, 1;
MOV R8, 1;
SYSCALL;
MOV R1, R2;
ADD R5, 1;
LT R5, 100;
MOV [FP-104], R5;
JNZ -362;
MOV R2, R1;
MOV R1, 0;
MOV R8, 2;
SYSCALL;
ADD SP, 104;"};

static SOURCE2: &'static str = indoc! {"
JMP +11;
MOV R2, 1;
MOV R1, 1;
"};

#[derive(Debug, Clone)]
pub enum Atom {
    Register(Register),
    Immediate(u32),
    MemoryAccess {
        register: Register,
        offset: Option<i32>,
    },
}

#[derive(Debug, Clone)]
pub enum JumpLocation {
    Register(Register),
    Absolute(u32),
    RelativeToProgramCounter(i32),
}

#[derive(Debug, Clone)]
pub enum Opcode {
    Add(Atom, Atom),
    And(Atom, Atom),
    Call(Atom),
    Jump(JumpLocation),
    JumpNotZero(JumpLocation),
    LessThan(Atom, Atom),
    LessThanOrEqual(Atom, Atom),
    Move(Atom, Atom),
    Multiply(Atom, Atom),
    Pop(Atom),
    Push(Atom),
    Return,
    ShiftLeft(Atom, Atom),
    Subtract(Atom, Atom),
    SystemCall,
    ExclusiveOr(Atom, Atom),
}

peg::parser! {
  grammar bauhinia_parser() for [Token] {
    pub rule imm() -> Atom = [Token::Immediate(x)] { Atom::Immediate(x) }
    pub rule reg() -> Atom = [Token::Register(x)] { Atom::Register(x) }
    pub rule mem() -> Atom = [Token::LeftSquareBracket] x:(
      [Token::Register(register)] [Token::Plus] [Token::Immediate(offset)] { Atom::MemoryAccess { register, offset: Some(offset as i32) } } /
      [Token::Register(register)] [Token::Minus] [Token::Immediate(offset)] { Atom::MemoryAccess { register, offset: Some(-(offset as i32)) } } /
      [Token::Register(register)] { Atom::MemoryAccess { register, offset: None } }
    ) [Token::RightSquareBracket] { x }
    pub rule regmem() -> Atom = reg() / mem()
    pub rule add() -> Opcode = [Token::Add] x:(
      dst:regmem() src:reg() { Opcode::Add(dst, src) } /
      dst:reg() src:regmem() { Opcode::Add(dst, src) } /
      dst:regmem() src:imm() { Opcode::Add(dst, src) }
    ) { x }
    pub rule and() -> Opcode = [Token::And] x:(
      dst:regmem() src:reg() { Opcode::And(dst, src) } /
      dst:reg() src:regmem() { Opcode::And(dst, src) } /
      dst:regmem() src:imm() { Opcode::And(dst, src) }
    ) { x }
    pub rule call() -> Opcode = [Token::Call] location:( imm() / regmem()  ) { Opcode::Call(location) }

    rule jump_location() -> JumpLocation = (
      [Token::Register(register)] { JumpLocation::Register(register) } /
      [Token::Immediate(offset)] { JumpLocation::Absolute(offset as u32) } /
      [Token::Plus] [Token::Immediate(offset)] { JumpLocation::RelativeToProgramCounter(offset as i32) } /
      [Token::Minus] [Token::Immediate(offset)] { JumpLocation::RelativeToProgramCounter(-(offset as i32)) }
    )
    pub rule jmp() -> Opcode = [Token::Jump] location:jump_location() { Opcode::Jump(location) }
    pub rule jnz() -> Opcode = [Token::JumpNonZero] location:jump_location() { Opcode::JumpNotZero(location) }
    pub rule lt() -> Opcode = [Token::LessThan] x:(
      dst:imm() src:imm() { Opcode::LessThan(dst, src) } /
      dst:regmem() src:reg() { Opcode::LessThan(dst, src) } /
      dst:reg() src:regmem() { Opcode::LessThan(dst, src) } /
      dst:regmem() src:imm() { Opcode::LessThan(dst, src) } /
      dst:imm() src:regmem() { Opcode::LessThan(dst, src) }
    ) {x}
    pub rule lte() -> Opcode = [Token::LessThanOrEqual] x:(
      dst:imm() src:imm() { Opcode::LessThanOrEqual(dst, src) } /
      dst:regmem() src:reg() { Opcode::LessThanOrEqual(dst, src) } /
      dst:reg() src:regmem() { Opcode::LessThanOrEqual(dst, src) } /
      dst:regmem() src:imm() { Opcode::LessThanOrEqual(dst, src) } /
      dst:imm() src:regmem() { Opcode::LessThanOrEqual(dst, src) }
    ) {x}
    pub rule mov() -> Opcode = [Token::Move] x:(
      dst:regmem() src:imm() { Opcode::Move(dst, src) } /
      dst:regmem() src:reg() { Opcode::Move(dst, src) } /
      dst:reg() src:regmem() { Opcode::Move(dst, src) }
    ) {x}
    pub rule mul() -> Opcode = [Token::Multiply] dst:reg() src:reg() { Opcode::Multiply(dst, src) }
    pub rule pop() -> Opcode = [Token::Pop] target:(regmem() / reg()) { Opcode::Pop(target) }
    pub rule push() -> Opcode = [Token::Push] value:(imm() / regmem()) { Opcode::Push(value) }
    pub rule ret() -> Opcode = [Token::Return] { Opcode::Return }
    pub rule shl() -> Opcode = [Token::ShiftLeft] x:(
      dst:regmem() src:imm() { Opcode::ShiftLeft(dst, src) } /
      dst:regmem() src:reg() { Opcode::ShiftLeft(dst, src) }
    ) {x}
    pub rule sub() -> Opcode = [Token::Subtract] x:(
      dst:regmem() src:reg() { Opcode::Subtract(dst, src) } /
      dst:reg() src:regmem() { Opcode::Subtract(dst, src) } /
      dst:regmem() src:imm() { Opcode::Subtract(dst, src) }
    ) {x}
    pub rule syscall() -> Opcode = [Token::SystemCall] { Opcode::SystemCall }
    pub rule xor() -> Opcode = [Token::ExclusiveOr] x:(
      dst:regmem() src:reg() { Opcode::ExclusiveOr(dst, src) } /
      dst:reg() src:regmem() { Opcode::ExclusiveOr(dst, src) } /
      dst:regmem() src:imm() { Opcode::ExclusiveOr(dst, src) }
     ) {x}
    pub rule opcode() -> Opcode = add() / and() / call() / jmp() / jnz() / lt() / lte() / mov() / mul() / pop() / push() / ret() / shl() / sub() / syscall() / xor()
  }
}

macro_rules! box_array {
    ($val:expr ; $len:expr) => {{
        // Use a generic function so that the pointer cast remains type-safe
        fn vec_to_boxed_array<T>(vec: Vec<T>) -> Box<[T; $len]> {
            let boxed_slice = vec.into_boxed_slice();
            let ptr = ::std::boxed::Box::into_raw(boxed_slice) as *mut [T; $len];
            unsafe { Box::from_raw(ptr) }
        }

        vec_to_boxed_array(vec![$val; $len])
    }};
}

pub const INITIAL_PC: u32 = 0x00400000;
pub const INITIAL_SP: u32 = 0xfffffff0;

struct VirtualMachine {
    pub pc: u32,
    pub fp: u32,
    pub sp: u32,
    pub general: [u32; 9], // 0 is unused for sentinel and easy debugging
    pub stack: Box<[u32; 262140]>,
    pub source: &'static str,
}

impl VirtualMachine {
    pub fn new(source: &'static str) -> VirtualMachine {
        VirtualMachine {
            pc: INITIAL_PC,
            fp: INITIAL_SP,
            sp: INITIAL_SP,
            general: Default::default(),
            stack: box_array![0; 262140],
            source,
        }
    }

    pub fn compute_atom(&self, atom: Atom) -> u32 {
        match atom {
            Atom::Register(Register::General(num)) => self.general[num],
            Atom::Register(Register::FramePointer) => self.fp,
            Atom::Register(Register::ProgramCounter) => self.pc,
            Atom::Register(Register::StackPointer) => self.sp,
            Atom::Immediate(reg) => {
                // BIG HACK
                reg
            }
            Atom::MemoryAccess { register, offset } => {
                let base = self
                    .compute_atom(Atom::Register(register))
                    .checked_add_signed(offset.unwrap_or_default());

                todo!()
            }
        }
    }

    pub fn compute_atom_mut(&mut self, atom: Atom) -> &mut u32 {
        match atom {
            Atom::Register(Register::General(num)) => &mut self.general[num],
            Atom::Register(Register::FramePointer) => &mut self.fp,
            Atom::Register(Register::ProgramCounter) => &mut self.pc,
            Atom::Register(Register::StackPointer) => &mut self.sp,
            Atom::Immediate(reg) => {
                // BIG HACK
                self.general[0] = reg;
                &mut self.general[0]
            }
            Atom::MemoryAccess { register, offset } => {
                let base = self
                    .compute_atom(Atom::Register(register))
                    .checked_add_signed(offset.unwrap_or_default())
                    .unwrap();
                let stack_idx = (INITIAL_SP - base) / 4;
                &mut self.stack[stack_idx as usize]
            }
        }
    }

    pub fn stack_top(&self) -> u32 {
        let stack_idx = (INITIAL_SP - self.sp) / 4;
        self.stack[stack_idx as usize]
    }

    pub unsafe fn stack_u8(&self) -> &[u8] {
        unsafe { self.stack.align_to::<u8>() }.1
    }

    pub unsafe fn stack_u8_mut(&mut self) -> &mut [u8] {
        unsafe { self.stack.align_to_mut::<u8>() }.1
    }

    pub fn eval(&mut self) -> Result<(), ()> {
        'step: loop {
            let source_loc = (self.pc - INITIAL_PC) as usize;
            let mut lines = self.source[source_loc..].lines();
            if let Some(assembly) = lines.next() {
                println!("{assembly}");
                let lexer: logos::Lexer<'_, Token> = Token::lexer(assembly);
                let tokens = lexer.collect::<Vec<_>>();

                if tokens.clone().into_iter().find_map(Result::err).is_some() {
                    eprintln!("Illegal program: {} {tokens:?}", assembly);
                    return Err(());
                }
                let tokens = tokens
                    .into_iter()
                    .filter_map(Result::ok)
                    .collect::<Vec<_>>();
                match bauhinia_parser::opcode(&tokens) {
                    Ok(opcode) => {
                        match opcode {
                            Opcode::Add(dst, src) => {
                                let src = self.compute_atom(src);
                                let dst = self.compute_atom_mut(dst);
                                let res = (*dst).checked_add(src).unwrap();
                                *dst = res;
                            }
                            Opcode::And(dst, src) => {
                                let src = self.compute_atom(src);
                                let dst = self.compute_atom_mut(dst);
                                let res = (*dst) & src;
                                *dst = res;
                            }
                            Opcode::Call(callee) => {
                                self.sp -= 4;
                                let stack_idx = (INITIAL_SP - self.sp) / 4;

                                let new_pc = (INITIAL_PC
                                    + self
                                        .source
                                        .subslice_offset_stable(lines.next().unwrap())
                                        .unwrap() as u32);

                                self.stack[stack_idx as usize] = new_pc;
                                self.pc = self.compute_atom(callee);
                                continue 'step;
                            }
                            Opcode::Jump(JumpLocation::Register(Register::General(reg))) => {
                                self.pc = self.general[reg];
                                continue 'step;
                            }
                            Opcode::Jump(JumpLocation::Register(Register::FramePointer)) => {
                                self.pc = self.fp;
                                continue 'step;
                            }
                            Opcode::Jump(JumpLocation::Register(Register::ProgramCounter)) => {
                                self.pc = self.pc;
                                continue 'step;
                            }
                            Opcode::Jump(JumpLocation::Register(Register::StackPointer)) => {
                                self.pc = self.sp;
                                continue 'step;
                            }
                            Opcode::Jump(JumpLocation::Absolute(absolute)) => {
                                self.pc = absolute;
                                continue 'step;
                            }
                            Opcode::Jump(JumpLocation::RelativeToProgramCounter(rela)) => {
                                let new_pc = (INITIAL_PC
                                    + self
                                        .source
                                        .subslice_offset_stable(lines.next().unwrap())
                                        .unwrap() as u32);
                                self.pc = new_pc.checked_add_signed(rela).unwrap();
                                continue 'step;
                            }

                            Opcode::JumpNotZero(JumpLocation::Register(Register::General(reg)))
                                if self.stack_top() != 0 =>
                            {
                                self.sp += 4;
                                self.pc = self.general[reg];
                                continue 'step;
                            }
                            Opcode::JumpNotZero(JumpLocation::Absolute(absolute))
                                if self.stack_top() != 0 =>
                            {
                                self.sp += 4;
                                self.pc = absolute - 1;
                                continue 'step;
                            }
                            Opcode::JumpNotZero(JumpLocation::RelativeToProgramCounter(rela))
                                if self.stack_top() != 0 =>
                            {
                                self.sp += 4;
                                let new_pc = (INITIAL_PC
                                    + self
                                        .source
                                        .subslice_offset_stable(lines.next().unwrap())
                                        .unwrap() as u32);
                                self.pc = new_pc.checked_add_signed(rela).unwrap();
                                continue 'step;
                            }
                            Opcode::JumpNotZero(_) => {}

                            Opcode::LessThan(dst, src) => {
                                let flag = (self.compute_atom(dst) as i32)
                                    < (self.compute_atom(src) as i32);
                                self.sp -= 4;
                                let stack_idx = (INITIAL_SP - self.sp) / 4;
                                self.stack[stack_idx as usize] = flag as u32;
                            }
                            Opcode::LessThanOrEqual(dst, src) => {
                                let flag = (self.compute_atom(dst) as i32)
                                    <= (self.compute_atom(src) as i32);
                                self.sp -= 4;
                                let stack_idx = (INITIAL_SP - self.sp) / 4;
                                self.stack[stack_idx as usize] = flag as u32;
                            }
                            Opcode::Move(dst, src) => {
                                *self.compute_atom_mut(dst) = *self.compute_atom_mut(src);
                            }
                            Opcode::Multiply(dst, src) => {
                                let res = {
                                    let src_ = self.compute_atom(src.clone()) as u64;
                                    let dst_ = self.compute_atom(dst.clone()) as u64;
                                    dst_ * src_
                                };
                                let hi32 = (res >> 32) as u32;
                                let lo32 = res as u32;
                                *self.compute_atom_mut(dst) = lo32;
                                *self.compute_atom_mut(src) = hi32;

                                // println!("{:x?} {:x?}", res, self.general[1]);
                            }
                            Opcode::Pop(dst) => {
                                *self.compute_atom_mut(dst) = self.stack_top();
                                self.sp += 4;
                            }
                            Opcode::Push(value) => {
                                self.sp -= 4;
                                let stack_idx = (INITIAL_SP - self.sp) / 4;
                                self.stack[stack_idx as usize] = self.compute_atom(value);
                            }
                            Opcode::Return => {
                                let new_pc = self.stack_top();
                                self.sp += 4;
                                self.pc = new_pc;
                                continue 'step;
                            }
                            Opcode::ShiftLeft(dst, src) => {
                                let src = *self.compute_atom_mut(src);
                                let dst = self.compute_atom_mut(dst);
                                let res = (*dst).checked_shl(src).unwrap();
                                *dst = res;
                            }
                            Opcode::Subtract(dst, src) => {
                                let src = *self.compute_atom_mut(src);
                                let dst = self.compute_atom_mut(dst);
                                let res = (*dst).checked_sub(src).unwrap();
                                *dst = res;
                            }
                            Opcode::SystemCall if self.general[8] == 0 => todo!(),
                            Opcode::SystemCall if self.general[8] == 1 => {
                                let addr = INITIAL_SP.checked_sub(self.general[1]).unwrap() as usize;
                                let len: usize = self.general[2] as usize;
                                let data: &[u8] = unsafe { &self.stack_u8()[addr as usize..] };
                                let snoop: &[u8] = unsafe { &self.stack_u8()[(addr - 8)..(addr + 8)] };

                                println!("{:x} {:x?} {:x?} {:?}", self.general[1], addr, snoop, std::str::from_utf8(&data[..len]));
                            }
                            Opcode::SystemCall if self.general[8] == 2 => todo!(),
                            Opcode::SystemCall if self.general[8] == 3 => todo!(),
                            Opcode::ExclusiveOr(dst, src) => {
                                let src = *self.compute_atom_mut(src);
                                let dst = self.compute_atom_mut(dst);
                                let res = (*dst) ^ src;
                                *dst = res;
                            }
                            Opcode::SystemCall => todo!(),
                        }
                        let new_pc = (INITIAL_PC
                            + self
                                .source
                                .subslice_offset_stable(lines.next().unwrap())
                                .unwrap() as u32);
                        self.pc = new_pc;
                    }
                    Err(_) => {
                        eprintln!("Illegal program: {} {tokens:?}", assembly);
                        return Err(());
                    }
                }
            } else {
                eprintln!("Illegal program");
                return Err(());
            }
        }
    }
}

fn main() {
    let mut vm = VirtualMachine::new(SOURCE);
    vm.eval().unwrap()
}
