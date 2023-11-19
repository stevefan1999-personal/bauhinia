use crate::lexer::Register;
use crate::lexer::Token;

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
  pub grammar bauhinia_parser() for [Token] {
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
