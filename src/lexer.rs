use logos::Logos;

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
pub enum Token {
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
