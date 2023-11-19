use logos::Logos;

use crate::{
    box_array,
    lexer::{Register, Token},
    misc::SubsliceOffset,
    parser::{bauhinia_parser, Atom, JumpLocation, Opcode},
};

pub const INITIAL_PC: u32 = 0x00400000;
pub const INITIAL_SP: u32 = 0xfffffff0;

pub struct VirtualMachine {
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
                let _base = self
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

                                let new_pc = INITIAL_PC
                                    + self
                                        .source
                                        .subslice_offset_stable(lines.next().unwrap())
                                        .unwrap() as u32;

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
                                let new_pc = INITIAL_PC
                                    + self
                                        .source
                                        .subslice_offset_stable(lines.next().unwrap())
                                        .unwrap() as u32;
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
                                let new_pc = INITIAL_PC
                                    + self
                                        .source
                                        .subslice_offset_stable(lines.next().unwrap())
                                        .unwrap() as u32;
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
                                let addr =
                                    INITIAL_SP.checked_sub(self.general[1]).unwrap() as usize;
                                let len: usize = self.general[2] as usize;
                                let data: &[u8] = unsafe { &self.stack_u8()[addr as usize..] };
                                let snoop: &[u8] =
                                    unsafe { &self.stack_u8()[(addr - 8)..(addr + 8)] };

                                println!(
                                    "{:x} {:x?} {:x?} {:?}",
                                    self.general[1],
                                    addr,
                                    snoop,
                                    std::str::from_utf8(&data[..len])
                                );
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
                        let new_pc = INITIAL_PC
                            + self
                                .source
                                .subslice_offset_stable(lines.next().unwrap())
                                .unwrap() as u32;
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
