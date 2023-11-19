#![feature(iter_advance_by)]

use crate::vm::VirtualMachine;

pub mod lexer;
pub mod misc;
pub mod parser;
pub mod vm;

static SOURCE: &'static str = include_str!("atom.bsm");

static SOURCE2: &'static str = include_str!("test.bsm");

fn main() {
    let mut vm = VirtualMachine::new(SOURCE);
    vm.eval().unwrap()
}
