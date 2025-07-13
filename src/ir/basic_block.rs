use crate::ir::ir::Instr;

#[derive(Debug)]
pub struct BasicBlock {
    name: String,
    instrs: Vec<Instr>, // probably want doubly linked list there or some shit. dc for now
}
