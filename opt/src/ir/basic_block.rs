#![allow(dead_code)]

use crate::ir::instr::Instr;

/// Represents basic block of IR.
///
/// Basic block is linear code sequence without branches or jumps within it.
///
/// Basic block must end with some sort of terminator, either with jump or branch or return from
/// the function.
///
/// Consists of name and vector(maybe should be ilist) of instructions.(for now)
///
/// TODO: providerino examplerino
///
pub struct BasicBlock {
    pub name: String,
    pub instrs: Vec<Instr>,
}

impl BasicBlock {
    /// Creates basic block with specified name and instructions.
    pub fn make(name: String, instrs: Vec<Instr>) -> Self {
        BasicBlock { name, instrs }
    }

    /// Creates basic block with specified name and no instructions.
    pub fn new(name: String) -> Self {
        BasicBlock {
            name,
            instrs: vec![],
        }
    }

    /// Adds instruction to this basic block.
    ///
    /// Returns mutable reference on this basic block for `Builder` pattern.
    pub fn add_instr(&mut self, instr: Instr) -> &mut Self {
        self.instrs.push(instr);
        self
    }
}
