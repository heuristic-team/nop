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
    name: String,
    instrs: Vec<Instr>,
}
