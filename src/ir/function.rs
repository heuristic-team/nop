use crate::ir::basic_block::BasicBlock;

#[derive(Debug)]
pub struct Function {
    name: String,
    blocks: Vec<BasicBlock>,
}
