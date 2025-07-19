#![allow(dead_code)]

use std::rc::Rc;

use crate::ir::basic_block::BasicBlock;
use frontend::typesystem::types::Type;

/// Represents function inside IR.
///
/// Consists of name, return type and vector(maybe should use list there) of [`BasicBlock`]s of which this function consists.
///
/// TODO: providerino examplerino
///
pub struct Func {
    name: String,
    tp: Type,
    blocks: Vec<Rc<BasicBlock>>,
}
