#![allow(dead_code)]

use std::rc::Rc;

use crate::ir::basic_block::BasicBlock;
use frontend::typesystem::types::Type;

use super::operand::Var;

/// Represents function inside IR.
///
/// Consists of name, return type and vector(maybe should use list there) of [`BasicBlock`]s of which this function consists.
///
/// TODO: providerino examplerino
///
pub struct Func {
    pub name: String,
    pub tp: Type,
    pub blocks: Vec<Rc<BasicBlock>>,
    pub params: Vec<Var>,
}

impl Func {
    /// Creates new function with specified name and type.
    pub fn empty(name: String, tp: Type) -> Self {
        Self {
            name,
            tp,
            blocks: vec![],
            params: vec![],
        }
    }

    /// Creates new function with specified name type and blocks.
    pub fn new(name: String, tp: Type, blocks: Vec<Rc<BasicBlock>>, params: Vec<Var>) -> Self {
        Self {
            name,
            tp,
            blocks,
            params,
        }
    }

    /// Adds basic block to blocks of this function.
    ///
    /// Returns mutable reference to this function for `Builder` pattern.
    pub fn add_block(&mut self, block: Rc<BasicBlock>) -> &mut Self {
        self.blocks.push(block);
        self
    }

    /// Add basic block to blocks of function and return reference counted pointer.
    ///
    /// Useful for consuming block into function and then using reference
    /// counted pointer for labels.
    pub fn add_owned_block(&mut self, block: BasicBlock) -> Rc<BasicBlock> {
        let block = Rc::new(block);
        self.blocks.push(block.clone());
        block
    }

    /// Adds parameter to this function.
    ///
    /// Returns mutable reference to this function for `Builder` pattern.
    pub fn add_parameter(&mut self, param: Var) -> &mut Self {
        self.params.push(param);
        self
    }
}
