#![allow(dead_code)]

use std::{cell::RefCell, rc::Rc};

use crate::ir::basic_block::BasicBlock;
use frontend::typesystem::Type;

use super::{Control, Dest, instr::Instr, operand::Var};

/// Represents function inside IR.
///
/// Consists of name, return type and vector(maybe should use list there) of [`BasicBlock`]s of which this function consists.
///
/// TODO: providerino examplerino
///
pub struct Func {
    pub name: String,
    pub tp: Type,
    pub blocks: Vec<Control<BasicBlock>>,
    pub params: Vec<Dest>,
}

impl Func {
    pub fn start_block(&mut self, name: String) -> Control<BasicBlock> {
        let block = Rc::new(RefCell::new(BasicBlock::empty(name)));
        self.blocks.push(block.clone());
        block
    }

    pub fn get_nth_last_block(&mut self, index: usize) -> Option<&mut Control<BasicBlock>> {
        let len = self.blocks.len() - index;
        self.blocks.get_mut(len)
    }

    pub fn current_block(&mut self) -> Option<&mut Control<BasicBlock>> {
        self.blocks.last_mut()
    }

    pub fn pop_block(&mut self) -> Option<Control<BasicBlock>> {
        self.blocks.pop()
    }

    pub fn add_to_current_block(&mut self, instr: Instr) -> &mut Self {
        self.blocks.last().unwrap().borrow_mut().add_instr(instr); // TODO: print something nice
        self
    }
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
    pub fn new(
        name: String,
        tp: Type,
        blocks: Vec<Control<BasicBlock>>,
        params: Vec<Dest>,
    ) -> Self {
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
    pub fn add_block(&mut self, block: Control<BasicBlock>) -> &mut Self {
        self.blocks.push(block);
        self
    }

    /// Add basic block to blocks of function and return reference counted pointer.
    ///
    /// Useful for consuming block into function and then using reference
    /// counted pointer for labels.
    pub fn add_owned_block(&mut self, block: BasicBlock) -> Rc<RefCell<BasicBlock>> {
        let block = Rc::new(RefCell::new(block));
        self.blocks.push(block.clone());
        block
    }

    /// Adds parameter to this function.
    ///
    /// Returns mutable reference to this function for `Builder` pattern.
    pub fn add_parameter(&mut self, param: Dest) -> &mut Self {
        self.params.push(param);
        self
    }
}
