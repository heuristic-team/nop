#![allow(dead_code)]

use std::rc::Rc;

use frontend::typesystem::Type;

use super::{basic_block::BasicBlock, function::Func};

/// Standard operand of the IR instruction.
///
/// It always is either constant or variable.
pub enum Op {
    Variable(Var),
    Const(Const),
}

impl Op {
    /// Creates variable operand with a specified type.
    pub fn create_var(name: String, tp: Type) -> Self {
        Self::Variable(Var::new(name, tp))
    }

    /// Creates constant integer operand.
    pub fn create_int(int: u64) -> Self {
        Self::Const(Const::create_int(int))
    }

    /// Creates constant boolean operand.
    pub fn create_bool(b: bool) -> Self {
        Self::Const(Const::create_bool(b))
    }

    /// Checks whether this instance is of integer type.
    pub fn is_int(&self) -> bool {
        match self {
            Self::Variable(v) => v.tp == Type::I64, // TODO: do normal check for types
            Self::Const(Const::Int(_)) => true,
            _ => false,
        }
    }

    /// Checks whether this instance is of boolean type.
    pub fn is_bool(&self) -> bool {
        match self {
            Self::Variable(v) => v.tp == Type::Bool, // TODO: do normal check for types
            Self::Const(Const::Bool(_)) => true,
            _ => false,
        }
    }
}

/// Represents all constants in IR.
pub enum Const {
    Int(u64),
    Bool(bool),
}

impl Const {
    /// Creates integer constant.
    pub fn create_int(int: u64) -> Self {
        Self::Int(int)
    }

    /// Creates boolean constant.
    pub fn create_bool(b: bool) -> Self {
        Self::Bool(b)
    }
}

/// Represents label for jump instructions in IR.
pub enum Label {
    Fn(Rc<Func>),
    Block(Rc<BasicBlock>),
}

impl Label {
    /// Creates label of function.
    pub fn function_label(func: Rc<Func>) -> Self {
        Self::Fn(func)
    }

    /// Creates label of basic block.
    pub fn block_label(block: Rc<BasicBlock>) -> Self {
        Self::Block(block)
    }
}

/// Represents variables in IR.
///
/// probably will have defs and uses later on but for now it is what it is.
pub struct Var {
    pub name: String,
    pub tp: Type,
}

impl Var {
    /// Creates new IR variable.
    pub fn new(name: String, tp: Type) -> Self {
        Self { name, tp }
    }
}
