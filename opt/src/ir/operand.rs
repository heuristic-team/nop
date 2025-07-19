use std::rc::Rc;

use super::{basic_block::BasicBlock, function::Func};

/// Standard operand of the IR instruction.
///
/// It always is either constant or variable.
pub enum Op {
    Variable(Var),
    Const(Const),
}

/// Represents all constants in IR.
pub enum Const {
    Int(u64),
    Bool(bool),
}

/// Represents label for jump instructions in IR.
pub enum Label {
    Fn(Rc<Func>),
    Block(Rc<BasicBlock>),
}

/// Represents variables in IR.
pub struct Var {
    name: String,
}
