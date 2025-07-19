use std::rc::Rc;

use frontend::typesystem::types::Type;

use super::{basic_block::BasicBlock, function::Func};

/// Standard operand of the IR instruction.
///
/// It always is either constant or variable.
pub enum Op {
    Variable(Var),
    Const(Const),
}

impl Op {
    /// Factory method for creating operand which is variable.
    fn create_var(name: String, tp: Type) -> Self {
        Self::Variable(Var::new(name, tp))
    }

    /// Factory method for creating operand which is constant integer.
    fn create_int(int: u64) -> Self {
        Self::Const(Const::create_int(int))
    }

    /// Factory method for creating operand which is constant boolean.
    fn create_bool(b: bool) -> Self {
        Self::Const(Const::create_bool(b))
    }

    /// Checks whether this instance is constant.
    fn is_const(&self) -> bool {
        match self {
            Self::Const(_) => true,
            _ => false,
        }
    }

    /// Checks whether this instance is some variable.
    fn is_var(&self) -> bool {
        match self {
            Self::Variable(_) => true,
            _ => false,
        }
    }

    /// Checks whether this instance is of integer type.
    fn is_int(&self) -> bool {
        match self {
            Self::Variable(v) => v.tp == Type::I64, // TODO: do normal check for types
            Self::Const(Const::Int(_)) => true,
            _ => false,
        }
    }

    /// Checks whether this instance is of boolean type.
    fn is_bool(&self) -> bool {
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
    /// Factory method for creating integer constant.
    fn create_int(int: u64) -> Self {
        Self::Int(int)
    }

    /// Factory method for creating boolean constant.
    fn create_bool(b: bool) -> Self {
        Self::Bool(b)
    }

    /// Checks whether this constant is boolean.
    fn is_bool(&self) -> bool {
        match self {
            Self::Bool(_) => true,
            _ => false,
        }
    }

    /// Checks whether this constant is of integer type.
    fn is_int(&self) -> bool {
        match self {
            Self::Int(_) => true,
            _ => false,
        }
    }
}

/// Represents label for jump instructions in IR.
pub enum Label {
    Fn(Rc<Func>),
    Block(Rc<BasicBlock>),
}

impl Label {
    /// Factory method for creating label of function.
    fn function_label(func: Rc<Func>) -> Self {
        Self::Fn(func)
    }

    /// Factory method for creating label of basic block.
    fn block_label(block: Rc<BasicBlock>) -> Self {
        Self::Block(block)
    }

    /// Checks whether this is label of a basic block.
    fn is_block(&self) -> bool {
        match self {
            Self::Block(_) => true,
            _ => false,
        }
    }

    /// Checks whether this is label of a function.
    fn is_function(&self) -> bool {
        match self {
            Self::Fn(_) => true,
            _ => false,
        }
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
    fn new(name: String, tp: Type) -> Self {
        Self { name, tp }
    }
}
