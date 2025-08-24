use frontend::typesystem::Type;

use super::{
    instr::Instr,
    operand::{Const, Op, Var},
};

/// Trait that all IR elements should implement, allows getting their type.
pub trait Typed {
    fn get_type(&self) -> Type;
}

impl Typed for Const {
    fn get_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::I64,
            Self::Bool(_) => Type::Bool,
        }
    }
}

impl Typed for Var {
    fn get_type(&self) -> Type {
        self.tp.clone()
    }
}

impl Typed for Op {
    fn get_type(&self) -> Type {
        match self {
            Self::Const(c) => c.get_type(),
            Self::Variable(v) => v.get_type(),
        }
    }
}

impl Typed for Instr {
    fn get_type(&self) -> Type {
        match self {
            Self::Jmp(_) => Type::Unit,
            Self::Br { .. } => Type::Unit,
            Self::Ret(_) => Type::Unit,
            Self::Binary { dest, .. } => dest.borrow().get_type(),
            Self::Const { dest, .. } => dest.borrow().get_type(),
            Self::Call { dest, .. } => dest.borrow().get_type(),
            Self::Cmp { dest, .. } => {
                assert!(dest.borrow().get_type() == Type::Bool);
                Type::Bool
            }
        }
    }
}
