use std::rc::Rc;

use frontend::typesystem::Type;

use super::{
    instr::Instr,
    operand::{Const, Op, Var},
};

/// Trait that all IR elements should implement, allows getting their type.
pub trait Typed {
    fn get_type(&self) -> Rc<Type>;
}

impl Typed for Const {
    fn get_type(&self) -> Rc<Type> {
        match self {
            Self::Int(_) => Rc::new(Type::I64),
            Self::Bool(_) => Rc::new(Type::Bool),
        }
    }
}

impl Typed for Var {
    fn get_type(&self) -> Rc<Type> {
        self.tp.clone()
    }
}

impl Typed for Op {
    fn get_type(&self) -> Rc<Type> {
        match self {
            Self::Const(c) => c.get_type(),
            Self::Variable(v) => v.get_type(),
        }
    }
}

impl Typed for Instr {
    fn get_type(&self) -> Rc<Type> {
        match self {
            Self::Jmp(_) | Self::Br { .. } | Self::Ret(_) => Rc::new(Type::Unit),
            Self::Binary { dest, .. } => dest.get_type(),
            Self::Const { dest, .. } => dest.get_type(),
            Self::Call { dest, .. } => dest.get_type(),
            Self::Cmp { dest, .. } => {
                assert!(*dest.get_type() == Type::Bool);
                Rc::new(Type::Bool)
            }
        }
    }
}
