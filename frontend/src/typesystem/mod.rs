use std::fmt::Display;
use std::rc::Rc;

use crate::lexer::WithSpan;

mod type_decl;
pub use type_decl::TypeDecl;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub tp: WithSpan<Rc<Type>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bottom,
    Unit,
    I64,
    Bool,
    Function {
        params: Vec<Rc<Type>>,
        rettype: Rc<Type>,
    },
    Struct {
        name: WithSpan<String>,
        fields: Vec<Field>,
    },
    Alias(String),
    Undef,
}

impl Type {
    pub fn primitive_from_str(s: &str) -> Option<Self> {
        match s {
            "i64" => Some(Self::I64),
            "bool" => Some(Self::Bool),
            "unit" => Some(Self::Unit),
            _ => None,
        }
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Bottom | Type::Unit | Type::I64 | Type::Bool | Type::Undef => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Type::I64 => true,
            _ => false,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bottom => write!(f, "_"),
            Type::Unit => write!(f, "unit"),
            Type::I64 => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
            Type::Undef => write!(f, "?"),
            Type::Alias(name) => write!(f, "{}", name),
            Type::Function { params, rettype } => {
                write!(f, "fn (")?;
                if let Some((last_param, init)) = params.split_last() {
                    for param in init {
                        write!(f, "{}, ", param)?;
                    }
                    write!(f, "{}", last_param)?;
                }
                write!(f, ") -> {}", rettype)
            }
            Type::Struct { .. } => {
                // this `unreachable` is odd, but valid, because we have no anonymous structs
                unreachable!("struct type should not be formatted directly")
            }
        }
    }
}
