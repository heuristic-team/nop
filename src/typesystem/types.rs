use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Bottom,
    Unit,
    I64,
    Bool,
    Function {
        params: Vec<Type>,
        rettype: Box<Type>,
    },
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
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Bottom => write!(f, "_"),
            Type::Unit => write!(f, "unit"),
            Type::I64 => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
            Type::Undef => write!(f, "?"),
            Type::Function { params, rettype } => {
                write!(f, "fn (")?;
                if let Some((last_param, init)) = params.split_last() {
                    for param in init {
                        write!(f, "{}, ", param)?;
                    }
                    write!(f, "{}", last_param)?;
                }
                write!(f, ") {}", rettype)
            }
        }
    }
}
