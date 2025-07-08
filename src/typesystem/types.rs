use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    I64,
    Bool,
    Undef,
}

impl Type {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "i64" => Some(Self::I64),
            "bool" => Some(Self::Bool),
            _ => None,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "unit"),
            Self::I64 => write!(f, "i64"),
            Self::Bool => write!(f, "bool"),
            Self::Undef => write!(f, "?"),
        }
    }
}
