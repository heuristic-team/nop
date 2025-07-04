#[derive(Debug, Clone)]
pub enum Type {
    I64,
    Bool,
    Undef,
    Custom(String),
}
