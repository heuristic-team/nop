use crate::ir::basic_block::BasicBlock;
use crate::ir::function::Function;
use crate::typesystem::types::Type;

#[derive(Debug)]
pub enum BinaryType {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum CmpType {
    LE,
    LT,
    GE,
    GT,
    EQ,
    NEQ,
}

// will think of wrapper for it for conveying additional information.
#[derive(Debug)]
pub enum Instr {
    Binary(Variable, Value, Value, BinaryType),
    Call(Value, Label, Vec<Value>),
    Cmp(Variable, Value, Value, CmpType),
    Jmp(Label),
}

#[derive(Debug)]
pub enum Label {
    Fn(Box<Function>),
    BBlock(Box<BasicBlock>),
}

#[derive(Debug)]
pub enum Const {
    Int(u64),
    Bool(bool),
    Unit,
}

// Will be a lot of additional information such as defs/uses later.
#[derive(Debug)]
pub struct Variable {
    name: String,
    tp: Type,
}

#[derive(Debug)]
pub enum Value {
    Var(Variable),
    Val(Const),
}
