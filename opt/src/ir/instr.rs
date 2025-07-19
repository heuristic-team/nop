#![allow(dead_code)]

use crate::ir::operand::*;
use frontend::typesystem::types::Type;

/// Type of the binary [`Instr`]
pub enum BinaryType {
    Add,
    Sub,
    Div,
    Mul,
}

/// Type of the comparison performed by [`InstrContent::Cmp`] instruction.
pub enum CmpType {
    GE,  // Greater or equal.
    GT,  // Greater than.
    LE,  // Less or equal.
    LT,  // Less than.
    EQ,  // Equal.
    NEQ, // Not equal.
}

/// Instruction of IR
///
/// Encapsulates type (using [`frontend::typesystem`]) and content of the instruction.
///
/// Contents of the instruction consist of what instruction does, i.e. whether it's `add` or `call`
/// instruction, for example, and of all operands and the result of the instruction to it.
/// For further information about contents of instruction look at [`InstrContent`]
///
/// TODO: examplino providerino
///
pub struct Instr {
    tp: Type,
    content: InstrContent,
}

/// Content of instructions of IR
///
/// Convention of the structure:
/// - If instruction can have different subtype - i.e. `add` is subtype of [`InstrContent::Binary`], or ordering on
/// [`InstrContent::Cmp`], then it goes first.
/// - If instruction is of a jump type, i.e. it has a label it conditionally or unconditionally
/// passes control flow to, then it goes next.
/// - If this instruction has a result then it has to be [`Var`] and it goes next.
/// - Operands to the instruction go next.
///
/// TODO: examplino providerino
///
pub enum InstrContent {
    Binary(BinaryType, Var, Op, Op),
    Cmp(CmpType, Var, Op, Op),
    Mov(Var, Op),
    Call(Label, Var, Vec<Op>),
    Jmp(Label),
    Ret(Option<Op>),
    Br(Label, Label, Op),
}

impl InstrContent {
    /// Creates `call` instruction.
    pub fn create_call(label: Label, res: Var, ops: Vec<Op>) -> Self {
        Self::Call(label, res, ops)
    }

    /// Creates `cmp` instruction.
    pub fn create_cmp(cmp_type: CmpType, res: Var, lhs: Op, rhs: Op) -> Self {
        Self::Cmp(cmp_type, res, lhs, rhs)
    }

    /// Creates `jmp` instruction.
    pub fn create_jmp(label: Label) -> Self {
        Self::Jmp(label)
    }

    pub fn create_br(true_label: Label, false_label: Label, cond: Op) -> Self {
        Self::Br(true_label, false_label, cond)
    }

    pub fn create_void_ret() -> Self {
        Self::Ret(None)
    }

    pub fn create_ret(op: Op) -> Self {
        Self::Ret(Some(op))
    }

    pub fn is_terminator(&self) -> bool {
        match self {
            Self::Jmp(_) => true,
            Self::Br(_, _, _) => true,
            Self::Ret(_) => true,
            _ => false,
        }
    }
}
