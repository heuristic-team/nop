use std::rc::Rc;

use crate::ir::operand::*;
use frontend::typesystem::types::Type;

/// Type of the binary [`Instr`]
enum BinaryType {
    Add,
    Sub,
    Div,
    Mul,
}

/// Type of the comparison performed by [`InstrContent::Cmp`] instruction.
enum CmpType {
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
enum InstrContent {
    Binary(BinaryType, Var, Op, Op),
    Cmp(CmpType, Var, Op, Op),
    Mov(Var, Op),
    Call(Label, Var, Vec<Op>),
    Jmp(Label),
}
