#![allow(dead_code)]

use crate::{checker, ir::operand::*};
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

macro_rules! binary_factory {
    ($name: ident, $tp: expr) => {
        paste::paste! {
            /// Creates binary instruction `$name`.
            fn [<create_ $name>](res: Var, lhs: Op, rhs: Op) -> Self {
                Self::Binary($tp, res, lhs, rhs)
            }
        }
    };
}

impl InstrContent {
    binary_factory!(add, BinaryType::Add);
    binary_factory!(sub, BinaryType::Sub);
    binary_factory!(div, BinaryType::Div);
    binary_factory!(mul, BinaryType::Mul);

    checker!(add, Self::Binary(BinaryType::Add, _, _, _));
    checker!(sub, Self::Binary(BinaryType::Sub, _, _, _));
    checker!(div, Self::Binary(BinaryType::Div, _, _, _));
    checker!(mul, Self::Binary(BinaryType::Div, _, _, _));
    checker!(cmp, Self::Cmp(_, _, _, _));
    checker!(mov, Self::Mov(_, _));
    checker!(jmp, Self::Jmp(_));
    checker!(call, Self::Call(_, _, _));

    /// Creates `call` instruction.
    fn create_call(label: Label, res: Var, ops: Vec<Op>) -> Self {
        Self::Call(label, res, ops)
    }

    /// Creates `cmp` instruction.
    fn create_cmp(cmp_type: CmpType, res: Var, lhs: Op, rhs: Op) -> Self {
        Self::Cmp(cmp_type, res, lhs, rhs)
    }

    /// Creates `jmp` instruction.
    fn create_jmp(label: Label) -> Self {
        Self::Jmp(label)
    }
}
