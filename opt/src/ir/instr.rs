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

macro_rules! binary_factory {
    ($name: ident, $tp: expr) => {
        fn $name(res: Var, lhs: Op, rhs: Op) -> Self {
            Self::Binary($tp, res, lhs, rhs)
        }
    };
}

#[macro_export]
macro_rules! checker {
    ($name: ident, $pattern: pat) => {
        fn $name(&self) -> bool {
            match self {
                $pattern => true,
                _ => false,
            }
        }
    };
}

impl InstrContent {
    binary_factory!(create_add, BinaryType::Add);
    binary_factory!(create_sub, BinaryType::Sub);
    binary_factory!(create_div, BinaryType::Div);
    binary_factory!(create_mul, BinaryType::Mul);

    checker!(is_add, Self::Binary(BinaryType::Add, _, _, _));
    checker!(is_sub, Self::Binary(BinaryType::Sub, _, _, _));
    checker!(is_div, Self::Binary(BinaryType::Div, _, _, _));
    checker!(is_mul, Self::Binary(BinaryType::Div, _, _, _));
}
