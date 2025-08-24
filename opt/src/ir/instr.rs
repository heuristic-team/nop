#![allow(dead_code)]

use std::rc::Rc;

use crate::ir::operand::*;

use crate::ir::Dest;

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

/// Represents instruction in IR.
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
pub enum Instr {
    Binary {
        tp: BinaryType,
        dest: Dest,
        lhs: Op,
        rhs: Op,
    },
    Cmp {
        tp: CmpType,
        dest: Dest,
        lhs: Op,
        rhs: Op,
    },
    /// actually idk if mov is even going to be useful.
    /// once we ssa there's basically zero point in it, before ssa it is sort of needed though.
    Const {
        dest: Dest,
        imm: Const,
    },
    Call {
        func: Dest, // ??? what
        dest: Dest,
        args: Vec<Dest>,
    },
    Jmp(Label),
    Ret(Option<Dest>),
    Br {
        true_branch: Label,
        false_branch: Label,
        cond: Dest,
    },
}

impl Instr {
    /// Creates `call` instruction.
    pub fn create_call(func: Dest, dest: Dest, args: Vec<Dest>) -> Self {
        Self::Call { func, dest, args }
    }

    /// Creates `cmp` instruction.
    pub fn create_cmp(tp: CmpType, dest: Dest, lhs: Op, rhs: Op) -> Self {
        Self::Cmp { tp, dest, lhs, rhs }
    }

    /// Creates `jmp` instruction.
    pub fn create_jmp(label: Label) -> Self {
        Self::Jmp(label)
    }

    /// Creates 'branch' instruction.
    pub fn create_br(true_branch: Label, false_branch: Label, cond: Dest) -> Self {
        Self::Br {
            true_branch,
            false_branch,
            cond,
        }
    }

    pub fn create_const(dest: Dest, imm: Const) -> Self {
        Self::Const { dest, imm }
    }

    /// Creates return instruction that returns unit.
    pub fn create_void_ret() -> Self {
        Self::Ret(None)
    }

    /// Creates return instruction that returns specified operand.
    pub fn create_specified_ret(var: Dest) -> Self {
        Self::Ret(Some(var))
    }

    /// Creates return instruction that returns specified operand.
    pub fn create_ret(var: Option<Dest>) -> Self {
        Self::Ret(var)
    }

    /// Returns whether this instruction is terminator or not.
    ///
    /// Terminator is instruction on which basic block has to end, i.e. either jump or branching
    /// operation.
    pub fn is_terminator(&self) -> bool {
        match self {
            Self::Jmp(_) => true,
            Self::Br { .. } => true,
            Self::Ret(_) => true,
            _ => false,
        }
    }
}
