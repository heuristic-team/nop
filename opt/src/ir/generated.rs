use super::{
    instr::{BinaryType, InstrContent},
    operand::{Const, Label, Op, Var},
};

/// File for macros for generating functions and their uses.

/// Creates checks for IR instances.
///
/// [<is_ $name>] is paste!'s operation which
/// creates identifier concatenated from is_ and $name.
/// That is needed to create functions like `is_add`, while still
/// passing add as argument for nicer API.
///
/// Should be used to create functions to check if this instance is
/// certain instruction or certain type of variable for example.
///
/// # Examples
/// ```
/// // creates method `is_add` that checks whether this instruction is `add` or not
/// checker!(add, Instr::Binary(BinaryType::Add, _, _, _));
///
/// // creates method `is_const` that checks whether this operand is constant or not.
/// checker!(const, Op::Const(_));
/// ```
#[macro_export]
macro_rules! checker {
    ($name: ident, $pattern: pat) => {
        paste::paste! {
            #[doc ="Checks whether this instance is "]
            #[doc = stringify!($name)]
            pub fn [<is_ $name>](&self) -> bool {
                match self {
                    $pattern => true,
                    _ => false,
                }
            }
        }
    };
}

macro_rules! binary_factory {
    ($name: ident, $tp: expr) => {
        paste::paste! {
            #[doc ="Creates binary instruction "]
            #[doc = stringify!($name)]
            pub fn [<create_ $name>](res: Var, lhs: Op, rhs: Op) -> Self {
                Self::Binary($tp, res, lhs, rhs)
            }
        }
    };
}

impl Label {
    checker!(block, Self::Block(_));

    checker!(function, Self::Fn(_));
}

impl Const {
    checker!(bool, Self::Bool(_));

    checker!(int, Self::Int(_));
}

/// Generated methods go here.
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
    checker!(branch, Self::Br(_, _, _));
    checker!(ret, Self::Ret(_));
}

impl Op {
    checker!(const, Self::Const(_));

    checker!(var, Self::Variable(_));
}
