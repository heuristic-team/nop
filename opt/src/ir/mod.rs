pub mod basic_block;
pub mod function;
pub mod instr;
pub mod operand;

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
