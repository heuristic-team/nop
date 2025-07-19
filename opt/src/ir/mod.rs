pub mod basic_block;
pub mod function;
pub mod instr;
pub mod operand;

#[macro_export]
macro_rules! checker {
    ($name: ident, $pattern: pat) => {
        paste::paste! {
            /// Checks whether this instance is $name.
            pub fn [<is_ $name>](&self) -> bool {
                match self {
                    $pattern => true,
                    _ => false,
                }
            }
        }
    };
}
