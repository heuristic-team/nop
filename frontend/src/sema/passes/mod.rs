use super::{Res, pass::Pass, Diagnostic};

mod handle_implicit_rets;
pub use handle_implicit_rets::HandleImplicitRets;

mod name_correctness_check;
pub use name_correctness_check::NameCorrectnessCheck;

mod assignment_correctness_check;
pub use assignment_correctness_check::AssignmentCorrectnessCheck;

mod type_check;
pub use type_check::TypeCheck;
