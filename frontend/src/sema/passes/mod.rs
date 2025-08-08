use super::{Res, pass::Pass, util};

mod handle_implicit_rets;
pub use handle_implicit_rets::HandleImplicitRets;

mod typename_correctness_check;
pub use typename_correctness_check::TypeNameCorrectnessCheck;

mod name_correctness_check;
pub use name_correctness_check::NameCorrectnessCheck;

mod assignment_correctness_check;
pub use assignment_correctness_check::AssignmentCorrectnessCheck;

mod typing;
pub use typing::Typing;
