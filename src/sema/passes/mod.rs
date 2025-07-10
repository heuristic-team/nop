use super::{Diagnostic, Res, pass::Pass};

mod name_correctness_check;
pub use name_correctness_check::NameCorrectnessCheck;

mod assignment_correctness_check;
pub use assignment_correctness_check::AssignmentCorrectnessCheck;
