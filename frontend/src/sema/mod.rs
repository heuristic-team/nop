mod pass;
use pass::Pass;

mod passes;
use passes::*;

mod res;
pub use res::Res;

use crate::ast::*;

pub fn run(decls: Vec<FnDecl>) -> Res<AST> {
    HandleImplicitRets {}
        .and_then(NameCorrectnessCheck {})
        .and_then(AssignmentCorrectnessCheck {})
        .and_then(TypeCheck {})
        .run(decls)
}
