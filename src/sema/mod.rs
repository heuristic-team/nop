mod pass;

use pass::Pass;

mod passes;
use passes::*;

mod res;
pub use res::{Diagnostic, Res};

use crate::ast::*;

pub fn run(decls: Vec<FnDecl>) -> Res<AST> {
    NameCorrectnessCheck().run(decls)
}
