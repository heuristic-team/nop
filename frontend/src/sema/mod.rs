mod pass;

use pass::Pass;

mod passes;
use passes::*;

mod res;
pub use res::Res;

use crate::{TranslationUnit, ast::*, typesystem::TypeDecl};

pub fn run(fn_decls: Vec<FnDecl>, type_decls: Vec<TypeDecl>) -> Res<TranslationUnit> {
    let mut passes = HandleImplicitRets {}
        .and_then(TypeNameCorrectnessCheck {})
        .and_then(NameCorrectnessCheck {})
        .and_then(AssignmentCorrectnessCheck {})
        .and_then(TypeCheck {});

    passes.run((fn_decls, type_decls))
}
