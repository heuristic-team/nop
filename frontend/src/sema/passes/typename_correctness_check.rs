use std::borrow::Cow;

use std::collections::HashMap;
use std::collections::HashSet;

use super::{Pass, Res};
use crate::Diagnostic;
use crate::TypeAliasMap;
use crate::ast::*;
use crate::lexer::WithSpan;
use crate::typesystem::TypeDecl;

pub struct TypeNameCorrectnessCheck {}

type Names<'a> = HashSet<&'a str>;

impl Pass for TypeNameCorrectnessCheck {
    type Input = (Vec<FnDecl>, Vec<TypeDecl>);
    type Output = (Vec<FnDecl>, TypeAliasMap);

    fn run(&mut self, (fn_decls, type_decls): Self::Input) -> Res<Self::Output> {
        let mut typemap: TypeAliasMap = HashMap::new();
        let mut diags = Vec::new();

        for decl in type_decls.into_iter() {
            let prev = typemap.get(&decl.name.value);

            if let Some(prev) = prev {
                let note = WithSpan::new("previously declared here".to_string(), prev.name.span);
                let msg = format!("redeclaration of type {}", decl.name.value);
                diags.push(Diagnostic::new_with_notes(msg, decl.name.span, vec![note]));
            } else {
                typemap.insert(decl.name.value.clone(), decl);
            }
        }

        // TODO
        // let ctx = ast.keys().map(|s| s.as_str()).collect::<Names>();
        // for decl in ast.values() {
        //     check_references_in_decl(&mut diags, &mut Cow::Borrowed(&ctx), decl);
        // }

        if diags.is_empty() {
            Res::Ok((fn_decls, typemap))
        } else {
            Res::Fatal(diags)
        }
    }
}
