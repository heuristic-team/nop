use std::collections::HashSet;

use crate::{
    Diagnostic, TypeDeclMap,
    ast::FnDecl,
    typesystem::{Type, TypeDecl},
};

use super::{Pass, Res};

pub struct TypeAliasLoopCheck {}

impl Pass for TypeAliasLoopCheck {
    type Input = (Vec<FnDecl>, TypeDeclMap);
    type Output = (Vec<FnDecl>, TypeDeclMap);

    fn run(&mut self, (fn_decls, type_decl_map): Self::Input) -> Res<Self::Output> {
        let mut diags = Vec::new();

        for type_decl in type_decl_map.decls() {
            check_alias_loops(&mut diags, &type_decl_map, type_decl);
        }

        if diags.is_empty() {
            Res::Ok((fn_decls, type_decl_map))
        } else {
            Res::Fatal(diags)
        }
    }
}

fn check_alias_loops<'a: 'b, 'b>(
    diags: &mut Vec<Diagnostic>,
    type_decl_map: &'a TypeDeclMap,
    mut type_decl: &'b TypeDecl,
) {
    let mut seen_names: HashSet<&str> = HashSet::new();
    seen_names.insert(&type_decl.name.value);

    loop {
        match type_decl.value.value.as_ref() {
            Type::Alias(name) if seen_names.contains(name.as_str()) => {
                diags.push(Diagnostic::new(
                    format!(
                        "loop in type alias initializer: alias {} repeats in the chain",
                        name
                    ),
                    type_decl.name.span,
                ));
                return;
            }
            Type::Alias(name) => {
                seen_names.insert(name);
                type_decl = type_decl_map
                    .get_decl(&name)
                    .expect("expected valid type alias, see `TypeNameCorrectnessChec` pass");
            }
            _ => {
                return;
            }
        }
    }
}
