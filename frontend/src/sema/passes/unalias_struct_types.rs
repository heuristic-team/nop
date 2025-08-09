use std::rc::Rc;

use super::{Pass, Res};

use crate::TranslationUnit;
use crate::TypeDeclMap;
use crate::ast::*;
use crate::sema::util::for_each_expr_mut;
use crate::typesystem::Type;

/// Pass to unalias single-layer type aliases to struct types.
///
/// For example, if `struct Foo` is defined, all references to `Foo` will be *aliases*.
///
/// This pass converts all these references to direct references to `struct Foo` type instead.
///
/// In case of aliases like `type Bar = Foo`, `Bar` is left untouched
pub struct UnaliasStructTypes {}

impl Pass for UnaliasStructTypes {
    type Input = TranslationUnit;
    type Output = TranslationUnit;

    fn run(&mut self, (mut ast, type_decl_map): Self::Input) -> Res<Self::Output> {
        for decl in ast.values_mut() {
            unalias_struct_types_in_decl(&type_decl_map, decl);
        }

        Res::Ok((ast, type_decl_map))
    }
}

fn unalias_struct_types_in_decl(type_decl_map: &TypeDeclMap, decl: &mut FnDecl) {
    for param in decl.params.iter_mut() {
        let tp = &mut param.tp.value;
        if let Type::Alias(name) = tp.as_ref() {
            if let Some(unaliased) = unalias_struct_type(type_decl_map, name) {
                *tp = unaliased;
            }
        }
    }

    for_each_expr_mut(
        &mut unalias_struct_types_in_expr(type_decl_map),
        &mut decl.body,
    )
}

fn unalias_struct_types_in_expr(type_decl_map: &TypeDeclMap) -> impl FnMut(&mut Expr) {
    |e: &mut Expr| match e {
        Expr::Declare { tp, .. } => {
            if let Type::Alias(name) = tp.value.as_ref() {
                if let Some(unaliased) = unalias_struct_type(type_decl_map, name) {
                    tp.value = unaliased;
                }
            }
        }
        _ => {}
    }
}

fn unalias_struct_type(type_decl_map: &TypeDeclMap, name: &str) -> Option<Rc<Type>> {
    let tp = &type_decl_map
        .get_decl(name)
        .expect("valid type declaration, see `TypeNameCorrectnessCheck` pass")
        .value
        .value;

    match tp.as_ref() {
        Type::Struct {
            name: struct_name, ..
        } if struct_name.value == name => Some(tp.clone()),
        _ => None,
    }
}
