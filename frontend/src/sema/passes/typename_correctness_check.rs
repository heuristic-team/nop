use std::collections::HashMap;

use super::{Pass, Res};

use crate::Diagnostic;
use crate::TypeAliasMap;
use crate::ast::*;
use crate::lexer::Span;
use crate::lexer::WithSpan;
use crate::sema::util::for_each_expr;
use crate::typesystem::Type;
use crate::typesystem::TypeDecl;

pub struct TypeNameCorrectnessCheck {}

fn check_tp(diags: &mut Vec<Diagnostic>, tp: &Type, type_alias_map: &TypeAliasMap, span: Span) {
    match tp {
        Type::Alias(name) => {
            if !type_alias_map.contains_key(name) {
                diags.push(Diagnostic::new(
                    format!("use of undefined type {}", name),
                    span,
                ));
            }
        }
        Type::Undef | Type::Bottom | Type::Unit | Type::I64 | Type::Bool => {}

        // TODO: nicer spans when reporting for functions (somehow)
        // this case is unreachable currently because function types
        // cannot be written explicitly yet
        Type::Function { .. } => unreachable!(),

        Type::Struct(fields) => fields
            .iter()
            .for_each(|t| check_tp(diags, &t.tp.value, type_alias_map, t.tp.span)),
    }
}

fn check_typename_usage_in_expr(
    diags: &mut Vec<Diagnostic>,
    type_alias_map: &TypeAliasMap,
    expr: &Expr,
) {
    match expr {
        Expr::Declare { tp, .. } => {
            check_tp(diags, &tp.value, type_alias_map, tp.span);
        }
        Expr::Block { .. }
        | Expr::While { .. }
        | Expr::If { .. }
        | Expr::Ret { .. }
        | Expr::Num { .. }
        | Expr::Bool { .. }
        | Expr::Ref { .. }
        | Expr::Call { .. }
        | Expr::Binary { .. } => {}
    }
}

fn check_typename_usage_in_decl(
    diags: &mut Vec<Diagnostic>,
    type_alias_map: &TypeAliasMap,
    decl: &FnDecl,
) {
    decl.params
        .iter()
        .map(|p| &p.tp)
        .for_each(|WithSpan { value: tp, span }| check_tp(diags, &tp, type_alias_map, *span));

    for_each_expr(
        &mut |e: &Expr| check_typename_usage_in_expr(diags, type_alias_map, e),
        &decl.body,
    );
}

impl Pass for TypeNameCorrectnessCheck {
    type Input = (Vec<FnDecl>, Vec<TypeDecl>);
    type Output = (Vec<FnDecl>, TypeAliasMap);

    fn run(&mut self, (fn_decls, type_decls): Self::Input) -> Res<Self::Output> {
        let mut type_alias_map: TypeAliasMap = HashMap::new();
        let mut diags = Vec::new();

        for decl in type_decls.into_iter() {
            if let Some(primitive) = Type::primitive_from_str(&decl.name.value) {
                diags.push(Diagnostic::new(
                    format!(
                        "cannot use name of primitive type {} in type alias",
                        primitive
                    ),
                    decl.name.span,
                ));
                continue;
            }

            let prev = type_alias_map.get(&decl.name.value);

            if let Some(prev) = prev {
                let note = WithSpan::new("previously declared here".to_string(), prev.name.span);
                let msg = format!("redeclaration of type {}", decl.name.value);
                diags.push(Diagnostic::new_with_notes(msg, decl.name.span, vec![note]));
                continue;
            }

            type_alias_map.insert(decl.name.value.clone(), decl);
        }

        for decl in type_alias_map.values() {
            check_tp(
                &mut diags,
                &decl.value.value,
                &type_alias_map,
                decl.value.span,
            );
        }

        for decl in &fn_decls {
            check_typename_usage_in_decl(&mut diags, &type_alias_map, &decl);
        }

        if diags.is_empty() {
            Res::Ok((fn_decls, type_alias_map))
        } else {
            Res::Fatal(diags)
        }
    }
}
