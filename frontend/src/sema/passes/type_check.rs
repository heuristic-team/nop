use std::borrow::Cow;
use std::collections::HashMap;

use super::{Pass, Res};
use crate::Diagnostic;
use crate::ast::*;
use crate::lexer::{Span, WithSpan};
use crate::typesystem::Type;

type TypeMap = HashMap<String, Type>;

pub struct TypeCheck {}

/// Deduce and check types in declaration body
fn process_decl(diags: &mut Vec<Diagnostic>, typemap: &mut Cow<TypeMap>, decl: &mut FnDecl) {
    let mut typemap = typemap.clone();

    for FnParam { name, tp, .. } in &decl.params {
        typemap
            .to_mut()
            .insert(name.value.clone(), tp.value.clone());
    }

    process_expr(diags, &mut typemap, &mut decl.body);

    // check that all `ret` expressions in function body return values of expected type
    for_each_expr(
        |e| match e {
            Expr::Ret { value, span } => {
                let tp = value.as_ref().map(|e| e.tp()).unwrap_or(&Type::Unit);
                if !match_types(tp, &decl.tp.value) {
                    let msg = format!(
                        "return type mismatch: expected {}, but got {}",
                        decl.tp.value, tp
                    );
                    let note = WithSpan::new(
                        "function return type declared here".to_string(),
                        decl.tp.span,
                    );
                    diags.push(Diagnostic::new_with_notes(msg, *span, vec![note]));
                }
            }
            _ => {}
        },
        &decl.body,
    );
}

/// Recursively go through expression tree, deduce types where needed and possible,
/// and check that all types are valid and compatible
fn process_expr(diags: &mut Vec<Diagnostic>, typemap: &mut Cow<TypeMap>, expr: &mut Expr) {
    match expr {
        Expr::Num { tp, value } => {
            if let Type::Undef = tp {
                *tp = deduce_integer_type(value.value);
            }
        }
        Expr::Bool { .. } => {}
        Expr::Ref { tp, name } => {
            *tp = typemap.get(&name.value).expect("valid reference").clone();
        }
        Expr::Call {
            tp,
            callee,
            args,
            span: call_span,
        } => {
            process_expr(diags, typemap, callee);

            args.iter_mut()
                .for_each(|arg| process_expr(diags, typemap, arg));

            *tp = if let Type::Function { params, rettype } = callee.tp() {
                check_call_args(diags, *call_span, params, &args);
                rettype.as_ref().clone()
            } else {
                diags.push(Diagnostic::new(
                    "expression is not callable".to_string(),
                    callee.span(),
                ));
                Type::Bottom
            }
        }
        Expr::Binary { tp, op, lhs, rhs } => {
            process_expr(diags, typemap, lhs);
            process_expr(diags, typemap, rhs);

            let mut report_incompatible_types = || {
                let note_lhs = WithSpan::new(format!("left is {}", lhs.tp()), lhs.span());
                let note_rhs = WithSpan::new(format!("right is {}", rhs.tp()), rhs.span());
                diags.push(Diagnostic::new_with_notes(
                    "incompatible types for operation".to_string(),
                    op.span,
                    vec![note_lhs, note_rhs],
                ))
            };

            *tp = if op.value.is_cmp() {
                if !match_types(lhs.tp(), rhs.tp()) {
                    report_incompatible_types();
                }
                Type::Bool
            } else {
                merge_types(lhs.tp(), rhs.tp()).cloned().unwrap_or_else(|| {
                    report_incompatible_types();
                    Type::Bottom
                })
            };
        }
        Expr::Declare {
            tp, value, name, ..
        } => {
            if let Type::Undef = tp.value {
                process_expr(diags, typemap, value);
                tp.value = value.tp().clone();
            } else {
                propagate_type(value, &tp.value);
                process_expr(diags, typemap, value);

                if !match_types(value.tp(), &tp.value) {
                    let msg = format!(
                        "initializer type mismatch: expected {}, but got {}",
                        tp.value,
                        value.tp()
                    );
                    let note = WithSpan::new("variable type delared here".to_string(), tp.span);
                    diags.push(Diagnostic::new_with_notes(msg, value.span(), vec![note]));
                }
            }

            typemap
                .to_mut()
                .insert(name.value.clone(), tp.value.clone());
        }
        Expr::Block { tp, body, .. } => {
            let mut typemap = Cow::Borrowed(typemap.as_ref());
            body.iter_mut()
                .for_each(|e| process_expr(diags, &mut typemap, e));
            *tp = body.last().map(|e| e.tp()).cloned().unwrap_or(Type::Unit);
        }
        Expr::Ret { value, .. } => {
            if let Some(value) = value {
                process_expr(diags, typemap, value);
            }
        }
    }
}

fn propagate_type(expr: &mut Expr, propagated: &Type) {
    match expr {
        Expr::Declare { .. } | Expr::Ret { .. } => {} // always unit and bottom respectively
        Expr::Block { tp, body, .. } => {
            *tp = propagated.clone();
            if let Some(e) = body.last_mut() {
                propagate_type(e, propagated);
            }
        }
        Expr::Num { tp, .. } => {
            *tp = propagated.clone();
        }
        Expr::Ref { .. } | Expr::Call { .. } => {} // these types should be set in `process_expr` by lookup
        Expr::Binary { op, .. } if op.value.is_cmp() => {} // always bool
        Expr::Bool { .. } => {}                    // always bool
        Expr::Binary { tp, lhs, rhs, .. } => {
            *tp = propagated.clone();
            propagate_type(lhs, propagated);
            propagate_type(rhs, propagated);
        }
    }
}

fn match_types(lhs: &Type, rhs: &Type) -> bool {
    merge_types(lhs, rhs).is_some()
}

fn merge_types<'a>(lhs: &'a Type, rhs: &'a Type) -> Option<&'a Type> {
    match (lhs, rhs) {
        (Type::Bottom, other) | (other, Type::Bottom) => Some(other),
        (a, b) if a == b => Some(a),
        _ => None,
    }
}

fn deduce_integer_type(value: u64) -> Type {
    Type::I64 // TODO: unhardcode this
}

fn check_call_args(diags: &mut Vec<Diagnostic>, call_span: Span, params: &[Type], args: &[Expr]) {
    if args.len() != params.len() {
        diags.push(Diagnostic::new(
            format!(
                "too {} arguments passed to function: expected {}, but got {}",
                if args.len() < params.len() {
                    "few"
                } else {
                    "many"
                },
                params.len(),
                args.len()
            ),
            call_span,
        ));
    }

    params
        .iter()
        .zip(args)
        .filter(|(param, arg)| !match_types(arg.tp(), *param))
        .map(|(param, arg)| {
            Diagnostic::new(
                format!(
                    "argument type mismatch: expected {}, but got {}",
                    param,
                    arg.tp()
                ),
                arg.span(),
            )
        })
        .for_each(|diag| diags.push(diag));
}

fn for_each_expr(mut f: impl FnMut(&Expr), root: &Expr) {
    f(root);
    match root {
        Expr::Declare { value, .. } => f(value),
        Expr::Ret { value, .. } => {
            if let Some(value) = value {
                f(value);
            }
        }
        Expr::Block { body, .. } => body.iter().for_each(f),
        Expr::Num { .. } | Expr::Ref { .. } | Expr::Bool { .. } => {}
        Expr::Call { callee, args, .. } => {
            f(&callee);
            args.iter().for_each(f)
        }
        Expr::Binary { lhs, rhs, .. } => {
            f(&lhs);
            f(&rhs);
        }
    }
}

fn typemap_from_ast(ast: &AST) -> TypeMap {
    let mut res = HashMap::new();
    for (name, decl) in ast {
        res.insert(name.clone(), decl.formal_type());
    }
    res
}

impl Pass for TypeCheck {
    type Input = AST;
    type Output = AST;

    fn run(&mut self, mut ast: Self::Input) -> Res<Self::Output> {
        let mut diags = vec![];

        let typemap = typemap_from_ast(&ast);
        for decl in ast.values_mut() {
            process_decl(&mut diags, &mut Cow::Borrowed(&typemap), decl);
        }

        if diags.is_empty() {
            Res::Ok(ast)
        } else {
            Res::Fatal(diags)
        }
    }
}
