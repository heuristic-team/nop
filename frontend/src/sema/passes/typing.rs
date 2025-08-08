use std::rc::Rc;

use super::util::for_each_expr;
use super::{Pass, Res};

use crate::Diagnostic;
use crate::TranslationUnit;
use crate::ast::*;
use crate::lexer::{Span, WithSpan};
use crate::support::ScopedMap;
use crate::typesystem::Type;

type Ctx = ScopedMap<String, Rc<Type>>;

/// Pass for type inference and type checking.
///
/// Does a lot of work like checking variable declarations, expression type correctness and return type checking, because all of this needs a lot of context that is gathered during this pass.
pub struct Typing {}

/// Deduce and check types in declaration body
fn type_decl_body(diags: &mut Vec<Diagnostic>, typemap: &mut Ctx, decl: &mut FnDecl) {
    typemap.enter_scope();

    for FnParam { name, tp, .. } in &decl.params {
        typemap.insert(name.value.clone(), tp.value.clone());
    }

    type_expr(diags, typemap, &mut decl.body);

    check_ret_type(diags, &decl);

    typemap.leave_scope();
}

/// Find all `ret` expressions in function body and check that their argument type matches the return type of the function.
fn check_ret_type(diags: &mut Vec<Diagnostic>, decl: &FnDecl) {
    let check_ret = |value: Option<&Expr>, span: Span| -> Option<Diagnostic> {
        let tp = value.map(|e| e.tp()).unwrap_or(&Type::Unit);
        if !match_types(tp, &decl.return_type.value) {
            let msg = format!(
                "return type mismatch: expected {}, but got {}",
                decl.return_type.value, tp
            );
            let note = WithSpan::new(
                "function return type declared here".to_string(),
                decl.return_type.span,
            );
            Some(Diagnostic::new_with_notes(msg, span, vec![note]))
        } else {
            None
        }
    };

    for_each_expr(
        &mut |e: &Expr| {
            if let Expr::Ret { value, span } = e {
                if let Some(diag) = check_ret(value.as_ref().map(|b| b.as_ref()), *span) {
                    diags.push(diag);
                }
            }
        },
        &decl.body,
    );
}

/// Recursively go through expression tree, deduce types where needed and possible,
/// and check that all types are valid and compatible.
///
/// If typing fails, expression type is set to `bottom`, so that it does not break anything further.
fn type_expr(diags: &mut Vec<Diagnostic>, typemap: &mut Ctx, expr: &mut Expr) {
    match expr {
        Expr::Num { tp, value } => {
            // TODO: check that `tp` is valid and can contain the literal

            if let Type::Undef = tp.as_ref() {
                *tp = Rc::new(deduce_integer_type(value.value));
            }
        }
        Expr::Bool { .. } => {}
        Expr::Ref { tp, name } => {
            *tp = typemap.get(&name.value).expect("valid reference").clone();
        }
        Expr::If {
            tp,
            cond,
            on_true,
            on_false,
            kw_span,
            in_stmt_pos,
        } => {
            type_expr(diags, typemap, cond);
            type_expr(diags, typemap, on_true);
            if let Some(on_false) = on_false {
                type_expr(diags, typemap, on_false);
            }

            if !match_types(cond.tp(), &Type::Bool) {
                diags.push(Diagnostic::new(
                    format!(
                        "invalid type for condition: expected {}, but got {}",
                        Type::Bool,
                        cond.tp(),
                    ),
                    cond.span(),
                ));
            }

            if *in_stmt_pos {
                // in statement position we do not care about the type of the conditional,
                // its value is ignored either way
                *tp = Rc::new(Type::Unit);
            } else {
                if let Some(on_false) = on_false {
                    // in expression position, check that both `then` and `else` branches have the same type
                    *tp = merge_types(on_true.tp_rc(), on_false.tp_rc()).unwrap_or_else(|| {
                        let then_note =
                            WithSpan::new(format!("`then` is {}", on_true.tp()), on_true.span());
                        let else_note =
                            WithSpan::new(format!("`else` is {}", on_false.tp()), on_false.span());
                        diags.push(Diagnostic::new_with_notes(
                            "branches types mismatch".to_string(),
                            *kw_span,
                            vec![then_note, else_note],
                        ));

                        Rc::new(Type::Bottom)
                    })
                } else {
                    // otherwise if `else` branch is not present in expression position,
                    // conditional expression is invaid
                    *tp = Rc::new(Type::Bottom);

                    diags.push(Diagnostic::new(
                        "conditional expression is missing the `else` branch".to_string(),
                        expr.span(),
                    ));
                }
            }
        }
        Expr::While { cond, body, .. } => {
            type_expr(diags, typemap, cond);
            type_expr(diags, typemap, body);

            if !match_types(cond.tp(), &Type::Bool) {
                diags.push(Diagnostic::new(
                    format!(
                        "invalid type for while loop condition: expected {}, but got {}",
                        Type::Bool,
                        cond.tp(),
                    ),
                    cond.span(),
                ));
            }
        }
        Expr::Call {
            tp,
            callee,
            args,
            span: call_span,
        } => {
            type_expr(diags, typemap, callee);

            args.iter_mut()
                .for_each(|arg| type_expr(diags, typemap, arg));

            *tp = if let Type::Function { params, rettype } = callee.tp() {
                check_call_args(diags, *call_span, params, &args);
                rettype.clone()
            } else {
                diags.push(Diagnostic::new(
                    "expression is not callable".to_string(),
                    callee.span(),
                ));
                Rc::new(Type::Bottom)
            }
        }
        Expr::Binary { tp, op, lhs, rhs } => {
            type_expr(diags, typemap, lhs);
            type_expr(diags, typemap, rhs);

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
                Rc::new(Type::Bool)
            } else {
                merge_types(lhs.tp_rc(), rhs.tp_rc()).unwrap_or_else(|| {
                    report_incompatible_types();
                    Rc::new(Type::Bottom)
                })
            };
        }
        Expr::Declare {
            tp, value, name, ..
        } => {
            if let Type::Undef = *tp.value {
                type_expr(diags, typemap, value);
                tp.value = value.tp_rc().clone();
            } else {
                propagate_type(diags, value, tp.value.clone());
                type_expr(diags, typemap, value);

                if !match_types(value.tp(), &tp.value) {
                    let msg = format!(
                        "initializer type mismatch: expected {}, but got {}",
                        tp.value,
                        value.tp()
                    );
                    let note = WithSpan::new("variable type declared here".to_string(), tp.span);
                    diags.push(Diagnostic::new_with_notes(msg, value.span(), vec![note]));
                }
            }

            typemap.insert(name.value.clone(), tp.value.clone());
        }
        Expr::Block { tp, body, .. } => {
            typemap.enter_scope();

            body.iter_mut().for_each(|e| type_expr(diags, typemap, e));

            *tp = body
                .last()
                .map(|e| e.tp_rc())
                .unwrap_or_else(|| Rc::new(Type::Unit));

            typemap.leave_scope();
        }
        Expr::Ret { value, .. } => {
            if let Some(value) = value {
                type_expr(diags, typemap, value);
            }
        }
    }
}

/// Try set given type to the expression and all its subexpressions. This is used when the type is known and should be propagated rather than deduced.
fn propagate_type(diags: &mut Vec<Diagnostic>, expr: &mut Expr, propagated: Rc<Type>) {
    match expr {
        Expr::Declare { .. } | Expr::Ret { .. } => {} // always unit and bottom respectively

        Expr::Block { tp, body, .. } => {
            *tp = propagated.clone();
            if let Some(e) = body.last_mut() {
                propagate_type(diags, e, propagated);
            }
        }
        Expr::If { on_false: None, .. } => {}
        Expr::If {
            tp,
            on_true,
            on_false: Some(on_false),
            ..
        } => {
            *tp = propagated.clone();
            propagate_type(diags, on_true, propagated.clone());
            propagate_type(diags, on_false, propagated);
        }
        Expr::Num {
            tp,
            value: WithSpan { span, .. },
        } => {
            *tp = if propagated.is_integer() {
                propagated.clone()
            } else {
                diags.push(Diagnostic::new(
                    format!("integer literal cannot have type {}", *propagated),
                    *span,
                ));
                Rc::new(Type::Bottom)
            }
        }

        Expr::While { .. } => {} // will change in the future, see issue #18

        // these types should always be set in `type_expr` by lookup
        Expr::Ref { .. } | Expr::Call { .. } => {}

        Expr::Binary { op, .. } if op.value.is_cmp() => {} // always bool
        Expr::Bool { .. } => {}                            // always bool

        Expr::Binary { tp, lhs, rhs, .. } => {
            *tp = propagated.clone();
            propagate_type(diags, lhs, propagated.clone());
            propagate_type(diags, rhs, propagated);
        }
    }
}

/// Check that types are "compatible". That means that either `lhs` or `rhs` should be a subtype of second one.
fn match_types(lhs: &Type, rhs: &Type) -> bool {
    match (lhs, rhs) {
        (Type::Bottom, _) | (_, Type::Bottom) => true,
        (a, b) if a == b => true,
        _ => false,
    }
}

/// Find the common supertype between `lhs` and `rhs` if possible.
fn merge_types<'a>(lhs: Rc<Type>, rhs: Rc<Type>) -> Option<Rc<Type>> {
    match (lhs.as_ref(), rhs.as_ref()) {
        (Type::Bottom, _) => Some(rhs),
        (_, Type::Bottom) => Some(lhs),
        (a, b) if a == b => Some(lhs),
        _ => None,
    }
}

fn deduce_integer_type(_value: u64) -> Type {
    Type::I64 // TODO: unhardcode this
}

/// Check that function call arguments are in the expected amount and their types match those declared.
fn check_call_args<T: AsRef<Type>>(
    diags: &mut Vec<Diagnostic>,
    call_span: Span,
    params: &[T],
    args: &[Expr],
) {
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
        .filter(|(param, arg)| !match_types(arg.tp(), param.as_ref()))
        .map(|(param, arg)| {
            Diagnostic::new(
                format!(
                    "argument type mismatch: expected {}, but got {}",
                    param.as_ref(),
                    arg.tp()
                ),
                arg.span(),
            )
        })
        .for_each(|diag| diags.push(diag));
}

/// Initialize context with functions from AST
fn ctx_from_ast(ast: &AST) -> Ctx {
    ScopedMap::with_scope(
        ast.iter()
            .map(|decl| (decl.0.clone(), Rc::new(decl.1.full_type())))
            .collect(),
    )
}

impl Pass for Typing {
    type Input = TranslationUnit;
    type Output = TranslationUnit;

    fn run(&mut self, (mut ast, typemap): Self::Input) -> Res<Self::Output> {
        let mut diags = vec![];

        let mut ctx = ctx_from_ast(&ast);
        for decl in ast.values_mut() {
            type_decl_body(&mut diags, &mut ctx, decl);
        }

        if diags.is_empty() {
            Res::Ok((ast, typemap))
        } else {
            Res::Fatal(diags)
        }
    }
}
