use std::rc::Rc;

use super::util::for_each_expr;
use super::{Pass, Res};

use crate::TranslationUnit;
use crate::ast::*;
use crate::lexer::{Span, WithSpan};
use crate::support::ScopedMap;
use crate::typesystem::Type;
use crate::{Diagnostic, TypeDeclMap};

type DeclTypeMap = ScopedMap<String, Rc<Type>>;

/// Pass for type inference and type checking.
///
/// Does a lot of work like checking variable declarations, expression type correctness and return type checking, because all of this needs a lot of context that is gathered during this pass.
pub struct Typing {}

struct TypingImpl<'a> {
    pub diags: Vec<Diagnostic>,
    typemap: DeclTypeMap,
    alias_map: &'a TypeDeclMap,
}

impl<'a> TypingImpl<'a> {
    /// Deduce and check types in declaration body
    fn type_decl_body(&mut self, decl: &mut FnDecl) {
        self.typemap.enter_scope();

        for FnParam { name, tp, .. } in &decl.params {
            self.typemap.insert(name.value.clone(), tp.value.clone());
        }

        self.type_expr(&mut decl.body);

        self.check_ret_type(&decl);

        self.typemap.leave_scope();
    }

    /// Find all `ret` expressions in function body and check that their argument type matches the return type of the function.
    fn check_ret_type(&mut self, decl: &FnDecl) {
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
                        self.diags.push(diag);
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
    fn type_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Num { tp, value } => {
                // TODO: check that `tp` is valid and can contain the literal

                if let Type::Undef = tp.as_ref() {
                    *tp = Rc::new(deduce_integer_type(value.value));
                }
            }
            Expr::Bool { .. } => {}
            Expr::Ref { tp, name } => {
                *tp = self
                    .typemap
                    .get(&name.value)
                    .expect("expected valid reference")
                    .clone();
            }
            Expr::MemberRef { tp, target, member } => {
                self.type_expr(target);

                *tp = self
                    .get_member_type(target, member.value.as_str())
                    .unwrap_or_else(|| {
                        self.diags.push(Diagnostic::new(
                            format!(
                                "expression of type {} has no member {}",
                                target.tp(),
                                member.value
                            ),
                            member.span,
                        ));
                        Rc::new(Type::Bottom)
                    })
            }
            Expr::If {
                tp,
                cond,
                on_true,
                on_false,
                kw_span,
                in_stmt_pos,
            } => {
                self.type_expr(cond);
                self.type_expr(on_true);
                if let Some(on_false) = on_false {
                    self.type_expr(on_false);
                }

                if !match_types(cond.tp(), &Type::Bool) {
                    self.diags.push(Diagnostic::new(
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
                } else if let Some(on_false) = on_false {
                    // in expression position, check that both `then` and `else` branches have the same type
                    *tp = if match_types(on_true.tp(), on_false.tp()) {
                        on_true.tp_rc()
                    } else {
                        let then_note =
                            WithSpan::new(format!("`then` is {}", on_true.tp()), on_true.span());
                        let else_note =
                            WithSpan::new(format!("`else` is {}", on_false.tp()), on_false.span());
                        self.diags.push(Diagnostic::new_with_notes(
                            "branches types mismatch".to_string(),
                            *kw_span,
                            vec![then_note, else_note],
                        ));

                        Rc::new(Type::Bottom)
                    }
                } else {
                    // otherwise if `else` branch is not present in expression position,
                    // conditional expression is invaid

                    *tp = Rc::new(Type::Bottom);

                    self.diags.push(Diagnostic::new(
                        "conditional expression is missing the `else` branch".to_string(),
                        expr.span(),
                    ));
                }
            }
            Expr::While { cond, body, .. } => {
                self.type_expr(cond);
                self.type_expr(body);

                if !match_types(cond.tp(), &Type::Bool) {
                    self.diags.push(Diagnostic::new(
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
                self.type_expr(callee);

                args.iter_mut().for_each(|arg| self.type_expr(arg));

                *tp = if let Type::Function { params, rettype } = callee.tp() {
                    self.check_call_args(*call_span, params, &args);
                    rettype.clone()
                } else {
                    self.diags.push(Diagnostic::new(
                        "expression is not callable".to_string(),
                        callee.span(),
                    ));
                    Rc::new(Type::Bottom)
                }
            }
            Expr::Binary { tp, op, lhs, rhs } => {
                self.type_expr(lhs);
                self.type_expr(rhs);

                let mut report_incompatible_types = || {
                    let note_lhs = WithSpan::new(format!("left is {}", lhs.tp()), lhs.span());
                    let note_rhs = WithSpan::new(format!("right is {}", rhs.tp()), rhs.span());
                    self.diags.push(Diagnostic::new_with_notes(
                        "incompatible types for operation".to_string(),
                        op.span,
                        vec![note_lhs, note_rhs],
                    ))
                };

                let types_matched = match_types(lhs.tp(), rhs.tp());
                if !types_matched {
                    report_incompatible_types();
                }

                *tp = if op.value.is_cmp() {
                    Rc::new(Type::Bool)
                } else if types_matched {
                    lhs.tp_rc()
                } else {
                    Rc::new(Type::Bottom)
                };
            }
            Expr::Declare {
                tp, value, name, ..
            } => {
                eprintln!("on decl of {}", name.value);
                eprintln!("{:?}", self.typemap);

                if let Type::Undef = *tp.value {
                    self.type_expr(value);
                    tp.value = value.tp_rc().clone();
                } else {
                    self.propagate_type(value, tp.value.clone());
                    self.type_expr(value);

                    if !match_types(value.tp(), &tp.value) {
                        let msg = format!(
                            "initializer type mismatch: expected {}, but got {}",
                            tp.value,
                            value.tp()
                        );
                        let note =
                            WithSpan::new("variable type declared here".to_string(), tp.span);
                        self.diags
                            .push(Diagnostic::new_with_notes(msg, value.span(), vec![note]));
                    }
                }

                self.typemap.insert(name.value.clone(), tp.value.clone());
            }
            Expr::Block { tp, body, .. } => {
                self.typemap.enter_scope();

                body.iter_mut().for_each(|e| self.type_expr(e));

                *tp = body
                    .last()
                    .map(|e| e.tp_rc())
                    .unwrap_or_else(|| Rc::new(Type::Unit));

                self.typemap.leave_scope();
            }
            Expr::Ret { value, .. } => {
                if let Some(value) = value {
                    self.type_expr(value);
                }
            }
        }
    }

    fn get_member_type(&self, target: &Expr, member: &str) -> Option<Rc<Type>> {
        match self.alias_map.unalias_type(target.tp()) {
            Type::Bottom => Some(Rc::new(Type::Bottom)),
            Type::Struct { fields, .. } => fields
                .iter()
                .find(|field| field.name == member)
                .map(|field| field.tp.value.clone()),
            _ => None,
        }
    }

    /// Try set given type to the expression and all its subexpressions. This is used when the type is known and should be propagated rather than deduced.
    fn propagate_type(&mut self, expr: &mut Expr, propagated: Rc<Type>) {
        match expr {
            Expr::Declare { .. } | Expr::Ret { .. } => {} // always unit and bottom respectively

            Expr::Block { tp, body, .. } => {
                *tp = propagated.clone();
                if let Some(e) = body.last_mut() {
                    self.propagate_type(e, propagated);
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
                self.propagate_type(on_true, propagated.clone());
                self.propagate_type(on_false, propagated);
            }
            Expr::Num {
                tp,
                value: WithSpan { span, .. },
            } => {
                *tp = if propagated.is_integer() {
                    propagated.clone()
                } else {
                    self.diags.push(Diagnostic::new(
                        format!("integer literal cannot have type {}", *propagated),
                        *span,
                    ));
                    Rc::new(Type::Bottom)
                }
            }

            Expr::While { .. } => {} // will change in the future, see issue #18

            // these types should always be set in `type_expr` by lookup
            Expr::Ref { .. } | Expr::Call { .. } | Expr::MemberRef { .. } => {}

            Expr::Binary { op, .. } if op.value.is_cmp() => {} // always bool
            Expr::Bool { .. } => {}                            // always bool

            Expr::Binary { tp, lhs, rhs, .. } => {
                *tp = propagated.clone();
                self.propagate_type(lhs, propagated.clone());
                self.propagate_type(rhs, propagated);
            }
        }
    }

    /// Check that function call arguments are in the expected amount and their types match those declared.
    fn check_call_args<T: AsRef<Type>>(&mut self, call_span: Span, params: &[T], args: &[Expr]) {
        if args.len() != params.len() {
            self.diags.push(Diagnostic::new(
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
            .for_each(|diag| self.diags.push(diag));
    }
}

/// Check that types are "compatible".
///
/// That means that either `lhs == rhs` or one of them is `bottom`.
fn match_types(lhs: &Type, rhs: &Type) -> bool {
    match (lhs, rhs) {
        (Type::Bottom, _) | (_, Type::Bottom) => true,
        (a, b) if a == b => true,
        _ => false,
    }
}

fn deduce_integer_type(_value: u64) -> Type {
    Type::I64 // TODO: unhardcode this
}

/// Initialize context with functions from AST
fn ctx_from_ast(ast: &AST) -> DeclTypeMap {
    ScopedMap::with_scope(
        ast.iter()
            .map(|decl| (decl.0.clone(), Rc::new(decl.1.full_type())))
            .collect(),
    )
}

impl Pass for Typing {
    type Input = TranslationUnit;
    type Output = TranslationUnit;

    fn run(&mut self, (mut ast, alias_map): Self::Input) -> Res<Self::Output> {
        let mut typing_impl = TypingImpl {
            diags: vec![],
            typemap: ctx_from_ast(&ast),
            alias_map: &alias_map,
        };

        for decl in ast.values_mut() {
            typing_impl.type_decl_body(decl);
        }

        if typing_impl.diags.is_empty() {
            Res::Ok((ast, alias_map))
        } else {
            Res::Fatal(typing_impl.diags)
        }
    }
}
