use std::rc::Rc;

use crate::ast::{Associativity, BinaryOp, Expr, FnDecl, FnParam, Precedence};
use crate::lexer::{Lexeme, Lexemes, Span, Token, WithSpan};
use crate::typesystem::{Field, Type, TypeDecl};

mod res;
pub use res::ParseError;
use res::Res;

#[derive(Debug)]
pub struct Parser {
    lexemes: Lexemes,
}

macro_rules! token {
    ($($p:pat),*) => {
        |_token|
        {
            match _token {
                $($p => true),*,
                _ => false
            }
        }
    };
    ($($p:pat => $e:expr),*) => {
        |_token|
        {
            match _token {
                $($p => Some($e)),*,
                _ => None
            }
        }
    };
}

macro_rules! expected {
    ($($e:expr),*) => {
        vec![$($e.to_string()),*]
    };
}

impl Parser {
    pub fn new(lexemes: Lexemes) -> Self {
        Self { lexemes }
    }

    pub fn parse(&mut self) -> Res<(Vec<FnDecl>, Vec<TypeDecl>)> {
        let mut fn_decls = Vec::new();
        let mut type_decls = Vec::new();

        self.eat_while(token!(Token::EOL));
        while !self.lexemes.is_eof() {
            let WithSpan { value: token, span } = self.lexemes.peek();
            match token {
                Token::Fn => fn_decls.push(self.parse_fn_decl()?),
                Token::Type => type_decls.push(self.parse_type_decl()?),
                t => {
                    return Err(ParseError::new(
                        expected!(Token::Fn, Token::Type),
                        t.to_string(),
                        span,
                    ));
                }
            }

            self.eat_while(token!(Token::EOL));
        }

        Ok((fn_decls, type_decls))
    }

    fn eat_if(&mut self, matcher: impl FnOnce(&Token) -> bool) -> bool {
        let WithSpan { value: token, .. } = self.lexemes.peek();
        let res = matcher(&token);
        if res {
            self.lexemes.next();
        }
        res
    }

    fn eat_while(&mut self, mut matcher: impl FnMut(&Token) -> bool) {
        loop {
            let WithSpan { value: token, .. } = self.lexemes.peek();
            if matcher(&token) {
                self.lexemes.next();
            } else {
                break;
            }
        }
    }

    fn get(&mut self, matcher: impl FnOnce(&Token) -> bool, expected: Vec<String>) -> Res<Lexeme> {
        let lexeme = self.lexemes.next();
        if matcher(&lexeme.value) {
            Ok(lexeme)
        } else {
            let err = ParseError::new(expected, lexeme.value.to_string(), lexeme.span);
            Err(err)
        }
    }

    fn get_map<T>(
        &mut self,
        matcher: impl FnOnce(Token) -> Option<T>,
        expected: Vec<String>,
    ) -> Res<WithSpan<T>> {
        let WithSpan { value: token, span } = self.lexemes.next();
        let formatted_token = token.to_str();

        if let Some(res) = matcher(token) {
            Ok(WithSpan::new(res, span))
        } else {
            let err = ParseError::new(expected, formatted_token.to_string(), span);
            Err(err)
        }
    }

    fn parse_id(&mut self) -> Res<WithSpan<String>> {
        self.get_map(token!(Token::Id(id) => id), expected!("identifier"))
    }

    fn parse_struct_type(&mut self) -> Res<Type> {
        self.get(token!(Token::Struct), expected!(Token::Struct))?;
        self.eat_while(token!(Token::EOL));
        self.get(token!(Token::LBrace), expected!(Token::LBrace))?;

        let mut fields = vec![];

        while Token::RBrace != self.lexemes.peek().value {
            self.eat_while(token!(Token::EOL));

            let name = self.parse_id()?.value;
            self.eat_while(token!(Token::EOL));

            self.get(token!(Token::Colon), expected!(Token::Colon))?;
            self.eat_while(token!(Token::EOL));

            let tp = self.parse_type()?;
            self.eat_while(token!(Token::EOL));

            fields.push(Field {
                name,
                tp: tp.map(Rc::new),
            });

            let WithSpan { value: token, span } = self.lexemes.peek();
            match token {
                Token::Comma => {
                    self.lexemes.next();
                }
                Token::RBrace => {}
                t => {
                    return Err(ParseError::new(
                        expected!(Token::Comma, Token::RBrace),
                        t.to_string(),
                        span,
                    ));
                }
            }
        }

        self.get(token!(Token::RBrace), expected!(Token::RBrace))?;
        Ok(Type::Struct(fields))
    }

    fn parse_type(&mut self) -> Res<WithSpan<Type>> {
        let WithSpan { value: token, span } = self.lexemes.peek();

        let tp = match token {
            Token::Id(id) => {
                self.lexemes.next();
                Type::primitive_from_str(&id).unwrap_or(Type::Alias(id))
            }
            Token::Struct => self.parse_struct_type()?,
            t => return Err(ParseError::new(expected!("type"), t.to_string(), span)),
        };

        Ok(WithSpan::new(tp, span))
    }

    fn parse_type_decl(&mut self) -> Res<TypeDecl> {
        self.get(token!(Token::Type), expected!(Token::Type))?;

        let name = self.parse_id()?;

        self.get(token!(Token::Assign), expected!(Token::Assign))?;

        let value = self.parse_type()?;

        Ok(TypeDecl {
            name,
            value: value.map(Rc::new),
        })
    }

    fn parse_fn_decl(&mut self) -> Res<FnDecl> {
        self.get(token!(Token::Fn), expected!(Token::Fn))?;

        let name = self.parse_id()?;
        let params = self.parse_fn_params()?;

        let WithSpan {
            value: tp_token,
            span: tp_span,
        } = self.lexemes.peek();
        let tp = if let Token::Id(_) = tp_token {
            self.parse_type()?
        } else {
            WithSpan::new(Type::Unit, tp_span)
        };

        self.get(token!(Token::Assign), expected!(Token::Assign))?;
        self.eat_while(token!(Token::EOL));

        let body = self.parse_top_level_expr()?;

        let decl = FnDecl {
            name,
            tp: tp.map(Rc::new),
            params,
            body,
        };

        Ok(decl)
    }

    fn parse_fn_params(&mut self) -> Res<Vec<FnParam>> {
        self.get(token!(Token::LParen), expected!(Token::LParen))?;

        let mut res = Vec::new();

        while Token::RParen != self.lexemes.peek().value {
            self.eat_while(token!(Token::EOL));

            let is_mut = self.eat_if(token!(Token::Mut));
            self.eat_while(token!(Token::EOL));

            let name = self.parse_id()?;
            self.eat_while(token!(Token::EOL));

            self.get(token!(Token::Colon), expected!(Token::Colon))?;
            self.eat_while(token!(Token::EOL));

            let tp = self.parse_type()?;
            self.eat_while(token!(Token::EOL));

            res.push(FnParam {
                is_mut,
                name,
                tp: tp.map(Rc::new),
            });

            let WithSpan { value: token, span } = self.lexemes.peek();
            match token {
                Token::Comma => {
                    self.lexemes.next();
                }
                Token::RParen => {}
                t => {
                    return Err(ParseError::new(
                        expected!(Token::Comma, Token::RParen),
                        t.to_string(),
                        span,
                    ));
                }
            }
        }
        self.get(token!(Token::RParen), expected!(Token::RParen))?;

        Ok(res)
    }

    fn parse_top_level_expr(&mut self) -> Res<Expr> {
        match self.lexemes.peek_n::<3>().map(|l| l.value) {
            [Token::Mut, Token::Id(_), Token::Define]
            | [Token::Id(_), Token::Define, _]
            | [Token::Mut, Token::Id(_), Token::Colon]
            | [Token::Id(_), Token::Colon, _] => self.parse_declaration_expr(),

            [Token::Ret, ..] => self.parse_ret_expr(),
            _ => self.parse_expr(true),
        }
    }

    fn parse_ret_expr(&mut self) -> Res<Expr> {
        let WithSpan { span: kw_span, .. } = self.get(token!(Token::Ret), expected!(Token::Ret))?;

        let state = self.lexemes.get_state();
        let value = self.parse_expr(false).ok().map(|e| Box::new(e));
        let span = match value {
            Some(ref e) => Span::new(kw_span.start, e.span().end),
            None => {
                self.lexemes.set_state(state);
                kw_span
            }
        };
        Ok(Expr::Ret { value, span })
    }

    fn parse_declaration_expr(&mut self) -> Res<Expr> {
        // NAME := VALUE or NAME: TYPE = VALUE

        let is_mut = self.eat_if(token!(Token::Mut));

        let name = self.parse_id()?;

        let WithSpan { value: token, span } = self.lexemes.peek();
        let tp = match token {
            Token::Colon => {
                self.lexemes.next(); // eat `:`
                let tp = self.parse_type()?;
                self.get(token!(Token::Assign), expected!(Token::Assign))?;
                tp
            }
            Token::Define => self.lexemes.next().replace(Type::Undef),
            t => {
                return Err(ParseError::new(
                    expected!(Token::Colon, Token::Define),
                    t.to_string(),
                    span,
                ));
            }
        };

        let value = self.parse_expr(false)?;
        Ok(Expr::Declare {
            is_mut,
            name: name,
            tp: tp.map(Rc::new),
            value: Box::new(value),
        })
    }

    fn parse_expr(&mut self, in_stmt_pos: bool) -> Res<Expr> {
        let lhs = self.parse_term(in_stmt_pos)?;
        self.parse_full_expr(0, lhs)
    }

    fn parse_term(&mut self, in_stmt_pos: bool) -> Res<Expr> {
        let WithSpan { value: token, span } = self.lexemes.peek();
        let term = match token {
            Token::LParen => {
                self.lexemes.next();
                let res = self.parse_expr(in_stmt_pos)?;
                self.get(token!(Token::RParen), expected!(Token::RParen))?;
                Ok(res)
            }
            Token::LBrace => self.parse_block(),
            Token::If => self.parse_conditional(in_stmt_pos),
            Token::For => self.parse_loop(),
            Token::True => {
                self.lexemes.next();
                Ok(Expr::Bool { value: true, span })
            }
            Token::False => {
                self.lexemes.next();
                Ok(Expr::Bool { value: false, span })
            }
            Token::Id(name) => {
                self.lexemes.next();
                Ok(Expr::Ref {
                    tp: Rc::new(Type::Undef),
                    name: WithSpan::new(name, span),
                })
            }
            Token::Num(value) => {
                self.lexemes.next();
                Ok(Expr::Num {
                    tp: Rc::new(Type::I64), // TODO: unhardcode this
                    value: WithSpan::new(value, span),
                })
            }
            t => Err(ParseError::new(expected!("term"), t.to_string(), span)),
        }?;

        if let Token::LParen = self.lexemes.peek().value {
            self.parse_call_expr(term)
        } else {
            Ok(term)
        }
    }

    fn parse_call_expr(&mut self, callee: Expr) -> Res<Expr> {
        self.get(token!(Token::LParen), expected!(Token::LParen))?;

        let mut args = Vec::new();

        while Token::RParen != self.lexemes.peek().value {
            self.eat_while(token!(Token::EOL));

            let arg = self.parse_expr(false)?;
            args.push(arg);

            self.eat_while(token!(Token::EOL));

            let WithSpan { value: token, span } = self.lexemes.peek();
            match token {
                Token::Comma => {
                    self.lexemes.next();
                }
                Token::RParen => {}
                t => {
                    return Err(ParseError::new(
                        expected!(Token::Comma, Token::RParen),
                        t.to_string(),
                        span,
                    ));
                }
            }
        }

        let rparen_span = self
            .get(token!(Token::RParen), expected!(Token::RParen))?
            .span;
        let span = Span::new(callee.span().start, rparen_span.end);

        Ok(Expr::Call {
            tp: Rc::new(Type::Undef),
            callee: Box::new(callee),
            args,
            span,
        })
    }

    fn parse_block(&mut self) -> Res<Expr> {
        let lbrace_offset = self
            .get(token!(Token::LBrace), expected!(Token::LBrace))?
            .span
            .start;

        let mut body = Vec::new();
        while self.lexemes.peek().value != Token::RBrace {
            self.eat_while(token!(Token::EOL));
            body.push(self.parse_top_level_expr()?);

            let WithSpan { value: token, span } = self.lexemes.peek();
            match token {
                Token::EOL => self.eat_while(token!(Token::EOL)),
                Token::RBrace => {}
                t => {
                    return Err(ParseError::new(
                        expected!(Token::EOL, Token::RBrace),
                        t.to_string(),
                        span,
                    ));
                }
            }
        }

        let rbrace_offset = self
            .get(token!(Token::RBrace), expected!(Token::RBrace))?
            .span
            .end;

        let span = Span::new(lbrace_offset, rbrace_offset);

        Ok(Expr::Block {
            body,
            tp: Rc::new(Type::Undef),
            span,
        })
    }

    fn parse_loop(&mut self) -> Res<Expr> {
        let kw_span = self.get(token!(Token::For), expected!(Token::For))?.span;

        let cond = self.parse_expr(false)?;

        self.get(token!(Token::Do), expected!(Token::Do))?;

        let body = self.parse_expr(true)?;

        let span = Span::new(kw_span.start, body.span().end);

        Ok(Expr::While {
            cond: Box::new(cond),
            body: Box::new(body),
            span,
        })
    }

    fn parse_conditional(&mut self, in_stmt_pos: bool) -> Res<Expr> {
        let kw_span = self.get(token!(Token::If), expected!(Token::If))?.span;

        let cond = self.parse_expr(false)?;

        self.get(token!(Token::Then), expected!(Token::Then))?;

        let on_true = self.parse_expr(in_stmt_pos)?;

        let on_false = if self.eat_if(token!(Token::Else)) {
            Some(self.parse_expr(in_stmt_pos)?)
        } else {
            None
        };

        Ok(Expr::If {
            tp: Rc::new(Type::Undef),
            cond: Box::new(cond),
            on_true: Box::new(on_true),
            on_false: on_false.map(|e| Box::new(e)),
            kw_span,
            in_stmt_pos,
        })
    }

    fn get_cur_op_prec(&self) -> Option<Precedence> {
        bin_op_from_token(&self.lexemes.peek().value).map(|op| op.prec())
    }

    fn parse_full_expr(&mut self, prev_prec: Precedence, mut lhs: Expr) -> Res<Expr> {
        loop {
            let cur_prec = match self.get_cur_op_prec() {
                None => return Ok(lhs),
                Some(prec) if prec < prev_prec => return Ok(lhs),
                Some(prec) => prec,
            };

            let op = self.get_map(|t| bin_op_from_token(&t), expected!("binary operator"))?;

            let rhs = self.parse_term(false)?;
            let rhs = match op.value.assoc() {
                Associativity::Left => {
                    if self.get_cur_op_prec().is_some_and(|prec| cur_prec < prec) {
                        self.parse_full_expr(cur_prec + 1, rhs)?
                    } else {
                        rhs
                    }
                }
                Associativity::Right => {
                    if self.get_cur_op_prec().is_some_and(|prec| cur_prec <= prec) {
                        self.parse_full_expr(cur_prec, rhs)?
                    } else {
                        rhs
                    }
                }
            };

            lhs = Expr::Binary {
                op,
                tp: Rc::new(Type::Undef),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }
    }
}

fn bin_op_from_token(token: &Token) -> Option<BinaryOp> {
    match token {
        Token::Plus => Some(BinaryOp::Plus),
        Token::Minus => Some(BinaryOp::Minus),
        Token::Mul => Some(BinaryOp::Mul),
        Token::Assign => Some(BinaryOp::Assign),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    fn create_parser(input: &'static str) -> Parser {
        Parser::new(lex(input))
    }

    #[test]
    fn fn_params() {
        // empty
        let mut parser = create_parser("() type");
        let res = parser.parse_fn_params();
        assert!(res.is_ok());
        let params = res.unwrap();
        assert_eq!(params.len(), 0);
        assert_eq!(parser.lexemes.next().value, Token::Type);

        // single param
        let mut parser = create_parser("(foo: bool) 37");
        let res = parser.parse_fn_params();
        assert!(res.is_ok());
        let params = res.unwrap();
        assert_eq!(params.len(), 1);
        assert_eq!(params[0].name.value, "foo");
        assert_eq!(*params[0].tp.value, Type::Bool);
        assert!(!params[0].is_mut);
        assert_eq!(parser.lexemes.next().value, Token::Num(37));

        // multiple params
        let mut parser = create_parser("(foo: bool, mut bar: i64, c: i64) fn");
        let res = parser.parse_fn_params();
        assert!(res.is_ok());
        let params = res.unwrap();
        assert_eq!(params.len(), 3);
        assert_eq!(
            params
                .into_iter()
                .map(|p| (p.is_mut, p.name.value, p.tp.value))
                .collect::<Vec<(bool, String, Rc<Type>)>>(),
            vec![
                (false, "foo".to_string(), Rc::new(Type::Bool)),
                (true, "bar".to_string(), Rc::new(Type::I64)),
                (false, "c".to_string(), Rc::new(Type::I64))
            ]
        );
        assert_eq!(parser.lexemes.next().value, Token::Fn);
    }

    #[test]
    fn call_expr() {
        let mut parser = create_parser("()");

        let callee = create_parser("42").parse_expr(true).unwrap();
        let res = parser.parse_call_expr(callee.clone());
        assert!(res.is_ok());
        let expr = res.unwrap();
        assert!(matches!(expr, Expr::Call { .. }));

        match expr {
            Expr::Call { callee, args, .. } => {
                assert!(matches!(
                    *callee,
                    Expr::Num {
                        value: WithSpan { value: 42, .. },
                        ..
                    }
                ));
                assert!(args.is_empty());
            }
            _ => unreachable!(),
        }

        let mut parser = create_parser("(4, (5))");
        let res = parser.parse_call_expr(callee);
        assert!(res.is_ok());
        let expr = res.unwrap();
        assert!(matches!(expr, Expr::Call { .. }));

        match expr {
            Expr::Call { callee, args, .. } => {
                assert!(matches!(
                    *callee,
                    Expr::Num {
                        value: WithSpan { value: 42, .. },
                        ..
                    }
                ));

                assert_eq!(args.len(), 2);
                assert!(matches!(
                    args[0],
                    Expr::Num {
                        value: WithSpan { value: 4, .. },
                        ..
                    }
                ));
                assert!(matches!(
                    args[1],
                    Expr::Num {
                        value: WithSpan { value: 5, .. },
                        ..
                    }
                ));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn term() {
        let mut parser = create_parser("4");
        let res = parser.parse_term(true);
        assert!(res.is_ok());
        let expr = res.unwrap();
        assert!(matches!(
            expr,
            Expr::Num {
                value: WithSpan { value: 4, .. },
                ..
            }
        ));

        let mut parser = create_parser("foo");
        let res = parser.parse_term(true);
        assert!(res.is_ok());
        let expr = res.unwrap();
        assert!(
            match &expr {
                Expr::Ref { name, .. } => name.value.as_str() == "foo",
                _ => false,
            },
            "got {:?} instead of ref",
            expr
        );

        let mut parser = create_parser("(4 + 5)");
        let res = parser.parse_term(true);
        assert!(res.is_ok());
        let expr = res.unwrap();

        assert!(matches!(expr, Expr::Binary { .. }));
        match expr {
            Expr::Binary { op, lhs, rhs, .. } => {
                assert!(matches!(
                    op,
                    WithSpan {
                        value: BinaryOp::Plus,
                        ..
                    }
                ));
                assert!(matches!(
                    *lhs,
                    Expr::Num {
                        value: WithSpan { value: 4, .. },
                        ..
                    }
                ));
                assert!(matches!(
                    *rhs,
                    Expr::Num {
                        value: WithSpan { value: 5, .. },
                        ..
                    }
                ));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn parse_expr() {
        let mut parser = create_parser("4 + 5 * 6");
        let res = parser.parse_expr(true);
        assert!(res.is_ok());
        let expr = res.unwrap();

        assert!(matches!(expr, Expr::Binary { .. }));
        match expr {
            Expr::Binary { op, lhs, rhs, .. } => {
                assert!(matches!(
                    op,
                    WithSpan {
                        value: BinaryOp::Plus,
                        ..
                    }
                ));
                assert!(matches!(
                    *lhs,
                    Expr::Num {
                        value: WithSpan { value: 4, .. },
                        ..
                    }
                ));
                assert!(matches!(*rhs, Expr::Binary { .. }));
                match *rhs {
                    Expr::Binary { op, lhs, rhs, .. } => {
                        assert!(matches!(
                            op,
                            WithSpan {
                                value: BinaryOp::Mul,
                                ..
                            }
                        ));
                        assert!(matches!(
                            lhs.as_ref(),
                            Expr::Num {
                                value: WithSpan { value: 5, .. },
                                ..
                            }
                        ));
                        assert!(matches!(
                            rhs.as_ref(),
                            Expr::Num {
                                value: WithSpan { value: 6, .. },
                                ..
                            }
                        ));
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn block() {
        let mut parser = create_parser("{ mut x: i64 = 4\n  y := 2\n  42\n}");
        let res = parser.parse_term(true);

        println!("{:?}", res);

        assert!(res.is_ok());

        let expr = res.unwrap();
        assert!(matches!(expr, Expr::Block { .. }));

        let block = match expr {
            Expr::Block { body, .. } => body,
            _ => unreachable!(),
        };

        assert_eq!(block.len(), 3);

        assert!(matches!(block[0], Expr::Declare { .. }));
        match &block[0] {
            Expr::Declare {
                is_mut,
                name,
                tp,
                value,
            } => {
                assert!(is_mut);
                assert_eq!(name.value.as_str(), "x");
                assert_eq!(*tp.value, Type::I64);
                assert!(matches!(
                    **value,
                    Expr::Num {
                        value: WithSpan { value: 4, .. },
                        ..
                    }
                ));
            }
            _ => unreachable!(),
        }

        assert!(matches!(block[1], Expr::Declare { .. }));
        match &block[1] {
            Expr::Declare {
                is_mut,
                name,
                tp,
                value,
            } => {
                assert!(!is_mut);
                assert_eq!(name.value.as_str(), "y");
                assert_eq!(*tp.value, Type::Undef);
                assert!(matches!(
                    **value,
                    Expr::Num {
                        value: WithSpan { value: 2, .. },
                        ..
                    }
                ));
            }
            _ => unreachable!(),
        }

        assert!(matches!(
            block[2],
            Expr::Num {
                value: WithSpan { value: 42, .. },
                ..
            }
        ));
    }
}
