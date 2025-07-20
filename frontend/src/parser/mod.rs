use crate::ast::{BinaryOp, Expr, FnDecl, FnParam, OpPrecedence};
use crate::lexer::{Lexeme, Lexemes, Span, Token, WithSpan};
use crate::typesystem::types::Type;

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

    pub fn parse(&mut self) -> Res<Vec<FnDecl>> {
        let mut decls = Vec::new();

        self.eat_while(token!(Token::EOL));
        while !self.lexemes.is_eof() {
            match self.parse_fn_decl() {
                Ok(decl) => decls.push(decl),
                Err(err) => return Err(err),
            }
            self.eat_while(token!(Token::EOL));
        }

        Ok(decls)
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

    fn parse_type(&mut self) -> Res<WithSpan<Type>> {
        let WithSpan { value: id, span } =
            self.get_map(token!(Token::Id(id) => id), expected!("type"))?;

        let tp = match Type::primitive_from_str(&id) {
            Some(tp) => Ok(tp),
            None => Err(ParseError::new(expected!("type"), id, span)),
        }?;

        Ok(WithSpan::new(tp, span))
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
            name: name,
            tp: tp,
            params,
            body,
        };

        Ok(decl)
    }

    fn parse_fn_params(&mut self) -> Res<Vec<FnParam>> {
        self.get(token!(Token::LParen), expected!(Token::LParen))?;

        let mut res = Vec::new();

        while Token::RParen != self.lexemes.peek().value {
            // TODO: add `eat_while(EOL)` everywhere for flexibility

            let is_mut = self.eat_if(token!(Token::Mut));

            let name = self.parse_id()?;
            self.get(token!(Token::Colon), expected!(Token::Colon))?;

            let tp = self.parse_type()?;

            res.push(FnParam { is_mut, name, tp });

            // TODO: trailing comma
            self.eat_if(token!(Token::Comma));
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
            _ => self.parse_expr(),
        }
    }

    fn parse_ret_expr(&mut self) -> Res<Expr> {
        let WithSpan { span: kw_span, .. } = self.get(token!(Token::Ret), expected!(Token::Ret))?;

        let state = self.lexemes.get_state();
        let value = self.parse_expr().ok().map(|e| Box::new(e));
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

        let value = self.parse_expr()?;
        Ok(Expr::Declare {
            is_mut,
            name: name,
            tp: tp,
            value: Box::new(value),
        })
    }

    fn parse_expr(&mut self) -> Res<Expr> {
        let lhs = self.parse_term()?;
        self.parse_full_expr(0, lhs)
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
            tp: Type::Undef,
            span,
        })
    }

    fn parse_term(&mut self) -> Res<Expr> {
        let WithSpan { value: token, span } = self.lexemes.peek();
        let term = match token {
            Token::LBrace => self.parse_block(),
            Token::LParen => {
                self.lexemes.next();
                let res = self.parse_expr()?;
                self.get(token!(Token::RParen), expected!(Token::RParen))?;
                Ok(res)
            }
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
                    tp: Type::Undef,
                    name: WithSpan::new(name, span),
                })
            }
            Token::Num(value) => {
                self.lexemes.next();
                Ok(Expr::Num {
                    tp: Type::I64, // TODO: unhardcode this
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
            let arg = self.parse_expr()?;
            args.push(arg);

            // TODO: trailing comma
            self.eat_if(token!(Token::Comma));
        }

        let rparen_span = self
            .get(token!(Token::RParen), expected!(Token::RParen))?
            .span;
        let span = Span::new(callee.span().start, rparen_span.end);

        Ok(Expr::Call {
            tp: Type::Undef,
            callee: Box::new(callee),
            args,
            span,
        })
    }

    fn get_cur_op_prec(&self) -> Option<OpPrecedence> {
        bin_op_from_token(&self.lexemes.peek().value).map(|op| op.prec())
    }

    fn parse_full_expr(&mut self, prev_prec: OpPrecedence, mut lhs: Expr) -> Res<Expr> {
        // TODO: handle operator associativity

        loop {
            let cur_prec = match self.get_cur_op_prec() {
                None => return Ok(lhs),
                Some(prec) if prec < prev_prec => return Ok(lhs),
                Some(prec) => prec,
            };

            let op = self.get_map(|t| bin_op_from_token(&t), expected!("binary operator"))?;

            let rhs = self.parse_term()?;
            let rhs = if self.get_cur_op_prec().is_some_and(|prec| cur_prec < prec) {
                self.parse_full_expr(cur_prec + 1, rhs)?
            } else {
                rhs
            };

            lhs = Expr::Binary {
                op,
                tp: Type::Undef,
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
        assert_eq!(params[0].name.value, "foo".to_string());
        assert_eq!(params[0].tp.value, Type::Bool);
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
                .collect::<Vec<(bool, String, Type)>>(),
            vec![
                (false, "foo".to_string(), Type::Bool),
                (true, "bar".to_string(), Type::I64),
                (false, "c".to_string(), Type::I64)
            ]
        );
        assert_eq!(parser.lexemes.next().value, Token::Fn);
    }

    #[test]
    fn call_expr() {
        let mut parser = create_parser("()");

        let callee = create_parser("42").parse_expr().unwrap();
        let res = parser.parse_call_expr(callee.clone());
        assert!(res.is_ok());
        let expr = res.unwrap();
        assert!(matches!(expr, Expr::Call { .. }));

        match expr {
            Expr::Call { callee, args, .. } => {
                assert!(matches!(
                    *callee,
                    Expr::Num {
                        tp: Type::I64,
                        value: WithSpan { value: 42, .. }
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
                        tp: Type::I64,
                        value: WithSpan { value: 42, .. }
                    }
                ));

                assert_eq!(args.len(), 2);
                assert!(matches!(
                    args[0],
                    Expr::Num {
                        tp: Type::I64,
                        value: WithSpan { value: 4, .. }
                    }
                ));
                assert!(matches!(
                    args[1],
                    Expr::Num {
                        tp: Type::I64,
                        value: WithSpan { value: 5, .. }
                    }
                ));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn term() {
        let mut parser = create_parser("4");
        let res = parser.parse_term();
        assert!(res.is_ok());
        let expr = res.unwrap();
        assert!(matches!(
            expr,
            Expr::Num {
                tp: Type::I64,
                value: WithSpan { value: 4, .. }
            }
        ));

        let mut parser = create_parser("foo");
        let res = parser.parse_term();
        assert!(res.is_ok());
        let expr = res.unwrap();
        assert!(
            match &expr {
                Expr::Ref {
                    tp: Type::Undef,
                    name,
                } => name.value.as_str() == "foo",
                _ => false,
            },
            "got {:?} instead of ref",
            expr
        );

        let mut parser = create_parser("(4 + 5)");
        let res = parser.parse_term();
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
                    lhs.as_ref(),
                    Expr::Num {
                        tp: Type::I64,
                        value: WithSpan { value: 4, .. }
                    }
                ));
                assert!(matches!(
                    rhs.as_ref(),
                    Expr::Num {
                        tp: Type::I64,
                        value: WithSpan { value: 5, .. }
                    }
                ));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn parse_expr() {
        let mut parser = create_parser("4 + 5 * 6");
        let res = parser.parse_expr();
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
                    lhs.as_ref(),
                    Expr::Num {
                        tp: Type::I64,
                        value: WithSpan { value: 4, .. }
                    }
                ));
                assert!(matches!(rhs.as_ref(), Expr::Binary { .. }));
                match rhs.as_ref() {
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
                                tp: Type::I64,
                                value: WithSpan { value: 5, .. }
                            }
                        ));
                        assert!(matches!(
                            rhs.as_ref(),
                            Expr::Num {
                                tp: Type::I64,
                                value: WithSpan { value: 6, .. }
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
        let res = parser.parse_top_level_expr();

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
                assert_eq!(tp.value, Type::I64);
                assert!(matches!(
                    value.as_ref(),
                    Expr::Num {
                        tp: Type::I64,
                        value: WithSpan { value: 4, .. }
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
                assert_eq!(tp.value, Type::Undef);
                assert!(matches!(
                    value.as_ref(),
                    Expr::Num {
                        tp: Type::I64,
                        value: WithSpan { value: 2, .. }
                    }
                ));
            }
            _ => unreachable!(),
        }

        assert!(matches!(
            block[2],
            Expr::Num {
                tp: Type::I64,
                value: WithSpan { value: 42, .. }
            }
        ));
    }
}
