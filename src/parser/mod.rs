use crate::ast::{BinaryOp, Block, Expr, FnDecl, OpPrecedence, Stmt};
use crate::lexer::{Lexeme, Lexemes, Span, Token, WithSpan};
use crate::typesystem::types::Type;

mod res;
pub use res::ParseError;
use res::Res;

#[derive(Debug)]
pub struct Parser<L: Lexemes> {
    lexemes: L,
}

macro_rules! filter_token {
    ($($p:pat),*) => {
        |_token|
        {
            match _token {
                $($p => true),*,
                _ => false
            }
        }
    };
}

macro_rules! filter_map_token {
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

impl<L: Lexemes> Parser<L> {
    pub fn new(lexemes: L) -> Self {
        Self { lexemes }
    }

    pub fn parse(&mut self) -> Res<Vec<FnDecl>> {
        let mut decls = Vec::new();
        while !self.lexemes.is_eof() {
            match self.parse_decl() {
                Ok(decl) => decls.push(decl),
                Err(err) => return Err(err),
            }
        }

        Ok(decls)
    }

    fn try_get<T>(&mut self, matcher: impl FnOnce(Token) -> Option<T>) -> Option<WithSpan<T>> {
        let ws = self.lexemes.peek();
        if let Some(res) = matcher(ws.value) {
            self.lexemes.next();
            Some(WithSpan::new(res, ws.span))
        } else {
            None
        }
    }

    fn eat_if(&mut self, matcher: impl FnOnce(&Token) -> bool) {
        let WithSpan { value: token, .. } = self.lexemes.peek();
        if matcher(&token) {
            self.lexemes.next();
        }
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
        let ws = self.lexemes.next();
        if let Some(res) = matcher(ws.value.clone()) {
            Ok(ws.replace(res))
        } else {
            let err = ParseError::new(expected, ws.value.to_string(), ws.span);
            Err(err)
        }
    }

    fn parse_id(&mut self) -> Res<WithSpan<String>> {
        self.get_map(
            filter_map_token!(Token::Id(id) => id),
            expected!("identifier"),
        )
    }

    fn parse_type(&mut self) -> Res<WithSpan<Type>> {
        let WithSpan { value: tp, span } = self.get_map(
            filter_map_token!(Token::Id(tp) => Type::from_str(&tp).ok_or(tp)),
            expected!("type"),
        )?;
        let tp = tp.map_err(|id| ParseError::new(expected!("type"), id, span))?;
        Ok(WithSpan::new(tp, span))
    }

    fn parse_decl(&mut self) -> Res<FnDecl> {
        self.get(filter_token!(Token::Fn), expected!(Token::Fn))?;

        let name = self.parse_id()?;
        let params = self.parse_fn_params()?;

        let WithSpan {
            value: tp,
            span: tp_span,
        } = self
            .try_get(filter_map_token!(Token::Id(tp) => Type::from_str(&tp).ok_or(tp)))
            .unwrap_or_else(|| {
                let tp_dummy_span = self.lexemes.peek().span;
                WithSpan::new(Ok(Type::Unit), tp_dummy_span)
            });
        let tp = tp.map_err(|id| ParseError::new(expected!("type"), id, tp_span))?;

        self.get(filter_token!(Token::Assign), expected!(Token::Assign))?;

        let body = self.parse_block_or_expr()?;

        let decl = FnDecl {
            name: name.value,
            tp,
            params,
            body,
        };
        Ok(decl)
    }

    fn parse_fn_params(&mut self) -> Res<Vec<(String, Type)>> {
        self.get(filter_token!(Token::LParen), expected!(Token::LParen))?;

        let mut res = Vec::new();

        while Token::RParen != self.lexemes.peek().value {
            let name = self.parse_id()?;
            self.get(filter_token!(Token::Colon), expected!(Token::Colon))?;

            let tp = self.parse_type()?;

            res.push((name.value, tp.value));

            // TODO: trailing comma
            self.eat_if(filter_token!(Token::Comma));
        }
        self.get(filter_token!(Token::RParen), expected!(Token::RParen))?;

        Ok(res)
    }

    fn parse_block_or_expr(&mut self) -> Res<Block> {
        // here we should parse either single expression on the same line
        // or a block starting on a new line because it is either
        //
        // fn foo() = expr
        //
        // or
        //
        // fn foo() =
        //   some
        //   statements
        //   here
        //
        // and the following is ugly (and weird if the first line is a declaration):
        //
        // fn foo() = expr
        //   then
        //   some
        //   more
        //
        // these examples are valid with conditionals and loops as well

        if self.lexemes.peek().value != Token::EOL {
            let expr = self.parse_expr()?;
            let stmt = Stmt::Expr(expr);
            let body = vec![stmt];
            self.get(filter_token!(Token::EOL), expected!(Token::EOL))?;
            return Ok(body);
        }

        self.eat_while(filter_token!(Token::EOL));
        self.get(
            filter_token!(Token::ScopeStart),
            expected!("indented block"),
        )?;

        let mut body = Vec::new();
        while self.lexemes.peek().value != Token::ScopeEnd {
            self.eat_while(filter_token!(Token::EOL));
            body.push(self.parse_stmt()?);
        }

        self.get(filter_token!(Token::ScopeEnd), expected!("scope end"))
            .expect("scopes should be closed");

        Ok(body)
    }

    fn parse_stmt(&mut self) -> Res<Stmt> {
        let stmt = match (self.lexemes.peek().value, self.lexemes.peek_nth(1).value) {
            (Token::Id(_), Token::Define) => self.parse_definition_stmt(),
            (Token::Id(_), Token::Colon) => self.parse_declaration_stmt(),
            _ => self.parse_expr().map(|x| Stmt::Expr(x)),
        }?;

        let WithSpan { value: token, span } = self.lexemes.peek();
        match token {
            Token::EOL => self.eat_while(filter_token!(Token::EOL)),
            Token::ScopeEnd => {}
            t => {
                return Err(ParseError::new(
                    expected!("end of statement"),
                    t.to_string(),
                    span,
                ));
            }
        }
        Ok(stmt)
    }

    fn parse_definition_stmt(&mut self) -> Res<Stmt> {
        // NAME := VALUE

        let name = self.parse_id()?;
        self.get(filter_token!(Token::Define), expected!(Token::Define))?;
        let value = self.parse_expr()?;

        Ok(Stmt::Declare(name.value, Type::Undef, value))
    }

    fn parse_declaration_stmt(&mut self) -> Res<Stmt> {
        // NAME: TYPE = VALUE

        let name = self.parse_id()?;
        self.get(filter_token!(Token::Colon), expected!(Token::Colon))?;
        let tp = self.parse_type()?;
        self.get(filter_token!(Token::Assign), expected!(Token::Assign))?;
        let value = self.parse_expr()?;

        Ok(Stmt::Declare(name.value, tp.value, value))
    }

    fn parse_expr(&mut self) -> Res<Expr> {
        let lhs = self.parse_term()?;
        self.parse_full_expr(0, lhs)
    }

    fn parse_term(&mut self) -> Res<Expr> {
        let WithSpan { value: token, span } = self.lexemes.peek();
        let term = match token {
            Token::LParen => {
                self.lexemes.next();
                let res = self.parse_expr()?;
                self.get(filter_token!(Token::RParen), expected!(Token::RParen))?;
                Ok(res)
            }
            Token::Id(name) => {
                self.lexemes.next();
                Ok(Expr::Ref {
                    tp: Type::Undef,
                    name,
                })
            }
            Token::Num(value) => {
                self.lexemes.next();
                Ok(Expr::Num {
                    tp: Type::I64, // TODO: unhardcode this
                    value,
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
        self.get(filter_token!(Token::LParen), expected!(Token::LParen))?;

        let mut args = Vec::new();

        while Token::RParen != self.lexemes.peek().value {
            let arg = self.parse_expr()?;
            args.push(arg);

            // TODO: trailing comma
            self.eat_if(filter_token!(Token::Comma));
        }
        self.get(filter_token!(Token::RParen), expected!(Token::RParen))?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            args,
        })
    }

    fn get_cur_op_prec(&self) -> Option<OpPrecedence> {
        bin_op_from_token(&self.lexemes.peek().value).map(|op| op.prec())
    }

    fn parse_full_expr(&mut self, prev_prec: OpPrecedence, mut lhs: Expr) -> Res<Expr> {
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
                op: op.value,
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
        _ => None,
    }
}

#[allow(dead_code)]
mod tests {
    use super::*;
    use crate::{ast::print::print_decl, lexer::lex};

    #[test]
    fn huh() {
        let lexemes = lex("fn foo(a: i64, b: bool) =\n  x := f(4) - 42\n  y : i64 = 4\n  x + y");
        let mut parser = Parser::new(lexemes);

        print_decl(&parser.parse_decl().unwrap());
        assert!(false);
    }
}
