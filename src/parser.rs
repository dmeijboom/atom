use std::{borrow::Cow, collections::VecDeque};

use crate::{
    ast::{BinaryOp, Expr, Literal, Stmt, UnaryOp},
    lexer::{Token, TokenKind},
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected token {actual}, expected: {expected}")]
    UnexpectedToken {
        expected: Cow<'static, str>,
        actual: Token,
    },
    #[error("unexpected eof")]
    UnexpectedEof,
    #[error("invalid expr: {0}")]
    InvalidExpr(TokenKind),
}

pub struct Parser {
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: VecDeque::from(tokens),
        }
    }

    fn hdr(&self) -> Option<&Token> {
        self.tokens.front()
    }

    fn advance(&mut self) {
        let _ = self.next();
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), Error> {
        match self.next() {
            Some(token) if token.kind == expected => Ok(()),
            Some(token) => Err(Error::UnexpectedToken {
                expected: Cow::Owned(format!("{expected}")),
                actual: token.clone(),
            }),
            None => Err(Error::UnexpectedEof),
        }
    }

    fn accept(&mut self, kind: &TokenKind) -> bool {
        if matches!(self.hdr(), Some(token) if &token.kind == kind) {
            self.advance();
            return true;
        }

        false
    }

    fn array(&mut self) -> Result<Expr, Error> {
        let items = self.expr_list(TokenKind::SqrBracketRight, 1)?;
        Ok(Expr::Array(items))
    }

    fn semi(&mut self) -> Result<(), Error> {
        self.expect(TokenKind::Semi)
    }

    fn arg_list(&mut self, end: TokenKind) -> Result<Vec<String>, Error> {
        let mut args = vec![];

        while !self.accept(&end) {
            args.push(self.ident()?);

            if !self.accept(&TokenKind::Comma) {
                self.expect(end)?;
                break;
            }
        }

        Ok(args)
    }

    fn expr_list(&mut self, end: TokenKind, min_prec: u8) -> Result<Vec<Expr>, Error> {
        let mut exprs = vec![];

        while !self.accept(&end) {
            exprs.push(self.expr(min_prec)?);

            if !self.accept(&TokenKind::Comma) {
                self.expect(end)?;
                break;
            }
        }

        Ok(exprs)
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        match self.next() {
            Some(token) => match token.kind {
                TokenKind::IntLit(i) => Ok(Expr::Literal(Literal::Int(i))),
                TokenKind::FloatLit(f) => Ok(Expr::Literal(Literal::Float(f))),
                TokenKind::BoolLit(b) => Ok(Expr::Literal(Literal::Bool(b))),
                TokenKind::StringLit(s) => Ok(Expr::Literal(Literal::String(s))),
                TokenKind::Ident(id) => Ok(Expr::Ident(id)),
                TokenKind::SqrBracketLeft => Ok(self.array()?),
                kind => Err(Error::InvalidExpr(kind)),
            },
            None => Err(Error::UnexpectedEof),
        }
    }

    fn compute(&mut self, expr: Expr, min_prec: u8) -> Result<Expr, Error> {
        let lhs = match self.hdr() {
            Some(token) => match token.kind {
                TokenKind::Dot if min_prec <= 4 => {
                    self.advance();
                    let rhs = self.expr(min_prec + 1)?;
                    Expr::Member(Box::new(expr), Box::new(rhs))
                }
                TokenKind::ParentLeft if min_prec <= 4 => {
                    self.advance();
                    let args = self.expr_list(TokenKind::ParentRight, min_prec + 1)?;
                    Expr::Call(Box::new(expr), args)
                }
                TokenKind::Mul if min_prec <= 2 => {
                    self.advance();
                    let rhs = self.expr(min_prec + 1)?;
                    Expr::Binary(Box::new(expr), BinaryOp::Mul, Box::new(rhs))
                }
                TokenKind::Div if min_prec <= 2 => {
                    self.advance();
                    let rhs = self.expr(min_prec + 1)?;
                    Expr::Binary(Box::new(expr), BinaryOp::Div, Box::new(rhs))
                }
                TokenKind::Add if min_prec <= 1 => {
                    self.advance();
                    let rhs = self.expr(min_prec + 1)?;
                    Expr::Binary(Box::new(expr), BinaryOp::Add, Box::new(rhs))
                }
                TokenKind::Sub if min_prec <= 1 => {
                    self.advance();
                    let rhs = self.expr(min_prec + 1)?;
                    Expr::Binary(Box::new(expr), BinaryOp::Sub, Box::new(rhs))
                }
                _ => return Ok(expr),
            },
            None => return Ok(expr),
        };

        self.compute(lhs, min_prec)
    }

    fn expr(&mut self, min_prec: u8) -> Result<Expr, Error> {
        match self.hdr() {
            Some(token) => match token.kind {
                TokenKind::Not if min_prec <= 3 => {
                    self.advance();
                    let expr = self.expr(min_prec + 1)?;
                    let lhs = Expr::Unary(UnaryOp::Not, Box::new(expr));

                    self.compute(lhs, min_prec + 1)
                }
                _ => {
                    let lhs = self.primary()?;
                    self.compute(lhs, min_prec)
                }
            },
            None => Err(Error::UnexpectedEof),
        }
    }

    fn expr_stmt(&mut self) -> Result<Stmt, Error> {
        let expr = self.expr(1)?;
        self.semi()?;
        Ok(Stmt::Expr(expr))
    }

    fn ident(&mut self) -> Result<String, Error> {
        match self.next() {
            Some(token) => match token.kind {
                TokenKind::Ident(id) => Ok(id),
                _ => Err(Error::UnexpectedToken {
                    expected: Cow::Borrowed("ident"),
                    actual: token,
                }),
            },
            None => Err(Error::UnexpectedEof),
        }
    }

    fn let_stmt(&mut self) -> Result<Stmt, Error> {
        self.advance();
        let name = self.ident()?;
        self.expect(TokenKind::Eq)?;
        let value = self.expr(1)?;
        self.semi()?;

        Ok(Stmt::Let(name, value))
    }

    fn fn_stmt(&mut self) -> Result<Stmt, Error> {
        self.advance();
        let name = self.ident()?;
        self.expect(TokenKind::ParentLeft)?;
        let args = self.arg_list(TokenKind::ParentRight)?;
        self.expect(TokenKind::BracketLeft)?;
        let body = self.body(false)?;
        self.expect(TokenKind::BracketRight)?;

        Ok(Stmt::Fn(name, args, body))
    }

    fn body(&mut self, global: bool) -> Result<Vec<Stmt>, Error> {
        let mut stmts = vec![];

        while let Some(token) = self.hdr() {
            let stmt = match &token.kind {
                TokenKind::Eof => break,
                TokenKind::Keyword(keyword) if keyword == "let" => self.let_stmt()?,
                TokenKind::Keyword(keyword) if keyword == "fn" => self.fn_stmt()?,
                TokenKind::BracketRight if !global => break,
                _ => self.expr_stmt()?,
            };

            stmts.push(stmt);
        }

        Ok(stmts)
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, Error> {
        self.body(true)
    }
}
