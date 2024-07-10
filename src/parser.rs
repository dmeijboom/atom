use std::{borrow::Cow, collections::VecDeque};

use crate::{
    ast::{Expr, Literal, Op, Stmt},
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

    fn array(&mut self) -> Result<Expr, Error> {
        let items = self.expr_list(TokenKind::SqrBracketRight, 1)?;
        Ok(Expr::Array(items))
    }

    fn semi(&mut self) -> Result<(), Error> {
        self.expect(TokenKind::Semi)
    }

    fn expr_list(&mut self, sep: TokenKind, min_prec: u8) -> Result<Vec<Expr>, Error> {
        let mut exprs = vec![];

        loop {
            if matches!(self.hdr(), Some(token) if token.kind == sep) {
                self.advance();
                break;
            }

            exprs.push(self.expr(1)?);

            match self.next() {
                Some(token) if token.kind == TokenKind::Comma => {
                    exprs.push(self.expr(min_prec)?);
                }
                Some(token) if token.kind == sep => {
                    break;
                }
                Some(token) => {
                    return Err(Error::UnexpectedToken {
                        expected: Cow::Owned(format!("{sep}")),
                        actual: token.clone(),
                    })
                }
                None => return Err(Error::UnexpectedEof),
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
        match self.hdr() {
            Some(token) => match token.kind {
                TokenKind::Dot if min_prec <= 3 => {
                    self.advance();
                    let rhs = self.expr(4)?;
                    let lhs = Expr::Member(Box::new(expr), Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                TokenKind::ParentLeft if min_prec <= 3 => {
                    self.advance();
                    let args = self.expr_list(TokenKind::ParentRight, 4)?;
                    let lhs = Expr::Call(Box::new(expr), args);
                    self.compute(lhs, min_prec)
                }
                TokenKind::Mul if min_prec <= 2 => {
                    self.advance();
                    let rhs = self.expr(3)?;
                    let lhs = Expr::Binary(Box::new(expr), Op::Mul, Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                TokenKind::Div if min_prec <= 2 => {
                    self.advance();
                    let rhs = self.expr(3)?;
                    let lhs = Expr::Binary(Box::new(expr), Op::Div, Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                TokenKind::Add if min_prec <= 1 => {
                    self.advance();
                    let rhs = self.expr(2)?;
                    let lhs = Expr::Binary(Box::new(expr), Op::Add, Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                TokenKind::Sub if min_prec <= 1 => {
                    self.advance();
                    let rhs = self.expr(2)?;
                    let lhs = Expr::Binary(Box::new(expr), Op::Sub, Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                _ => Ok(expr),
            },
            None => Ok(expr),
        }
    }

    fn expr(&mut self, min_prec: u8) -> Result<Expr, Error> {
        let lhs = self.primary()?;
        self.compute(lhs, min_prec)
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

    pub fn parse(mut self) -> Result<Vec<Stmt>, Error> {
        let mut stmts = vec![];

        while let Some(token) = self.hdr() {
            let stmt = match &token.kind {
                TokenKind::Eof => break,
                TokenKind::Keyword(keyword) if keyword == "let" => self.let_stmt()?,
                _ => self.expr_stmt()?,
            };

            stmts.push(stmt);
        }

        Ok(stmts)
    }
}
