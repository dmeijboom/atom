use std::{borrow::Cow, collections::VecDeque};

use crate::{
    ast::{BinaryOp, Expr, ExprKind, Literal, Stmt, StmtKind, UnaryOp},
    lexer::{Span, Token, TokenKind},
};

const PREC_ASS: u8 = 1;
const PREC_EQ: u8 = 2;
const PREC_REL: u8 = 3;
const PREC_ADD: u8 = 4;
const PREC_MUL: u8 = 5;
const PREC_PRE: u8 = 6;
const PREC_CALL: u8 = 7;
const PREC_GROUP: u8 = 8;

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

    fn hdr2(&self) -> (Option<&Token>, Option<&Token>) {
        (self.tokens.front(), self.tokens.get(1))
    }

    fn span(&self) -> Span {
        self.tokens
            .front()
            .map(|t| t.span.clone())
            .unwrap_or_default()
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

    fn array(&mut self) -> Result<ExprKind, Error> {
        let items = self.expr_list(TokenKind::SqrBracketRight, 1)?;
        Ok(ExprKind::Array(items))
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
                TokenKind::IntLit(i) => Ok(ExprKind::Literal(Literal::Int(i)).at(token.span)),
                TokenKind::FloatLit(f) => Ok(ExprKind::Literal(Literal::Float(f)).at(token.span)),
                TokenKind::BoolLit(b) => Ok(ExprKind::Literal(Literal::Bool(b)).at(token.span)),
                TokenKind::StringLit(s) => Ok(ExprKind::Literal(Literal::String(s)).at(token.span)),
                TokenKind::Ident(id) => Ok(ExprKind::Ident(id).at(token.span)),
                TokenKind::SqrBracketLeft => Ok(self.array()?.at(token.span)),
                kind => Err(Error::InvalidExpr(kind)),
            },
            None => Err(Error::UnexpectedEof),
        }
    }

    fn binary(&mut self, lhs: Expr, op: BinaryOp, min_prec: u8) -> Result<Expr, Error> {
        let span = lhs.span.clone();
        self.advance();
        let rhs = self.expr(min_prec + 1)?;

        Ok(ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)).at(span))
    }

    fn compute(&mut self, expr: Expr, min_prec: u8) -> Result<Expr, Error> {
        let lhs = match self.hdr2() {
            (Some(token), next) => match (&token.kind, next.map(|t| &t.kind)) {
                (TokenKind::Dot, _) if min_prec <= PREC_CALL => {
                    self.advance();
                    let rhs = self.expr(PREC_CALL + 1)?;
                    let span = expr.span.clone();

                    ExprKind::Member(Box::new(expr), Box::new(rhs)).at(span)
                }
                (TokenKind::ParentLeft, _) if min_prec <= PREC_CALL => {
                    self.advance();
                    let args = self.expr_list(TokenKind::ParentRight, PREC_CALL + 1)?;
                    let span = expr.span.clone();

                    ExprKind::Call(Box::new(expr), args).at(span)
                }
                (TokenKind::Mul, _) if min_prec <= PREC_MUL => {
                    self.binary(expr, BinaryOp::Mul, PREC_MUL)?
                }
                (TokenKind::Div, _) if min_prec <= PREC_MUL => {
                    self.binary(expr, BinaryOp::Div, PREC_MUL)?
                }
                (TokenKind::Add, _) if min_prec <= PREC_ADD => {
                    self.binary(expr, BinaryOp::Add, PREC_ADD)?
                }
                (TokenKind::Sub, _) if min_prec <= PREC_ADD => {
                    self.binary(expr, BinaryOp::Sub, PREC_ADD)?
                }
                (TokenKind::Lt, Some(TokenKind::Eq)) if min_prec <= PREC_REL => {
                    self.advance();
                    self.binary(expr, BinaryOp::Lte, PREC_REL)?
                }
                (TokenKind::Gt, Some(TokenKind::Eq)) if min_prec <= PREC_REL => {
                    self.advance();
                    self.binary(expr, BinaryOp::Gte, PREC_REL)?
                }
                (TokenKind::Lt, _) if min_prec <= PREC_REL => {
                    self.binary(expr, BinaryOp::Lt, PREC_REL)?
                }
                (TokenKind::Gt, _) if min_prec <= PREC_REL => {
                    self.binary(expr, BinaryOp::Gt, PREC_REL)?
                }
                (TokenKind::Eq, Some(TokenKind::Eq)) if min_prec <= PREC_EQ => {
                    self.advance();
                    self.binary(expr, BinaryOp::Eq, PREC_EQ)?
                }
                (TokenKind::Not, Some(TokenKind::Eq)) if min_prec <= PREC_EQ => {
                    self.advance();
                    self.binary(expr, BinaryOp::Ne, PREC_EQ)?
                }
                (TokenKind::Eq, _) if min_prec <= PREC_ASS => {
                    self.binary(expr, BinaryOp::Assign, PREC_ASS - 1)?
                }
                _ => return Ok(expr),
            },
            (None, _) => return Ok(expr),
        };

        self.compute(lhs, min_prec)
    }

    fn expr(&mut self, min_prec: u8) -> Result<Expr, Error> {
        match self.hdr() {
            Some(token) => match token.kind {
                TokenKind::ParentLeft if min_prec <= PREC_GROUP => {
                    self.advance();
                    let lhs = self.expr(PREC_GROUP + 1)?;
                    let expr = self.compute(lhs, 1)?;
                    self.expect(TokenKind::ParentRight)?;

                    Ok(expr)
                }
                TokenKind::Not if min_prec <= PREC_PRE => {
                    let span = self.span();
                    self.advance();
                    let expr = self.expr(PREC_PRE + 1)?;
                    let lhs = ExprKind::Unary(UnaryOp::Not, Box::new(expr)).at(span);

                    self.compute(lhs, 1)
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
        let span = self.span();
        let expr = self.expr(1)?;
        self.semi()?;

        Ok(StmtKind::Expr(expr).at(span))
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
        let span = self.span();
        self.advance();
        let name = self.ident()?;
        self.expect(TokenKind::Eq)?;
        let value = self.expr(1)?;
        self.semi()?;

        Ok(StmtKind::Let(name, value).at(span))
    }

    fn fn_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        self.advance();
        let name = self.ident()?;
        self.expect(TokenKind::ParentLeft)?;
        let args = self.arg_list(TokenKind::ParentRight)?;
        self.expect(TokenKind::BracketLeft)?;
        let body = self.body(false)?;
        self.expect(TokenKind::BracketRight)?;

        Ok(StmtKind::Fn(name, args, body).at(span))
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
