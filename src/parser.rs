use std::{borrow::Cow, cmp::Ordering, collections::VecDeque};

use crate::{
    ast::{BinaryOp, Expr, ExprKind, Literal, Stmt, StmtKind, UnaryOp},
    lexer::{Span, Token, TokenKind},
};

const PREC_LOR: u8 = 1;
const PREC_LAN: u8 = 2;
const PREC_BOR: u8 = 3;
const PREC_XOR: u8 = 4;
const PREC_BAN: u8 = 5;
const PREC_EQ: u8 = 6;
const PREC_REL: u8 = 7;
const PREC_ADD: u8 = 8;
const PREC_MUL: u8 = 9;
const PREC_PRE: u8 = 10;
const PREC_CALL: u8 = 11;
//const PREC_GROUP: u8 = 12;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected token {actual}, expected: {expected}")]
    UnexpectedToken {
        expected: Cow<'static, str>,
        actual: Token,
    },
    #[error("unexpected eof")]
    UnexpectedEof,
    #[error("invalid expr '{kind}' at: {span}")]
    InvalidExpr { kind: TokenKind, span: Span },
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

    fn hdr(&self) -> Option<&TokenKind> {
        self.tokens.front().map(|t| &t.kind)
    }

    fn hdr2(&self) -> (Option<&TokenKind>, Option<&TokenKind>) {
        (
            self.tokens.front().map(|t| &t.kind),
            self.tokens.get(1).map(|t| &t.kind),
        )
    }

    fn span(&self) -> Span {
        self.tokens.front().map(|t| t.span).unwrap_or_default()
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
        if matches!(self.hdr(), Some(token) if token == kind) {
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
                TokenKind::ParentLeft => {
                    let lhs = self.expr(1)?;
                    self.expect(TokenKind::ParentRight)?;
                    Ok(lhs)
                }
                TokenKind::Not => {
                    let span = self.span();
                    let expr = self.expr(PREC_PRE + 1)?;
                    Ok(ExprKind::Unary(UnaryOp::Not, Box::new(expr)).at(span))
                }
                kind => Err(Error::InvalidExpr {
                    kind,
                    span: token.span,
                }),
            },
            None => Err(Error::UnexpectedEof),
        }
    }

    fn binary(&mut self, lhs: Expr, op: BinaryOp, min_prec: u8) -> Result<Expr, Error> {
        let span = lhs.span;
        self.advance();
        let rhs = self.expr(min_prec + 1)?;

        Ok(ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)).at(span))
    }

    fn expr(&mut self, min_prec: u8) -> Result<Expr, Error> {
        let mut lhs = self.primary()?;

        loop {
            lhs = match self.hdr2() {
                (Some(token), next) => match (token, next) {
                    (TokenKind::Dot, _) if min_prec <= PREC_CALL => {
                        self.advance();
                        let rhs = self.expr(PREC_CALL + 1)?;
                        let span = lhs.span;

                        ExprKind::Member(Box::new(lhs), Box::new(rhs)).at(span)
                    }
                    (TokenKind::SqrBracketLeft, _) if min_prec <= PREC_CALL => {
                        self.advance();
                        let rhs = self.expr(PREC_CALL + 1)?;
                        self.expect(TokenKind::SqrBracketRight)?;
                        let span = lhs.span;

                        ExprKind::CompMember(Box::new(lhs), Box::new(rhs)).at(span)
                    }
                    (TokenKind::ParentLeft, _) if min_prec <= PREC_CALL => {
                        self.advance();
                        let args = self.expr_list(TokenKind::ParentRight, PREC_CALL + 1)?;
                        let span = lhs.span;

                        ExprKind::Call(Box::new(lhs), args).at(span)
                    }
                    (TokenKind::Mul, _) if min_prec <= PREC_MUL => {
                        self.binary(lhs, BinaryOp::Mul, PREC_MUL)?
                    }
                    (TokenKind::Div, _) if min_prec <= PREC_MUL => {
                        self.binary(lhs, BinaryOp::Div, PREC_MUL)?
                    }
                    (TokenKind::Add, _) if min_prec <= PREC_ADD => {
                        self.binary(lhs, BinaryOp::Add, PREC_ADD)?
                    }
                    (TokenKind::Sub, _) if min_prec <= PREC_ADD => {
                        self.binary(lhs, BinaryOp::Sub, PREC_ADD)?
                    }
                    (TokenKind::Lt, Some(TokenKind::Eq)) => match min_prec.cmp(&PREC_REL) {
                        Ordering::Less | Ordering::Equal => {
                            self.advance();
                            self.binary(lhs, BinaryOp::Lte, PREC_REL)?
                        }
                        Ordering::Greater => break,
                    },
                    (TokenKind::Gt, Some(TokenKind::Eq)) => match min_prec.cmp(&PREC_REL) {
                        Ordering::Less | Ordering::Equal => {
                            self.advance();
                            self.binary(lhs, BinaryOp::Gte, PREC_REL)?
                        }
                        Ordering::Greater => break,
                    },
                    (TokenKind::Lt, _) if min_prec <= PREC_REL => {
                        self.binary(lhs, BinaryOp::Lt, PREC_REL)?
                    }
                    (TokenKind::Gt, _) if min_prec <= PREC_REL => {
                        self.binary(lhs, BinaryOp::Gt, PREC_REL)?
                    }
                    (TokenKind::Eq, Some(TokenKind::Eq)) => match min_prec.cmp(&PREC_EQ) {
                        Ordering::Less | Ordering::Equal => {
                            self.advance();
                            self.binary(lhs, BinaryOp::Eq, PREC_EQ)?
                        }
                        Ordering::Greater => break,
                    },
                    (TokenKind::Not, Some(TokenKind::Eq)) => match min_prec.cmp(&PREC_EQ) {
                        Ordering::Less | Ordering::Equal => {
                            self.advance();
                            self.binary(lhs, BinaryOp::Ne, PREC_EQ)?
                        }
                        Ordering::Greater => break,
                    },
                    (TokenKind::And, Some(TokenKind::And)) => match min_prec.cmp(&PREC_LAN) {
                        Ordering::Less | Ordering::Equal => {
                            self.advance();
                            self.binary(lhs, BinaryOp::LogicalAnd, PREC_LAN)?
                        }
                        Ordering::Greater => break,
                    },
                    (TokenKind::Or, Some(TokenKind::Or)) => match min_prec.cmp(&PREC_LOR) {
                        Ordering::Less | Ordering::Equal => {
                            self.advance();
                            self.binary(lhs, BinaryOp::LogicalOr, PREC_LOR)?
                        }
                        Ordering::Greater => break,
                    },
                    (TokenKind::And, _) if min_prec <= PREC_BAN => {
                        self.binary(lhs, BinaryOp::BitwiseAnd, PREC_BAN)?
                    }
                    (TokenKind::Pow, _) if min_prec <= PREC_XOR => {
                        self.binary(lhs, BinaryOp::BitwiseXor, PREC_XOR)?
                    }
                    (TokenKind::Or, _) if min_prec <= PREC_BOR => {
                        self.binary(lhs, BinaryOp::BitwiseOr, PREC_BOR)?
                    }
                    _ => break,
                },
                (None, _) => break,
            };
        }

        Ok(lhs)
    }

    fn expr_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        let expr = self.expr(1)?;

        if self.accept(&TokenKind::Semi) {
            return Ok(StmtKind::Expr(expr).at(span));
        }

        match self.hdr() {
            Some(TokenKind::BracketRight | TokenKind::Eof) => Ok(StmtKind::Return(expr).at(span)),
            None => Err(Error::UnexpectedEof),
            _ => Err(Error::UnexpectedToken {
                expected: Cow::Borrowed(";"),
                actual: self.tokens.pop_front().unwrap(),
            }),
        }
    }

    fn return_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        self.advance();
        let expr = self.expr(1)?;
        self.semi()?;

        Ok(StmtKind::Return(expr).at(span))
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

    fn assign_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        let name = self.ident()?;
        self.expect(TokenKind::Eq)?;
        let value = self.expr(1)?;
        self.semi()?;

        Ok(StmtKind::Assign(name, value).at(span))
    }

    fn body(&mut self, global: bool) -> Result<Vec<Stmt>, Error> {
        let mut stmts = vec![];

        while let (Some(token), next) = self.hdr2() {
            let stmt = match token {
                TokenKind::Eof => break,
                TokenKind::Keyword(keyword) if keyword == "let" => self.let_stmt()?,
                TokenKind::Keyword(keyword) if keyword == "fn" => self.fn_stmt()?,
                TokenKind::Keyword(keyword) if keyword == "return" => self.return_stmt()?,
                TokenKind::Ident(_) if next == Some(&TokenKind::Eq) => self.assign_stmt()?,
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
