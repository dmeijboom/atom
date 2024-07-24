use std::{borrow::Cow, collections::VecDeque};

use crate::{
    ast::{AssignOp, BinaryOp, Expr, ExprKind, IfStmt, Literal, Stmt, StmtKind, UnaryOp},
    lexer::{Span, Token, TokenKind},
};

const PREC_ASS: u8 = 0;
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

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected token {actual}, expected: {expected}")]
    UnexpectedToken {
        expected: Cow<'static, str>,
        actual: String,
    },
    #[error("unexpected eof")]
    UnexpectedEof,
    #[error("invalid expr '{kind}' at: {span}")]
    InvalidExpr { kind: TokenKind, span: Span },
}

fn supports_assign(expr: &Expr) -> bool {
    matches!(
        expr.kind,
        ExprKind::Ident(_) | ExprKind::Member(..) | ExprKind::CompMember(..)
    )
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

    fn test_keyword(&self, keyword: &str) -> bool {
        matches!(self.peek(), Some(TokenKind::Keyword(ref k)) if k == keyword)
    }

    fn peek(&self) -> Option<&TokenKind> {
        self.tokens.front().map(|t| &t.kind)
    }

    fn peek2(&self) -> (Option<&TokenKind>, Option<&TokenKind>) {
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
                actual: token.to_string(),
            }),
            None => Err(Error::UnexpectedToken {
                expected: Cow::Owned(format!("{expected}")),
                actual: "EOF".to_string(),
            }),
        }
    }

    fn accept(&mut self, kind: &TokenKind) -> bool {
        if matches!(self.peek(), Some(token) if token == kind) {
            self.advance();
            return true;
        }

        false
    }

    fn array(&mut self) -> Result<ExprKind, Error> {
        let items = self.expr_list(TokenKind::Punct("["), 1)?;
        Ok(ExprKind::Array(items))
    }

    fn semi(&mut self) -> Result<(), Error> {
        self.expect(TokenKind::Punct(";"))
    }

    fn arg_list(&mut self, end: TokenKind) -> Result<Vec<String>, Error> {
        let mut args = vec![];

        while !self.accept(&end) {
            args.push(self.ident()?);

            if !self.accept(&TokenKind::Punct(",")) {
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

            if !self.accept(&TokenKind::Punct(",")) {
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
                TokenKind::Punct("[") => Ok(self.array()?.at(token.span)),
                TokenKind::Punct("(") => {
                    let lhs = self.expr(1)?;
                    self.expect(TokenKind::Punct(")"))?;
                    Ok(lhs)
                }
                TokenKind::Punct("!") => {
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

    fn assign(&mut self, lhs: Expr, op: Option<AssignOp>) -> Result<Expr, Error> {
        let span = lhs.span;
        self.advance();
        let rhs = self.expr(1)?;

        Ok(ExprKind::Assign(op, Box::new(lhs), Box::new(rhs)).at(span))
    }

    fn expr(&mut self, min_prec: u8) -> Result<Expr, Error> {
        let mut lhs = self.primary()?;

        loop {
            lhs = match self.peek2() {
                (Some(token), next) => match token {
                    TokenKind::Punct(".")
                        if matches!(next, Some(TokenKind::Ident(_))) && min_prec <= PREC_CALL =>
                    {
                        self.advance();
                        let span = lhs.span;
                        let name = self.ident()?;

                        ExprKind::Member(Box::new(lhs), name).at(span)
                    }
                    TokenKind::Punct("[") if min_prec <= PREC_CALL => {
                        self.advance();
                        let rhs = self.expr(1)?;
                        self.expect(TokenKind::Punct("]"))?;
                        let span = lhs.span;

                        ExprKind::CompMember(Box::new(lhs), Box::new(rhs)).at(span)
                    }
                    TokenKind::Punct("(") if min_prec <= PREC_CALL => {
                        self.advance();
                        let args = self.expr_list(TokenKind::Punct(")"), 1)?;
                        let span = lhs.span;

                        ExprKind::Call(Box::new(lhs), args).at(span)
                    }
                    TokenKind::Punct("%") if min_prec <= PREC_MUL => {
                        self.binary(lhs, BinaryOp::Rem, PREC_MUL)?
                    }
                    TokenKind::Punct("*") if min_prec <= PREC_MUL => {
                        self.binary(lhs, BinaryOp::Mul, PREC_MUL)?
                    }
                    TokenKind::Punct("/") if min_prec <= PREC_MUL => {
                        self.binary(lhs, BinaryOp::Div, PREC_MUL)?
                    }
                    TokenKind::Punct("+") if min_prec <= PREC_ADD => {
                        self.binary(lhs, BinaryOp::Add, PREC_ADD)?
                    }
                    TokenKind::Punct("-") if min_prec <= PREC_ADD => {
                        self.binary(lhs, BinaryOp::Sub, PREC_ADD)?
                    }
                    TokenKind::Punct("<=") if min_prec <= PREC_REL => {
                        self.binary(lhs, BinaryOp::Lte, PREC_REL)?
                    }
                    TokenKind::Punct(">=") if min_prec <= PREC_REL => {
                        self.binary(lhs, BinaryOp::Gte, PREC_REL)?
                    }
                    TokenKind::Punct("<") if min_prec <= PREC_REL => {
                        self.binary(lhs, BinaryOp::Lt, PREC_REL)?
                    }
                    TokenKind::Punct(">") if min_prec <= PREC_REL => {
                        self.binary(lhs, BinaryOp::Gt, PREC_REL)?
                    }
                    TokenKind::Punct("==") if min_prec <= PREC_EQ => {
                        self.binary(lhs, BinaryOp::Eq, PREC_EQ)?
                    }
                    TokenKind::Punct("!=") if min_prec <= PREC_EQ => {
                        self.binary(lhs, BinaryOp::Ne, PREC_EQ)?
                    }
                    TokenKind::Punct("&") if min_prec <= PREC_BAN => {
                        self.binary(lhs, BinaryOp::BitwiseAnd, PREC_BAN)?
                    }
                    TokenKind::Punct("^") if min_prec <= PREC_XOR => {
                        self.binary(lhs, BinaryOp::BitwiseXor, PREC_XOR)?
                    }
                    TokenKind::Punct("|") if min_prec <= PREC_BOR => {
                        self.binary(lhs, BinaryOp::BitwiseOr, PREC_BOR)?
                    }
                    TokenKind::Punct("&&") if min_prec <= PREC_LAN => {
                        self.binary(lhs, BinaryOp::LogicalAnd, PREC_LAN)?
                    }
                    TokenKind::Punct("||") if min_prec <= PREC_LOR => {
                        self.binary(lhs, BinaryOp::LogicalOr, PREC_LOR)?
                    }
                    TokenKind::Punct("=") if supports_assign(&lhs) && min_prec == PREC_ASS => {
                        self.assign(lhs, None)?
                    }
                    TokenKind::Punct("+=") if min_prec == PREC_ASS => {
                        self.assign(lhs, Some(AssignOp::Add))?
                    }
                    TokenKind::Punct("-=") if min_prec == PREC_ASS => {
                        self.assign(lhs, Some(AssignOp::Sub))?
                    }
                    TokenKind::Punct("*=") if min_prec == PREC_ASS => {
                        self.assign(lhs, Some(AssignOp::Mul))?
                    }
                    TokenKind::Punct("/=") if min_prec == PREC_ASS => {
                        self.assign(lhs, Some(AssignOp::Div))?
                    }
                    TokenKind::Punct("%=") if min_prec == PREC_ASS => {
                        self.assign(lhs, Some(AssignOp::Rem))?
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
        let expr = self.expr(0)?;

        if self.accept(&TokenKind::Punct(";")) {
            return Ok(StmtKind::Expr(expr).at(span));
        }

        match self.peek() {
            Some(TokenKind::Punct("}")) => Ok(StmtKind::Return(expr).at(span)),
            None => Err(Error::UnexpectedEof),
            _ => Err(Error::UnexpectedToken {
                expected: Cow::Borrowed(";"),
                actual: self.tokens.pop_front().unwrap().to_string(),
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
                    actual: token.to_string(),
                }),
            },
            None => Err(Error::UnexpectedEof),
        }
    }

    fn let_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        self.advance();
        let name = self.ident()?;

        if self.accept(&TokenKind::Punct(";")) {
            return Ok(StmtKind::Let(name, None).at(span));
        }

        self.expect(TokenKind::Punct("="))?;
        let value = self.expr(1)?;
        self.semi()?;

        Ok(StmtKind::Let(name, Some(value)).at(span))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, Error> {
        self.expect(TokenKind::Punct("{"))?;
        let body = self.body(false)?;
        self.expect(TokenKind::Punct("}"))?;

        Ok(body)
    }

    fn fn_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        self.advance();
        let name = self.ident()?;
        self.expect(TokenKind::Punct("("))?;
        let args = self.arg_list(TokenKind::Punct(")"))?;
        let body = self.block()?;

        Ok(StmtKind::Fn(name, args, body).at(span))
    }

    fn if_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        self.advance();
        let expr = self.expr(1)?;
        let body = self.block()?;

        let mut if_stmt = Box::new(IfStmt(Some(expr), body, None));
        let mut cur = &mut if_stmt;

        loop {
            if self.test_keyword("elif") {
                self.advance();
                let expr = self.expr(1)?;
                let body = self.block()?;

                cur.2 = Some(Box::new(IfStmt(Some(expr), body, None)));
                cur = cur.2.as_mut().unwrap();

                continue;
            }

            if self.test_keyword("else") {
                self.advance();
                cur.2 = Some(Box::new(IfStmt(None, self.block()?, None)));
            }

            break;
        }

        Ok(StmtKind::If(*if_stmt).at(span))
    }

    fn for_cond_stmt(&mut self, span: Span, pre: Stmt) -> Result<Stmt, Error> {
        let expr = self.expr(1)?;
        self.semi()?;
        let post = self.expr(0)?;
        let body = self.block()?;

        Ok(StmtKind::ForCond(Box::new(pre), expr, post, body).at(span))
    }

    fn class_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        self.advance();
        let name = self.ident()?;
        self.expect(TokenKind::Punct("{"))?;
        let mut methods = vec![];

        while self.test_keyword("fn") {
            methods.push(self.fn_stmt()?);
        }

        self.expect(TokenKind::Punct("}"))?;
        Ok(StmtKind::Class(name, methods).at(span))
    }

    fn for_stmt(&mut self) -> Result<Stmt, Error> {
        let span = self.span();
        self.advance();

        if self.test_keyword("let") {
            let pre = self.let_stmt()?;
            return self.for_cond_stmt(span, pre);
        }

        let expr = self.expr(0)?;

        if self.accept(&TokenKind::Punct(";")) {
            let span = expr.span;
            return self.for_cond_stmt(span, StmtKind::Expr(expr).at(span));
        }

        let body = self.block()?;
        Ok(StmtKind::For(expr, body).at(span))
    }

    fn body(&mut self, global: bool) -> Result<Vec<Stmt>, Error> {
        let mut stmts = vec![];

        while let Some(token) = self.peek() {
            let stmt = match token {
                TokenKind::Keyword(keyword) if keyword == "let" => self.let_stmt()?,
                TokenKind::Keyword(keyword) if keyword == "fn" => self.fn_stmt()?,
                TokenKind::Keyword(keyword) if keyword == "if" => self.if_stmt()?,
                TokenKind::Keyword(keyword) if keyword == "for" => self.for_stmt()?,
                TokenKind::Keyword(keyword) if keyword == "class" => self.class_stmt()?,
                TokenKind::Keyword(keyword) if keyword == "return" => self.return_stmt()?,
                TokenKind::Punct("}") if !global => break,
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
