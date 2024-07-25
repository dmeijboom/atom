use std::{borrow::Cow, collections::VecDeque};

use crate::{
    ast::{AssignOp, BinaryOp, Expr, ExprKind, IfStmt, Literal, Stmt, StmtKind, UnaryOp},
    error::{IntoSpanned, SpannedError},
    lexer::{Span, Token, TokenKind},
};

struct Prec {}

impl Prec {
    const ASS: u8 = 0;
    const LOR: u8 = 1;
    const LAN: u8 = 2;
    const BOR: u8 = 3;
    const XOR: u8 = 4;
    const BAN: u8 = 5;
    const EQ: u8 = 6;
    const REL: u8 = 7;
    const ADD: u8 = 8;
    const MUL: u8 = 9;
    const PRE: u8 = 10;
    const CALL: u8 = 11;
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unexpected token {actual}, expected: {expected}")]
    UnexpectedToken {
        expected: Cow<'static, str>,
        actual: String,
    },
    #[error("invalid expr: {0}")]
    InvalidExpr(TokenKind),
    #[error("unexpected end-of-file")]
    UnexpectedEof,
}

pub type ParseError = SpannedError<ErrorKind>;

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

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        match self.next() {
            Some(token) if token.kind == expected => Ok(()),
            Some(token) => Err(ErrorKind::UnexpectedToken {
                expected: Cow::Owned(format!("{expected}")),
                actual: token.to_string(),
            }
            .at(self.span())),
            None => Err(ErrorKind::UnexpectedToken {
                expected: Cow::Owned(format!("{expected}")),
                actual: "EOF".to_string(),
            }
            .at(self.span())),
        }
    }

    fn accept(&mut self, kind: &TokenKind) -> bool {
        if matches!(self.peek(), Some(token) if token == kind) {
            self.advance();
            return true;
        }

        false
    }

    fn array(&mut self) -> Result<ExprKind, ParseError> {
        let items = self.expr_list(TokenKind::Punct("]"), 1)?;
        Ok(ExprKind::Array(items))
    }

    fn semi(&mut self) -> Result<(), ParseError> {
        self.expect(TokenKind::Punct(";"))
    }

    fn arg_list(&mut self, end: TokenKind) -> Result<Vec<String>, ParseError> {
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

    fn expr_list(&mut self, end: TokenKind, min_prec: u8) -> Result<Vec<Expr>, ParseError> {
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

    fn primary(&mut self) -> Result<Expr, ParseError> {
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
                    let expr = self.expr(Prec::PRE + 1)?;
                    Ok(ExprKind::Unary(UnaryOp::Not, Box::new(expr)).at(span))
                }
                kind => Err(ErrorKind::InvalidExpr(kind).at(self.span())),
            },
            None => Err(ErrorKind::UnexpectedEof.at(self.span())),
        }
    }

    fn binary(&mut self, lhs: Expr, op: BinaryOp, min_prec: u8) -> Result<Expr, ParseError> {
        let span = lhs.span;
        self.advance();
        let rhs = self.expr(min_prec + 1)?;

        Ok(ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)).at(span))
    }

    fn assign(&mut self, lhs: Expr, op: Option<AssignOp>) -> Result<Expr, ParseError> {
        let span = lhs.span;
        self.advance();
        let rhs = self.expr(1)?;

        Ok(ExprKind::Assign(op, Box::new(lhs), Box::new(rhs)).at(span))
    }

    fn expr(&mut self, min_prec: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.primary()?;

        loop {
            lhs = match self.peek2() {
                (Some(token), next) => match token {
                    TokenKind::Punct(".")
                        if matches!(next, Some(TokenKind::Ident(_))) && min_prec <= Prec::CALL =>
                    {
                        self.advance();
                        let span = lhs.span;
                        let name = self.ident()?;

                        ExprKind::Member(Box::new(lhs), name).at(span)
                    }
                    TokenKind::Punct("[") if min_prec <= Prec::CALL => {
                        self.advance();
                        let rhs = self.expr(1)?;
                        self.expect(TokenKind::Punct("]"))?;
                        let span = lhs.span;

                        ExprKind::CompMember(Box::new(lhs), Box::new(rhs)).at(span)
                    }
                    TokenKind::Punct("(") if min_prec <= Prec::CALL => {
                        self.advance();
                        let args = self.expr_list(TokenKind::Punct(")"), 1)?;
                        let span = lhs.span;

                        ExprKind::Call(Box::new(lhs), args).at(span)
                    }
                    TokenKind::Punct("%") if min_prec <= Prec::MUL => {
                        self.binary(lhs, BinaryOp::Rem, Prec::MUL)?
                    }
                    TokenKind::Punct("*") if min_prec <= Prec::MUL => {
                        self.binary(lhs, BinaryOp::Mul, Prec::MUL)?
                    }
                    TokenKind::Punct("/") if min_prec <= Prec::MUL => {
                        self.binary(lhs, BinaryOp::Div, Prec::MUL)?
                    }
                    TokenKind::Punct("+") if min_prec <= Prec::ADD => {
                        self.binary(lhs, BinaryOp::Add, Prec::ADD)?
                    }
                    TokenKind::Punct("-") if min_prec <= Prec::ADD => {
                        self.binary(lhs, BinaryOp::Sub, Prec::ADD)?
                    }
                    TokenKind::Punct("<=") if min_prec <= Prec::REL => {
                        self.binary(lhs, BinaryOp::Lte, Prec::REL)?
                    }
                    TokenKind::Punct(">=") if min_prec <= Prec::REL => {
                        self.binary(lhs, BinaryOp::Gte, Prec::REL)?
                    }
                    TokenKind::Punct("<") if min_prec <= Prec::REL => {
                        self.binary(lhs, BinaryOp::Lt, Prec::REL)?
                    }
                    TokenKind::Punct(">") if min_prec <= Prec::REL => {
                        self.binary(lhs, BinaryOp::Gt, Prec::REL)?
                    }
                    TokenKind::Punct("==") if min_prec <= Prec::EQ => {
                        self.binary(lhs, BinaryOp::Eq, Prec::EQ)?
                    }
                    TokenKind::Punct("!=") if min_prec <= Prec::EQ => {
                        self.binary(lhs, BinaryOp::Ne, Prec::EQ)?
                    }
                    TokenKind::Punct("&") if min_prec <= Prec::BAN => {
                        self.binary(lhs, BinaryOp::BitwiseAnd, Prec::BAN)?
                    }
                    TokenKind::Punct("^") if min_prec <= Prec::XOR => {
                        self.binary(lhs, BinaryOp::BitwiseXor, Prec::XOR)?
                    }
                    TokenKind::Punct("|") if min_prec <= Prec::BOR => {
                        self.binary(lhs, BinaryOp::BitwiseOr, Prec::BOR)?
                    }
                    TokenKind::Punct("&&") if min_prec <= Prec::LAN => {
                        self.binary(lhs, BinaryOp::LogicalAnd, Prec::LAN)?
                    }
                    TokenKind::Punct("||") if min_prec <= Prec::LOR => {
                        self.binary(lhs, BinaryOp::LogicalOr, Prec::LOR)?
                    }
                    TokenKind::Punct("=") if supports_assign(&lhs) && min_prec == Prec::ASS => {
                        self.assign(lhs, None)?
                    }
                    TokenKind::Punct("+=") if min_prec == Prec::ASS => {
                        self.assign(lhs, Some(AssignOp::Add))?
                    }
                    TokenKind::Punct("-=") if min_prec == Prec::ASS => {
                        self.assign(lhs, Some(AssignOp::Sub))?
                    }
                    TokenKind::Punct("*=") if min_prec == Prec::ASS => {
                        self.assign(lhs, Some(AssignOp::Mul))?
                    }
                    TokenKind::Punct("/=") if min_prec == Prec::ASS => {
                        self.assign(lhs, Some(AssignOp::Div))?
                    }
                    TokenKind::Punct("%=") if min_prec == Prec::ASS => {
                        self.assign(lhs, Some(AssignOp::Rem))?
                    }
                    _ => break,
                },
                (None, _) => break,
            };
        }

        Ok(lhs)
    }

    fn expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        let expr = self.expr(0)?;

        if self.accept(&TokenKind::Punct(";")) {
            return Ok(StmtKind::Expr(expr).at(span));
        }

        match self.peek() {
            Some(TokenKind::Punct("}")) => Ok(StmtKind::Return(expr).at(span)),
            None => Err(ErrorKind::UnexpectedEof.at(self.span())),
            _ => Err(ErrorKind::UnexpectedToken {
                expected: Cow::Borrowed(";"),
                actual: self.tokens.pop_front().unwrap().to_string(),
            }
            .at(self.span())),
        }
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        self.advance();
        let expr = self.expr(1)?;
        self.semi()?;

        Ok(StmtKind::Return(expr).at(span))
    }

    fn ident(&mut self) -> Result<String, ParseError> {
        match self.next() {
            Some(token) => match token.kind {
                TokenKind::Ident(id) => Ok(id),
                _ => Err(ErrorKind::UnexpectedToken {
                    expected: Cow::Borrowed("ident"),
                    actual: token.to_string(),
                }
                .at(self.span())),
            },
            None => Err(ErrorKind::UnexpectedEof.at(self.span())),
        }
    }

    fn let_stmt(&mut self) -> Result<Stmt, ParseError> {
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

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.expect(TokenKind::Punct("{"))?;
        let body = self.body(false)?;
        self.expect(TokenKind::Punct("}"))?;

        Ok(body)
    }

    fn fn_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        self.advance();
        let name = self.ident()?;
        self.expect(TokenKind::Punct("("))?;
        let args = self.arg_list(TokenKind::Punct(")"))?;
        let body = self.block()?;

        Ok(StmtKind::Fn(name, args, body).at(span))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
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

    fn for_cond_stmt(&mut self, span: Span, pre: Stmt) -> Result<Stmt, ParseError> {
        let expr = self.expr(1)?;
        self.semi()?;
        let post = self.expr(0)?;
        let body = self.block()?;

        Ok(StmtKind::ForCond(Box::new(pre), expr, post, body).at(span))
    }

    fn class_stmt(&mut self) -> Result<Stmt, ParseError> {
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

    fn for_stmt(&mut self) -> Result<Stmt, ParseError> {
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

    fn body(&mut self, global: bool) -> Result<Vec<Stmt>, ParseError> {
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

    pub fn parse(mut self) -> Result<Vec<Stmt>, ParseError> {
        self.body(true)
    }
}
