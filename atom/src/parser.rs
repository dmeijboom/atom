use std::{
    borrow::Cow,
    collections::{HashMap, VecDeque},
    ops::Add,
};

use lazy_static::lazy_static;

use crate::{
    ast::{
        AssignOp, BinaryOp, Expr, ExprKind, FnArg, FnStmt, IfStmt, Literal, MatchArm, Path, Stmt,
        StmtKind, UnaryOp,
    },
    error::{IntoSpanned, SpannedError},
    lexer::{Span, Token, TokenKind},
};

lazy_static! {
    static ref TOKEN_TYPES: HashMap<&'static str, TokenType> = {
        [
            ("%", TokenType::Binary((BinaryOp::Rem, Prec::Mul))),
            ("*", TokenType::Binary((BinaryOp::Mul, Prec::Mul))),
            ("/", TokenType::Binary((BinaryOp::Div, Prec::Mul))),
            ("+", TokenType::Binary((BinaryOp::Add, Prec::Add))),
            ("-", TokenType::Binary((BinaryOp::Sub, Prec::Add))),
            ("<=", TokenType::Binary((BinaryOp::Lte, Prec::Relational))),
            (">=", TokenType::Binary((BinaryOp::Gte, Prec::Relational))),
            ("<", TokenType::Binary((BinaryOp::Lt, Prec::Relational))),
            (">", TokenType::Binary((BinaryOp::Gt, Prec::Relational))),
            ("==", TokenType::Binary((BinaryOp::Eq, Prec::Equal))),
            ("!=", TokenType::Binary((BinaryOp::Ne, Prec::Equal))),
            ("is", TokenType::Binary((BinaryOp::TypeAssert, Prec::Equal))),
            ("&", TokenType::Binary((BinaryOp::BitAnd, Prec::BitwiseAnd))),
            ("<<", TokenType::Binary((BinaryOp::ShiftLeft, Prec::Shift))),
            (">>", TokenType::Binary((BinaryOp::ShiftRight, Prec::Shift))),
            ("^", TokenType::Binary((BinaryOp::Xor, Prec::BitwiseXor))),
            ("|", TokenType::Binary((BinaryOp::BitOr, Prec::BitwiseOr))),
            ("&&", TokenType::Binary((BinaryOp::And, Prec::LogicalAnd))),
            ("||", TokenType::Binary((BinaryOp::Or, Prec::LogicalOr))),
            ("=", TokenType::Assign(None)),
            ("+=", TokenType::Assign(Some(AssignOp::Add))),
            ("-=", TokenType::Assign(Some(AssignOp::Sub))),
            ("*=", TokenType::Assign(Some(AssignOp::Mul))),
            ("/=", TokenType::Assign(Some(AssignOp::Div))),
            ("%=", TokenType::Assign(Some(AssignOp::Rem))),
            (":", TokenType::Range),
            ("(", TokenType::Postfix(PostfixType::Call)),
            ("[", TokenType::Postfix(PostfixType::CompMember)),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

#[derive(Clone, Copy)]
enum PostfixType {
    Call,
    Member,
    CompMember,
}

#[derive(Clone)]
enum TokenType {
    Range,
    Postfix(PostfixType),
    Binary((BinaryOp, Prec)),
    Assign(Option<AssignOp>),
}

#[derive(PartialEq, PartialOrd, Clone, Copy)]
#[repr(u8)]
enum Prec {
    Assign,
    Range,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equal,
    Relational,
    Shift,
    Add,
    Mul,
    Prefix,
    Postfix,
}

impl Default for Prec {
    // The first precedence level after assignments
    fn default() -> Self {
        Self::Range
    }
}

impl TryFrom<u8> for Prec {
    type Error = &'static str;

    fn try_from(n: u8) -> Result<Self, Self::Error> {
        match n {
            0 => Ok(Prec::Assign),
            1 => Ok(Prec::Range),
            2 => Ok(Prec::LogicalOr),
            3 => Ok(Prec::LogicalAnd),
            4 => Ok(Prec::BitwiseOr),
            5 => Ok(Prec::BitwiseXor),
            6 => Ok(Prec::BitwiseAnd),
            7 => Ok(Prec::Equal),
            8 => Ok(Prec::Relational),
            9 => Ok(Prec::Shift),
            10 => Ok(Prec::Add),
            11 => Ok(Prec::Mul),
            12 => Ok(Prec::Prefix),
            13 => Ok(Prec::Postfix),
            _ => Err("invalid precedence"),
        }
    }
}

impl Add<u8> for Prec {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        let n = self as u8 + rhs;
        n.try_into().expect("invalid precedence")
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("invalid expr '{0}'")]
    InvalidExpr(TokenKind),
    #[error("unexpected token '{actual}', expected: '{expected}'")]
    UnexpectedToken {
        expected: Cow<'static, str>,
        actual: String,
    },
    #[error("unexpected break outside of loop")]
    UnexpectedBreak,
    #[error("unexpected continue outside of loop")]
    UnexpectedContinue,
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

fn is_modifier(token: &TokenKind) -> bool {
    matches!(
        token,
        TokenKind::Keyword(keyword) if keyword == "pub"
    )
}

pub struct Parser {
    loop_counter: usize,
    tokens: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            loop_counter: 0,
            tokens: VecDeque::from(tokens),
        }
    }

    fn test_keyword(&self, keyword: &str) -> bool {
        matches!(self.peek(), Some(TokenKind::Keyword(ref k)) if k == keyword)
    }

    fn accept_keyword(&mut self, keyword: &str) -> bool {
        if self.test_keyword(keyword) {
            self.advance();
            return true;
        }

        false
    }

    fn expect_keyword(&mut self, keyword: &str) -> Result<(), ParseError> {
        let token = self
            .peek()
            .ok_or_else(|| ErrorKind::UnexpectedEof.at(self.span()))?;

        if !self.test_keyword(keyword) {
            return Err(ErrorKind::UnexpectedToken {
                expected: keyword.to_string().into(),
                actual: token.to_string(),
            }
            .at(self.span()));
        }

        self.advance();
        Ok(())
    }

    fn peek(&self) -> Option<&TokenKind> {
        self.tokens.front().map(|t| &t.kind)
    }

    fn peek_skip_modifiers(&self) -> Option<&TokenKind> {
        let mut n = 0;

        while let Some(token) = self.tokens.get(n) {
            if !is_modifier(&token.kind) {
                return Some(&token.kind);
            }
            n += 1;
        }

        None
    }

    fn peek2(&self) -> Option<&TokenKind> {
        self.tokens.get(1).map(|t| &t.kind)
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
                expected: Cow::Owned(expected.to_string()),
                actual: token.to_string(),
            }
            .at(self.span())),
            None => Err(ErrorKind::UnexpectedToken {
                expected: Cow::Owned(expected.to_string()),
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
        let items = self.expr_list(TokenKind::Punct("]"), Prec::default())?;
        Ok(ExprKind::Array(items))
    }

    fn semi(&mut self) -> Result<(), ParseError> {
        self.expect(TokenKind::Punct(";"))
    }

    fn arg_list(&mut self, end: TokenKind) -> Result<Vec<FnArg>, ParseError> {
        let mut args = vec![];

        while !self.accept(&end) {
            args.push(FnArg::new(self.span(), self.ident()?));

            if !self.accept(&TokenKind::Punct(",")) {
                self.expect(end)?;
                break;
            }
        }

        Ok(args)
    }

    fn expr_list(&mut self, end: TokenKind, min_prec: Prec) -> Result<Vec<Expr>, ParseError> {
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

    fn range(&mut self, span: Span, lhs: Option<Box<Expr>>) -> Result<Expr, ParseError> {
        let rhs = match self.peek() {
            Some(TokenKind::Punct("]")) => None,
            _ => Some(Box::new(self.expr(Prec::Range)?)),
        };

        Ok(ExprKind::Range(lhs, rhs).at(span))
    }

    fn match_expr(&mut self) -> Result<ExprKind, ParseError> {
        let expr = self.expr(Prec::default())?;
        self.expect(TokenKind::Punct("{"))?;

        let mut arms = vec![];
        let mut alt = None;

        loop {
            if self.accept_keyword("else") {
                self.expect(TokenKind::Punct("=>"))?;
                let expr = self.expr(Prec::default())?;
                alt = Some(Box::new(expr));
                break;
            }

            let pat = self.expr(Prec::default())?;
            self.expect(TokenKind::Punct("=>"))?;
            let expr = self.expr(Prec::default())?;

            arms.push(MatchArm { pat, expr });

            if !self.accept(&TokenKind::Punct(",")) {
                break;
            }
        }

        self.expect(TokenKind::Punct("}"))?;

        Ok(ExprKind::Match(Box::new(expr), arms, alt))
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        match self.next() {
            Some(token) => match token.kind {
                TokenKind::Int(i) => Ok(ExprKind::Literal(Literal::Int(i)).at(token.span)),
                TokenKind::BigInt(i) => Ok(ExprKind::Literal(Literal::BigInt(i)).at(token.span)),
                TokenKind::Float(f) => Ok(ExprKind::Literal(Literal::Float(f)).at(token.span)),
                TokenKind::Atom(s) => Ok(ExprKind::Literal(Literal::Atom(s)).at(token.span)),
                TokenKind::String(s) => Ok(ExprKind::Literal(Literal::String(s)).at(token.span)),
                TokenKind::Ident(id) => Ok(ExprKind::Ident(id).at(token.span)),
                TokenKind::Keyword(w) if w == "match" => Ok(self.match_expr()?.at(token.span)),
                TokenKind::Punct("@") => {
                    Ok(ExprKind::Ident(format!("@{}", self.ident()?)).at(token.span))
                }
                TokenKind::Punct("[") => Ok(self.array()?.at(token.span)),
                TokenKind::Punct("(") => {
                    let lhs = self.expr(Prec::default())?;
                    self.expect(TokenKind::Punct(")"))?;
                    Ok(lhs)
                }
                TokenKind::Punct("!") => {
                    let span = self.span();
                    let expr = self.expr(Prec::Prefix + 1)?;
                    Ok(ExprKind::Unary(UnaryOp::Not, Box::new(expr)).at(span))
                }
                kind => Err(ErrorKind::InvalidExpr(kind).at(self.span())),
            },
            None => Err(ErrorKind::UnexpectedEof.at(self.span())),
        }
    }

    fn binary(&mut self, lhs: Expr, op: BinaryOp, min_prec: Prec) -> Result<Expr, ParseError> {
        let span = lhs.span;
        self.advance();
        let rhs = self.expr(min_prec + 1)?;

        Ok(ExprKind::Binary(Box::new(lhs), op, Box::new(rhs)).at(span))
    }

    fn assign(&mut self, lhs: Expr, op: Option<AssignOp>) -> Result<Expr, ParseError> {
        let span = lhs.span;
        self.advance();
        let rhs = self.expr(Prec::default())?;

        Ok(ExprKind::Assign(op, Box::new(lhs), Box::new(rhs)).at(span))
    }

    fn postfix(&mut self, ty: PostfixType, expr: Expr) -> Result<Expr, ParseError> {
        Ok(match ty {
            PostfixType::Member => {
                self.advance();
                let span = expr.span;
                let name = self.ident()?;
                ExprKind::Member(Box::new(expr), name).at(span)
            }
            PostfixType::CompMember => {
                self.advance();
                let span = expr.span;
                let rhs = if self.accept(&TokenKind::Punct(":")) {
                    self.range(span, None)?
                } else {
                    self.expr(Prec::default())?
                };
                self.expect(TokenKind::Punct("]"))?;
                ExprKind::CompMember(Box::new(expr), Box::new(rhs)).at(span)
            }
            PostfixType::Call => {
                self.advance();
                let span = expr.span;
                let args = self.expr_list(TokenKind::Punct(")"), Prec::default())?;
                ExprKind::Call(Box::new(expr), args).at(span)
            }
        })
    }

    fn peek_typed(&self) -> Option<TokenType> {
        match self.peek() {
            Some(token) => match token {
                TokenKind::Punct(".") if matches!(self.peek2(), Some(TokenKind::Ident(_))) => {
                    Some(TokenType::Postfix(PostfixType::Member))
                }
                TokenKind::Punct(tok) => TOKEN_TYPES.get(tok).cloned(),
                TokenKind::Keyword(tok) => TOKEN_TYPES.get(tok.as_str()).cloned(),
                _ => None,
            },
            None => None,
        }
    }

    fn expr(&mut self, min_prec: Prec) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            expr = match self.peek_typed() {
                Some(annotated) => match annotated {
                    TokenType::Postfix(ty) if min_prec <= Prec::Postfix => {
                        self.postfix(ty, expr)?
                    }
                    TokenType::Range if min_prec <= Prec::Range => {
                        self.advance();
                        self.range(expr.span, Some(Box::new(expr)))?
                    }
                    TokenType::Binary((op, prec)) if min_prec <= prec => {
                        self.binary(expr, op, prec)?
                    }
                    TokenType::Assign(op) if supports_assign(&expr) && min_prec == Prec::Assign => {
                        self.assign(expr, op)?
                    }
                    _ => break,
                },
                None => break,
            };
        }

        Ok(expr)
    }

    fn expr_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        let expr = self.expr(Prec::Assign)?;

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

    fn path(&mut self) -> Result<Path, ParseError> {
        let mut path = Path::default();

        loop {
            path.push(self.ident()?);

            if !self.accept(&TokenKind::Punct("/")) {
                break;
            }
        }

        Ok(path)
    }

    fn import_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        self.advance();
        let path = self.path()?;
        self.semi()?;

        Ok(StmtKind::Import(path).at(span))
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        self.advance();
        let expr = self.expr(Prec::default())?;
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
        let value = self.expr(Prec::default())?;
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
        let public = self.accept_keyword("pub");
        self.expect_keyword("fn")?;
        let name = self.ident()?;
        self.expect(TokenKind::Punct("("))?;
        let args = self.arg_list(TokenKind::Punct(")"))?;

        let body = if self.accept(&TokenKind::Punct("=>")) {
            let span = self.span();
            let expr = self.expr(Prec::Assign)?;
            self.semi()?;

            vec![StmtKind::Return(expr).at(span)]
        } else {
            self.block()?
        };

        Ok(StmtKind::Fn(FnStmt {
            name,
            args,
            body,
            public,
        })
        .at(span))
    }

    fn if_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        self.advance();
        let expr = self.expr(Prec::default())?;
        let body = self.block()?;

        let mut if_stmt = Box::new(IfStmt(Some(expr), body, None));
        let mut cur = &mut if_stmt;

        loop {
            if self.accept_keyword("elif") {
                let expr = self.expr(Prec::Range)?;
                let body = self.block()?;

                cur.2 = Some(Box::new(IfStmt(Some(expr), body, None)));
                cur = cur.2.as_mut().unwrap();

                continue;
            }

            if self.accept_keyword("else") {
                cur.2 = Some(Box::new(IfStmt(None, self.block()?, None)));
            }

            break;
        }

        Ok(StmtKind::If(*if_stmt).at(span))
    }

    fn for_cond_stmt(&mut self, span: Span, init: Stmt) -> Result<Stmt, ParseError> {
        let cond = self.expr(Prec::Range)?;
        self.semi()?;
        let step = self.expr(Prec::Assign)?;
        self.loop_counter += 1;
        let body = self.block()?;
        self.loop_counter -= 1;
        let init = Box::new(init);

        Ok(StmtKind::ForCond {
            init,
            cond,
            step,
            body,
        }
        .at(span))
    }

    fn class_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        let is_public = self.accept_keyword("pub");
        self.expect_keyword("class")?;
        let name = self.ident()?;
        self.expect(TokenKind::Punct("{"))?;
        let mut methods = vec![];

        while matches!(self.peek_skip_modifiers(), Some(TokenKind::Keyword(w)) if w == "fn") {
            methods.push(self.fn_stmt()?);
        }

        self.expect(TokenKind::Punct("}"))?;
        Ok(StmtKind::Class(name, methods, is_public).at(span))
    }

    fn for_stmt(&mut self) -> Result<Stmt, ParseError> {
        let span = self.span();
        self.advance();

        if self.test_keyword("let") {
            let pre = self.let_stmt()?;
            return self.for_cond_stmt(span, pre);
        }

        let expr = self.expr(Prec::Assign)?;

        if self.accept(&TokenKind::Punct(";")) {
            let span = expr.span;
            return self.for_cond_stmt(span, StmtKind::Expr(expr).at(span));
        }

        self.loop_counter += 1;
        let body = self.block()?;
        self.loop_counter -= 1;

        Ok(StmtKind::For(expr, body).at(span))
    }

    fn break_stmt(&mut self) -> Result<Stmt, ParseError> {
        if self.loop_counter == 0 {
            return Err(ErrorKind::UnexpectedBreak.at(self.span()));
        }

        let span = self.span();
        self.advance();
        self.semi()?;
        Ok(StmtKind::Break.at(span))
    }

    fn continue_stmt(&mut self) -> Result<Stmt, ParseError> {
        if self.loop_counter == 0 {
            return Err(ErrorKind::UnexpectedContinue.at(self.span()));
        }

        let span = self.span();
        self.advance();
        self.semi()?;
        Ok(StmtKind::Continue.at(span))
    }

    fn body(&mut self, global: bool) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = vec![];

        while let Some(token) = self.peek_skip_modifiers() {
            let stmt = match token {
                TokenKind::Keyword(w) if w == "let" => self.let_stmt()?,
                TokenKind::Keyword(w) if w == "fn" => self.fn_stmt()?,
                TokenKind::Keyword(w) if w == "if" => self.if_stmt()?,
                TokenKind::Keyword(w) if w == "for" => self.for_stmt()?,
                TokenKind::Keyword(w) if w == "class" => self.class_stmt()?,
                TokenKind::Keyword(w) if w == "return" => self.return_stmt()?,
                TokenKind::Keyword(w) if w == "import" => self.import_stmt()?,
                TokenKind::Keyword(w) if w == "break" => self.break_stmt()?,
                TokenKind::Keyword(w) if w == "continue" => self.continue_stmt()?,
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
