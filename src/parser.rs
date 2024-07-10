use crate::{
    ast::{Expr, Literal, Op, Stmt},
    lexer::{Token, TokenKind},
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected token {actual}, expected: {expected}")]
    UnexpectedToken { expected: TokenKind, actual: Token },
    #[error("unexpected eof")]
    UnexpectedEof,
    #[error("invalid expr: {0}")]
    InvalidExpr(TokenKind),
}

pub struct Parser {
    n: usize,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { n: 0, tokens }
    }

    fn hdr(&self) -> Option<&Token> {
        self.tokens.get(self.n)
    }

    fn advance(&mut self) {
        self.n += 1;
    }

    fn move_back(&mut self) {
        self.n -= 1;
    }

    fn next(&mut self) -> Option<&Token> {
        self.advance();
        self.tokens.get(self.n - 1)
    }

    fn expect(&mut self, expected: &TokenKind) -> Result<(), Error> {
        match self.next() {
            Some(token) if &token.kind == expected => Ok(()),
            Some(token) => Err(Error::UnexpectedToken {
                expected: expected.clone(),
                actual: token.clone(),
            }),
            None => Err(Error::UnexpectedEof),
        }
    }

    fn array(&mut self) -> Result<Expr, Error> {
        let items = self.expr_list(TokenKind::SqrBracketRight)?;
        Ok(Expr::Array(items))
    }

    fn semi(&mut self) -> Result<(), Error> {
        self.expect(&TokenKind::Semi)
    }

    fn expr_list(&mut self, sep: TokenKind) -> Result<Vec<Expr>, Error> {
        let mut exprs = vec![];

        loop {
            if matches!(self.hdr(), Some(token) if token.kind == sep) {
                self.advance();
                break;
            }

            exprs.push(self.expr(1)?);

            match self.next() {
                Some(token) if token.kind == TokenKind::Comma => {
                    exprs.push(self.expr(1)?);
                }
                Some(token) if token.kind == sep => {
                    break;
                }
                Some(token) => {
                    return Err(Error::UnexpectedToken {
                        expected: sep,
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
            Some(token) => match &token.kind {
                TokenKind::IntLit(i) => Ok(Expr::Literal(Literal::Int(*i))),
                TokenKind::FloatLit(f) => Ok(Expr::Literal(Literal::Float(*f))),
                TokenKind::BoolLit(b) => Ok(Expr::Literal(Literal::Bool(*b))),
                TokenKind::StringLit(s) => Ok(Expr::Literal(Literal::String(s.clone()))),
                TokenKind::Ident(id) => Ok(Expr::Ident(id.clone())),
                TokenKind::SqrBracketLeft => Ok(self.array()?),
                kind => Err(Error::InvalidExpr(kind.clone())),
            },
            None => Err(Error::UnexpectedEof),
        }
    }

    fn compute(&mut self, expr: Expr, min_prec: u8) -> Result<Expr, Error> {
        match self.next() {
            Some(token) => match token.kind {
                TokenKind::Dot if min_prec <= 3 => {
                    let rhs = self.expr(4)?;
                    let lhs = Expr::Member(Box::new(expr), Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                TokenKind::ParentLeft if min_prec <= 3 => {
                    let args = self.expr_list(TokenKind::ParentRight)?;
                    let lhs = Expr::Call(Box::new(expr), args);
                    self.compute(lhs, min_prec)
                }
                TokenKind::Mul if min_prec <= 2 => {
                    let rhs = self.expr(3)?;
                    let lhs = Expr::Binary(Box::new(expr), Op::Mul, Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                TokenKind::Div if min_prec <= 2 => {
                    let rhs = self.expr(3)?;
                    let lhs = Expr::Binary(Box::new(expr), Op::Div, Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                TokenKind::Add if min_prec <= 1 => {
                    let rhs = self.expr(2)?;
                    let lhs = Expr::Binary(Box::new(expr), Op::Add, Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                TokenKind::Sub if min_prec <= 1 => {
                    let rhs = self.expr(2)?;
                    let lhs = Expr::Binary(Box::new(expr), Op::Sub, Box::new(rhs));
                    self.compute(lhs, min_prec)
                }
                _ => {
                    self.move_back();
                    Ok(expr)
                }
            },
            None => {
                self.move_back();
                Ok(expr)
            }
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

    pub fn parse(mut self) -> Result<Vec<Stmt>, Error> {
        let mut stmts = vec![];

        while let Some(token) = self.hdr() {
            if token.kind == TokenKind::Eof {
                break;
            }

            let stmt = self.expr_stmt()?;
            stmts.push(stmt);
        }

        Ok(stmts)
    }
}
