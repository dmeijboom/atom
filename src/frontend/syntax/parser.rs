use std::mem;

use crate::frontend::syntax::lexer::StringToken;
use crate::frontend::syntax::{
    Binary, BinaryOp, Error, Expr, ExprKind, FnDef, FnSig, Literal, LiteralKind, Node, NodeKind,
    Scanner, Stmt, StmtKind, Token, Type,
};

type Result<T> = std::result::Result<T, Error>;

macro_rules! expect {
    ($scanner:expr, $token:path) => {{
        let token = $scanner.advance();

        if !matches!(token, Some($token)) {
            return Err(Error::new(
                $scanner.span(),
                format!(
                    "unexpected token '{}', expected '{:?}'",
                    token
                        .map(|t| format!("{:?}", t))
                        .unwrap_or_else(|| "EOF".to_string()),
                    stringify!($token),
                ),
            ));
        }

        token
    }};

    (withValue $scanner:expr, $token:path) => {{
        let token = $scanner.advance();

        match token {
            Some($token(value)) => value,
            _ => {
                return Err(Error::new(
                    $scanner.span(),
                    format!(
                        "unexpected token '{}', expected '{}'",
                        token
                            .map(|t| format!("{:?}", t))
                            .unwrap_or_else(|| "EOF".to_string()),
                        stringify!($token),
                    ),
                ));
            }
        }
    }};
}

macro_rules! accept {
    ($scanner:expr, $token:path) => {{
        let token = $scanner.peek();
        matches!(token, Some($token))
    }};
}

macro_rules! make_lit {
    ($span:expr, $type:ident, $source:expr) => {
        Ok(Expr::new(
            $span.clone(),
            ExprKind::Literal(Literal::new($span, LiteralKind::$type($source))),
        ))
    };
}

macro_rules! make_bin {
    ($expr:expr, $right:expr, $op:ident) => {
        $expr = Expr::new(
            $expr.span.clone(),
            ExprKind::Binary(
                Binary {
                    span: $expr.span.clone(),
                    op: BinaryOp::$op,
                    left: $expr,
                    right: $right,
                }
                .into(),
            ),
        )
    };
}

pub struct Parser<'s> {
    scanner: Scanner<'s>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Parser {
            scanner: Scanner::new(source),
        }
    }

    fn string(&mut self) -> Result<String> {
        let mut scanner = Scanner::new("");
        mem::swap(&mut scanner, &mut self.scanner);

        let mut scanner = scanner.into_string_scanner();
        let mut s = String::new();

        while let Some(token) = scanner.advance() {
            match token {
                StringToken::Text => s.push_str(scanner.text()),
                StringToken::Escaped(c) => s.push(c),
                StringToken::End => {
                    self.scanner = scanner.into_scanner();
                    return Ok(s);
                }
                StringToken::Error => {
                    return Err(Error::new(
                        scanner.span(),
                        format!("unexpected error in string: {}", scanner.text()),
                    ));
                }
                _ => unimplemented!(),
            }
        }

        Err(Error::new(
            scanner.span(),
            "unexpected EOF in string literal".to_string(),
        ))
    }

    fn term(&mut self) -> Result<Expr> {
        match self.scanner.advance() {
            Some(Token::BeginString) => {
                make_lit!(self.scanner.span(), String, self.string()?)
            }
            Some(Token::Int(i)) => make_lit!(self.scanner.span(), Int, i),
            Some(Token::Float(f)) => make_lit!(self.scanner.span(), Float, f),
            Some(Token::Error) => Err(Error::new(
                self.scanner.span(),
                format!("{} in expr", self.scanner.error_description()),
            )),
            Some(token) => Err(Error::new(
                self.scanner.span(),
                format!("unexpected token '{:?}' in expr", token),
            )),
            None => Err(Error::new(
                self.scanner.span(),
                "unexpected EOF in expr".to_string(),
            )),
        }
    }

    fn bitwise_shift(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        loop {
            let token = self.scanner.peek();

            match token {
                Some(Token::ShiftLeft) => {
                    self.scanner.advance();
                    make_bin!(expr, self.term()?, BitShiftLeft);
                }
                Some(Token::ShiftRight) => {
                    self.scanner.advance();
                    make_bin!(expr, self.term()?, BitShiftRight);
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn multiplicative(&mut self) -> Result<Expr> {
        let mut expr = self.bitwise_shift()?;

        loop {
            let token = self.scanner.peek();

            match token {
                Some(Token::Mul) => {
                    self.scanner.advance();
                    make_bin!(expr, self.bitwise_shift()?, Mul);
                }
                Some(Token::Div) => {
                    self.scanner.advance();
                    make_bin!(expr, self.bitwise_shift()?, Div);
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn additive(&mut self) -> Result<Expr> {
        let mut expr = self.multiplicative()?;

        loop {
            let token = self.scanner.peek();

            match token {
                Some(Token::Add) => {
                    self.scanner.advance();
                    make_bin!(expr, self.multiplicative()?, Add);
                }
                Some(Token::Sub) => {
                    self.scanner.advance();
                    make_bin!(expr, self.multiplicative()?, Sub);
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    #[inline]
    fn expr(&mut self) -> Result<Expr> {
        self.additive()
    }

    fn return_stmt(&mut self) -> Result<Stmt> {
        let span = self.scanner.span();
        self.scanner.advance();
        let expr = self.expr()?;
        expect!(self.scanner, Token::End);

        Ok(Stmt::new(span, StmtKind::Return(expr)))
    }

    fn body(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = vec![];

        while let Some(token) = self.scanner.peek() {
            match token {
                Token::Return => {
                    stmts.push(self.return_stmt()?);
                }
                Token::BracketRight => break,
                _ => {
                    let span = self.scanner.span();
                    let expr = self.expr()?;

                    stmts.push(Stmt::new(
                        span,
                        if accept!(self.scanner, Token::End) {
                            self.scanner.advance();
                            StmtKind::ExprEnd(expr)
                        } else {
                            StmtKind::Expr(expr)
                        },
                    ));
                }
            }
        }

        Ok(stmts)
    }

    fn ty(&mut self) -> Result<Type> {
        let name = expect!(withValue self.scanner, Token::Ident);
        Ok(Type::new(name))
    }

    fn fn_sig(&mut self) -> Result<FnSig> {
        let mut params = vec![];

        let token = self.scanner.peek();

        match token {
            Some(Token::Ident(_)) => loop {
                let ty = self.ty()?;

                params.push(ty);

                if !accept!(self.scanner, Token::Separator) {
                    break;
                }

                self.scanner.advance();
            },
            Some(Token::ParentLeft) => {
                expect!(self.scanner, Token::ParentLeft);
                expect!(self.scanner, Token::ParentRight);
            }
            _ => {
                let token = token
                    .map(|t| format!("{:?}", t))
                    .unwrap_or_else(|| "EOF".to_string());

                return Err(Error::new(
                    self.scanner.span(),
                    format!("unexpected token '{}' in fn signature", token),
                ));
            }
        }

        let mut return_type = None;

        if accept!(self.scanner, Token::ResultType) {
            self.scanner.advance();

            return_type = Some(self.ty()?);
        }

        Ok(FnSig {
            params,
            return_type,
        })
    }

    fn func(&mut self) -> Result<FnDef> {
        let mut sig = FnSig {
            params: vec![],
            return_type: None,
        };

        expect!(self.scanner, Token::Fn);
        let name = expect!(withValue self.scanner, Token::Ident);

        if accept!(self.scanner, Token::TypeSeparator) {
            self.scanner.advance();
            sig = self.fn_sig()?;
        }

        expect!(self.scanner, Token::BracketLeft);
        let body = self.body()?;
        expect!(self.scanner, Token::BracketRight);

        Ok(FnDef { name, sig, body })
    }

    pub fn parse(mut self) -> Result<Vec<Node>> {
        let mut nodes = vec![];

        while let Some(token) = self.scanner.peek() {
            match token {
                Token::Fn => nodes.push(Node::new(
                    self.scanner.span(),
                    NodeKind::FnDef(self.func()?),
                )),
                _ => unimplemented!(),
            }
        }

        Ok(nodes)
    }
}
