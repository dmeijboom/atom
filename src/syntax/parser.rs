use std::mem;

use crate::syntax::lexer::StringToken;
use crate::syntax::{
    Error, Expr, ExprKind, FnDef, FnSig, Literal, LiteralKind, Node, NodeKind, Scanner, Stmt,
    StmtKind, Token,
};

type Result<T> = std::result::Result<T, Error>;

macro_rules! expect {
    ($scanner:expr, $token:path) => {{
        let token = $scanner.advance();

        if !matches!(token, Some($token)) {
            return Err(Error::new(
                $scanner.span(),
                format!("unexpected token: {:?}", token),
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
                    format!("unexpected token: {:?}", token),
                ))
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

pub struct Parser<'s> {
    scanner: Scanner<'s>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Parser {
            scanner: Scanner::new(source),
        }
    }

    fn parse_string(&mut self) -> Result<String> {
        expect!(self.scanner, Token::BeginString);

        let mut scanner = Scanner::new("");
        mem::swap(&mut scanner, &mut self.scanner);

        let mut scanner = scanner.into_string_scanner();
        let mut s = String::new();

        while let Some(token) = scanner.advance() {
            println!("{:#?}", token);

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

    fn parse_expr(&mut self) -> Result<Expr> {
        match self.scanner.peek() {
            Some(Token::BeginString) => {
                make_lit!(self.scanner.span(), String, self.parse_string()?)
            }
            Some(Token::Int(i)) => {
                let val = *i;
                self.scanner.advance();
                make_lit!(self.scanner.span(), Int, val)
            }
            Some(Token::Float(f)) => {
                let val = *f;
                self.scanner.advance();
                make_lit!(self.scanner.span(), Float, val)
            }
            None => Err(Error::new(
                self.scanner.span(),
                "unexpected EOF in expr".to_string(),
            )),
            Some(Token::Error) => {
                return Err(Error::new(
                    self.scanner.span(),
                    format!("{} in expr", self.scanner.error_description()),
                ));
            }
            Some(token) => {
                let message = format!("unexpected token '{:?}' in expr", token);

                Err(Error::new(self.scanner.span(), message))
            }
        }
    }

    fn parse_body(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = vec![];

        while let Some(token) = self.scanner.peek() {
            match token {
                Token::BracketRight => break,
                _ => {
                    let span = self.scanner.span();
                    let expr = self.parse_expr()?;

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

    fn parse_fn(&mut self) -> Result<FnDef> {
        expect!(self.scanner, Token::Fn);
        let name = expect!(withValue self.scanner, Token::Ident);
        expect!(self.scanner, Token::BracketLeft);
        let body = self.parse_body()?;
        expect!(self.scanner, Token::BracketRight);

        Ok(FnDef {
            name,
            sig: FnSig {},
            body,
        })
    }

    pub fn parse(mut self) -> Result<Vec<Node>> {
        let mut nodes = vec![];

        while let Some(token) = self.scanner.peek() {
            match token {
                Token::Fn => nodes.push(Node::new(
                    self.scanner.span(),
                    NodeKind::FnDef(self.parse_fn()?),
                )),
                _ => unimplemented!(),
            }
        }

        Ok(nodes)
    }
}
