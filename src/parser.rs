use std::result;

use logos::Lexer;
use thiserror::Error;

use crate::{
    ast::{Location, LocationExt, Node, Stmt, StmtKind},
    token::Token,
};

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid syntax")]
    Syntax { location: Location },
    #[error("unexpected token: {actual:?} (expected: {expected:?})")]
    UnexpectedToken {
        location: Location,
        expected: String,
        actual: Token,
    },
    #[error("unexpected EOF")]
    UnexpectedEof { location: Location },
}

pub type Result<T> = std::result::Result<T, Error>;

macro_rules! matches_token {
    (Number) => {
        |actual| match actual {
            Token::Number(value) => Ok(value),
            actual => Err((actual, "Number".to_string())),
        }
    };

    ($expected:ident) => {
        matches_token!(Token::$expected)
    };

    ($expected:pat) => {
        |actual| match actual {
            $expected => Ok(()),
            actual => Err((actual, stringify!($expected).to_string())),
        }
    };
}

pub struct Parser<'s> {
    lexer: Lexer<'s, Token>,
    tree: Vec<Node<'s>>,
    peeked: Option<Token>,
}

impl<'s> Parser<'s> {
    pub fn new(lexer: Lexer<'s, Token>) -> Self {
        Self {
            lexer,
            tree: vec![],
            peeked: None,
        }
    }

    fn location(&self) -> Location {
        let span = self.lexer.span();

        Location {
            line: self.lexer.extras.line,
            column: match self.lexer.extras.line_idx {
                Some(idx) => span.start - idx,
                None => span.start,
            },
            offset: span.start,
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        if self.peeked.is_none() {
            self.peeked = self.lexer.next();
        }

        self.peeked.as_ref()
    }

    fn next(&mut self) -> Result<Option<Token>> {
        let next = self.peeked.take().or_else(|| self.lexer.next());

        if let Some(Token::Error) = next {
            return Err(Error::Syntax {
                location: self.location(),
            });
        }

        Ok(next)
    }

    fn assert<T, F>(&mut self, assertion: F) -> Result<T>
    where
        F: FnOnce(Token) -> result::Result<T, (Token, String)>,
    {
        let actual = self.next()?.ok_or_else(|| Error::UnexpectedEof {
            location: self.location(),
        })?;

        match assertion(actual) {
            Ok(value) => Ok(value),
            Err((actual, expected)) => {
                self.peeked = Some(actual.clone());

                Err(Error::UnexpectedToken {
                    location: self.location(),
                    expected,
                    actual,
                })
            }
        }
    }

    fn stmt(&mut self) -> Result<bool> {
        let start = self.location();
        let token = self.peek();

        match token {
            Some(Token::Fn) => {
                self.assert(matches_token!(Fn))?;
                self.assert(matches_token!(Ident))?;
                let name = self.lexer.slice();

                if self.assert(matches_token!(LParent)).is_ok() {
                    self.assert(matches_token!(RParent))?;
                }

                self.assert(matches_token!(LBracket))?;
                self.assert(matches_token!(RBracket))?;

                self.tree.push(Node::Stmt(Stmt::new(
                    start.until(self.location()),
                    StmtKind::FnDef { name },
                )));
            }
            None => return Ok(false),
            _ => unreachable!(),
        }

        Ok(true)
    }

    pub fn parse(mut self) -> Result<Vec<Node<'s>>> {
        while self.stmt()? {}
        Ok(self.tree)
    }
}
