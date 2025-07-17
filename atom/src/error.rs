use std::fmt::{Debug, Display};

use crate::{
    frontend::{IRError, ParseError, Span, TokenError},
    runtime,
};

pub trait IntoSpanned {
    fn at(self, span: Span) -> SpannedError<Self>
    where
        Self: Sized + Display;
}

impl<T: Display> IntoSpanned for T {
    fn at(self, span: Span) -> SpannedError<Self>
    where
        Self: Sized + Display,
    {
        SpannedError { kind: self, span }
    }
}

#[derive(Debug)]
pub struct SpannedError<T: Display> {
    pub kind: T,
    #[allow(dead_code)]
    pub span: Span,
}

impl<T: Display> Display for SpannedError<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl<T: Display + Debug> std::error::Error for SpannedError<T> {}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O error: {0}")]
    IO(#[from] std::io::Error),
    #[error("ParseError: {0}")]
    Parse(#[from] ParseError),
    #[error("IRError: {0}")]
    IR(#[from] IRError),
    #[error("TokenError: {0}")]
    Lex(#[from] TokenError),
    #[error("SerializeError: {0}")]
    Serialize(#[from] ron::Error),
    #[error("{0}")]
    Runtime(#[from] runtime::Error),
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Error::IO(_) => Span::default(),
            Error::IR(e) => e.span,
            Error::Parse(e) => e.span,
            Error::Lex(e) => e.span,
            Error::Serialize(_) => Span::default(),
            Error::Runtime(e) => match e {
                runtime::Error::Runtime(e) => e.span,
                runtime::Error::Import(e) => e.span(),
            },
        }
    }
}
