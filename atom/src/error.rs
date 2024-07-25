use std::fmt::{Debug, Display};

use crate::{
    compiler::CompileError,
    lexer::{Span, TokenError},
    parser::ParseError,
    vm,
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
    #[error("TokenError: {0}")]
    Lex(#[from] TokenError),
    #[error("CompileError: {0}")]
    Compile(#[from] CompileError),
    #[error("{0}")]
    Runtime(#[from] vm::Error),
}
