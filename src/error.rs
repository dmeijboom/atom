use std::fmt::{Display, Formatter};

use peg::error::ParseError;
use peg::str::LineCol;

use crate::compiler::CompileError;
use crate::runtime::RuntimeError;

#[derive(Debug, PartialEq)]
pub enum Error {
    Compile(CompileError),
    Runtime(RuntimeError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Compile(err) => err.fmt(f),
            Error::Runtime(err) => err.fmt(f),
        }
    }
}

impl From<CompileError> for Error {
    fn from(err: CompileError) -> Self {
        Self::Compile(err)
    }
}

impl From<RuntimeError> for Error {
    fn from(err: RuntimeError) -> Self {
        Self::Runtime(err)
    }
}

impl From<ParseError<LineCol>> for Error {
    fn from(err: ParseError<LineCol>) -> Self {
        CompileError::from(err).into()
    }
}
