use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};

use peg::error::{ExpectedSet, ParseError};
use peg::str::LineCol;

use crate::compiler::ir::Location;

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    Compile(String),
    Parse(ExpectedSet),
}

impl From<String> for ErrorKind {
    fn from(message: String) -> Self {
        Self::Compile(message)
    }
}

impl From<ExpectedSet> for ErrorKind {
    fn from(expected: ExpectedSet) -> Self {
        Self::Parse(expected)
    }
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Compile(message) => write!(f, "{}", message),
            ErrorKind::Parse(expected) => {
                write!(f, "unexpected token, expected {}", expected)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CompileError {
    pub kind: ErrorKind,
    pub location: Option<Location>,
    pub filename: Option<String>,
}

impl CompileError {
    pub fn new<T: Into<ErrorKind>>(kind: T) -> Self {
        Self {
            kind: kind.into(),
            filename: None,
            location: None,
        }
    }

    pub fn with_location(mut self, location: Location) -> Self {
        self.location = Some(location);

        self
    }

    pub fn with_filename(mut self, filename: String) -> Self {
        self.filename = Some(filename);

        self
    }

    pub fn message(&self) -> String {
        match &self.kind {
            ErrorKind::Compile(message) => message.clone(),
            ErrorKind::Parse(_) => format!("{}", self.kind),
        }
    }
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {} in {}",
            match self.kind {
                ErrorKind::Compile(_) => "CompileError",
                ErrorKind::Parse(_) => "ParseError",
            },
            self.kind,
            self.filename.as_deref().unwrap_or("<unknown>")
        )?;

        if let Some(location) = &self.location {
            write!(f, ":{}", location)?;
        }

        Ok(())
    }
}

impl From<ParseError<LineCol>> for CompileError {
    fn from(err: ParseError<LineCol>) -> Self {
        Self::new(ErrorKind::Parse(err.expected)).with_location(Location::new(
            err.location.offset..err.location.offset + 1,
            err.location.line,
            err.location.column,
        ))
    }
}

pub type Result<T> = std::result::Result<T, CompileError>;
