use std::error;
use std::fmt::{Display, Formatter};

use crate::syntax::Span;

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

impl Error {
    pub fn new(span: Span, message: String) -> Self {
        Self { span, message }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CompileError: {}", self.message)
    }
}

impl error::Error for Error {}
