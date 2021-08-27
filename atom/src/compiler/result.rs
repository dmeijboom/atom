use std::error::Error;
use std::fmt;

use crate::ast::Pos;

#[derive(Debug, PartialEq)]
pub struct CompileError {
    pub pos: Pos,
    pub message: String,
}

impl CompileError {
    pub fn new(message: String, pos: Pos) -> Self {
        Self { message, pos }
    }
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.pos.start, self.pos.end
        )
    }
}

pub type Result<T> = std::result::Result<T, CompileError>;
