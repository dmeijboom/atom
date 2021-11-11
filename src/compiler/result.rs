use std::error::Error;
use std::fmt;

use crate::compiler::ir::Location;

#[derive(Debug, PartialEq)]
pub struct CompileError {
    pub location: Location,
    pub message: String,
    pub filename: Option<String>,
}

impl CompileError {
    pub fn new(message: String) -> Self {
        Self {
            message,
            filename: None,
            location: Location::default(),
        }
    }

    pub fn with_location(mut self, location: Location) -> Self {
        self.location = location;

        self
    }

    pub fn with_filename(mut self, filename: String) -> Self {
        self.filename = Some(filename);

        self
    }
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.message, self.location)
    }
}

pub type Result<T> = std::result::Result<T, CompileError>;
