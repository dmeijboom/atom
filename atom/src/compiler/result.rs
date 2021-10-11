use std::error::Error;
use std::fmt;

use atom_ir::Location;

#[derive(Debug, PartialEq)]
pub struct CompileError {
    pub location: Location,
    pub message: String,
}

impl CompileError {
    pub fn new(message: String) -> Self {
        Self {
            message,
            location: Location::default(),
        }
    }

    pub fn with_location(mut self, location: Location) -> Self {
        self.location = location;

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
