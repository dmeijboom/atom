use std::error::Error;
use std::fmt;

use crate::ast::Pos;

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub pos: Option<Pos>,
    pub module_name: Option<String>,
}

impl RuntimeError {
    pub fn new(message: String) -> Self {
        Self {
            message,
            pos: None,
            module_name: None,
        }
    }

    pub fn with_pos(mut self, pos: Pos) -> Self {
        self.pos = Some(pos);

        self
    }
}

impl Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)?;

        if let Some(module_name) = &self.module_name {
            write!(f, " in {}", module_name)?;
        }

        if let Some(pos) = &self.pos {
            write!(f, " at {}..{}", pos.start, pos.end)?;
        }

        Ok(())
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;
