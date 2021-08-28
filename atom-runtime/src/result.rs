use std::error::Error;
use std::fmt;

use atom_ir::Location;

use super::origin::Origin;

#[derive(Debug, PartialEq)]
pub struct Trace {
    pub origin: Origin,
    pub target: String,
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub message: String,
    pub location: Option<Location>,
    pub module_name: Option<String>,
    pub filename: Option<String>,
    pub stack_trace: Vec<Trace>,
}

impl RuntimeError {
    pub fn new(message: String) -> Self {
        Self {
            message,
            location: None,
            module_name: None,
            filename: None,
            stack_trace: vec![],
        }
    }

    pub fn with_message(mut self, message: String) -> Self {
        self.message = message;

        self
    }

    pub fn with_location(mut self, location: Location) -> Self {
        self.location = Some(location);

        self
    }

    pub fn with_stack_trace(mut self, stack_trace: Vec<Trace>) -> Self {
        self.stack_trace = stack_trace;

        self
    }

    pub fn with_module(mut self, module_name: &str) -> Self {
        self.module_name = Some(module_name.to_string());

        self
    }

    pub fn with_filename(mut self, filename: &str) -> Self {
        self.filename = Some(filename.to_string());

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

        if let Some(location) = &self.location {
            write!(f, " {}", location)?;
        }

        Ok(())
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;
