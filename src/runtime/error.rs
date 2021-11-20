use std::error::Error;
use std::fmt;
use std::fmt::{Display, Formatter};

use crate::compiler::ir::Location;

use super::origin::Origin;

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    FatalError,
    TypeError,
    IOError,
    UserError,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::FatalError => write!(f, "FatalError"),
            ErrorKind::TypeError => write!(f, "TypeError"),
            ErrorKind::IOError => write!(f, "IOError"),
            ErrorKind::UserError => write!(f, "UserError"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Trace {
    pub origin: Origin,
    pub target: String,
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub kind: ErrorKind,
    pub message: String,
    pub location: Option<Location>,
    pub module_name: Option<String>,
    pub filename: Option<String>,
    pub stack_trace: Vec<Trace>,
}

impl RuntimeError {
    pub fn new(kind: ErrorKind, message: String) -> Self {
        Self {
            kind,
            message,
            location: None,
            module_name: None,
            filename: None,
            stack_trace: vec![],
        }
    }

    pub fn with_context(mut self, context: &str) -> Self {
        self.message = format!("{}: {}", context, self.message);

        self
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

    pub fn with_filename(mut self, filename: String) -> Self {
        self.filename = Some(filename);

        self
    }
}

impl Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.stack_trace.is_empty() {
            writeln!(f, "Stack trace:")?;
        }

        for trace in self.stack_trace.iter() {
            if let Some(filename) = &trace.origin.filename {
                writeln!(
                    f,
                    "  > at {}(..) in {}:{}",
                    trace.target, filename, trace.origin.location
                )?;
            } else {
                writeln!(f, "  > at {}(..)", trace.target)?;
            }
        }

        write!(f, "{}: {}", self.kind, self.message)?;

        if let Some(location) = &self.location {
            if let Some(filename) = &self.filename {
                write!(f, " in {}:{}:{}", filename, location.line, location.column)?;
            }
        }

        Ok(())
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;
