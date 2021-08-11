use std::error::Error;
use std::fmt;

use crate::ast::Pos;
use crate::vm::Module;

#[derive(Debug, PartialEq)]
pub struct Trace {
    pub pos: Pos,
    pub target: String,
}

#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    pub message: String,
    pub pos: Option<Pos>,
    pub module_name: Option<String>,
    pub filename: Option<String>,
    pub stack_trace: Vec<Trace>,
}

impl RuntimeError {
    pub fn new(message: String) -> Self {
        Self {
            message,
            pos: None,
            module_name: None,
            filename: None,
            stack_trace: vec![],
        }
    }

    pub fn with_message(mut self, message: String) -> Self {
        self.message = message;

        self
    }

    pub fn with_pos(mut self, pos: Pos) -> Self {
        self.pos = Some(pos);

        self
    }

    pub fn with_stack_trace(mut self, stack_trace: Vec<Trace>) -> Self {
        self.stack_trace = stack_trace;

        self
    }

    pub fn with_module(mut self, module: &Module) -> Self {
        self.module_name = Some(module.name.clone());
        self.filename = module
            .filename
            .clone()
            .and_then(|filename| filename.to_str().and_then(|s| Some(s.to_string())));

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
