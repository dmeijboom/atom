use std::{
    borrow::Cow,
    fmt::{Debug, Display},
};

use crate::lexer::Span;

use super::value::{Type, TypeAssert};

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("type mismatch: {left} != {right}")]
    TypeMismatch { left: Type, right: TypeAssert },
    #[error("index out of bounds: {0}")]
    IndexOutOfBounds(usize),
    #[error("cannot call non-function: {0}")]
    NotCallable(Type),
    #[error(
        "invalid argument count on '{}(..)': expected {}, got: {}",
        func_name,
        arg_count,
        func_arg_count
    )]
    ArgCountMismatch {
        arg_count: u32,
        func_name: Cow<'static, str>,
        func_arg_count: u32,
    },
    #[error("no such field '{field}' in {ty}")]
    UnknownField { ty: Type, field: String },
    #[error("no such attribute '{attribute}' in {}", class_name)]
    UnknownAttr {
        class_name: Cow<'static, str>,
        attribute: String,
    },
    #[error("invalid memory layout")]
    InvalidMemoryLayout,
    #[error("missing attribute: {0}")]
    MissingAttribute(&'static str),
    #[error("unsupported operation: {left} {op} {right}")]
    UnsupportedBinaryOp {
        left: Type,
        right: Type,
        op: &'static str,
    },
    #[error("unsupported operation {lty} {op} {rty}")]
    UnsupportedOp {
        lty: Type,
        rty: Type,
        op: &'static str,
    },
}

impl ErrorKind {
    pub fn at(self, span: Span) -> RuntimeError {
        RuntimeError {
            kind: self,
            span,
            trace: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    #[allow(dead_code)]
    pub span: Span,
    pub func_name: Option<Cow<'static, str>>,
}

impl Call {
    pub fn new(span: Span, func_name: Option<Cow<'static, str>>) -> Self {
        Call { span, func_name }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.func_name {
            Some(name) => write!(f, "in function '{}'", name),
            None => write!(f, "in main"),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: ErrorKind,
    #[allow(dead_code)]
    pub span: Span,
    pub trace: Option<Vec<Call>>,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

impl std::error::Error for RuntimeError {}
