use crate::syntax::{LiteralKind, Span};

#[derive(Debug, Clone)]
pub enum BasicType {
    Array,
    Float,
    Fn,
    Int,
    Ptr,
    Struct,
    Vec,
    Void,
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return,
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub span: Span,
    pub kind: InstrKind,
}

impl Instr {
    pub fn new(span: Span, kind: InstrKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum InstrKind {
    Const(LiteralKind),
}

#[derive(Debug, Default, Clone)]
pub struct Block {
    pub body: Vec<Instr>,
    pub term: Option<Terminator>,
}

#[derive(Debug, Default, Clone)]
pub struct Fn {
    pub name: String,
    pub body: Vec<Block>,
}

#[derive(Debug, Default, Clone)]
pub struct Module {
    pub name: String,
    pub funcs: Vec<Fn>,
}
