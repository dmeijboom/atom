use crate::frontend::syntax::{LiteralKind, Span};

#[derive(Debug, Clone)]
pub enum Type {
    Array,
    Float32,
    Float64,
    Fn,
    Int1,
    Int8,
    Int16,
    Int32,
    Int64,
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
    IntAdd,
    IntSub,
    IntMul,
    IntSDiv,
    IntUDiv,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
}

#[derive(Debug, Default, Clone)]
pub struct Block {
    pub body: Vec<Instr>,
    pub term: Option<Terminator>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub body: Vec<Block>,
    pub return_type: Type,
}

#[derive(Debug, Default, Clone)]
pub struct Module {
    pub name: String,
    pub funcs: Vec<Fn>,
}
