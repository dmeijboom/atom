use crate::lexer::Span;

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
}

#[derive(Debug, Clone, Copy)]
pub enum CompareOp {
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Copy)]
pub enum Op {
    LoadConst(usize),
    BinaryOp(BinaryOp),
    CompareOp(CompareOp),
    Store(usize),
    Load(usize),
    Discard,
    Return,
    JumpIfFalse(usize),
    JumpIfTrue(usize),
    MakeArray(usize),
    LoadElement,
    UnaryNot,
}

impl Op {
    pub fn at(self, span: Span) -> Code {
        Code { op: self, span }
    }
}

#[derive(Debug)]
pub struct Code {
    pub op: Op,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}
