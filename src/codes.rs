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
    LoadFunc(usize),
    LoadNativeFunc(usize),
    Discard,
    Return,
    JumpIfFalse(usize),
    PushJumpIfFalse(usize),
    PushJumpIfTrue(usize),
    MakeArray(usize),
    Call(usize),
    TailCall(usize),
    UnaryNot,
    LoadElement,
    LoadMember(usize),
    LoadArg(usize),
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

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

impl Eq for Const {}
