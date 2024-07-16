use crate::lexer::Span;

#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Copy)]
pub enum Op {
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Add,
    Sub,
    Mul,
    Div,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    LoadConst(usize),
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

#[derive(Debug, Clone)]
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
