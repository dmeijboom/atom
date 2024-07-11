use crate::lexer::Span;

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
}

#[derive(Debug)]
pub enum Op {
    LoadConst(usize),
    BinaryOp(BinaryOp),
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

#[derive(Debug)]
pub enum Const {
    Int(i64),
}
