use std::rc::Rc;

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

#[derive(Debug, Default, Clone)]
pub struct Func {
    pub name: String,
    pub codes: Rc<Vec<Code>>,
}

impl Func {
    pub fn with_codes(name: String, codes: Vec<Code>) -> Self {
        Self {
            name,
            codes: Rc::new(codes),
        }
    }
}

pub struct Cursor {
    n: usize,
    codes: Rc<Vec<Code>>,
}

impl Cursor {
    pub fn new(codes: &Rc<Vec<Code>>) -> Self {
        Self {
            n: 0,
            codes: Rc::clone(codes),
        }
    }

    pub fn next(&mut self) -> Option<&Code> {
        let code = self.codes.get(self.n);
        self.n += 1;
        code
    }

    pub fn cur(&self) -> Option<&Code> {
        self.codes.get(self.n)
    }

    pub fn goto(&mut self, n: usize) {
        self.n = n;
    }

    pub fn goto_end(&mut self) {
        self.n = self.codes.len();
    }
}
