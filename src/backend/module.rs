use crate::frontend::syntax::Span;
use std::slice::Iter;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Terminator {
    Return,
}

#[derive(Debug)]
pub struct Instr {
    pub span: Span,
    pub kind: InstrKind,
}

impl Instr {
    pub fn new(span: Span, kind: InstrKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug)]
pub enum InstrKind {
    IntAdd,
    IntSub,
    IntMul,
    IntSDiv,
    IntUDiv,
    IntShl,
    IntSShr,
    IntUShr,
    IntSLte,
    IntSLt,
    IntULte,
    IntULt,
    IntSGte,
    IntSGt,
    IntUGte,
    IntUGt,
    IntEq,
    IntNeq,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatLte,
    FloatLt,
    FloatGte,
    FloatGt,
    FloatEq,
    FloatNeq,
    Load(usize),
    Store(usize),
    ConstInt(i64),
    ConstUint(u64),
    ConstBool(bool),
    ConstFloat(f64),
    Branch(Block, Block),
}

#[derive(Debug, Default)]
pub struct Block {
    pub body: Vec<Instr>,
    pub term: Option<Terminator>,
}

impl Block {
    #[inline]
    pub fn iter(&self) -> Iter<'_, Instr> {
        self.body.iter()
    }
}

#[derive(Debug)]
pub struct Fn {
    pub name: String,
    pub body: Block,
    pub return_type: Type,
    pub locals: Vec<Type>,
}

#[derive(Default, Debug)]
pub struct Module {
    pub name: String,
    pub funcs: Vec<Fn>,
}
