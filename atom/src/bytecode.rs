use std::{fmt::Display, ops::Deref};

use bytes::{Buf, BufMut};
use num_enum::{FromPrimitive, IntoPrimitive};

use crate::{
    gc::Gc,
    lexer::Span,
    runtime::{
        error::RuntimeError,
        value::{IntoAtom, Value},
    },
};

#[derive(FromPrimitive, IntoPrimitive)]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Op {
    #[default]
    Nop,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Add,
    Sub,
    Mul,
    Rem,
    Div,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    LoadConst,
    Store,
    Load,
    LoadFn,
    LoadClass,
    Discard,
    Return,
    ReturnArg,
    Jump,
    JumpIfFalse,
    PushJumpIfFalse,
    PushJumpIfTrue,
    MakeArray,
    MakeSlice,
    Call,
    CallFn,
    CallExtern,
    TailCall,
    UnaryNot,
    LoadElement,
    StoreElement,
    LoadMember,
    StoreMember,
    LoadArg,
    Import,
}

pub trait Serializable {
    fn serialize(&self, buff: &mut impl BufMut);
    fn deserialize(buff: impl Buf) -> Self;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T: Serializable + Clone> {
    pub inner: T,
    pub span: Span,
}

impl<T: Serializable + Clone> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Serializable + Clone> Serializable for Spanned<T> {
    fn serialize(&self, buff: &mut impl BufMut) {
        self.inner.serialize(buff);
        buff.put_uint(self.span.offset as u64, 3);
    }

    fn deserialize(mut buff: impl Buf) -> Self {
        let inner = T::deserialize(&mut buff);
        let offset = buff.get_uint(3) as u32;

        Self {
            inner,
            span: Span { offset },
        }
    }
}

/// There are two types of bytecodes:
/// 1. opcode (8) > code (32) > offset (24)
/// 2. opcode (8) > code1 (16) > code2 (16) > offset (24)
#[derive(Debug, Clone, PartialEq)]
pub struct Bytecode {
    pub op: Op,
    pub code: u32,
}

impl Bytecode {
    pub fn new(op: Op) -> Self {
        Self::with_code(op, 0)
    }

    pub fn with_code(op: Op, code: u32) -> Self {
        Self { op, code }
    }

    pub fn with_code2(op: Op, code1: u16, code2: u16) -> Self {
        Self::with_code(op, (code1 as u32) << 16 | code2 as u32)
    }

    pub fn at(self, span: Span) -> Spanned<Self> {
        Spanned { inner: self, span }
    }

    pub fn code2(&self) -> (u16, u16) {
        let code = self.code;
        ((code >> 16) as u16, code as u16)
    }
}

impl Serializable for Bytecode {
    fn serialize(&self, buff: &mut impl BufMut) {
        buff.put_u8(self.op.into());
        buff.put_u32(self.code);
    }

    fn deserialize(mut buff: impl Buf) -> Self {
        let op = buff.get_u8().into();
        let code = buff.get_u32();

        Self { op, code }
    }
}

impl Display for Bytecode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.op {
            Op::CallFn => {
                let (hi, lo) = self.code2();
                write!(f, "{:?} {} {}", self.op, hi, lo)
            }
            Op::Store
            | Op::Load
            | Op::LoadFn
            | Op::LoadClass
            | Op::LoadConst
            | Op::Jump
            | Op::JumpIfFalse
            | Op::PushJumpIfFalse
            | Op::PushJumpIfTrue
            | Op::MakeArray
            | Op::MakeSlice
            | Op::Call
            | Op::CallExtern
            | Op::TailCall
            | Op::LoadElement
            | Op::LoadMember
            | Op::LoadArg
            | Op::ReturnArg => write!(f, "{:?} {}", self.op, self.code),
            _ => write!(f, "{:?}", self.op),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Nil,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

impl Eq for Const {}

impl<'gc> IntoAtom<'gc> for Const {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        Ok(match self {
            Const::Nil => Value::NIL,
            Const::Int(n) => n.into_atom(gc)?,
            Const::Float(n) => Value::from(n),
            Const::Bool(b) => Value::from(b),
            Const::Str(s) => s.into_atom(gc)?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_with_code() {
        let opcode = Bytecode::with_code(Op::Eq, 0);
        assert_eq!(opcode.op, Op::Eq);
        assert_eq!(opcode.code, 0);

        let opcode = Bytecode::with_code(Op::LoadArg, 2394);
        assert_eq!(opcode.op, Op::LoadArg);
        assert_eq!(opcode.code, 2394);
    }

    #[test]
    fn test_with_code2() {
        let opcode = Bytecode::with_code2(Op::CallFn, 0, 1);
        assert_eq!(opcode.op, Op::CallFn);
        assert_eq!(opcode.code2(), (0, 1));
    }
}
