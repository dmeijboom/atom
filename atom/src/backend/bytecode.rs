use std::{
    fmt::Display,
    hash::{Hash, Hasher},
};

use bytes::{Buf, BufMut};
use num_enum::{FromPrimitive, IntoPrimitive};

use crate::{
    frontend::{Span, Spanned},
    runtime::{errors::RuntimeError, BigInt, Gc, IntoAtom, Value},
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
    TypeAssert,
    Add,
    Sub,
    Mul,
    Rem,
    Div,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    LoadConst,
    LoadConstInt,
    Store,
    Load,
    LoadFn,
    LoadClass,
    LoadAtom,
    Discard,
    Yield,
    Return,
    ReturnLocal,
    Jump,
    JumpIfFalse,
    PushJumpIfFalse,
    PushJumpIfTrue,
    MakeArray,
    MakeSlice,
    Call,
    CallFn,
    CallBuiltin,
    TailCall,
    UnaryNot,
    LoadElement,
    StoreElement,
    LoadMember,
    StoreMember,
    LoadLocal,
    StoreLocal,
    Import,
}

pub trait Serializable {
    fn serialize(&self, buff: &mut impl BufMut);
    fn deserialize(buff: impl Buf) -> Self;
}

pub const BYTECODE_SIZE: usize = 5;

/// There are two types of bytecodes:
/// 1. opcode (8) > code (32)
/// 2. opcode (8) > code1 (16) > code2 (16)
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
            Op::CallFn | Op::CallBuiltin => {
                let (hi, lo) = self.code2();
                write!(f, "{:?} {} {}", self.op, hi, lo)
            }
            Op::Store
            | Op::Load
            | Op::LoadFn
            | Op::LoadClass
            | Op::LoadConst
            | Op::LoadConstInt
            | Op::LoadAtom
            | Op::Jump
            | Op::JumpIfFalse
            | Op::PushJumpIfFalse
            | Op::PushJumpIfTrue
            | Op::MakeArray
            | Op::MakeSlice
            | Op::Call
            | Op::TailCall
            | Op::LoadElement
            | Op::LoadMember
            | Op::StoreMember
            | Op::LoadLocal
            | Op::ReturnLocal => write!(f, "{:?} {}", self.op, self.code),
            _ => write!(f, "{:?}", self.op),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Int(i64),
    BigInt(BigInt),
    Float(f64),
    Str(String),
}

impl Hash for Const {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Const::Int(i) => i.hash(state),
            Const::BigInt(i) => i.hash(state),
            Const::Float(f) => f.to_string().hash(state),
            Const::Str(s) => s.hash(state),
        }
    }
}

impl Eq for Const {}

impl<'gc> IntoAtom<'gc> for Const {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        Ok(match self {
            Const::Int(i) => i.into_atom(gc)?,
            Const::BigInt(i) => i.into_atom(gc)?,
            Const::Float(n) => Value::from(n),
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

        let opcode = Bytecode::with_code(Op::LoadLocal, 2394);
        assert_eq!(opcode.op, Op::LoadLocal);
        assert_eq!(opcode.code, 2394);
    }

    #[test]
    fn test_with_code2() {
        let opcode = Bytecode::with_code2(Op::CallFn, 0, 1);
        assert_eq!(opcode.op, Op::CallFn);
        assert_eq!(opcode.code2(), (0, 1));
    }
}
