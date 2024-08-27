use std::fmt::Display;

use bytes::{Buf, BufMut};

use crate::lexer::Span;

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    Rem,
    Div,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    LoadConst,
    Store,
    Load,
    LoadFn,
    LoadSelf,
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
    TailCallFn,
    UnaryNot,
    LoadElement,
    StoreElement,
    LoadMember,
    StoreMember,
    LoadArg,
}

const TAG_MASK: u64 = 0b111111 << 48;
const INT_MASK: u64 = 0xffff_ffff_ffff;

#[derive(Debug, Clone, PartialEq)]
pub struct Opcode {
    bits: u64,
    span: Span,
}

impl Opcode {
    pub fn new(op: Op) -> Self {
        Self::with_code(op, 0)
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn with_code(op: Op, code: usize) -> Self {
        Self {
            bits: (op as u64) << 48 | code as u64,
            span: Span::default(),
        }
    }

    pub fn with_code2(op: Op, code1: u32, code2: u32) -> Self {
        let code = (code1 as u64) << 32 | code2 as u64;
        Self::with_code(op, code as usize)
    }

    pub fn at(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn op(&self) -> Op {
        match (self.bits & TAG_MASK) >> 48 {
            o if o == Op::Eq as u64 => Op::Eq,
            o if o == Op::Ne as u64 => Op::Ne,
            o if o == Op::Lt as u64 => Op::Lt,
            o if o == Op::Lte as u64 => Op::Lte,
            o if o == Op::Gt as u64 => Op::Gt,
            o if o == Op::Gte as u64 => Op::Gte,
            o if o == Op::Add as u64 => Op::Add,
            o if o == Op::Sub as u64 => Op::Sub,
            o if o == Op::Mul as u64 => Op::Mul,
            o if o == Op::Div as u64 => Op::Div,
            o if o == Op::Rem as u64 => Op::Rem,
            o if o == Op::BitwiseOr as u64 => Op::BitwiseOr,
            o if o == Op::BitwiseAnd as u64 => Op::BitwiseAnd,
            o if o == Op::BitwiseXor as u64 => Op::BitwiseXor,
            o if o == Op::LoadConst as u64 => Op::LoadConst,
            o if o == Op::Store as u64 => Op::Store,
            o if o == Op::Load as u64 => Op::Load,
            o if o == Op::LoadFn as u64 => Op::LoadFn,
            o if o == Op::LoadSelf as u64 => Op::LoadSelf,
            o if o == Op::LoadClass as u64 => Op::LoadClass,
            o if o == Op::Discard as u64 => Op::Discard,
            o if o == Op::Return as u64 => Op::Return,
            o if o == Op::ReturnArg as u64 => Op::ReturnArg,
            o if o == Op::Jump as u64 => Op::Jump,
            o if o == Op::JumpIfFalse as u64 => Op::JumpIfFalse,
            o if o == Op::PushJumpIfFalse as u64 => Op::PushJumpIfFalse,
            o if o == Op::PushJumpIfTrue as u64 => Op::PushJumpIfTrue,
            o if o == Op::MakeArray as u64 => Op::MakeArray,
            o if o == Op::MakeSlice as u64 => Op::MakeSlice,
            o if o == Op::Call as u64 => Op::Call,
            o if o == Op::CallFn as u64 => Op::CallFn,
            o if o == Op::CallExtern as u64 => Op::CallExtern,
            o if o == Op::TailCall as u64 => Op::TailCall,
            o if o == Op::TailCallFn as u64 => Op::TailCallFn,
            o if o == Op::UnaryNot as u64 => Op::UnaryNot,
            o if o == Op::LoadElement as u64 => Op::LoadElement,
            o if o == Op::StoreElement as u64 => Op::StoreElement,
            o if o == Op::LoadMember as u64 => Op::LoadMember,
            o if o == Op::StoreMember as u64 => Op::StoreMember,
            o if o == Op::LoadArg as u64 => Op::LoadArg,
            _ => unreachable!(),
        }
    }

    pub fn code(&self) -> usize {
        (self.bits & INT_MASK) as usize
    }

    pub fn code2(&self) -> (u32, u32) {
        let code = self.code();
        ((code >> 32) as u32, code as u32)
    }

    pub fn serialize(&self, buff: &mut impl BufMut) {
        buff.put_u64(self.bits);
        self.span.serialize(buff)
    }

    pub fn deserialize(mut buff: impl Buf) -> Self {
        let bits = buff.get_u64();
        let span = Span::deserialize(buff);

        Self { bits, span }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.op() {
            Op::CallFn | Op::TailCallFn => {
                let (hi, lo) = self.code2();
                write!(f, "{:?} {} {}", self.op(), hi, lo)
            }
            Op::Store
            | Op::Load
            | Op::LoadFn
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
            | Op::ReturnArg => write!(f, "{:?} {}", self.op(), self.code()),
            _ => write!(f, "{:?}", self.op(),),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_with_code() {
        let opcode = Opcode::with_code(Op::Eq, 0);
        assert_eq!(opcode.op(), Op::Eq);
        assert_eq!(opcode.code(), 0);

        let opcode = Opcode::with_code(Op::LoadArg, 2394);
        assert_eq!(opcode.op(), Op::LoadArg);
        assert_eq!(opcode.code(), 2394);
    }

    #[test]
    fn test_with_code2() {
        let opcode = Opcode::with_code2(Op::CallFn, 0, 1);
        assert_eq!(opcode.op(), Op::CallFn);
        assert_eq!(opcode.code2(), (0, 1));
    }
}
