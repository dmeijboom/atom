use crate::lexer::Span;

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    LoadConst,
    Store,
    Load,
    LoadFunc,
    LoadNativeFunc,
    Discard,
    Return,
    JumpIfFalse,
    PushJumpIfFalse,
    PushJumpIfTrue,
    MakeArray,
    Call,
    DirectCall,
    TailCall,
    UnaryNot,
    LoadElement,
    LoadMember,
    LoadArg,
}

const TAG_MASK: u64 = 0b11111 << 48;
const INT_MASK: u64 = 0xffff_ffff_ffff;

#[derive(Debug, Clone)]
pub struct Opcode {
    bits: u64,
    pub span: Span,
}

impl Opcode {
    pub fn new(op: Op) -> Self {
        Self::with_code(op, 0)
    }

    pub fn with_code2(op: Op, hi: u32, low: u32) -> Self {
        Self::with_code(op, (hi as usize) << 32 | low as usize)
    }

    pub fn with_code(op: Op, code: usize) -> Self {
        Self {
            bits: (op as u64) << 48 | code as u64,
            span: Span::default(),
        }
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
            o if o == Op::BitwiseOr as u64 => Op::BitwiseOr,
            o if o == Op::BitwiseAnd as u64 => Op::BitwiseAnd,
            o if o == Op::BitwiseXor as u64 => Op::BitwiseXor,
            o if o == Op::LoadConst as u64 => Op::LoadConst,
            o if o == Op::Store as u64 => Op::Store,
            o if o == Op::Load as u64 => Op::Load,
            o if o == Op::LoadFunc as u64 => Op::LoadFunc,
            o if o == Op::DirectCall as u64 => Op::DirectCall,
            o if o == Op::LoadNativeFunc as u64 => Op::LoadNativeFunc,
            o if o == Op::Discard as u64 => Op::Discard,
            o if o == Op::Return as u64 => Op::Return,
            o if o == Op::JumpIfFalse as u64 => Op::JumpIfFalse,
            o if o == Op::PushJumpIfFalse as u64 => Op::PushJumpIfFalse,
            o if o == Op::PushJumpIfTrue as u64 => Op::PushJumpIfTrue,
            o if o == Op::MakeArray as u64 => Op::MakeArray,
            o if o == Op::Call as u64 => Op::Call,
            o if o == Op::TailCall as u64 => Op::TailCall,
            o if o == Op::UnaryNot as u64 => Op::UnaryNot,
            o if o == Op::LoadElement as u64 => Op::LoadElement,
            o if o == Op::LoadMember as u64 => Op::LoadMember,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
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
        let opcode = Opcode::with_code2(Op::Eq, 0, 0);
        assert_eq!(opcode.op(), Op::Eq);
        assert_eq!(opcode.code2(), (0, 0));

        let opcode = Opcode::with_code2(Op::LoadArg, 2394, 403);
        assert_eq!(opcode.op(), Op::LoadArg);
        assert_eq!(opcode.code2(), (2394, 403));
    }
}
