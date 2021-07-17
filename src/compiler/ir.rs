use crate::ast::Pos;

#[derive(Debug, Clone, PartialEq)]
pub enum Code {
    ConstInt(i64),
    ConstBool(bool),
    ConstFloat(f64),
    ConstChar(char),
    ConstString(String),
    MakeArray(usize),
    MakeMap(usize),
    LogicalOr,
    LogicalAnd,
    ArithmeticBitOr,
    ArithmeticBitAnd,
    ArithmeticAdd,
    ArithmeticSub,
    ArithmeticMul,
    ArithmeticDiv,
    ComparisonEq,
    ComparisonNeq,
    ComparisonGt,
    ComparisonGte,
    ComparisonLt,
    ComparisonLte,
    Not,
    Call(usize),
    Store(String),
    StoreMut(String),
    Load(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IR {
    pub code: Code,
    pub pos: Pos,
}

impl IR {
    pub fn new(code: Code, pos: Pos) -> Self {
        Self {
            code,
            pos,
        }
    }
}
