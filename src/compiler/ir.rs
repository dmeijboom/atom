use crate::ast::Pos;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct LocalId {
    pub name: String,
    pub scope_hint: Option<usize>,
}

impl LocalId {
    pub fn new(name: String) -> Self {
        Self {
            name,
            scope_hint: None,
        }
    }

    pub fn new_in_scope(name: String, scope_hint: usize) -> Self {
        Self {
            name,
            scope_hint: Some(scope_hint),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Code {
    ConstInt(i64),
    ConstBool(bool),
    ConstFloat(f64),
    ConstChar(char),
    ConstString(String),
    MakeArray(usize),
    MakeMap(usize),
    MakeRange,
    SetLabel(String),
    Jump(String),
    JumpIfTrue(String),
    Branch((String, String)),
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
    Discard,
    Return,
    Call((Vec<String>, usize)),
    Store(LocalId),
    StoreMut(LocalId),
    Load(LocalId),
    LoadIndex,
    LoadMember(String),
    LoadMemberPtr(String),
    StorePtr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IR {
    pub code: Code,
    pub pos: Pos,
}

impl IR {
    pub fn new(code: Code, pos: Pos) -> Self {
        Self { code, pos }
    }
}
