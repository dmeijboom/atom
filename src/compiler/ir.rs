use std::fmt::{Debug, Formatter};

use crate::ast::Pos;

#[derive(Clone, PartialEq)]
pub enum Code {
    ConstInt(i64),
    ConstBool(bool),
    ConstFloat(f64),
    ConstChar(char),
    ConstByte(u8),
    ConstString(String),
    MakeArray(usize),
    MakeMap(usize),
    MakeRange,
    SetLabel(String),
    Jump(String),
    JumpIfTrue(String),
    Branch((String, String)),
    MakeRef,
    Deref,
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
    Validate,
    Cast(String),
    Call(usize),
    CallWithKeywords((Vec<String>, usize)),
    Store(usize),
    StoreMut(usize),
    Load(usize),
    LoadName(String),
    LoadIndex,
    StoreIndex,
    LoadMember(String),
    TeeMember(String),
    StoreMember(String),
    Raise,
}

impl Code {
    fn description(&self) -> String {
        match self {
            Code::ConstInt(val) => format!("  constInt({})", val),
            Code::ConstBool(val) => format!("  constBool({})", val),
            Code::ConstFloat(val) => format!("  constFloat({})", val),
            Code::ConstChar(val) => format!("  constChar({})", val),
            Code::ConstByte(val) => format!("  constByte({})", val),
            Code::ConstString(val) => format!("  constString({})", val),
            Code::MakeArray(size) => format!("  makeArray(size: {})", size),
            Code::MakeMap(size) => format!("  makeMap(size: {})", size),
            Code::MakeRange => "  makeRange".to_string(),
            Code::SetLabel(label) => format!("{}:", label),
            Code::Jump(label) => format!("  jump(label: '{}')", label),
            Code::JumpIfTrue(label) => format!("  jumpIfTrue(label: '{}')", label),
            Code::Branch((label_a, label_b)) => {
                format!("  branch(label_a: '{}', label_b: '{}')", label_a, label_b)
            }
            Code::MakeRef => "  makeRef".to_string(),
            Code::Deref => "  deref".to_string(),
            Code::LogicalAnd => "  and".to_string(),
            Code::ArithmeticBitOr => "  bit_or".to_string(),
            Code::ArithmeticBitAnd => "  bin_and".to_string(),
            Code::ArithmeticAdd => "  add".to_string(),
            Code::ArithmeticSub => "  sub".to_string(),
            Code::ArithmeticMul => "  mul".to_string(),
            Code::ArithmeticDiv => "  div".to_string(),
            Code::ComparisonEq => "  eq".to_string(),
            Code::ComparisonNeq => "  neq".to_string(),
            Code::ComparisonGt => "  gt".to_string(),
            Code::ComparisonGte => "  gte".to_string(),
            Code::ComparisonLt => "  lt".to_string(),
            Code::ComparisonLte => "  lte".to_string(),
            Code::Not => "  not".to_string(),
            Code::Discard => "  discard".to_string(),
            Code::Return => "  return".to_string(),
            Code::Validate => "  validate".to_string(),
            Code::Cast(type_name) => format!("  cast(type_name: '{}')", type_name),
            Code::Call(arg_count) => format!("  call(arg_count: {})", arg_count),
            Code::CallWithKeywords((keywords, arg_count)) => format!(
                "  callKw(keywords: {}, arg_count: {})",
                keywords
                    .iter()
                    .map(|name| format!("'{}'", name))
                    .collect::<Vec<_>>()
                    .join(", "),
                arg_count
            ),
            Code::Store(id) => format!("  store(id: {})", id),
            Code::StoreMut(id) => format!("  storeMut(id: {})", id),
            Code::StoreMember(name) => format!("  storeMember(name: '{}')", name),
            Code::Load(id) => format!("  load(id: {})", id),
            Code::LoadName(name) => format!("  loadName(name: '{}')", name),
            Code::LoadIndex => "  loadIndex".to_string(),
            Code::StoreIndex => "  storeIndex".to_string(),
            Code::LoadMember(name) => format!("  loadMember(name: '{}')", name),
            Code::TeeMember(name) => format!("  teeMember(name: '{}')", name),
            Code::Raise => "  raise".to_string(),
        }
    }
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.description())
    }
}

#[derive(Clone, PartialEq)]
pub struct IR {
    pub code: Code,
    pub pos: Pos,
}

impl IR {
    pub fn new(code: Code, pos: Pos) -> Self {
        Self { code, pos }
    }
}

impl Debug for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}:\t {:?}", self.pos.start, self.pos.end, self.code)
    }
}
