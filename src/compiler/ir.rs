use std::fmt::{Debug, Formatter};

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

#[derive(Clone, PartialEq)]
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
    Import(String),
    Call(usize),
    CallWithKeywords((Vec<String>, usize)),
    Store(LocalId),
    StoreMut(LocalId),
    Load(LocalId),
    LoadIndex,
    LoadMember(String),
    TeeMember(String),
    LoadMemberPtr(String),
    StorePtr,
}

fn format_local_id(id: &LocalId) -> String {
    if let Some(scope_hint) = id.scope_hint {
        return format!("name: '{}', scope: {}", id.name, scope_hint);
    }

    format!("name: '{}'", id.name)
}

impl Code {
    fn description(&self) -> String {
        match self {
            Code::ConstInt(val) => format!("  constInt({})", val),
            Code::ConstBool(val) => format!("  constBool({})", val),
            Code::ConstFloat(val) => format!("  constFloat({})", val),
            Code::ConstChar(val) => format!("  constChar({})", val),
            Code::ConstString(val) => format!("  constString({})", val),
            Code::Import(name) => format!("  import(name: '{}')", name),
            Code::MakeArray(size) => format!("  makeArray(size: {})", size),
            Code::MakeMap(size) => format!("  makeMap(size: {})", size),
            Code::MakeRange => "  makeRange".to_string(),
            Code::SetLabel(label) => format!("{}:", label),
            Code::Jump(label) => format!("  jump(label: '{}')", label),
            Code::JumpIfTrue(label) => format!("  jumpIfTrue(label: '{}')", label),
            Code::Branch((label_a, label_b)) => {
                format!("  branch(label_a: '{}', label_b: '{}')", label_a, label_b)
            }
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
            Code::Store(id) => format!("  store({})", format_local_id(id)),
            Code::StoreMut(id) => format!("  storeMut({})", format_local_id(id)),
            Code::Load(id) => format!("  load({})", format_local_id(id)),
            Code::LoadIndex => "  loadIndex".to_string(),
            Code::LoadMember(name) => format!("  loadMember(name: '{}')", name),
            Code::TeeMember(name) => format!("  teeMember(name: '{}')", name),
            Code::LoadMemberPtr(name) => format!("  loadMemberPtr(name: '{}')", name),
            Code::StorePtr => "  storePtr".to_string(),
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
