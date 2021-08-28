use std::fmt::{Debug, Display, Formatter};
use std::ops::Range;

pub type Pos = Range<usize>;

#[derive(Clone, PartialEq)]
pub struct Label {
    pub name: String,
    pub index: Option<usize>,
}

impl Label {
    pub fn new(name: String) -> Self {
        Self { name, index: None }
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(index) = self.index {
            return write!(f, "label: '{}', index: {}", self.name, index);
        }

        write!(f, "label: '{}'", self.name)
    }
}

fn format_keywords(keywords: &[String]) -> String {
    keywords
        .iter()
        .map(|name| format!("'{}'", name))
        .collect::<Vec<_>>()
        .join(", ")
}

#[derive(Clone, PartialEq)]
pub enum Code {
    ConstNil,
    ConstInt(i64),
    ConstBool(bool),
    ConstFloat(f64),
    ConstChar(char),
    ConstByte(u8),
    ConstString(String),
    MakeArray(usize),
    MakeMap(usize),
    MakeTemplate(usize),
    MakeRange,
    SetLabel(String),
    Jump(Label),
    JumpIfTrue(Label),
    Branch((Label, Label)),
    MakeRef,
    Deref,
    LogicalAnd,
    ArithmeticBitOr,
    ArithmeticBitAnd,
    ArithmeticAdd,
    ArithmeticSub,
    ArithmeticMul,
    ArithmeticDiv,
    ArithmeticExp,
    ComparisonEq,
    ComparisonNeq,
    ComparisonGt,
    ComparisonGte,
    ComparisonLt,
    ComparisonLte,
    AssertIsType,
    Not,
    Unwrap,
    Discard,
    Return,
    Validate,
    Cast(String),
    Call(usize),
    CallKeywords((Vec<String>, usize)),
    CallVoid(usize),
    CallKeywordsVoid((Vec<String>, usize)),
    TailCall(usize),
    Store(usize),
    StoreMut(usize),
    Load(usize),
    LoadReceiver,
    LoadGlobal(usize),
    LoadFn(usize),
    LoadClass(usize),
    LoadInterface(usize),
    LoadTarget,
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
            Code::ConstNil => "  constNil".to_string(),
            Code::ConstInt(val) => format!("  constInt({})", val),
            Code::ConstBool(val) => format!("  constBool({})", val),
            Code::ConstFloat(val) => format!("  constFloat({})", val),
            Code::ConstChar(val) => format!("  constChar({})", val),
            Code::ConstByte(val) => format!("  constByte({})", val),
            Code::ConstString(val) => format!("  constString({})", val),
            Code::MakeArray(size) => format!("  makeArray(size: {})", size),
            Code::MakeMap(size) => format!("  makeMap(size: {})", size),
            Code::MakeTemplate(size) => format!("  makeTemplate(size: {})", size),
            Code::MakeRange => "  makeRange".to_string(),
            Code::SetLabel(label) => format!("{}:", label),
            Code::Jump(label) => format!("  jump({})", label),
            Code::JumpIfTrue(label) => format!("  jumpIfTrue({})", label),
            Code::Branch((label_a, label_b)) => {
                format!("  branch(true: {{{}}}, false: {{{}}})", label_a, label_b)
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
            Code::ArithmeticExp => "  exp".to_string(),
            Code::ComparisonEq => "  eq".to_string(),
            Code::ComparisonNeq => "  neq".to_string(),
            Code::ComparisonGt => "  gt".to_string(),
            Code::ComparisonGte => "  gte".to_string(),
            Code::ComparisonLt => "  lt".to_string(),
            Code::ComparisonLte => "  lte".to_string(),
            Code::AssertIsType => "  assertIsType".to_string(),
            Code::Not => "  not".to_string(),
            Code::Unwrap => "  unwrap".to_string(),
            Code::Discard => "  discard".to_string(),
            Code::Return => "  return".to_string(),
            Code::Validate => "  validate".to_string(),
            Code::Cast(type_name) => format!("  cast(type_name: '{}')", type_name),
            Code::Call(arg_count) => format!("  call(arg_count: {})", arg_count),
            Code::CallKeywords((keywords, arg_count)) => format!(
                "  callKw(keywords: {}, arg_count: {})",
                format_keywords(keywords),
                arg_count
            ),
            Code::CallVoid(arg_count) => format!("  callVoid(arg_count: {})", arg_count),
            Code::CallKeywordsVoid((keywords, arg_count)) => format!(
                "  callKwVoid(keywords: {}, arg_count: {})",
                format_keywords(keywords),
                arg_count
            ),
            Code::TailCall(arg_count) => format!("  tailCall(arg_count: {})", arg_count),
            Code::Store(id) => format!("  store(id: {})", id),
            Code::StoreMut(id) => format!("  storeMut(id: {})", id),
            Code::StoreMember(name) => format!("  storeMember(name: '{}')", name),
            Code::LoadReceiver => "  loadSelf".to_string(),
            Code::Load(id) => format!("  load(id: {})", id),
            Code::LoadGlobal(id) => format!("  loadGlobal(id: {})", id),
            Code::LoadFn(id) => format!("  loadFn(id: {})", id),
            Code::LoadClass(id) => format!("  loadClass(id: {})", id),
            Code::LoadInterface(id) => format!("  loadInterface(id: {})", id),
            Code::LoadIndex => "  loadIndex".to_string(),
            Code::LoadTarget => "  loadTarget".to_string(),
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
        write!(f, "{:?}", self.code)
    }
}
