use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::{Index, IndexMut, Range};
use std::slice::{Iter, IterMut};

#[derive(Debug, Clone, PartialEq, Hash, Default)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub offset: Range<usize>,
}

impl Location {
    pub fn new(offset: Range<usize>, line: usize, column: usize) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "on line {} at column {}", self.line, self.column)
    }
}

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
    ConstInt(i64),
    ConstUint(u64),
    ConstBool(bool),
    ConstFloat(f64),
    ConstChar(char),
    ConstByte(u8),
    ConstSymbol(String),
    ConstString(String),
    MakeArray(usize),
    MakeTuple(usize),
    MakeTemplate(usize),
    MakeRange,
    SetLabel(String),
    Jump(Label),
    JumpIfTrue(Label),
    Branch((Label, Label)),
    MakeRef,
    Deref,
    LogicalAnd,
    ArithmeticAdd,
    ArithmeticSub,
    ArithmeticMul,
    ArithmeticDiv,
    ArithmeticExp,
    ArithmeticBitOr,
    ArithmeticBitAnd,
    ArithmeticBitXor,
    ArithmeticBitShiftLeft,
    ArithmeticBitShiftRight,
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
    MakeClosure(usize),
    LoadClass(usize),
    LoadInterface(usize),
    LoadTarget,
    LoadIndex,
    TeeIndex,
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
            Code::ConstUint(val) => format!("  constUint({})", val),
            Code::ConstBool(val) => format!("  constBool({})", val),
            Code::ConstFloat(val) => format!("  constFloat({})", val),
            Code::ConstChar(val) => format!("  constChar({})", val),
            Code::ConstByte(val) => format!("  constByte({})", val),
            Code::ConstSymbol(name) => format!("  constSymbol({})", name),
            Code::ConstString(val) => format!("  constString({})", val),
            Code::MakeArray(size) => format!("  makeArray(size: {})", size),
            Code::MakeTuple(size) => format!("  makeTuple(size: {})", size),
            Code::MakeTemplate(size) => format!("  makeTemplate(size: {})", size),
            Code::MakeRange => "  makeRange".to_string(),
            Code::MakeClosure(id) => format!("  makeClosure(id: {})", id),
            Code::SetLabel(label) => format!("{}:", label),
            Code::Jump(label) => format!("  jump({})", label),
            Code::JumpIfTrue(label) => format!("  jumpIfTrue({})", label),
            Code::Branch((label_a, label_b)) => {
                format!("  branch(true: {{{}}}, false: {{{}}})", label_a, label_b)
            }
            Code::MakeRef => "  makeRef".to_string(),
            Code::Deref => "  deref".to_string(),
            Code::LogicalAnd => "  and".to_string(),
            Code::ArithmeticAdd => "  add".to_string(),
            Code::ArithmeticSub => "  sub".to_string(),
            Code::ArithmeticMul => "  mul".to_string(),
            Code::ArithmeticDiv => "  div".to_string(),
            Code::ArithmeticExp => "  exp".to_string(),
            Code::ArithmeticBitOr => "  bit_or".to_string(),
            Code::ArithmeticBitAnd => "  bit_and".to_string(),
            Code::ArithmeticBitXor => "  bit_xor".to_string(),
            Code::ArithmeticBitShiftLeft => "  bit_shift_left".to_string(),
            Code::ArithmeticBitShiftRight => "  bit_shift_right".to_string(),
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
            Code::TeeIndex => "  teeIndex".to_string(),
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

#[derive(Clone)]
pub struct IR {
    codes: Vec<Code>,
    locations: HashMap<usize, Location>,
}

impl IR {
    pub fn new() -> Self {
        Self {
            codes: vec![],
            locations: HashMap::new(),
        }
    }

    pub fn with_codes(codes: Vec<Code>) -> Self {
        Self {
            codes,
            locations: HashMap::new(),
        }
    }

    pub fn remove(&mut self, index: usize) -> Code {
        self.codes.remove(index)
    }

    pub fn len(&self) -> usize {
        self.codes.len()
    }

    pub fn get(&self, index: usize) -> Option<&Code> {
        self.codes.get(index)
    }

    pub unsafe fn get_unchecked(&self, index: usize) -> &Code {
        self.codes.get_unchecked(index)
    }

    pub fn iter(&self) -> Iter<Code> {
        self.codes.iter()
    }

    pub fn iter_mut(&mut self) -> IterMut<Code> {
        self.codes.iter_mut()
    }

    pub fn add(&mut self, code: Code, location: Option<&Location>) {
        let id = self.codes.len();

        self.codes.push(code);

        if let Some(location) = location {
            if id > 0 {
                if let Some(other) = self.get_location(id - 1) {
                    if other == location {
                        return;
                    }
                }
            }

            self.locations.insert(self.codes.len(), location.clone());
        }
    }

    pub fn append(&mut self, mut ir: IR) {
        for (i, location) in ir.locations {
            self.locations.insert(self.codes.len() + i, location);
        }

        self.codes.append(&mut ir.codes);
    }

    pub fn get_location(&self, index: usize) -> Option<&Location> {
        let mut result = None;

        for (i, location) in self.locations.iter() {
            if *i < index {
                continue;
            }

            if let Some((other_idx, _)) = result {
                if *i < other_idx {
                    continue;
                }
            }

            result = Some((*i, location));
        }

        result.map(|(_, location)| location)
    }
}

impl Index<usize> for IR {
    type Output = Code;

    fn index(&self, index: usize) -> &Self::Output {
        &self.codes[index]
    }
}

impl IndexMut<usize> for IR {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.codes[index]
    }
}

impl Debug for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for code in self.codes.iter() {
            write!(f, "{:?}", code)?;
        }

        Ok(())
    }
}
