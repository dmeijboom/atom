use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::{Deref, DerefMut, Range};

use crate::runtime::types::AtomString;

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
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Label {
    Name(String),
    Index(usize),
}

impl Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Label::Name(name) => write!(f, "name: '{}'", name),
            Label::Index(index) => write!(f, "index: {}", index),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Code {
    ConstInt64(i64),
    ConstUint64(u64),
    ConstInt32(i32),
    ConstUint32(u32),
    ConstInt16(i16),
    ConstUint16(u16),
    ConstInt8(i8),
    ConstUint8(u8),
    ConstByte(u8),
    ConstBool(bool),
    ConstFloat(f64),
    ConstChar(char),
    ConstSymbol(usize),
    ConstString(usize),
    MakeArray(usize),
    MakeTuple(usize),
    MakeTemplate(usize),
    MakeRange,
    GetType,
    SetLabel(String),
    Jump(Label),
    JumpIfTrue(Label),
    JumpOnError(Label),
    Branch((Label, Label)),
    StoreTryOk,
    MakeRef,
    Deref,
    LogicalAnd,
    ArithmeticAdd,
    ArithmeticSub,
    ArithmeticMul,
    ArithmeticDiv,
    ArithmeticExp,
    ArithmeticMod,
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
    Discard,
    Return,
    Cast(usize),
    Call(usize),
    CallVoid(usize),
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
    MakeSlice,
    MakeInstance(([usize; 2], usize)),
    StoreIndex,
    LoadMember(usize),
    StoreMember(usize),
}

fn format_name(id: usize, ir: Option<&IR>) -> String {
    format!(
        "{}: '{}'",
        if ir.is_some() {
            "name".to_string()
        } else {
            "id".to_string()
        },
        ir.map(|ir| ir.get_data(id).to_string())
            .unwrap_or_else(|| id.to_string())
    )
}

fn format_names(range: [usize; 2], ir: Option<&IR>) -> String {
    format!(
        "{}: [{}]",
        if ir.is_some() {
            "names".to_string()
        } else {
            "ids".to_string()
        },
        (range[0]..range[1])
            .map(|id| format!(
                "'{}'",
                ir.map(|ir| ir.get_data(id).to_string())
                    .unwrap_or_else(|| id.to_string())
            ))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

impl Code {
    pub fn description(&self, ir: Option<&IR>) -> String {
        match self {
            Code::ConstInt64(val) => format!("  constInt64({})", val),
            Code::ConstUint64(val) => format!("  constUint64({})", val),
            Code::ConstInt32(val) => format!("  constInt32({})", val),
            Code::ConstUint32(val) => format!("  constUint32({})", val),
            Code::ConstInt16(val) => format!("  constInt16({})", val),
            Code::ConstUint16(val) => format!("  constUint16({})", val),
            Code::ConstInt8(val) => format!("  constInt8({})", val),
            Code::ConstUint8(val) => format!("  constUint8({})", val),
            Code::ConstBool(val) => format!("  constBool({})", val),
            Code::ConstFloat(val) => format!("  constFloat({})", val),
            Code::ConstChar(val) => format!("  constChar({})", val),
            Code::ConstByte(val) => format!("  constByte({})", val),
            Code::ConstSymbol(id) => format!("  constSymbol({})", format_name(*id, ir)),
            Code::ConstString(id) => format!("  constString({})", format_name(*id, ir)),
            Code::MakeArray(size) => format!("  makeArray(size: {})", size),
            Code::MakeTuple(size) => format!("  makeTuple(size: {})", size),
            Code::MakeTemplate(size) => format!("  makeTemplate(size: {})", size),
            Code::MakeRange => "  makeRange".to_string(),
            Code::GetType => "  getType".to_string(),
            Code::MakeClosure(id) => format!("  makeClosure(id: {})", id),
            Code::SetLabel(label) => format!("{}:", label),
            Code::Jump(label) => format!("  jump({})", label),
            Code::JumpIfTrue(label) => format!("  jumpIfTrue({})", label),
            Code::JumpOnError(label) => format!("  jumpOnError({})", label),
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
            Code::ArithmeticMod => "  mod".to_string(),
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
            Code::Discard => "  discard".to_string(),
            Code::Return => "  return".to_string(),
            Code::Cast(id) => format!("  cast({})", format_name(*id, ir)),
            Code::Call(arg_count) => format!("  call(arg_count: {})", arg_count),
            Code::CallVoid(arg_count) => format!("  callVoid(arg_count: {})", arg_count),
            Code::TailCall(arg_count) => format!("  tailCall(arg_count: {})", arg_count),
            Code::StoreTryOk => "  storeTryResult".to_string(),
            Code::Store(id) => format!("  store(id: {})", id),
            Code::StoreMut(id) => format!("  storeMut(id: {})", id),
            Code::StoreMember(id) => format!("  storeMember({})", format_name(*id, ir)),
            Code::LoadReceiver => "  loadSelf".to_string(),
            Code::Load(id) => format!("  load(id: {})", id),
            Code::LoadGlobal(id) => format!("  loadGlobal(id: {})", id),
            Code::LoadFn(id) => format!("  loadFn(id: {})", id),
            Code::LoadClass(id) => format!("  loadClass(id: {})", id),
            Code::LoadInterface(id) => format!("  loadInterface(id: {})", id),
            Code::LoadIndex => "  loadIndex".to_string(),
            Code::MakeSlice => "  makeSlice".to_string(),
            Code::MakeInstance((keywords, arg_count)) => {
                format!(
                    "  makeInstance({}, field_count: {})",
                    format_names(*keywords, ir),
                    arg_count
                )
            }
            Code::LoadTarget => "  loadTarget".to_string(),
            Code::StoreIndex => "  storeIndex".to_string(),
            Code::LoadMember(id) => format!("  loadMember({})", format_name(*id, ir)),
        }
    }
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.description(None))
    }
}

#[derive(Clone, Default)]
pub struct IR {
    codes: Vec<Code>,
    data: Vec<AtomString>,
    locations: HashMap<usize, Location>,
    locals_size: Option<usize>,
}

impl IR {
    pub fn new() -> Self {
        Self {
            data: vec![],
            codes: vec![],
            locations: HashMap::new(),
            locals_size: None,
        }
    }

    #[inline]
    pub fn get_locals_size(&self) -> Option<usize> {
        self.locals_size
    }

    pub fn set_locals_size(&mut self, size: usize) {
        self.locals_size = Some(size);
    }

    #[inline]
    pub fn get_data(&self, id: usize) -> &AtomString {
        &self.data[id]
    }

    pub fn add_data(&mut self, val: &str) -> usize {
        let id = self.data.len();

        self.data.push(AtomString::from(val));

        id
    }

    pub fn append(&mut self, mut ir: IR) {
        if self.codes.is_empty() && self.locations.is_empty() {
            *self = ir;

            return;
        }

        for (i, location) in ir.locations {
            self.locations.insert(self.codes.len() + i, location);
        }

        let size = self.data.len();

        self.data.append(&mut ir.data.drain(0..).collect());

        // Recalculate data segment IDs
        for code in ir.codes.iter_mut() {
            match code {
                Code::LoadMember(id) => *id += size,
                Code::StoreMember(id) => *id += size,
                Code::Cast(id) => *id += size,
                Code::MakeInstance((keywords, _)) => {
                    for keyword in keywords.iter_mut() {
                        *keyword += size;
                    }
                }
                _ => {}
            }
        }

        self.codes.append(&mut ir.codes);
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

impl Deref for IR {
    type Target = Vec<Code>;

    fn deref(&self) -> &Self::Target {
        &self.codes
    }
}

impl DerefMut for IR {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.codes.as_mut()
    }
}

impl Debug for IR {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for code in self.codes.iter() {
            write!(f, "{}", code.description(Some(self)))?;
        }

        Ok(())
    }
}
