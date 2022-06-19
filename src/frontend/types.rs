use std::borrow::Cow;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

macro_rules! static_types {
    ($(($name:ident $const_name:ident $kind:expr)),+) => {
        impl Type {
            pub fn from_name(name: &str) -> Option<Self> {
                match name {
                    $(stringify!($name) => Some($const_name),)+
                    _ => None,
                }
            }
        }

        $(pub const $const_name: Type = Type {
            name: Cow::Borrowed(&stringify!($name)),
            kind: $kind,
            primitive: true,
            args: vec![]
        };)+
    };
}

type TypeName = Cow<'static, str>;

#[derive(Debug, PartialEq, Clone)]
pub struct FnArg {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct FnType {
    pub name: String,
    pub args: Vec<FnArg>,
    pub return_type: Type,
}

#[derive(Clone)]
pub enum TypeKind {
    Bool,
    String,
    Int { size: usize, signed: bool },
    Float { size: usize },
    Fn(Rc<FnType>),
    Void,
    Unknown,
}

impl TypeKind {
    pub fn id(&self) -> usize {
        match self {
            TypeKind::Bool => 1,
            TypeKind::String => 2,
            TypeKind::Int { .. } => 3,
            TypeKind::Float { .. } => 4,
            TypeKind::Fn(_) => 5,
            TypeKind::Void => 6,
            TypeKind::Unknown => 7,
        }
    }
}

#[derive(Clone)]
pub struct Type {
    pub name: TypeName,
    pub kind: TypeKind,
    pub primitive: bool,
    pub args: Vec<Type>,
}

impl Type {
    pub fn new(name: String, kind: TypeKind) -> Self {
        Self {
            name: TypeName::Owned(name),
            kind,
            primitive: false,
            args: vec![],
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self.kind, TypeKind::Int { .. } | TypeKind::Float { .. })
    }

    pub fn is_int(&self) -> bool {
        matches!(self.kind, TypeKind::Int { .. })
    }
}

static_types!(
    (Bool BOOL TypeKind::Bool),
    (String STRING TypeKind::String),
    (Int8 INT8 TypeKind::Int { size: 8, signed: true }),
    (Int16 INT16 TypeKind::Int { size: 16, signed: true }),
    (Int INT TypeKind::Int { size: 32, signed: true }),
    (Int64 INT64 TypeKind::Int { size: 64, signed: true }),
    (Uint8 UINT8 TypeKind::Int { size: 8, signed: false }),
    (Uint16 UINT16 TypeKind::Int { size: 16, signed: false }),
    (Uint UINT TypeKind::Int { size: 32, signed: false }),
    (Uint64 UINT64 TypeKind::Int { size: 64, signed: false }),
    (Float FLOAT TypeKind::Float { size: 32 }),
    (Float64 FLOAT64 TypeKind::Float { size: 64 }),
    (Void VOID TypeKind::Void),
    (Unknown UNKNOWN TypeKind::Unknown)
);

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && !matches!(self.kind, TypeKind::Unknown)
            && !matches!(other.kind, TypeKind::Unknown)
            && self.primitive == other.primitive
            && self.kind.id() == other.kind.id()
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;

        if !self.args.is_empty() {
            write!(f, "<")?;
        }

        for arg in &self.args {
            write!(f, " {}", arg)?;
        }

        if !self.args.is_empty() {
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
