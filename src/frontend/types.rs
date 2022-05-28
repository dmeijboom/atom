use std::borrow::Cow;
use std::fmt::{Display, Formatter};

macro_rules! static_types {
    ($(($name:ident $const_name:ident $numeric:expr)),+) => {
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
            attr: TypeAttr {
                primitive: true,
                numeric: $numeric,
            },
            args: vec![]
        };)+
    };
}

#[derive(PartialEq, Clone)]
pub enum Numeric {
    Int { size: usize, signed: bool },
    Float { size: usize },
}

#[derive(PartialEq, Clone)]
pub struct TypeAttr {
    pub primitive: bool,
    pub numeric: Option<Numeric>,
}

impl Default for TypeAttr {
    fn default() -> Self {
        Self {
            primitive: true,
            numeric: None,
        }
    }
}

type TypeName = Cow<'static, &'static str>;

#[derive(Clone)]
pub struct Type {
    pub name: TypeName,
    pub attr: TypeAttr,
    pub args: Vec<Type>,
}

static_types!(
    (Bool BOOL None),
    (String STRING None),
    (Int8 INT8 Some(Numeric::Int { size: 8, signed: true })),
    (Int16 INT16 Some(Numeric::Int { size: 16, signed: true })),
    (Int INT Some(Numeric::Int { size: 32, signed: true })),
    (Int64 INT64 Some(Numeric::Int { size: 64, signed: true })),
    (Uint8 UINT8 Some(Numeric::Int { size: 8, signed: false })),
    (Uint16 UINT16 Some(Numeric::Int { size: 16, signed: false })),
    (Uint UINT Some(Numeric::Int { size: 32, signed: false })),
    (Uint64 UINT64 Some(Numeric::Int { size: 64, signed: false })),
    (Float FLOAT Some(Numeric::Float { size: 32 })),
    (Float64 FLOAT64 Some(Numeric::Float { size: 64 })),
    (Void VOID None)
);

impl Type {
    pub fn new(name: impl Into<TypeName>) -> Self {
        Self {
            name: name.into(),
            args: vec![],
            attr: TypeAttr::default(),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.attr == other.attr
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
