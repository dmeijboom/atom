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

#[derive(PartialEq)]
pub struct TypeAttr {
    pub primitive: bool,
    pub numeric: bool,
}

impl Default for TypeAttr {
    fn default() -> Self {
        Self {
            primitive: true,
            numeric: false,
        }
    }
}

type TypeName = Cow<'static, &'static str>;

pub struct Type {
    pub name: TypeName,
    pub attr: TypeAttr,
    pub args: Vec<Type>,
}

static_types!(
    (Bool BOOL false),
    (String STRING false),
    (Int8 INT8 true),
    (Int16 INT16 true),
    (Int INT true),
    (Int64 INT64 true),
    (Uint8 UINT8 true),
    (Uint16 UINT16 true),
    (Uint UINT true),
    (Uint64 UINT64 true),
    (Float FLOAT true),
    (Float64 FLOAT64 true),
    (Void VOID false)
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

impl Type {
    #[inline]
    pub fn is_int(&self) -> bool {
        matches!(
            *self.name.as_ref(),
            "Int8" | "Int16" | "Int" | "Int32" | "Int64"
        )
    }

    #[inline]
    pub fn is_float(&self) -> bool {
        matches!(*self.name.as_ref(), "Float" | "Float64")
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

        write!(f, ">")
    }
}
