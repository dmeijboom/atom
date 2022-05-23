use std::fmt::{Display, Formatter};
use std::str::FromStr;

use crate::syntax::{LiteralKind, Span};

macro_rules! impl_basic_type {
    ($($ty:ident),+) => {
        #[derive(Debug, PartialEq, Clone)]
        pub enum BasicType {
            $($ty,)+
        }

        impl FromStr for BasicType {
            type Err = String;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $(stringify!($ty) => Ok(BasicType::$ty),)+
                    _ => Err(format!("invalid basic type: {}", s)),
                }
            }
        }
    };
}

impl_basic_type!(Array, Float, Float64, Fn, Int8, Int16, Int, Int64, Ptr, Struct, Vec, Void);

#[derive(Debug, Clone)]
pub enum Terminator {
    Return,
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub span: Span,
    pub kind: InstrKind,
}

impl Instr {
    pub fn new(span: Span, kind: InstrKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum InstrKind {
    Const(LiteralKind),
}

#[derive(Debug, Default, Clone)]
pub struct Block {
    pub body: Vec<Instr>,
    pub term: Option<Terminator>,
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub body: Vec<Block>,
    pub return_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Basic(BasicType),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Basic(basic_type) => match basic_type {
                BasicType::Array => write!(f, "Array"),
                BasicType::Float => write!(f, "Float"),
                BasicType::Float64 => write!(f, "Float64"),
                BasicType::Fn => write!(f, "Fn"),
                BasicType::Int8 => write!(f, "Int8"),
                BasicType::Int16 => write!(f, "Int16"),
                BasicType::Int64 => write!(f, "Int64"),
                BasicType::Int => write!(f, "Int"),
                BasicType::Ptr => write!(f, "Ptr"),
                BasicType::Struct => write!(f, "Struct"),
                BasicType::Vec => write!(f, "Vec"),
                BasicType::Void => write!(f, "Void"),
            },
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Module {
    pub name: String,
    pub funcs: Vec<Fn>,
}
