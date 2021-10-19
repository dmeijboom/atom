use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    Int,
    Float,
    Char,
    Byte,
    Bool,
    String,
    Symbol,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapType {
    pub key: Type,
    pub value: Type,
}

impl MapType {
    pub fn new(key: Type, value: Type) -> Self {
        Self { key, value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    Option(Box<Type>),
    Fn(String),
    Class(String),
    Closure(String),
    Interface(String),
    // @TODO: use this
    //Object(String),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Ref(Box<Type>),
    // @TODO: use this
    //Extern(String),
    Map(Box<MapType>),
    Unknown,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Primitive(primitive) => match primitive {
                PrimitiveType::Int => write!(f, "Int"),
                PrimitiveType::Float => write!(f, "Float"),
                PrimitiveType::Char => write!(f, "Char"),
                PrimitiveType::Byte => write!(f, "Byte"),
                PrimitiveType::Bool => write!(f, "Bool"),
                PrimitiveType::String => write!(f, "String"),
                PrimitiveType::Symbol => write!(f, "Symbol"),
            },
            Type::Option(inner_type) => write!(f, "Option<{}>", inner_type),
            Type::Fn(_) => write!(f, "Fn"),
            Type::Class(name) => write!(f, "{}", name),
            Type::Closure(_) => write!(f, "Closure"),
            Type::Interface(name) => write!(f, "{}", name),
            // @TODO: implement this
            //Type::Object(inner_type) => write!(f, "Object<{}>", inner_type),
            Type::Tuple(types) => write!(
                f,
                "Tuple<{}>",
                types
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Array(inner_type) => write!(f, "Array<{}>", inner_type),
            Type::Ref(ref_type) => write!(f, "Ref<{}>", ref_type),
            // @TODO: implement this
            //Type::Extern(_) => write!(f, "Extern"),
            Type::Map(map_type) => {
                write!(f, "Map<{}, {}>", map_type.key, map_type.value)
            }
            Type::Unknown => write!(f, "Unknown"),
        }
    }
}

pub const INT: Type = Type::Primitive(PrimitiveType::Int);
pub const FLOAT: Type = Type::Primitive(PrimitiveType::Float);
pub const CHAR: Type = Type::Primitive(PrimitiveType::Char);
pub const BYTE: Type = Type::Primitive(PrimitiveType::Byte);
pub const BOOL: Type = Type::Primitive(PrimitiveType::Bool);
pub const STRING: Type = Type::Primitive(PrimitiveType::String);
pub const SYMBOL: Type = Type::Primitive(PrimitiveType::Symbol);
