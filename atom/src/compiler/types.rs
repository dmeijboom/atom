#[derive(Debug, Clone)]
pub enum PrimitiveType {
    Int,
    Float,
    Char,
    Byte,
    Bool,
    String,
    Symbol,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Option(Box<Type>),
    Fn(String),
    Class(String),
    Closure(String),
    Interface(String),
    Object(String),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Ref(Box<Type>),
    Extern(String),
}

impl Type {
    pub fn is_array(&self) -> bool {
        matches!(self, Type::Array(_))
    }
}

pub const INT: Type = Type::Primitive(PrimitiveType::Int);
pub const FLOAT: Type = Type::Primitive(PrimitiveType::Float);
pub const CHAR: Type = Type::Primitive(PrimitiveType::Char);
pub const BYTE: Type = Type::Primitive(PrimitiveType::Byte);
pub const BOOL: Type = Type::Primitive(PrimitiveType::Bool);
pub const STRING: Type = Type::Primitive(PrimitiveType::String);
pub const SYMBOL: Type = Type::Primitive(PrimitiveType::Symbol);
