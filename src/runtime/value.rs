use std::fmt::{Display, Formatter};

use strum_macros::EnumIter;

use super::atom_ref::AtomRef;
use super::class::Class;
use super::closure::Closure;
use super::int::Int;
use super::interface::Interface;
use super::method::Method;
use super::object::Object;
use super::r#fn::Fn;
use super::result::{Result, RuntimeError};
use super::rust::RustObject;
use super::symbol::Symbol;

pub trait Convert<T> {
    fn convert(self) -> Result<T>;
}

macro_rules! map_ref {
    (Ref, $value:expr, $method:ident) => {
        $value.$method()
    };
    (Map, $value:expr, $method:ident) => {
        $value.$method()
    };
    (Array, $value:expr, $method:ident) => {
        $value.$method()
    };
    (Object, $value:expr, $method:ident) => {
        $value.$method()
    };
    (String, $value:expr, $method:ident) => {
        $value.$method()
    };

    ($atom_type:ident, $value:expr, $method:ident) => {
        $value
    };
}

macro_rules! impl_convert {
    (& $atom_type:ident, $rust_type:ty) => {
        impl<'v> Convert<&'v $rust_type> for &'v Value {
            fn convert(self) -> Result<&'v $rust_type> {
                let type_val = self.get_type();

                if let Value::$atom_type(val) = self {
                    return Ok(val);
                }

                Err(RuntimeError::new(format!(
                    "expected '{}', found: {}",
                    stringify!($atom_type),
                    type_val.name()
                ))
                .with_kind("TypeError".to_string()))
            }
        }
    };

    (&mut $atom_type:ident, $rust_type:ty) => {
        impl<'v> Convert<&'v mut $rust_type> for &'v mut Value {
            fn convert(self) -> Result<&'v mut $rust_type> {
                let type_val = self.get_type();

                if let Value::$atom_type(val) = self {
                    return Ok(map_ref!($atom_type, val, as_mut));
                }

                Err(RuntimeError::new(format!(
                    "expected '{}', found: {}",
                    stringify!($atom_type),
                    type_val.name()
                ))
                .with_kind("TypeError".to_string()))
            }
        }
    };

    ($atom_type:ident, $rust_type:ty) => {
        impl Convert<$rust_type> for Value {
            fn convert(self) -> Result<$rust_type> {
                if let Value::$atom_type(val) = self {
                    return Ok(map_ref!($atom_type, val, unwrap_or_clone_inner));
                }

                Err(RuntimeError::new(format!(
                    "expected '{}', found: {}",
                    stringify!($atom_type),
                    self.get_type().name()
                ))
                .with_kind("TypeError".to_string()))
            }
        }
    };
}

macro_rules! impl_from {
    ($atom_type:ident, $rust_type:ty) => {
        impl From<$rust_type> for Value {
            fn from(val: $rust_type) -> Self {
                Value::$atom_type(val.into())
            }
        }
    };
}

macro_rules! impl_type {
    ($atom_type:ident, $rust_type:ty, from) => {
        impl_from!($atom_type, $rust_type);
    };

    ($atom_type:ident, $rust_type:ty, convert) => {
        impl_convert!($atom_type, $rust_type);
    };

    ($atom_type:ident, $rust_type:ty, convert_ref) => {
        impl_convert!(& $atom_type, $rust_type);
    };

    ($atom_type:ident, $rust_type:ty, convert_mut) => {
        impl_convert!(&mut $atom_type, $rust_type);
    };

    ($atom_type:ident, $rust_type:ty, [$($op:ident) +]) => {
        $(impl_type!($atom_type, $rust_type, $op);)*
    };
}

#[derive(Clone, Copy, PartialEq, Hash, Eq, EnumIter)]
pub enum ValueType {
    Void,
    Int,
    Float,
    Char,
    Byte,
    Bool,
    Symbol,
    Option,
    String,
    Fn,
    Class,
    Method,
    Closure,
    Interface,
    Object,
    Tuple,
    Array,
    Ref,
    RustObject,
}

impl ValueType {
    pub fn name(&self) -> &str {
        match self {
            Self::Void => "Void",
            Self::Int => "Int",
            Self::Float => "Float",
            Self::Char => "Char",
            Self::Byte => "Byte",
            Self::Bool => "Bool",
            Self::Symbol => "Symbol",
            Self::Option => "Option",
            Self::String => "String",
            Self::Fn => "Fn",
            Self::Class => "Class",
            Self::Method => "Method",
            Self::Closure => "Closure",
            Self::Object => "Object",
            Self::Interface => "Interface",
            Self::Tuple => "Tuple",
            Self::Array => "Array",
            Self::Ref => "Ref",
            Self::RustObject => "RustObject",
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Void,
    Int(Int),
    Float(f64),
    Char(char),
    Byte(u8),
    Bool(bool),
    Symbol(Symbol),
    Ref(AtomRef<Value>),
    Fn(AtomRef<Fn>),
    Tuple(Box<[Value]>),
    Class(AtomRef<Class>),
    Interface(AtomRef<Interface>),
    Closure(AtomRef<Closure>),
    Method(AtomRef<Method>),
    String(AtomRef<String>),
    Object(AtomRef<Object>),
    Array(AtomRef<Vec<Value>>),
    Option(Option<Box<Value>>),
    RustObject(RustObject),
}

// Setup base conversions between atom / Rust code

impl Convert<Value> for Value {
    fn convert(self) -> Result<Self> {
        Ok(self)
    }
}

impl_type!(Int, Int, [from convert_ref convert_mut]);
impl_type!(Float, f64, [from convert_ref convert_mut]);
impl_type!(Char, char, [from convert convert_ref convert_mut]);
impl_type!(Byte, u8, [from convert convert_ref convert_mut]);
impl_type!(Bool, bool, [from convert convert_ref convert_mut]);
impl_type!(String, String, [from convert convert_ref convert_mut]);
impl_type!(Object, Object, [from convert_ref convert_mut]);
impl_type!(Array, Vec<Value>, [from convert convert_ref convert_mut]);
impl_type!(Option, Option<Box<Value>>, [convert convert_ref convert_mut]);
impl_type!(RustObject, RustObject, [convert convert_ref convert_mut]);

// Conversions for Object / AtomRef<Object>

impl Convert<AtomRef<Object>> for Value {
    fn convert(self) -> Result<AtomRef<Object>> {
        if let Value::Object(val) = self {
            return Ok(val);
        }

        Err(RuntimeError::new(format!(
            "expected 'Object', found: {}",
            self.get_type().name()
        ))
        .with_kind("TypeError".to_string()))
    }
}

// Conversions for Array / AtomRef<Vec<Value>>

impl Convert<AtomRef<Vec<Value>>> for Value {
    fn convert(self) -> Result<AtomRef<Vec<Value>>> {
        if let Value::Array(val) = self {
            return Ok(val);
        }

        Err(RuntimeError::new(format!(
            "expected 'Array', found: {}",
            self.get_type().name()
        ))
        .with_kind("TypeError".to_string()))
    }
}

// Conversions for Int / i64

impl Convert<Int> for Value {
    fn convert(self) -> Result<Int> {
        match self {
            Self::Float(val) => Ok(Int::Int64(val as i64)),
            Self::Int(val) => Ok(val),
            _ => Err(RuntimeError::new(format!(
                "expected 'Int', found: {}",
                self.get_type().name()
            ))
            .with_kind("TypeError".to_string())),
        }
    }
}

// Conversions for Float / f64

impl Convert<f64> for Value {
    fn convert(self) -> Result<f64> {
        match self {
            Self::Float(val) => Ok(val),
            Self::Int(val) => Ok(val.to_float()),
            _ => Err(RuntimeError::new(format!(
                "expected 'Float', found: {}",
                self.get_type().name()
            ))
            .with_kind("TypeError".to_string())),
        }
    }
}

impl Clone for Value {
    #[inline(always)]
    fn clone(&self) -> Self {
        match self {
            Self::Void => Self::Void,
            Self::Int(val) => Value::Int(*val),
            Self::Float(val) => Value::Float(*val),
            Self::Char(val) => Value::Char(*val),
            Self::Byte(val) => Value::Byte(*val),
            Self::Bool(val) => Value::Bool(*val),
            Self::Symbol(name) => Value::Symbol(name.clone()),
            Self::Option(val) => Value::Option(val.clone()),
            Self::Ref(val) => Value::Ref(AtomRef::clone(val)),
            Self::Fn(atom_fn) => Value::Fn(AtomRef::clone(atom_fn)),
            Self::Class(class) => Value::Class(AtomRef::clone(class)),
            Self::Closure(closure) => Value::Closure(AtomRef::clone(closure)),
            Self::Method(method) => Value::Method(AtomRef::clone(method)),
            Self::String(val) => Value::String(AtomRef::clone(val)),
            Self::Object(val) => Value::Object(AtomRef::clone(val)),
            Self::Tuple(tuple) => Value::Tuple(tuple.clone()),
            Self::Array(val) => Value::Array(AtomRef::clone(val)),
            Self::Interface(interface) => Value::Interface(AtomRef::clone(interface)),
            Self::RustObject(_) => panic!("RustObject can't be cloned"),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone();
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Self::Void => ValueType::Void,
            Self::Int(_) => ValueType::Int,
            Self::Float(_) => ValueType::Float,
            Self::Char(_) => ValueType::Char,
            Self::Byte(_) => ValueType::Byte,
            Self::Bool(_) => ValueType::Bool,
            Self::Symbol(_) => ValueType::Symbol,
            Self::Option(_) => ValueType::Option,
            Self::Ref(_) => ValueType::Ref,
            Self::String(_) => ValueType::String,
            Self::Fn(_) => ValueType::Fn,
            Self::Class(_) => ValueType::Class,
            Self::Interface(_) => ValueType::Interface,
            Self::Closure(_) => ValueType::Closure,
            Self::Method(_) => ValueType::Method,
            Self::Object(_) => ValueType::Object,
            Self::Tuple(_) => ValueType::Tuple,
            Self::Array(_) => ValueType::Array,
            Self::RustObject(_) => ValueType::RustObject,
        }
    }
}

impl Eq for Value {}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "!"),
            Self::Int(val) => write!(f, "{}", val),
            Self::Float(val) => write!(f, "{}", val),
            Self::Char(val) => write!(f, "{}", val),
            Self::Byte(val) => write!(f, "{}", val),
            Self::Bool(val) => write!(f, "{}", val),
            Self::Symbol(val) => write!(f, ":{}", val.as_ref()),
            Self::Option(val) => match val {
                None => write!(f, "Option(None)"),
                Some(val) => write!(f, "Option({})", val),
            },
            Self::Ref(value) => {
                write!(f, "*{}", value.as_ref())
            }
            Self::String(val) => write!(f, "{}", val.as_ref()),
            Self::Fn(func) => write!(f, "{}(...)", func.as_ref()),
            Self::Interface(interface) => write!(f, "{}", interface.as_ref()),
            Self::Class(class) => write!(f, "{}", class.as_ref()),
            Self::Closure(closure) => write!(f, "{}(...)", closure.func.as_ref()),
            Self::Method(method) => write!(f, "{}(...)", method.as_ref()),
            Self::Object(object) => {
                let class = object.as_ref().class.as_ref();

                write!(
                    f,
                    "{}({})",
                    class,
                    class
                        .fields
                        .iter()
                        .map(|(key, field)| format!(
                            "{}{}: {}",
                            if field.public { "*" } else { "" },
                            key,
                            object.as_ref().get_field(field.id).unwrap(),
                        ))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Self::Tuple(tuple) => write!(
                f,
                "({})",
                tuple
                    .as_ref()
                    .iter()
                    .map(|item| format!("{}", item))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Array(array) => write!(
                f,
                "[{}]",
                array
                    .as_ref()
                    .iter()
                    .map(|item| format!("{}", item))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::RustObject(rust_object) => write!(f, "{:?}", rust_object),
        }
    }
}
