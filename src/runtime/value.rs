use std::fmt::{Display, Formatter};

use strum_macros::EnumIter;

use super::atom_ref::AtomRef;
use super::class::Class;
use super::closure::Closure;
use super::error::{ErrorKind, Result, RuntimeError};
use super::int::Int;
use super::interface::Interface;
use super::method::Method;
use super::object::Object;
use super::r#fn::Fn;
use super::rust::RustObject;
use super::symbol::Symbol;

pub trait Convert<T> {
    fn convert(self) -> Result<T>;
}

macro_rules! type_error {
    ($expected:ident, $value:expr) => {
        RuntimeError::new(
            ErrorKind::TypeError,
            format!(
                "expected '{}', found: {}",
                stringify!($expected),
                $value.get_type().name()
            ),
        )
    };
}

macro_rules! impl_from {
    ($name:ident, $rust_type:ty) => {
        impl From<$rust_type> for Value {
            fn from(value: $rust_type) -> Self {
                Value::$name(value.into())
            }
        }
    };
}

macro_rules! impl_convert {
    (Int, $rust_type:ty) => {
        impl Convert<$rust_type> for Value {
            fn convert(self) -> Result<$rust_type> {
                match self {
                    // @TODO: convert it to the right 'Int' type based on it's requirements
                    Self::Float(val) => Ok(Int::Int64(val as i64)),
                    Self::Int(val) => Ok(val),
                    _ => Err(type_error!(Int, self)),
                }
            }
        }
    };

    (Float, $rust_type:ty) => {
        impl Convert<$rust_type> for Value {
            fn convert(self) -> Result<$rust_type> {
                match self {
                    Self::Float(val) => Ok(val),
                    Self::Int(val) => Ok(val.to_float()),
                    _ => Err(type_error!(Float, self)),
                }
            }
        }
    };

    ($name:ident, $rust_type:ty) => {
        impl Convert<$rust_type> for Value {
            fn convert(self) -> Result<$rust_type> {
                if let Value::$name(value) = self {
                    return Ok(value);
                }

                Err(type_error!($name, self))
            }
        }

        impl<'v> Convert<&'v $rust_type> for &'v Value {
            fn convert(self) -> Result<&'v $rust_type> {
                if let Value::$name(value) = self {
                    return Ok(value);
                }

                Err(type_error!($name, self))
            }
        }

        impl<'v> Convert<&'v mut $rust_type> for &'v mut Value {
            fn convert(self) -> Result<&'v mut $rust_type> {
                if let Value::$name(value) = self {
                    return Ok(value);
                }

                Err(type_error!($name, self))
            }
        }
    };
}

macro_rules! make_value {
    ($(($name:ident, $rust_type:ty)),+) => {
        #[repr(usize)]
        #[derive(Clone, Copy, PartialEq, Hash, Eq, EnumIter)]
        pub enum ValueType {
            Void = 0,
            $($name),+
        }

        impl ValueType {
            pub fn name(&self) -> &str {
                match self {
                    Self::Void => "Void",
                    $(Self::$name => stringify!($name)),+
                }
            }
        }

        impl TryFrom<&str> for ValueType {
            type Error = RuntimeError;

            fn try_from(name: &str) -> Result<Self> {
                match name {
                    "Void" => Ok(Self::Void),
                    $(stringify!($name) => Ok(Self::$name)),+,
                    _ => Err(RuntimeError::new(ErrorKind::FatalError, format!("unknown type: {}", name))),
                }
            }
        }

        #[derive(Debug, PartialEq)]
        pub enum Value {
            Void,
            $($name($rust_type)),+
        }

        impl Value {
            pub fn get_type(&self) -> ValueType {
                match self {
                    Self::Void => ValueType::Void,
                    $(Self::$name(_) => ValueType::$name),+
                }
            }
        }

        $(impl_from!($name, $rust_type);)+
        $(impl_convert!($name, $rust_type);)+
    };
}

make_value!(
    (Int, Int),
    (Float, f64),
    (Char, char),
    (Byte, u8),
    (Bool, bool),
    (Symbol, Symbol),
    (Ref, AtomRef<Value>),
    (Fn, AtomRef<Fn>),
    (Tuple, Box<[Value]>),
    (Class, AtomRef<Class>),
    (Interface, AtomRef<Interface>),
    (Closure, AtomRef<Closure>),
    (Method, AtomRef<Method>),
    (String, AtomRef<String>),
    (Object, AtomRef<Object>),
    (Array, AtomRef<Vec<Value>>),
    (Option, Option<Box<Value>>),
    (RustObject, RustObject)
);

// Setup base conversions between atom / Rust code

impl_from!(String, String);
impl_from!(Array, Vec<Value>);

impl Convert<Value> for Value {
    fn convert(self) -> Result<Self> {
        Ok(self)
    }
}

impl Clone for Value {
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
