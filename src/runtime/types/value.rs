use std::fmt::{Display, Formatter};

use strum_macros::EnumIter;

use crate::runtime::types::*;
use crate::runtime::{Convert, ErrorKind, Result, RuntimeError};

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
                    Self::Int(val) => Ok(val),
                    Self::Uint(val) => Ok(val as i64),
                    Self::Float(val) => Ok(val as i64),
                    _ => Err(type_error!(Int, self)),
                }
            }
        }

        impl Convert<usize> for Value {
            fn convert(self) -> Result<usize> {
                match self {
                    Self::Int(val) => Ok(val as usize),
                    Self::Uint(val) => Ok(val as usize),
                    Self::Float(val) if val.is_sign_negative() => Ok(val as usize),
                    _ => Err(type_error!(Int, self)),
                }
            }
        }
    };

    (Uint, $rust_type:ty) => {
        impl Convert<$rust_type> for Value {
            fn convert(self) -> Result<$rust_type> {
                match self {
                    Self::Int(val) => Ok(val as u64),
                    Self::Uint(val) => Ok(val),
                    Self::Float(val) if val.is_sign_positive() => Ok(val as u64),
                    _ => Err(type_error!(Uint, self)),
                }
            }
        }
    };

    (Float, $rust_type:ty) => {
        impl Convert<$rust_type> for Value {
            fn convert(self) -> Result<$rust_type> {
                match self {
                    Self::Int(val) => Ok(val as f64),
                    Self::Uint(val) => Ok(val as f64),
                    Self::Float(val) => Ok(val),
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
    (Int, i64),
    (Uint, u64),
    (Float, f64),
    (Char, char),
    (Byte, u8),
    (Bool, bool),
    (Symbol, Symbol),
    (Ref, AtomRef<Value>),
    (Fn, AtomRef<Fn>),
    (Tuple, AtomArray<Value>),
    (Class, AtomRef<Class>),
    (Interface, AtomRef<Interface>),
    (Closure, AtomRef<Closure>),
    (Method, AtomRef<Method>),
    (String, AtomString),
    (Object, AtomRefMut<Object>),
    (Array, AtomRefMut<Vec<Value>>),
    (RustObject, RustObject),
    (Nil, AtomNil)
);

impl From<usize> for Value {
    fn from(size: usize) -> Self {
        Value::Uint(size as u64)
    }
}

impl From<Option<Value>> for Value {
    fn from(value: Option<Value>) -> Self {
        match value {
            None => Value::Nil(AtomNil {}),
            Some(value) => value,
        }
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::Void
    }
}

impl Convert<Value> for Value {
    fn convert(self) -> Result<Self> {
        Ok(self)
    }
}

impl Clone for Value {
    #[inline]
    fn clone(&self) -> Self {
        match self {
            Self::Void => Self::Void,
            Self::Int(val) => Value::Int(*val),
            Self::Uint(val) => Value::Uint(*val),
            Self::Float(val) => Value::Float(*val),
            Self::Char(val) => Value::Char(*val),
            Self::Byte(val) => Value::Byte(*val),
            Self::Bool(val) => Value::Bool(*val),
            Self::Symbol(name) => Value::Symbol(name.clone()),
            Self::Ref(val) => Value::Ref(AtomRef::clone(val)),
            Self::Fn(atom_fn) => Value::Fn(AtomRef::clone(atom_fn)),
            Self::Class(class) => Value::Class(AtomRef::clone(class)),
            Self::Closure(closure) => Value::Closure(AtomRef::clone(closure)),
            Self::Method(method) => Value::Method(AtomRef::clone(method)),
            Self::String(val) => Value::String(val.clone()),
            Self::Object(val) => Value::Object(AtomRefMut::clone(val)),
            Self::Tuple(val) => Value::Tuple(AtomRef::clone(val)),
            Self::Array(val) => Value::Array(AtomRefMut::clone(val)),
            Self::Interface(interface) => Value::Interface(AtomRef::clone(interface)),
            Self::Nil(nil) => Self::Nil(nil.clone()),
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
            Self::Uint(val) => write!(f, "{}", val),
            Self::Float(val) => write!(f, "{}", val),
            Self::Char(val) => write!(f, "{}", val),
            Self::Byte(val) => write!(f, "{}", val),
            Self::Bool(val) => write!(f, "{}", val),
            Self::Symbol(val) => write!(f, ":{}", val.name),
            Self::Ref(value) => {
                write!(f, "*{}", value.as_ref())
            }
            Self::String(s) => {
                write!(f, "{}", s)
            }
            Self::Fn(func) => write!(f, "{}(...)", func.as_ref()),
            Self::Interface(interface) => write!(f, "{}", interface.as_ref()),
            Self::Class(class) => write!(f, "{}", class.as_ref()),
            Self::Closure(closure) => write!(f, "{}(...)", closure.func.as_ref()),
            Self::Method(method) => write!(f, "{}(...)", method.as_ref()),
            Self::Object(object) => {
                let class = object.class.as_ref();

                write!(
                    f,
                    "{}({})",
                    class,
                    class
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, field)| {
                            format!(
                                "{}{}: {}",
                                if field.public { "*" } else { "" },
                                field.name,
                                object.fields.get(i).unwrap(),
                            )
                        })
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
                    .iter()
                    .map(|item| format!("{}", item))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Self::Nil(_) => write!(f, "nil"),
            Self::RustObject(rust_object) => write!(f, "{:?}", rust_object),
        }
    }
}
