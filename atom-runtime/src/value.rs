use std::any::Any;
use std::convert::TryInto;
use std::fmt::{Display, Formatter};
use std::hash::Hash;

use strum_macros::EnumIter;

use super::atom_ref::AtomRef;
use super::class::Class;
use super::closure::Closure;
use super::interface::Interface;
use super::method::Method;
use super::object::Object;
use super::r#extern::Extern;
use super::r#fn::Fn;
use super::result::RuntimeError;
use super::symbol::Symbol;

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

macro_rules! impl_try_into {
    (& $atom_type:ident, $rust_type:ty) => {
        impl<'v> TryInto<&'v $rust_type> for &'v Value {
            type Error = RuntimeError;

            fn try_into(self) -> Result<&'v $rust_type, Self::Error> {
                let type_val = self.get_type();

                if let Value::$atom_type(val) = self {
                    return Ok(val);
                }

                Err(RuntimeError::new(format!(
                    "invalid type '{}', expected: {}",
                    type_val.name(),
                    stringify!($atom_type)
                ))
                .with_kind("TypeError".to_string()))
            }
        }
    };

    (&mut $atom_type:ident, $rust_type:ty) => {
        impl<'v> TryInto<&'v mut $rust_type> for &'v mut Value {
            type Error = RuntimeError;

            fn try_into(self) -> Result<&'v mut $rust_type, Self::Error> {
                let type_val = self.get_type();

                if let Value::$atom_type(val) = self {
                    return Ok(map_ref!($atom_type, val, as_mut));
                }

                Err(RuntimeError::new(format!(
                    "invalid type '{}', expected: {}",
                    type_val.name(),
                    stringify!($atom_type)
                ))
                .with_kind("TypeError".to_string()))
            }
        }
    };

    ($atom_type:ident, $rust_type:ty) => {
        impl TryInto<$rust_type> for Value {
            type Error = RuntimeError;

            fn try_into(self) -> Result<$rust_type, Self::Error> {
                if let Value::$atom_type(val) = self {
                    return Ok(map_ref!($atom_type, val, unwrap_or_clone_inner));
                }

                Err(RuntimeError::new(format!(
                    "invalid type '{}', expected: {}",
                    self.get_type().name(),
                    stringify!($atom_type)
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

    ($atom_type:ident, $rust_type:ty, try_into) => {
        impl_try_into!($atom_type, $rust_type);
    };

    ($atom_type:ident, $rust_type:ty, try_into_ref) => {
        impl_try_into!(& $atom_type, $rust_type);
    };

    ($atom_type:ident, $rust_type:ty, try_into_mut) => {
        impl_try_into!(&mut $atom_type, $rust_type);
    };

    ($atom_type:ident, $rust_type:ty, [$($op:ident) +]) => {
        $(impl_type!($atom_type, $rust_type, $op);)*
    };
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, EnumIter)]
pub enum ValueType {
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
    Extern,
}

impl ValueType {
    pub fn name(&self) -> &str {
        match self {
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
            Self::Extern => "Extern",
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Void,
    Int(i64),
    Float(f64),
    Char(char),
    Byte(u8),
    Bool(bool),
    Symbol(Symbol),
    Extern(Extern),
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
    Map(AtomRef<HashMap<Value, Value>>),
}

impl_type!(Fn, Fn, [from try_into_ref try_into_mut]);
impl_type!(Class, Class, [from try_into_ref try_into_mut]);
impl_type!(Method, Method, [from try_into_ref try_into_mut]);
impl_type!(Closure, Closure, [from try_into_ref try_into_mut]);
impl_type!(Interface, Interface, [from try_into_ref try_into_mut]);
impl_type!(Int, i64, [from try_into_ref try_into_mut]);
impl_type!(Float, f64, [from try_into_ref try_into_mut]);
impl_type!(Char, char, [from try_into try_into_ref try_into_mut]);
impl_type!(Byte, u8, [from try_into try_into_ref try_into_mut]);
impl_type!(Bool, bool, [from try_into try_into_ref try_into_mut]);
impl_type!(Symbol, Symbol, [from try_into try_into_ref try_into_mut]);
impl_type!(String, String, [from try_into try_into_ref try_into_mut]);
impl_type!(Object, Object, [from try_into_ref try_into_mut]);
impl_type!(Tuple, Box<[Value]>, [from try_into try_into_ref try_into_mut]);
impl_type!(Array, Vec<Value>, [from try_into try_into_ref try_into_mut]);
impl_type!(Map, HashMap<Value, Value>, [from try_into try_into_ref try_into_mut]);
impl_type!(Option, Option<Box<Value>>, [try_into try_into_ref try_into_mut]);
impl_type!(Extern, Extern, [from try_into try_into_ref try_into_mut]);

impl_try_into!(&String, str);
impl_try_into!(&Array, [Value]);

impl TryInto<i64> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<i64, Self::Error> {
        match self {
            Value::Int(val) => Ok(val),
            Value::Float(val) if val.fract() == 0.0 => Ok(val as i64),
            Value::Float(val) if val.fract() != 0.0 => Err(RuntimeError::new(
                "unable to safely cast Float with fraction to Int".to_string(),
            )),
            _ => Err(RuntimeError::new(format!(
                "invalid type '{}', expected: Int",
                self.get_type().name()
            ))
            .with_kind("TypeError".to_string())),
        }
    }
}

impl TryInto<usize> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<usize, Self::Error> {
        let int: i64 = self.try_into()?;

        Ok(int as usize)
    }
}

impl From<usize> for Value {
    fn from(val: usize) -> Self {
        Value::Int(val as i64)
    }
}

impl TryInto<f64> for Value {
    type Error = RuntimeError;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Value::Int(val) => Ok(val as f64),
            Value::Float(val) => Ok(val),
            _ => Err(RuntimeError::new(format!(
                "invalid type '{}', expected: Float",
                self.get_type().name()
            ))
            .with_kind("TypeError".to_string())),
        }
    }
}

impl<T> From<Option<T>> for Value
where
    T: Into<Value>,
{
    fn from(opt: Option<T>) -> Self {
        Value::Option(opt.map(|value| Box::new(value.into())))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        eq!(Int, self, other);
        eq!(Float, self, other);
        eq!(Char, self, other);
        eq!(Byte, self, other);
        eq!(Bool, self, other);
        eq!(Symbol, self, other);
        eq!(Option, self, other);
        eq!(Ref, self, other);
        eq!(String, self, other);
        eq!(Fn, self, other);
        eq!(Closure, self, other);
        eq!(Class, self, other);
        eq!(Method, self, other);
        eq!(Interface, self, other);
        eq!(Object, self, other);
        eq!(Array, self, other);
        eq!(Map, self, other);
        eq!(Extern, self, other);

        false
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get_type().hash(state);

        match self {
            Value::Void => panic!("Void can't be hashed"),
            Value::Int(val) => val.hash(state),
            Value::Float(val) => val.to_string().hash(state),
            Value::Char(val) => val.hash(state),
            Value::Byte(val) => val.hash(state),
            Value::Bool(val) => val.hash(state),
            Value::Symbol(val) => val.hash(state),
            Value::Option(val) => val.hash(state),
            Value::Ref(val) => val.as_ref().hash(state),
            Value::String(val) => val.as_ref().hash(state),
            Value::Class(class) => class.as_ref().hash(state),
            Value::Closure(closure) => closure.func.as_ref().hash(state),
            Value::Fn(atom_fn) => atom_fn.as_ref().hash(state),
            Value::Method(method) => method.as_ref().hash(state),
            Value::Interface(interface) => interface.as_ref().hash(state),
            Value::Object(object) => object.as_ref().hash(state),
            Value::Tuple(val) => {
                for item in val.iter() {
                    item.hash(state);
                }
            }
            Value::Array(val) => val.as_ref().hash(state),
            Value::Map(map) => {
                for (key, value) in map.as_ref().iter() {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Extern(_) => {}
        }
    }
}

impl Clone for Value {
    #[inline(always)]
    fn clone(&self) -> Self {
        match self {
            Self::Void => panic!("Void can't be cloned"),
            Self::Int(val) => Value::Int(*val),
            Self::Float(val) => Value::Float(*val),
            Self::Char(val) => Value::Char(*val),
            Self::Byte(val) => Value::Byte(*val),
            Self::Bool(val) => Value::Bool(*val),
            Self::Symbol(name) => Value::Symbol(name.clone()),
            Self::Option(val) => Value::Option(val.clone()),
            Self::Ref(val) => Value::Ref(AtomRef::clone(val)),
            Self::Fn(atom_fn) => Value::Fn(atom_fn.clone()),
            Self::Class(class) => Value::Class(class.clone()),
            Self::Closure(closure) => Value::Closure(AtomRef::clone(closure)),
            Self::Method(method) => Value::Method(AtomRef::clone(method)),
            Self::String(val) => Value::String(AtomRef::clone(val)),
            Self::Object(val) => Value::Object(AtomRef::clone(val)),
            Self::Tuple(tuple) => Value::Tuple(tuple.clone()),
            Self::Array(val) => Value::Array(AtomRef::clone(val)),
            Self::Extern(val) => Value::Extern(Extern(AtomRef::clone(&val.0))),
            Self::Interface(interface) => Value::Interface(interface.clone()),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone();
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Self::Void => panic!("Void has no type"),
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
            Self::Extern(_) => ValueType::Extern,
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
            Self::Extern(external) => write!(f, "<{:?}>", external.type_id()),
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
        }
    }
}
