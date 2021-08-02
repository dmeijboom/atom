use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::rc::Rc;

use crate::compiler::IR;

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub body: Vec<IR>,
}

impl Hash for Func {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for item in data {
            item.hash(state);
        }
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        self.name.as_str() == other.name.as_str()
    }

    fn ne(&self, other: &Self) -> bool {
        self.name.as_str() != other.name.as_str()
    }
}

impl Eq for Func {}

pub enum ValueType {
    Int,
    Float,
    Char,
    Bool,
    Range,
    String,
    Class,
    Function,
    Method,
    Pointer,
    Object,
    Array,
    Map,
}

impl ValueType {
    pub fn name(&self) -> &str {
        match self {
            ValueType::Int => "Int",
            ValueType::Float => "Float",
            ValueType::Char => "Char",
            ValueType::Bool => "Bool",
            ValueType::Range => "Range",
            ValueType::String => "String",
            ValueType::Class => "Class",
            ValueType::Method => "Method",
            ValueType::Function => "Fn",
            ValueType::Object => "Object",
            ValueType::Map => "Map",
            ValueType::Array => "Array",
            ValueType::Pointer => "Ptr",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ClassId {
    pub name: String,
    pub module: String,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FuncId {
    pub name: String,
    pub module: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Method {
    pub name: String,
    pub object: Rc<RefCell<Object>>,
}

impl Hash for Method {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for item in data {
            item.hash(state);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FieldDesc {
    pub mutable: bool,
}

#[derive(Debug, PartialEq, Hash, Eq)]
pub struct Object {
    pub class: ClassId,
    pub fields: Vec<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PointerType {
    FieldPtr((Rc<RefCell<Object>>, usize)),
}

impl Hash for PointerType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            PointerType::FieldPtr((object, field_idx)) => {
                object.borrow().hash(state);
                field_idx.hash(state);
            }
        }
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for item in data {
            item.hash(state);
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Invalid,
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    Range(Range<i64>),
    String(String),
    Class(ClassId),
    Function(FuncId),
    Method(Method),
    Object(Rc<RefCell<Object>>),
    Pointer(PointerType),
    Array(Rc<RefCell<Vec<Value>>>),
    Map(Rc<RefCell<HashMap<Value, Value>>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Invalid => write!(f, "!"),
            Value::Int(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Char(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Range(val) => write!(f, "{:?}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::Class(id) => {
                write!(f, "{}.{}", id.module, id.name)
            }
            Value::Function(id) => {
                write!(f, "{}({}.{})", self.get_type().name(), id.module, id.name)
            }
            Value::Method(method) => {
                let obj = method.object.borrow();

                write!(
                    f,
                    "{}({}.{}.{})",
                    self.get_type().name(),
                    obj.class.module,
                    obj.class.name,
                    method.name
                )
            }
            Value::Object(object) => {
                let obj = object.borrow();

                write!(
                    f,
                    "{}.{}({})",
                    obj.class.module,
                    obj.class.name,
                    obj.fields
                        .iter()
                        .map(|field| field.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Value::Array(val) => write!(
                f,
                "[{}]",
                val.borrow()
                    .iter()
                    .map(|value| value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::Map(val) => write!(
                f,
                "{}(size: {})",
                self.get_type().name(),
                val.borrow().len()
            ),
            Value::Pointer(pointer) => match pointer {
                PointerType::FieldPtr((_, field_idx)) => {
                    write!(f, "{}({})", self.get_type().name(), field_idx)
                }
            },
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Invalid => panic!("Invalid can't be hashed"),
            Value::Int(val) => val.hash(state),
            Value::Float(val) => val.to_string().hash(state),
            Value::Char(val) => val.hash(state),
            Value::Bool(val) => val.hash(state),
            Value::Range(val) => val.hash(state),
            Value::String(val) => val.hash(state),
            Value::Class(class) => class.hash(state),
            Value::Function(func) => func.hash(state),
            Value::Method(method) => method.hash(state),
            Value::Object(object) => object.borrow().hash(state),
            Value::Array(val) => val.borrow().hash(state),
            Value::Map(map) => {
                let map = map.borrow();

                for (key, value) in map.iter() {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Pointer(pointer) => pointer.hash(state),
        }
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for item in data {
            item.hash(state);
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Invalid => panic!("Invalid can't be cloned"),
            Value::Int(val) => Value::Int(*val),
            Value::Float(val) => Value::Float(*val),
            Value::Char(val) => Value::Char(*val),
            Value::Bool(val) => Value::Bool(*val),
            Value::Range(val) => Value::Range(val.clone()),
            Value::String(val) => Value::String(val.clone()),
            Value::Class(id) => Value::Class(id.clone()),
            Value::Function(id) => Value::Function(id.clone()),
            Value::Method(id) => Value::Method(id.clone()),
            Value::Object(object) => Value::Object(Rc::clone(object)),
            Value::Array(array) => Value::Array(Rc::clone(array)),
            Value::Map(map) => Value::Map(Rc::clone(map)),
            Value::Pointer(pointer) => match pointer {
                PointerType::FieldPtr((object, field_idx)) => {
                    Value::Pointer(PointerType::FieldPtr((Rc::clone(object), *field_idx)))
                }
            },
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone();
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Invalid => panic!("Invalid has no type"),
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Char(_) => ValueType::Char,
            Value::Bool(_) => ValueType::Bool,
            Value::Range(_) => ValueType::Range,
            Value::String(_) => ValueType::String,
            Value::Class(_) => ValueType::Class,
            Value::Function(_) => ValueType::Function,
            Value::Method(_) => ValueType::Method,
            Value::Object(_) => ValueType::Object,
            Value::Array(_) => ValueType::Array,
            Value::Map(_) => ValueType::Map,
            Value::Pointer(_) => ValueType::Pointer,
        }
    }
}

impl Eq for Value {}
