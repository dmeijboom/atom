use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
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
    String,
    Class,
    Function,
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
            ValueType::String => "String",
            ValueType::Class => "Class",
            ValueType::Function => "Fn",
            ValueType::Object => "Object",
            ValueType::Map => "Map",
            ValueType::Array => "Array",
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

#[derive(Debug, PartialEq, Hash, Eq)]
pub struct ClassDesc {
    pub id: ClassId,
    pub fields: Vec<String>,
}

#[derive(Debug, PartialEq, Hash, Eq)]
pub struct Object {
    pub class: ClassDesc,
    pub fields: Vec<Value>,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Invalid,
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Class(ClassId),
    Function(FuncId),
    Object(Rc<Object>),
    Array(Rc<Vec<Value>>),
    Map(Rc<HashMap<Value, Value>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Invalid => write!(f, "!"),
            Value::Int(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::Char(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::Class(id) => {
                write!(f, "{}.{}", id.module, id.name)
            }
            Value::Function(id) => {
                write!(f, "{}({}.{})", self.get_type().name(), id.module, id.name)
            }
            Value::Object(object) => {
                let obj: &Object = object.borrow();
                let id = &obj.class.id;

                write!(f, "Object(id: {}.{})", id.module, id.name)
            }
            Value::Array(val) => write!(f, "{}(size: {})", self.get_type().name(), val.len()),
            Value::Map(val) => write!(f, "{}(size: {})", self.get_type().name(), val.len()),
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
            Value::String(val) => val.hash(state),
            Value::Class(class) => class.hash(state),
            Value::Function(func) => func.hash(state),
            Value::Object(object) => object.hash(state),
            Value::Array(val) => val.hash(state),
            Value::Map(map) => {
                let map: &HashMap<Value, Value> = map.borrow();

                for (key, value) in map {
                    key.hash(state);
                    value.hash(state);
                }
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

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Invalid => panic!("Invalid can't be cloned"),
            Value::Int(val) => Value::Int(*val),
            Value::Float(val) => Value::Float(*val),
            Value::Char(val) => Value::Char(*val),
            Value::Bool(val) => Value::Bool(*val),
            Value::String(val) => Value::String(val.clone()),
            Value::Class(id) => Value::Class(id.clone()),
            Value::Function(id) => Value::Function(id.clone()),
            Value::Object(object) => Value::Object(Rc::clone(object)),
            Value::Array(array) => Value::Array(Rc::clone(array)),
            Value::Map(map) => Value::Map(Rc::clone(map)),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        let clone = source.clone();

        *self = clone;
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
            Value::String(_) => ValueType::String,
            Value::Class(_) => ValueType::Class,
            Value::Function(_) => ValueType::Function,
            Value::Object(_) => ValueType::Object,
            Value::Array(_) => ValueType::Array,
            Value::Map(_) => ValueType::Map,
        }
    }
}

impl Eq for Value {}
