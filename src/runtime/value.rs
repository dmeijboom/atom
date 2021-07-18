use std::collections::HashMap;
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

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H) where Self: Sized {
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
    Array,
    Map,
    Function,
}

impl ValueType {
    pub fn name(&self) -> &str {
        match self {
            ValueType::Int => "Int",
            ValueType::Float => "Float",
            ValueType::Char => "Char",
            ValueType::Bool => "Bool",
            ValueType::String => "String",
            ValueType::Array => "Array",
            ValueType::Map => "Map",
            ValueType::Function => "Fn",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct FuncId {
    pub name: String,
    pub module: String,
}

pub type ValueRef = Rc<Value>;

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Array(Vec<ValueRef>),
    Map(HashMap<ValueRef, ValueRef>),
    Function(FuncId),
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Int(val) => val.hash(state),
            Value::Float(val) => val.to_string().hash(state),
            Value::Char(val) => val.hash(state),
            Value::Bool(val) => val.hash(state),
            Value::String(val) => val.hash(state),
            Value::Array(val) => val.hash(state),
            Value::Map(map) => {
                for (key, value) in map {
                    key.hash(state);
                    value.hash(state);
                }
            }
            Value::Function(func) => func.hash(state),
        }
    }

    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H) where Self: Sized {
        for item in data {
            item.hash(state);
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Int(val) => Value::Int(*val),
            Value::Float(val) => Value::Float(*val),
            Value::Char(val) => Value::Char(*val),
            Value::Bool(val) => Value::Bool(*val),
            Value::String(val) => Value::String(val.clone()),
            Value::Array(array) => Value::Array(
                array
                    .iter()
                    .map(|value| Rc::clone(value))
                    .collect::<Vec<_>>()
            ),
            Value::Map(map) => {
                let mut new_map = HashMap::new();

                for (key, value) in map.iter() {
                    new_map.insert(Rc::clone(key), Rc::clone(value));
                }

                Value::Map(new_map)
            },
            Value::Function(id) => Value::Function(id.clone()),
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
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::Char(_) => ValueType::Char,
            Value::Bool(_) => ValueType::Bool,
            Value::String(_) => ValueType::String,
            Value::Array(_) => ValueType::Array,
            Value::Map(_) => ValueType::Map,
            Value::Function(_) => ValueType::Function,
        }
    }
}

impl Eq for Value {}

//pub enum Rule {
//    HasFunction(String),
//    IsNumeric,
//}
//
//pub struct Contract {
//    pub rules: Rule,
//}
