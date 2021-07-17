use std::collections::HashMap;

use crate::compiler::IR;

#[derive(Debug, PartialEq)]
pub struct Func {
    pub name: String,
    pub body: Vec<IR>,
}

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

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
    Char(char),
    Bool(bool),
    String(String),
    Array(Vec<Value>),
    Map(HashMap<Value, Value>),
    Function(Func),
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

//pub enum Rule {
//    HasFunction(String),
//    IsNumeric,
//}
//
//pub struct Contract {
//    pub rules: Rule,
//}
