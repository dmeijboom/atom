use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::{ClassId, Object};

use super::result::{Result, RuntimeError};
use super::value::Value;

pub fn to_bool(value: &Value) -> Result<bool> {
    if let Value::Bool(val) = value {
        return Ok(*val);
    }

    Err(RuntimeError::new(format!(
        "invalid type: {} expected Bool",
        value.get_type().name()
    )))
}

pub fn to_int(value: &Value) -> Result<i64> {
    match value {
        Value::Int(val) => Ok(*val),
        Value::Float(val) => {
            let int_val = *val as i64;

            if *val == int_val as f64 {
                return Ok(int_val);
            }

            return Err(RuntimeError::new(format!(
                "unable to safely cast Float to Int"
            )));
        }
        _ => Err(RuntimeError::new(format!(
            "invalid type: {} expected Int",
            value.get_type().name()
        ))),
    }
}

pub fn to_float(value: &Value) -> Result<f64> {
    match value {
        Value::Int(val) => Ok(*val as f64),
        Value::Float(val) => Ok(*val),
        _ => Err(RuntimeError::new(format!(
            "invalid type: {} expected Float",
            value.get_type().name()
        ))),
    }
}

fn make_object(module: &str, name: &str, value: Value) -> Rc<RefCell<Object>> {
    Rc::new(RefCell::new(Object {
        class: ClassId {
            name: name.to_string(),
            module: module.to_string(),
        },
        fields: vec![value],
    }))
}

pub fn to_object(value: Value) -> Result<Rc<RefCell<Object>>> {
    match value {
        Value::Int(val) => Ok(make_object("std.core", "Int", Value::Int(val))),
        Value::Float(val) => Ok(make_object("std.core", "Float", Value::Float(val))),
        Value::Bool(val) => Ok(make_object("std.core", "Bool", Value::Bool(val))),
        Value::String(val) => Ok(make_object("std.core", "String", Value::String(val))),
        Value::Array(val) => Ok(make_object("std.core", "Array", Value::Array(val))),
        Value::Map(val) => Ok(make_object("std.core", "Map", Value::Map(val))),
        Value::Object(object) => Ok(object),
        _ => Err(RuntimeError::new(format!(
            "invalid type: {} expected Object",
            value.get_type().name()
        ))),
    }
}
