use crate::runtime::{Object, TypeId};

use super::result::{Result, RuntimeError};
use super::value::Value;

pub fn to_bool(value: Value) -> Result<bool> {
    if let Value::Bool(val) = value {
        return Ok(val);
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}' expected: Bool",
        value.get_type().name()
    )))
}

pub fn to_int(value: Value) -> Result<i64> {
    match value {
        Value::Int(val) => Ok(val),
        Value::Float(val) => {
            let int_val = val as i64;

            if val == int_val as f64 {
                return Ok(int_val);
            }

            return Err(RuntimeError::new(format!(
                "unable to safely cast Float to Int"
            )));
        }
        _ => Err(RuntimeError::new(format!(
            "invalid type '{}' expected: Int",
            value.get_type().name()
        ))),
    }
}

pub fn to_float(value: Value) -> Result<f64> {
    match value {
        Value::Int(val) => Ok(val as f64),
        Value::Float(val) => Ok(val),
        _ => Err(RuntimeError::new(format!(
            "invalid type '{}' expected: Float",
            value.get_type().name()
        ))),
    }
}

pub fn to_option(value: Option<Value>) -> Value {
    if let Some(value) = value {
        return Value::Object(Object::new(
            TypeId::new("std.core", "Option"),
            vec![value, Value::Bool(false)],
        ));
    }

    Value::Object(Object::new(
        TypeId::new("std.core", "Option"),
        vec![Value::Int(0), Value::Bool(true)],
    ))
}

pub fn to_byte(value: &Value) -> Result<u8> {
    if let Value::Byte(byte) = value {
        return Ok(*byte);
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}' expected: Byte",
        value.get_type().name()
    )))
}

pub fn to_array(value: Value) -> Result<Vec<Value>> {
    if let Value::Array(array) = value {
        return Ok(array);
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}' expected: Array",
        value.get_type().name()
    )))
}

pub fn to_object(value: Value) -> Result<Object> {
    match value {
        Value::Int(val) => Ok(Object::new(
            TypeId::new("std.core", "Int"),
            vec![Value::Int(val)],
        )),
        Value::Float(val) => Ok(Object::new(
            TypeId::new("std.core", "Float"),
            vec![Value::Float(val)],
        )),
        Value::Bool(val) => Ok(Object::new(
            TypeId::new("std.core", "Bool"),
            vec![Value::Bool(val)],
        )),
        Value::Range(val) => Ok(Object::new(
            TypeId::new("std.core", "Range"),
            vec![Value::Int(val.start), Value::Int(val.end)],
        )),
        Value::String(val) => {
            let length = val.len() as i64;

            Ok(Object::new(
                TypeId::new("std.core", "String"),
                vec![Value::String(val), Value::Int(length)],
            ))
        }
        Value::Array(val) => {
            let length = val.len() as i64;

            Ok(Object::new(
                TypeId::new("std.core", "Array"),
                vec![Value::Array(val), Value::Int(length)],
            ))
        }
        Value::Map(val) => Ok(Object::new(
            TypeId::new("std.core", "Map"),
            vec![Value::Map(val)],
        )),
        Value::Object(object) => Ok(object),
        _ => Err(RuntimeError::new(format!(
            "invalid type '{}' expected: Object",
            value.get_type().name()
        ))),
    }
}
