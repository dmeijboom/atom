use smallvec::smallvec;

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
        return Value::Object(
            Object::new(
                TypeId::new("std.core".to_string(), "Option".to_string()),
                smallvec![value, Value::Bool(false)],
            )
            .into(),
        );
    }

    Value::Object(
        Object::new(
            TypeId::new("std.core".to_string(), "Option".to_string()),
            smallvec![Value::Int(0), Value::Bool(true)],
        )
        .into(),
    )
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
    Ok(match value {
        Value::Int(val) => Object::new(
            TypeId::new("std.core".to_string(), "Int".to_string()),
            smallvec![Value::Int(val)],
        ),
        Value::Float(val) => Object::new(
            TypeId::new("std.core".to_string(), "Float".to_string()),
            smallvec![Value::Float(val)],
        ),
        Value::Bool(val) => Object::new(
            TypeId::new("std.core".to_string(), "Bool".to_string()),
            smallvec![Value::Bool(val)],
        ),
        Value::Range(val) => Object::new(
            TypeId::new("std.core".to_string(), "Range".to_string()),
            smallvec![Value::Int(val.start), Value::Int(val.end)],
        ),
        Value::String(val) => {
            let length = val.len() as i64;

            Object::new(
                TypeId::new("std.core".to_string(), "String".to_string()),
                smallvec![Value::String(val), Value::Int(length)],
            )
        }
        Value::Array(val) => {
            let length = val.len() as i64;

            Object::new(
                TypeId::new("std.core".to_string(), "Array".to_string()),
                smallvec![Value::Array(val), Value::Int(length)],
            )
        }
        Value::Map(val) => Object::new(
            TypeId::new("std.core".to_string(), "Map".to_string()),
            smallvec![Value::Map(val)],
        ),
        Value::Object(object) => *object,
        _ => {
            return Err(RuntimeError::new(format!(
                "invalid type '{}' expected: Object",
                value.get_type().name()
            )))
        }
    })
}
