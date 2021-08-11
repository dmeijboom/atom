use smallvec::smallvec;

use crate::runtime::Object;
use crate::vm::{ModuleCache, VM};

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

pub fn to_option(vm: &VM, value: Option<Value>) -> Result<Value> {
    if let Some(value) = value {
        return Ok(Value::Object(
            Object::new(
                vm.get_type_id("std.core", "Option")?,
                smallvec![value, Value::Bool(false)],
            )
            .into(),
        ));
    }

    Ok(Value::Object(
        Object::new(
            vm.get_type_id("std.core", "Option")?,
            smallvec![Value::Int(0), Value::Bool(true)],
        )
        .into(),
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

pub fn to_object(module_cache: &ModuleCache, value: Value) -> Result<Object> {
    Ok(match value {
        Value::Int(val) => Object::new(
            module_cache.lookup_type_id("std.core", "Int")?,
            smallvec![Value::Int(val)],
        ),
        Value::Float(val) => Object::new(
            module_cache.lookup_type_id("std.core", "Float")?,
            smallvec![Value::Float(val)],
        ),
        Value::Bool(val) => Object::new(
            module_cache.lookup_type_id("std.core", "Bool")?,
            smallvec![Value::Bool(val)],
        ),
        Value::Range(val) => Object::new(
            module_cache.lookup_type_id("std.core", "Range")?,
            smallvec![Value::Int(val.start), Value::Int(val.end)],
        ),
        Value::String(val) => {
            let length = val.len() as i64;

            Object::new(
                module_cache.lookup_type_id("std.core", "String")?,
                smallvec![Value::String(val), Value::Int(length)],
            )
        }
        Value::Array(val) => {
            let length = val.len() as i64;

            Object::new(
                module_cache.lookup_type_id("std.core", "Array")?,
                smallvec![Value::Array(val), Value::Int(length)],
            )
        }
        Value::Map(val) => Object::new(
            module_cache.lookup_type_id("std.core", "Map")?,
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
