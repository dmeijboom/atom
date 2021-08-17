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

            Err(RuntimeError::new(
                "unable to safely cast Float to Int".to_string(),
            ))
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
    Value::Option(value.and_then(|value| Some(value.into())))
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
