use super::result::{Result, RuntimeError};
use super::value::Value;

pub fn to_bool(value: &Value) -> Result<bool> {
    if let Value::Bool(val) = value {
        return Ok(*val);
    }

    Err(RuntimeError::new(
        format!("invalid type: {} expected Bool", value.get_type().name()),
    ))
}

pub fn to_int(value: &Value) -> Result<i64> {
    match value {
        Value::Int(val) => Ok(*val),
        Value::Float(val) => {
            let int_val = *val as i64;

            if *val == int_val as f64 {
                return Ok(int_val);
            }

            return Err(RuntimeError::new(
                format!("unable to safely cast Float to Int"),
            ));
        }
        _ => Err(RuntimeError::new(
            format!("invalid type: {} expected Int", value.get_type().name()),
        ))
    }
}

pub fn to_float(value: &Value) -> Result<f64> {
    match value {
        Value::Int(val) => Ok(*val as f64),
        Value::Float(val) => Ok(*val),
        _ => Err(RuntimeError::new(
            format!("invalid type: {} expected Float", value.get_type().name()),
        ))
    }
}
