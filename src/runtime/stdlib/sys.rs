use std::process;

use crate::runtime::macros::wrap_fn;
use crate::runtime::types::{ExternalFn, Input, Value};
use crate::runtime::{ErrorKind, Result, RuntimeError};

pub const FUNCTIONS: [(&str, ExternalFn); 1] = [("exit", wrap_fn!(exit))];

pub fn exit(mut input: Input<'_>) -> Result<()> {
    let exit_code: i32 = match input.take_arg()? {
        Value::Int(val) => val as i32,
        Value::Uint(val) => val as i32,
        _ => {
            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                "invalid value for exit-code".to_string(),
            ))
        }
    };

    process::exit(exit_code);
}
