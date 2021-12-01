use std::process;

use crate::runtime::macros::wrap_fn;
use crate::runtime::types::{ExternalFn, Input, Int};
use crate::runtime::{ErrorKind, Result, RuntimeError};

pub const FUNCTIONS: [(&str, ExternalFn); 1] = [("exit", wrap_fn!(exit))];

pub fn exit(mut input: Input<'_>) -> Result<()> {
    let exit_code: Int = input.take_arg()?;
    let exit_code: i32 = match exit_code {
        Int::Int32(val) => val,
        _ => {
            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!("invalid integer size '{}' for exit-code", exit_code.size()),
            ))
        }
    };

    process::exit(exit_code);
}
