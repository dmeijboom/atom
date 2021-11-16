use std::process;

use crate::runtime::{ExternalFn, Input, Int, Output, Result, RuntimeError};

pub const FUNCTIONS: [(&str, ExternalFn); 1] = [("exit", exit)];

pub fn exit(input: Input<'_>) -> Result<Output> {
    let exit_code: Int = input.single()?;
    let exit_code: i32 = match exit_code {
        Int::Int32(val) => val,
        _ => {
            return Err(RuntimeError::new(format!(
                "invalid integer size '{}' for exit-code",
                exit_code.size()
            )))
        }
    };

    process::exit(exit_code);
}
