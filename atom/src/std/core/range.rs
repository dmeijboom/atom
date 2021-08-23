use std::ops::Range;

use atom_macros::export;
use atom_runtime::Result;

use crate::vm::{ExternalFn, Module};

#[export]
fn range_start(this: &Range<i64>) -> Result<i64> {
    Ok(this.start)
}

#[export]
fn range_end(this: &Range<i64>) -> Result<i64> {
    Ok(this.end)
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![("start", range_start), ("end", range_end)];

    for (method_name, closure) in methods {
        module.register_external_method("Range", method_name, closure)?;
    }

    Ok(())
}
