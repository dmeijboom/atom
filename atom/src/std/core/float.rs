use atom_macros::export;
use atom_runtime::Result;

use crate::vm::{ExternalFn, Module};

#[export]
fn float_floor(this: &f64) -> Result<f64> {
    Ok(this.floor())
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![("floor", float_floor)];

    for (method_name, closure) in methods {
        module.register_external_method("Float", method_name, closure)?;
    }

    Ok(())
}
