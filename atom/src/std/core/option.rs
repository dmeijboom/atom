use atom_macros::export;
use atom_runtime::{Result, RuntimeError, Value};

use crate::vm::{ExternalFn, Module};

pub type Opt = Option<Box<Value>>;

#[export]
fn some(value: Value) -> Result<Value> {
    Ok(Value::Option(Some(value.into())))
}

#[export]
fn option_is_some(this: &Opt) -> Result<bool> {
    Ok(this.is_some())
}

#[export]
fn option_is_none(this: &Opt) -> Result<bool> {
    Ok(this.is_none())
}

#[export]
fn option_value(this: &Opt) -> Result<Value> {
    if let Some(value) = this {
        return Ok(*value.clone());
    }

    Err(RuntimeError::new(
        "unable to unpack option without a value".to_string(),
    ))
}

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("some", some);

    let methods: Vec<(_, ExternalFn)> = vec![
        ("isSome", option_is_some),
        ("isNone", option_is_none),
        ("value", option_value),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Option", method_name, closure)?;
    }

    Ok(())
}
