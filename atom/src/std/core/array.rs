use atom_macros::export;
use atom_runtime::{Result, Value};

use crate::vm::{ExternalFn, Module};

pub type Array = Vec<Value>;

#[export]
fn array_push(this: &mut Array, value: Value) -> Result<()> {
    this.push(value);

    Ok(())
}

#[export]
fn array_pop(this: &mut Array) -> Result<Option<Value>> {
    Ok(this.pop())
}

#[export]
fn array_remove(this: &mut Array, index: i64) -> Result<Value> {
    Ok(this.remove(index as usize))
}

#[export]
fn array_len(this: &Array) -> Result<i64> {
    Ok(this.len() as i64)
}

#[export]
fn array_clear(this: &mut Array) -> Result<()> {
    this.clear();

    Ok(())
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("push", array_push),
        ("pop", array_pop),
        ("remove", array_remove),
        ("len", array_len),
        ("clear", array_clear),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Array", method_name, closure)?;
    }

    Ok(())
}
