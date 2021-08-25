use atom_macros::export;
use atom_runtime::{ExternalFn, Result, Value};

use crate::vm::Module;

#[export]
fn array_push(this: &mut Vec<Value>, value: Value) -> Result<()> {
    this.push(value);

    Ok(())
}

#[export]
fn array_pop(this: &mut Vec<Value>) -> Result<Option<Value>> {
    Ok(this.pop())
}

#[export]
fn array_remove(this: &mut Vec<Value>, index: i64) -> Result<Value> {
    Ok(this.remove(index as usize))
}

#[export]
fn array_len(this: &[Value]) -> Result<i64> {
    Ok(this.len() as i64)
}

#[export]
fn array_clear(this: &mut Vec<Value>) -> Result<()> {
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
