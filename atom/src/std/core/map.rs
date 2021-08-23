use std::collections::HashMap;

use atom_macros::export;
use atom_runtime::{Result, Value};

use crate::vm::{ExternalFn, Module};

pub type Map = HashMap<Value, Value>;

#[export]
fn map_keys(this: &Map) -> Result<Vec<Value>> {
    Ok(this.keys().cloned().collect())
}

#[export]
fn map_len(this: &Map) -> Result<i64> {
    Ok(this.len() as i64)
}

#[export]
fn map_remove(this: &mut Map, key: Value) -> Result<Option<Value>> {
    Ok(this.remove(&key))
}

#[export]
fn map_clear(this: &mut Map) -> Result<()> {
    this.clear();

    Ok(())
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("keys", map_keys),
        ("len", map_len),
        ("remove", map_remove),
        ("clear", map_clear),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Map", method_name, closure)?;
    }

    Ok(())
}
