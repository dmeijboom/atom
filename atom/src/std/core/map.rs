use std::collections::HashMap;

use atom_macros::export;
use atom_runtime::{ExternalFn, Result, Value};

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

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.core" && name == "Map" {
        if let Some(method_name) = method_name {
            return Some(match method_name {
                "keys" => map_keys,
                "len" => map_len,
                "remove" => map_remove,
                "clear" => map_clear,
                _ => return None,
            });
        }
    }

    None
}
