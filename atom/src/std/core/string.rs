use atom_macros::export;
use atom_runtime::{ExternalFn, Result, Value};

use crate::vm::Module;

#[export]
fn string_upper(this: &str) -> Result<String> {
    Ok(this.to_uppercase())
}

#[export]
fn string_lower(this: &str) -> Result<String> {
    Ok(this.to_lowercase())
}

#[export]
fn string_split(this: &str, pattern: String) -> Result<Vec<Value>> {
    Ok(this
        .split(&pattern)
        .map(|component| component.to_string().into())
        .collect())
}

#[export]
fn string_splitn(this: &str, pattern: String, n: i64) -> Result<Vec<Value>> {
    Ok(this
        .splitn(n as usize, &pattern)
        .map(|component| component.to_string().into())
        .collect())
}

#[export]
fn string_starts_with(this: &str, pattern: String) -> Result<bool> {
    Ok(this.starts_with(&pattern))
}

#[export]
fn string_ends_with(this: &str, pattern: String) -> Result<bool> {
    Ok(this.ends_with(&pattern))
}

#[export]
fn string_contains(this: &str, pattern: String) -> Result<bool> {
    Ok(this.contains(&pattern))
}

#[export]
fn string_chars(this: &str) -> Result<Vec<Value>> {
    Ok(this.chars().map(Value::Char).collect())
}

#[export]
fn string_repeat(this: &str, n: i64) -> Result<String> {
    Ok(this.repeat(n as usize))
}

#[export]
fn string_len(this: &str) -> Result<i64> {
    Ok(this.len() as i64)
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("upper", string_upper),
        ("lower", string_lower),
        ("split", string_split),
        ("splitn", string_splitn),
        ("startsWith", string_starts_with),
        ("endsWith", string_ends_with),
        ("contains", string_contains),
        ("chars", string_chars),
        ("repeat", string_repeat),
        ("len", string_len),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("String", method_name, closure)?;
    }

    Ok(())
}
