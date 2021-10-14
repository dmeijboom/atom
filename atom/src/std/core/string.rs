use atom_macros::export;
use atom_runtime::{ExternalFn, Int, Result, Value};

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
fn string_splitn(this: &str, pattern: String, n: usize) -> Result<Vec<Value>> {
    Ok(this
        .splitn(n, &pattern)
        .map(|component| component.to_string().into())
        .collect())
}

#[export]
fn string_starts_with(this: &str, pattern: String) -> Result<bool> {
    Ok(this.starts_with(&pattern))
}

#[export]
fn string_count(this: &str, pattern: String) -> Result<Int> {
    Ok(this.matches(&pattern).count().into())
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
fn string_bytes(this: &str) -> Result<Vec<Value>> {
    Ok(this.bytes().map(Value::Byte).collect())
}

#[export]
fn string_repeat(this: &str, n: usize) -> Result<String> {
    Ok(this.repeat(n))
}

#[export]
fn string_len(this: &str) -> Result<Int> {
    Ok(this.len().into())
}

#[export]
fn string_find(this: &str, pattern: String) -> Result<Option<Int>> {
    Ok(this.find(&pattern).map(|i| i.into()))
}

#[export]
fn string_find_last(this: &str, pattern: String) -> Result<Option<Int>> {
    Ok(this.rfind(&pattern).map(|i| i.into()))
}

#[export]
fn string_replace(this: &str, pattern: String, replacement: String) -> Result<String> {
    Ok(this.replace(&pattern, &replacement))
}

#[export]
fn string_substr(this: &str, index: usize) -> Result<String> {
    Ok(this[index..].to_string())
}

#[export]
fn string_trim(this: &str) -> Result<String> {
    Ok(this.trim().to_string())
}

#[export]
fn string_append(this: &mut String, other: String) -> Result<()> {
    this.push_str(other.as_str());

    Ok(())
}

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.core" && name == "String" {
        if let Some(method_name) = method_name {
            return Some(match method_name {
                "upper" => string_upper,
                "lower" => string_lower,
                "split" => string_split,
                "splitn" => string_splitn,
                "startsWith" => string_starts_with,
                "endsWith" => string_ends_with,
                "contains" => string_contains,
                "chars" => string_chars,
                "bytes" => string_bytes,
                "repeat" => string_repeat,
                "append" => string_append,
                "count" => string_count,
                "find" => string_find,
                "substr" => string_substr,
                "replace" => string_replace,
                "trim" => string_trim,
                "len" => string_len,
                _ => return None,
            });
        }
    }

    None
}
