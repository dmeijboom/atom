use atom_macros::export;
use atom_runtime::{ExternalFn, Int, Result, Value};

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
fn array_remove(this: &mut Vec<Value>, index: usize) -> Result<Value> {
    Ok(this.remove(index))
}

#[export]
fn array_len(this: &[Value]) -> Result<Int> {
    Ok(this.len().into())
}

#[export]
fn array_clear(this: &mut Vec<Value>) -> Result<()> {
    this.clear();

    Ok(())
}

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.core" && name == "Array" {
        if let Some(method_name) = method_name {
            return Some(match method_name {
                "push" => array_push,
                "pop" => array_pop,
                "len" => array_len,
                "clear" => array_clear,
                "remove" => array_remove,
                _ => return None,
            });
        }
    }

    None
}
