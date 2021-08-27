use atom_macros::export;
use atom_runtime::{ExternalFn, Result, Value};

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

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.core" {
        if method_name.is_none() && name == "some" {
            return Some(some);
        }

        if let Some(method_name) = method_name {
            return Some(match method_name {
                "isSome" => option_is_some,
                "isNone" => option_is_none,
                _ => return None,
            });
        }
    }

    None
}
