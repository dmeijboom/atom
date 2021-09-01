use atom_macros::export;
use atom_runtime::{ExternalFn, Result, Value};

#[export]
fn tuple_len(this: &Box<[Value]>) -> Result<usize> {
    Ok(this.len())
}

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.core" && name == "Tuple" {
        if let Some("len") = method_name {
            return Some(tuple_len);
        }
    }

    None
}
