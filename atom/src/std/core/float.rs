use atom_macros::export;
use atom_runtime::{ExternalFn, Result};

#[export]
fn float_floor(this: &f64) -> Result<f64> {
    Ok(this.floor())
}

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.core" && name == "Float" {
        if let Some("floor") = method_name {
            return Some(float_floor);
        }
    }

    None
}
