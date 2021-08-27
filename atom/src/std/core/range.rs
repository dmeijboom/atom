use std::ops::Range;

use atom_macros::export;
use atom_runtime::{ExternalFn, Result};

#[export]
fn range_start(this: &Range<i64>) -> Result<i64> {
    Ok(this.start)
}

#[export]
fn range_end(this: &Range<i64>) -> Result<i64> {
    Ok(this.end)
}

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.core" && name == "Range" {
        if let Some("start") = method_name {
            return Some(range_start);
        }

        if let Some("end") = method_name {
            return Some(range_end);
        }
    }

    None
}
