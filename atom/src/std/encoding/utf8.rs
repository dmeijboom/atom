use std::convert::TryInto;

use atom_macros::export;
use atom_runtime::{ExternalFn, Result, RuntimeError, Value};

#[export]
fn utf8_decode(data: Vec<Value>) -> Result<String> {
    let mut bytes = vec![];

    for item in data.into_iter() {
        bytes.push(item.try_into()?);
    }

    String::from_utf8(bytes).map_err(|e| RuntimeError::new(format!("DecodeError: {}", e)))
}

pub fn hook(module_name: &str, name: &str, _method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.encoding.utf8" && name == "decode" {
        return Some(utf8_decode);
    }

    None
}
