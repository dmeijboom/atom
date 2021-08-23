use std::convert::TryInto;

use atom_macros::export;
use atom_runtime::{Result, RuntimeError};

use crate::std::core::array::Array;
use crate::vm::Module;

#[export]
fn utf8_decode(data: Array) -> Result<String> {
    let mut bytes = vec![];

    for item in data.into_iter() {
        bytes.push(item.try_into()?);
    }

    Ok(String::from_utf8(bytes).map_err(|e| RuntimeError::new(format!("DecodeError: {}", e)))?)
}

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("decode", utf8_decode);

    Ok(())
}
