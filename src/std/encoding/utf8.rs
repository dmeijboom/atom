use std::str;

use crate::parse_args;
use crate::runtime::{convert, AtomRef, Result, RuntimeError, Value};
use crate::vm::Module;

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("decode", |_, mut values| {
        let data = parse_args!(values => Array);
        let mut bytes = vec![];

        for item in data.iter() {
            bytes.push(convert::to_byte(item)?);
        }

        let text =
            str::from_utf8(&bytes).map_err(|e| RuntimeError::new(format!("DecodeError: {}", e)))?;

        Ok(Some(Value::String(AtomRef::new(text.to_string()))))
    });

    Ok(())
}
