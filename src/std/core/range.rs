use std::ops::Range;

use crate::parse_args;
use crate::runtime::{Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_range<T>(vm: &mut VM, handler: impl FnOnce(&mut Range<i64>) -> Result<T>) -> Result<T> {
    let value = vm.get_fn_self()?;

    if let Value::Range(range) = value {
        return handler(range);
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected Range",
        value.get_type().name()
    )))
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("start", |vm, values| {
            parse_args!(values);

            use_range(vm, |range| Ok(Some(Value::Int(range.start))))
        }),
        ("end", |vm, values| {
            parse_args!(values);

            use_range(vm, |range| Ok(Some(Value::Int(range.end))))
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Range", method_name, closure)?;
    }

    Ok(())
}
