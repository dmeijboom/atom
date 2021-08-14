use std::ops::Range;

use crate::parse_args;
use crate::runtime::{with_auto_deref_mut, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_range<T>(vm: &mut VM, handler: impl FnOnce(&mut Range<i64>) -> Result<T>) -> Result<T> {
    let mut value = vm.get_local_mut("this").unwrap();

    with_auto_deref_mut(&mut value, |value| {
        let type_val = value.get_type();

        if let Value::Range(range) = value {
            return handler(range);
        }

        Err(RuntimeError::new(format!(
            "invalid type '{}', expected Range",
            type_val.name()
        )))
    })
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
