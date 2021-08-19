use std::collections::HashMap;

use crate::parse_args;
use crate::runtime::{convert, AtomRef, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_map<T>(
    vm: &mut VM,
    handler: impl FnOnce(&mut HashMap<Value, Value>) -> Result<T>,
) -> Result<T> {
    let value = vm.get_fn_self()?;

    if let Value::Map(map) = value {
        return handler(map.as_mut());
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected Map",
        value.get_type().name()
    )))
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("keys", |vm, values| {
            parse_args!(values);

            use_map(vm, |m| {
                let keys = m.keys().into_iter().cloned().collect::<Vec<_>>();

                Ok(Some(Value::Array(AtomRef::new(keys))))
            })
        }),
        ("len", |vm, values| {
            parse_args!(values);

            use_map(vm, |m| Ok(Some(Value::Int(m.len() as i64))))
        }),
        ("pop", |vm, mut values| {
            let key = parse_args!(values => Any);
            let value = use_map(vm, |a| Ok(a.remove(&key)))?;

            Ok(Some(convert::to_option(value)))
        }),
        ("clear", |vm, values| {
            parse_args!(values);

            use_map(vm, |m| {
                m.clear();

                Ok(None)
            })
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Map", method_name, closure)?;
    }

    Ok(())
}
