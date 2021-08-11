use std::collections::HashMap;

use crate::parse_args;
use crate::runtime::{convert, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_map<T>(
    vm: &mut VM,
    handler: impl FnOnce(&mut HashMap<Value, Value>) -> Result<T>,
) -> Result<T> {
    let mut value = vm.get_local_mut("this").unwrap();
    let type_val = value.get_type();

    if let Value::Object(object) = &mut *value {
        if let Some(Value::Map(field_value)) = object.get_field_mut(0) {
            return Ok(handler(field_value)?);
        }
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected Map",
        type_val.name()
    )))
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("keys", |vm, values| {
            parse_args!(values);

            use_map(vm, |a| {
                Ok(Some(Value::Array(
                    a.keys().into_iter().cloned().collect::<Vec<_>>(),
                )))
            })
        }),
        ("pop", |vm, mut values| {
            let key = parse_args!(values => Any);
            let value = use_map(vm, |a| Ok(a.remove(&key)))?;

            Ok(Some(convert::to_option(vm, value)?))
        }),
        ("clear", |vm, values| {
            parse_args!(values);

            use_map(vm, |a| {
                a.clear();

                Ok(None)
            })
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Map", method_name, closure)?;
    }

    Ok(())
}
