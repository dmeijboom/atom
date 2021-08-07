use std::collections::HashMap;

use crate::parse_args;
use crate::runtime::{convert, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_map(
    vm: &mut VM,
    handler: impl FnOnce(&mut HashMap<Value, Value>) -> Option<Value>,
) -> Result<Option<Value>> {
    let mut value = vm.get_local_mut("this").unwrap();
    let type_val = value.get_type();

    if let Value::Object(object) = &mut *value {
        if let Value::Map(field_value) = &mut object.fields[0] {
            return Ok(handler(field_value));
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
                Some(Value::Array(
                    a.keys().into_iter().cloned().collect::<Vec<_>>(),
                ))
            })
        }),
        ("pop", |vm, mut values| {
            let key = parse_args!(values => Any);

            use_map(vm, |a| Some(convert::to_option(a.remove(&key))))
        }),
        ("clear", |vm, values| {
            parse_args!(values);

            use_map(vm, |a| {
                a.clear();

                None
            })
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Map", method_name, closure)?;
    }

    Ok(())
}
