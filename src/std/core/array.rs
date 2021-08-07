use crate::parse_args;
use crate::runtime::{convert, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

pub fn use_array(
    vm: &mut VM,
    handler: impl FnOnce(&mut Vec<Value>) -> Option<Value>,
) -> Result<Option<Value>> {
    let mut value = vm.get_local_mut("this").unwrap();
    let type_val = value.get_type();

    if let Value::Object(object) = &mut *value {
        if let Value::Array(field_value) = &mut object.fields[0] {
            return Ok(handler(field_value));
        }
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected Array",
        type_val.name()
    )))
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("push", |vm, mut values| {
            let value = parse_args!(values => Any);

            use_array(vm, |a| {
                a.push(value);

                None
            })
        }),
        ("pop", |vm, mut values| {
            if values.len() == 1 {
                let index = parse_args!(values => Int) as usize;

                return use_array(vm, |a| {
                    Some(convert::to_option(if a.get(index).is_some() {
                        Some(a.remove(index))
                    } else {
                        None
                    }))
                });
            }

            parse_args!(values);

            use_array(vm, |a| Some(convert::to_option(a.pop())))
        }),
        ("clear", |vm, values| {
            parse_args!(values);

            use_array(vm, |a| {
                a.clear();

                None
            })
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Array", method_name, closure)?;
    }

    Ok(())
}
