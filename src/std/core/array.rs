use std::cell::RefMut;

use crate::parse_args;
use crate::runtime::{convert, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

pub fn use_array(
    vm: &VM,
    handler: impl FnOnce(RefMut<Vec<Value>>) -> Option<Value>,
) -> Result<Option<Value>> {
    let value = vm.get_local("this").unwrap();

    if let Value::Object(object) = &value {
        let object = object.borrow();

        if let Value::Array(field_value) = &object.fields[0] {
            return Ok(handler(field_value.borrow_mut()));
        }
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected Array",
        value.get_type().name()
    )))
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("push", |vm, mut values| {
            let value = parse_args!(values => Any);

            use_array(vm, |mut a| {
                a.push(value);

                None
            })
        }),
        ("pop", |vm, mut values| {
            if values.len() == 1 {
                let index = parse_args!(values => Int) as usize;

                return use_array(vm, |mut a| {
                    Some(convert::to_option(if a.get(index).is_some() {
                        Some(a.remove(index))
                    } else {
                        None
                    }))
                });
            }

            parse_args!(values);

            use_array(vm, |mut a| Some(convert::to_option(a.pop())))
        }),
        ("clear", |vm, values| {
            parse_args!(values);

            use_array(vm, |mut a| {
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
