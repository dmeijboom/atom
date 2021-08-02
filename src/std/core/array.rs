use std::cell::RefMut;

use crate::parse_args;
use crate::runtime::{convert, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn void(
    handler: impl FnOnce(RefMut<Vec<Value>>),
) -> impl FnOnce(RefMut<Vec<Value>>) -> Option<Value> {
    |a| {
        handler(a);

        None
    }
}

fn use_array(
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
        "invalid type '{}', expected String",
        value.get_type().name()
    )))
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("push", |vm, mut values| {
            let value = parse_args!(values => Any);

            use_array(vm, void(|mut a| a.push(value)))
        }),
        ("pop", |vm, _| {
            use_array(vm, |mut a| Some(convert::to_option(a.pop())))
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Array", method_name, closure)?;
    }

    Ok(())
}
