use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::rc::Rc;

use crate::parse_args;
use crate::runtime::{convert, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_map(
    vm: &VM,
    handler: impl FnOnce(RefMut<HashMap<Value, Value>>) -> Option<Value>,
) -> Result<Option<Value>> {
    let value = vm.get_local("this").unwrap();

    if let Value::Object(object) = &value {
        let object = object.borrow();

        if let Value::Map(field_value) = &object.fields[0] {
            return Ok(handler(field_value.borrow_mut()));
        }
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

            use_map(vm, |a| {
                Some(Value::Array(Rc::new(RefCell::new(
                    a.keys().into_iter().cloned().collect::<Vec<_>>(),
                ))))
            })
        }),
        ("pop", |vm, mut values| {
            let key = parse_args!(values => Any);

            use_map(vm, |mut a| Some(convert::to_option(a.remove(&key))))
        }),
        ("clear", |vm, values| {
            parse_args!(values);

            use_map(vm, |mut a| {
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
