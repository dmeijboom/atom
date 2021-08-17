use crate::parse_args;
use crate::runtime::{convert, with_auto_deref_mut, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

pub fn use_array<T>(vm: &mut VM, handler: impl FnOnce(&mut Vec<Value>) -> Result<T>) -> Result<T> {
    let mut value = vm.get_local_mut("this").unwrap();

    with_auto_deref_mut(&mut value, |value| {
        let type_val = value.get_type();

        if let Value::Array(array) = value {
            return handler(array);
        }

        Err(RuntimeError::new(format!(
            "invalid type '{}', expected Array",
            type_val.name()
        )))
    })
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("push", |vm, mut values| {
            let value = parse_args!(values => Any);

            use_array(vm, |a| {
                a.push(value);

                Ok(None)
            })
        }),
        ("pop", |vm, mut values| {
            if values.len() == 1 {
                let index = parse_args!(values => Int) as usize;

                let item = use_array(vm, |a| {
                    Ok(if a.get(index).is_some() {
                        Some(a.remove(index))
                    } else {
                        None
                    })
                })?;

                return Ok(Some(convert::to_option(item)));
            }

            parse_args!(values);

            let item = use_array(vm, |a| Ok(a.pop()))?;

            Ok(Some(convert::to_option(item)))
        }),
        ("len", |vm, values| {
            parse_args!(values);

            use_array(vm, |a| Ok(Some(Value::Int(a.len() as i64))))
        }),
        ("clear", |vm, values| {
            parse_args!(values);

            use_array(vm, |a| {
                a.clear();

                Ok(None)
            })
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Array", method_name, closure)?;
    }

    Ok(())
}
