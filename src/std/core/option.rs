use crate::parse_args;
use crate::runtime::{with_auto_deref_mut, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_option<T>(
    vm: &mut VM,
    handler: impl FnOnce(&mut Option<Box<Value>>) -> Result<T>,
) -> Result<T> {
    let mut value = vm.get_local_mut("this").unwrap();

    with_auto_deref_mut(&mut value, |value| {
        let type_val = value.get_type();

        if let Value::Option(val) = value {
            return handler(val);
        }

        Err(RuntimeError::new(format!(
            "invalid type '{}', expected Option",
            type_val.name()
        )))
    })
}

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("some", |_, mut values| {
        let value = parse_args!(values => Any);

        Ok(Some(Value::Option(Some(value.into()))))
    });

    let methods: Vec<(_, ExternalFn)> = vec![
        ("isSome", |vm, values| {
            parse_args!(values);

            use_option(vm, |val| Ok(Some(Value::Bool(val.is_some()))))
        }),
        ("isNone", |vm, values| {
            parse_args!(values);

            use_option(vm, |val| Ok(Some(Value::Bool(val.is_none()))))
        }),
        ("value", |vm, values| {
            parse_args!(values);

            use_option(vm, |val| Ok(Some(*val.as_ref().unwrap().clone())))
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("Option", method_name, closure)?;
    }

    Ok(())
}
