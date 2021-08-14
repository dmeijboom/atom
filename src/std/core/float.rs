use crate::parse_args;
use crate::runtime::{with_auto_deref_mut, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_float<T>(vm: &mut VM, handler: impl FnOnce(&mut f64) -> Result<T>) -> Result<T> {
    let mut value = vm.get_local_mut("this").unwrap();

    with_auto_deref_mut(&mut value, |value| {
        let type_val = value.get_type();

        if let Value::Float(val) = value {
            return handler(val);
        }

        Err(RuntimeError::new(format!(
            "invalid type '{}', expected Int",
            type_val.name()
        )))
    })
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![("floor", |vm, values| {
        parse_args!(values);

        use_float(vm, |val| Ok(Some(Value::Float(val.floor()))))
    })];

    for (method_name, closure) in methods {
        module.register_external_method("Float", method_name, closure)?;
    }

    Ok(())
}
