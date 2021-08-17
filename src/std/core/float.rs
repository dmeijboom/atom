use std::ops::DerefMut;

use crate::parse_args;
use crate::runtime::{Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_float<T>(vm: &mut VM, handler: impl FnOnce(&mut f64) -> Result<T>) -> Result<T> {
    let value = vm.get_fn_self().unwrap();

    if let Value::Float(val) = value.borrow_mut().deref_mut() {
        return handler(val);
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected Int",
        value.borrow().get_type().name()
    )))
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
