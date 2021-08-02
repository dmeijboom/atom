use std::cell::RefCell;
use std::rc::Rc;

use crate::parse_args;
use crate::runtime::{Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_string(vm: &VM, handler: impl Fn(&String) -> Value) -> Result<Option<Value>> {
    let value = vm.get_local("this").unwrap();

    if let Value::Object(object) = &value {
        let object = object.borrow();

        if let Value::String(field_value) = &object.fields[0] {
            return Ok(Some(handler(field_value)));
        }
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected String",
        value.get_type().name()
    )))
}

fn use_into_string(vm: &VM, handler: impl Fn(&String) -> String) -> Result<Option<Value>> {
    use_string(vm, |s| Value::String(handler(s)))
}

pub fn register(module: &mut Module) -> Result<()> {
    let methods: Vec<(_, ExternalFn)> = vec![
        ("upper", |vm, values| {
            parse_args!(values);

            use_into_string(vm, |s| s.to_uppercase())
        }),
        ("lower", |vm, values| {
            parse_args!(values);

            use_into_string(vm, |s| s.to_lowercase())
        }),
        ("split", |vm, mut values| {
            let (split, count) = parse_args!(values => String, Int_);

            use_string(vm, |s| {
                let components = if let Some(count) = count {
                    s.splitn(count as usize, &split)
                        .map(|component| Value::String(component.to_string()))
                        .collect::<Vec<_>>()
                } else {
                    s.split(&split)
                        .map(|component| Value::String(component.to_string()))
                        .collect::<Vec<_>>()
                };

                Value::Array(Rc::new(RefCell::new(components)))
            })
        }),
        ("startsWith", |vm, mut values| {
            let search = parse_args!(values => String);

            use_string(vm, |s| Value::Bool(s.starts_with(&search)))
        }),
        ("endsWith", |vm, mut values| {
            let search = parse_args!(values => String);

            use_string(vm, |s| Value::Bool(s.ends_with(&search)))
        }),
        ("contains", |vm, mut values| {
            let search = parse_args!(values => String);

            use_string(vm, |s| Value::Bool(s.contains(&search)))
        }),
        ("chars", |vm, values| {
            parse_args!(values);

            use_string(vm, |s| {
                let items = s.chars().map(|c| Value::Char(c)).collect::<Vec<_>>();
                Value::Array(Rc::new(RefCell::new(items)))
            })
        }),
        ("repeat", |vm, mut values| {
            let count = parse_args!(values => Int);

            use_into_string(vm, |s| s.repeat(count as usize))
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("String", method_name, closure)?;
    }

    Ok(())
}
