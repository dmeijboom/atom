use crate::parse_args;
use crate::runtime::{AtomRef, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

fn use_string(vm: &mut VM, handler: impl Fn(&String) -> Value) -> Result<Option<Value>> {
    let value = vm.get_fn_self()?;

    if let Value::String(s) = value {
        return Ok(Some(handler(s.as_ref())));
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected String",
        value.get_type().name()
    )))
}

fn use_into_string(vm: &mut VM, handler: impl Fn(&String) -> String) -> Result<Option<Value>> {
    use_string(vm, |s| Value::String(AtomRef::new(handler(s))))
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
                    s.splitn(count as usize, split.as_ref())
                        .map(|component| Value::String(AtomRef::new(component.to_string())))
                        .collect::<Vec<_>>()
                } else {
                    s.split(split.as_ref())
                        .map(|component| Value::String(AtomRef::new(component.to_string())))
                        .collect::<Vec<_>>()
                };

                Value::Array(AtomRef::new(components))
            })
        }),
        ("startsWith", |vm, mut values| {
            let search = parse_args!(values => String);

            use_string(vm, |s| Value::Bool(s.starts_with(search.as_ref())))
        }),
        ("endsWith", |vm, mut values| {
            let search = parse_args!(values => String);

            use_string(vm, |s| Value::Bool(s.ends_with(search.as_ref())))
        }),
        ("contains", |vm, mut values| {
            let search = parse_args!(values => String);

            use_string(vm, |s| Value::Bool(s.contains(search.as_ref())))
        }),
        ("chars", |vm, values| {
            parse_args!(values);

            use_string(vm, |s| {
                let items = s.chars().map(Value::Char).collect::<Vec<_>>();

                Value::Array(AtomRef::new(items))
            })
        }),
        ("repeat", |vm, mut values| {
            let count = parse_args!(values => Int);

            use_into_string(vm, |s| s.repeat(count as usize))
        }),
        ("len", |vm, values| {
            parse_args!(values);

            use_string(vm, |s| Value::Int(s.len() as i64))
        }),
    ];

    for (method_name, closure) in methods {
        module.register_external_method("String", method_name, closure)?;
    }

    Ok(())
}
