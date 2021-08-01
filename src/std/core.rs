use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::{convert, Result, RuntimeError, Value};
use crate::vm::{ExternalFn, Module, VM};

macro_rules! parse_args {
    ($values:expr, String) => {
        $values.remove(0).to_string()
    };

    ($values:expr, Int) => {
        convert::to_int($values.remove(0).clone())?
    };

    ($values:expr, Int_) => {
        if $values.is_empty() {
            None
        } else {
            Some(parse_args!($values, Int))
        }
    };

    ($values:expr => $($types:ident),+) => {
        {
            let arg_count = vec![$(stringify!($types)),+]
                .into_iter()
                .filter(|name| !name.ends_with("_"))
                .collect::<Vec<_>>()
                .len();

            if $values.len() < arg_count {
                return Err(RuntimeError::new(format!("invalid argument count (expected {}, not {})", arg_count, $values.len())));
            } else {
                ($(parse_args!($values, $types)),+)
            }
        }
    };
}

pub fn println(_: &VM, values: Vec<Value>) -> Result<Option<Value>> {
    println!(
        "{}",
        values
            .into_iter()
            .map(|value| format!("{}", value))
            .collect::<Vec<_>>()
            .join(", "),
    );

    Ok(None)
}

/**
 * External methods for the 'std.core.String' type
 */

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
    module.register_external_fn("println", println);

    let class_methods: Vec<(_, Vec<(_, ExternalFn)>)> = vec![(
        "String",
        vec![
            ("upper", |vm, _| use_into_string(vm, |s| s.to_uppercase())),
            ("lower", |vm, _| use_into_string(vm, |s| s.to_lowercase())),
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
            ("chars", |vm, _| {
                use_string(vm, |s| {
                    let items = s.chars().map(|c| Value::Char(c)).collect::<Vec<_>>();
                    Value::Array(Rc::new(RefCell::new(items)))
                })
            }),
            ("repeat", |vm, mut values| {
                let count = parse_args!(values => Int);

                use_into_string(vm, |s| s.repeat(count as usize))
            }),
        ],
    )];

    for (class_name, methods) in class_methods {
        for (method_name, closure) in methods {
            module.register_external_method(class_name, method_name, closure)?;
        }
    }

    Ok(())
}

pub const DEFAULT_IMPORTS: &[&str; 10] = &[
    "std.core.println",
    "std.core.Option",
    "std.core.String",
    "std.core.Int",
    "std.core.Float",
    "std.core.RangeIter",
    "std.core.Range",
    "std.core.ArrayIter",
    "std.core.Array",
    "std.core.Map",
];
