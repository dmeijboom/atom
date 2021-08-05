use crate::runtime::{Result, Value};
use crate::vm::{Module, VM};

mod array;
mod map;
mod string;

#[macro_export]
macro_rules! parse_args {
    ($values:expr) => {
        if $values.len() > 0 {
            return Err(RuntimeError::new(format!("invalid argument count (expected 0, not {})", $values.len())));
        }
    };

    ($values:expr, Any) => {
        $values.remove(0)
    };

    ($values:expr, String) => {
        $values.remove(0).to_string()
    };

    ($values:expr, Int) => {
        crate::runtime::convert::to_int($values.remove(0))?
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
                return Err(crate::runtime::RuntimeError::new(format!("invalid argument count (expected {}, not {})", arg_count, $values.len())));
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

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("println", println);

    string::register(module)?;
    array::register(module)?;
    map::register(module)?;

    Ok(())
}

pub const DEFAULT_IMPORTS: &[&str; 15] = &[
    "std.core.println",
    "std.core.Option",
    "std.core.some",
    "std.core.none",
    "std.core.String",
    "std.core.Char",
    "std.core.Byte",
    "std.core.Int",
    "std.core.Float",
    "std.core.RangeIter",
    "std.core.Range",
    "std.core.ArrayIter",
    "std.core.Array",
    "std.core.Map",
    "std.core.KeyValue",
];
