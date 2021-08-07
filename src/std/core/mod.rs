use crate::runtime::{Result, Value};
use crate::vm::{Module, VM};

pub mod array;
pub mod map;
pub mod string;

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

pub const DEFAULT_IMPORTS: &[&str; 17] = &[
    "std.core.println",
    "std.core.Option",
    "std.core.some",
    "std.core.none",
    "std.core.String",
    "std.core.Char",
    "std.core.Byte",
    "std.core.Bool",
    "std.core.Int",
    "std.core.Float",
    "std.core.RangeIter",
    "std.core.Range",
    "std.core.ArrayIter",
    "std.core.Array",
    "std.core.Map",
    "std.core.KeyValue",
    "std.core.Iterable",
];
