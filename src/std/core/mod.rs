use crate::runtime::{Result, Value};
use crate::vm::{Module, VM};

pub mod array;
pub mod float;
pub mod map;
pub mod option;
pub mod range;
pub mod string;

pub fn println(vm: &mut VM, values: Vec<Value>) -> Result<Option<Value>> {
    println!(
        "{}",
        values
            .into_iter()
            .map(|value| vm.fmt_value(&value))
            .collect::<Vec<_>>()
            .join(", "),
    );

    Ok(None)
}

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("println", println);

    option::register(module)?;
    string::register(module)?;
    array::register(module)?;
    map::register(module)?;
    range::register(module)?;
    float::register(module)?;

    Ok(())
}

pub const DEFAULT_IMPORTS: &[&str; 6] = &[
    "std.core.println",
    "std.core.some",
    "std.core.RangeIter",
    "std.core.ArrayIter",
    "std.core.KeyValue",
    "std.core.Iterable",
];
