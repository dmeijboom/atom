use smallvec::SmallVec;

use crate::runtime::{Result, Value};
use crate::vm::{Module, VM};

pub mod array;
pub mod float;
pub mod map;
pub mod range;
pub mod string;

pub fn println(vm: &mut VM, values: SmallVec<[Value; 2]>) -> Result<Option<Value>> {
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

    string::register(module)?;
    array::register(module)?;
    map::register(module)?;
    range::register(module)?;
    float::register(module)?;

    Ok(())
}

pub const DEFAULT_IMPORTS: &[&str; 7] = &[
    "std.core.println",
    "std.core.some",
    "std.core.none",
    "std.core.RangeIter",
    "std.core.ArrayIter",
    "std.core.KeyValue",
    "std.core.Iterable",
];
