use crate::vm::Module;
use atom_runtime::Result;

pub mod array;
pub mod float;
pub mod map;
pub mod option;
pub mod range;
pub mod string;

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("println", |vm, values| {
        println!(
            "{}",
            values
                .into_iter()
                .map(|value| vm.fmt_value(&value))
                .collect::<Vec<_>>()
                .join(", "),
        );

        Ok(None)
    });

    option::register(module)?;
    string::register(module)?;
    array::register(module)?;
    map::register(module)?;
    range::register(module)?;
    float::register(module)?;

    Ok(())
}

pub const DEFAULT_IMPORTS: &[&str; 15] = &[
    "std.core.println",
    "std.core.some",
    "std.core.RangeIter",
    "std.core.ArrayIter",
    "std.core.KeyValue",
    "std.core.Iterable",
    // Also all basic types (for type assertions for example)
    "std.core.String",
    "std.core.Int",
    "std.core.Float",
    "std.core.Char",
    "std.core.Byte",
    "std.core.Bool",
    "std.core.Range",
    "std.core.Array",
    "std.core.Map",
];
