use atom_runtime::ExternalFn;

pub mod array;
pub mod float;
pub mod map;
pub mod option;
pub mod string;
pub mod tuple;

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.core" && name == "println" {
        return Some(|_, _, values| {
            println!(
                "{}",
                values
                    .into_iter()
                    .map(|value| format!("{}", value))
                    .collect::<Vec<_>>()
                    .join(", "),
            );

            Ok(None)
        });
    }

    option::hook(module_name, name, method_name).or_else(|| {
        string::hook(module_name, name, method_name).or_else(|| {
            array::hook(module_name, name, method_name).or_else(|| {
                map::hook(module_name, name, method_name).or_else(|| {
                    float::hook(module_name, name, method_name)
                        .or_else(|| tuple::hook(module_name, name, method_name))
                })
            })
        })
    })
}

pub const DEFAULT_IMPORTS: &[&str; 17] = &[
    "std.core.println",
    "std.core.some",
    "std.core.RangeIter",
    "std.core.ArrayIter",
    "std.core.Iterable",
    // Also all basic types (for type assertions for example)
    "std.core.String",
    "std.core.Int",
    "std.core.Float",
    "std.core.Char",
    "std.core.Byte",
    "std.core.Bool",
    "std.core.Symbol",
    "std.core.Range",
    "std.core.Option",
    "std.core.Tuple",
    "std.core.Array",
    "std.core.Map",
];
