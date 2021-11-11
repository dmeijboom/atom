use crate::runtime::ExternalFn;

mod core;
mod encoding;

pub fn hook(module: &str, name: &str, class_name: Option<&str>) -> Option<ExternalFn> {
    match class_name {
        None => match module {
            "std.core" => core::FUNCTIONS.iter(),
            "std.encoding.utf8" => encoding::utf8::FUNCTIONS.iter(),
            "std.encoding.binary" => encoding::binary::FUNCTIONS.iter(),
            _ => return None,
        }
        .find(|(function_name, _)| *function_name == name)
        .map(|(_, function)| *function),
        Some(match_class_name) => match module {
            "std.core" => core::METHODS,
            _ => return None,
        }
        .iter()
        .find(|(class_name, function_name, _)| {
            *class_name == match_class_name && *function_name == name
        })
        .map(|(_, _, function)| *function),
    }
}
