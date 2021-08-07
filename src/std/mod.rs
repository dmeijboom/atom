use std::collections::HashMap;

use crate::vm::Middleware;

pub mod core;
pub mod encoding;
pub mod io;

#[macro_export]
macro_rules! parse_args {
    ($values:expr) => {
        if $values.len() > 0 {
            return Err(crate::runtime::RuntimeError::new(format!("invalid argument count (expected 0, not {})", $values.len())));
        }
    };

    ($values:expr, Any) => {
        $values.remove(0)
    };

    ($values:expr, String) => {
        $values.remove(0).to_string()
    };

    ($values:expr, Array) => {
        crate::runtime::convert::to_array($values.remove(0))?
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

pub fn get_middleware() -> HashMap<&'static str, Middleware> {
    let mut middleware: HashMap<&str, Middleware> = HashMap::new();

    middleware.insert("std.core", core::register);
    middleware.insert("std.io", io::register);
    middleware.insert("std.encoding.utf8", encoding::utf8::register);

    middleware
}
