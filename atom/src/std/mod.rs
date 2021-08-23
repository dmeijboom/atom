use std::collections::HashMap;

use crate::vm::Middleware;

pub mod core;
pub mod encoding;
pub mod io;

pub fn get_middleware() -> HashMap<&'static str, Middleware> {
    let mut middleware: HashMap<&str, Middleware> = HashMap::new();

    middleware.insert("std.core", core::register);
    middleware.insert("std.io", io::register);
    middleware.insert("std.encoding.utf8", encoding::utf8::register);

    middleware
}
