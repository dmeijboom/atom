use crate::vm::ExternalHook;

pub mod core;
pub mod encoding;
pub mod io;

pub fn get_hooks() -> [ExternalHook; 3] {
    [core::hook, io::hook, encoding::utf8::hook]
}
