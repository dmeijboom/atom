pub mod array;
pub mod core;
mod ffi;
pub mod str;

use super::module::{Module, ModuleBuilder};

pub use ffi::{Context, Convert, FnHandler};

pub fn prelude() -> Module {
    let [f1, f2] = core::funcs();

    ModuleBuilder::default()
        .class(array::class())
        .class(str::class())
        .func(f1)
        .func(f2)
        .build()
}
