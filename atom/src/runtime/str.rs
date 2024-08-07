use atom_macros::export;

use crate::{
    gc::{Gc, Handle, Trace},
    runtime::{error::RuntimeError, value::Value},
};

use super::{array::Array, Context, Lib};

pub struct Str(pub Array<u8>);

impl Str {
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }

    pub fn from_string(gc: &mut Gc, s: String) -> Self {
        Self(Array::from_vec(gc, s.into_bytes()))
    }
}

impl Trace for Str {
    fn trace(&self, gc: &mut Gc) {
        self.0.trace(gc);
    }
}

impl AsRef<str> for Str {
    fn as_ref(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.0.as_slice()) }
    }
}

#[export]
fn str_len(ctx: Context<'_>, this: Handle<Str>) -> Result<usize, RuntimeError> {
    let Str(array) = ctx.gc.get(this);
    Ok(array.len())
}

macro_rules! map_fn {
    ($func:ident, $rust_fn:ident) => {
        #[export]
        fn $func(ctx: Context<'_>, this: Handle<Str>) -> Result<Handle<Str>, RuntimeError> {
            let atom_str = ctx.gc.get(this);
            let rust_string = atom_str.as_str().$rust_fn();
            let str = Str::from_string(ctx.gc, rust_string);
            ctx.gc.alloc(str)
        }
    };
}

map_fn!(str_upper, to_uppercase);
map_fn!(str_lower, to_lowercase);

pub fn register(lib: Lib) -> Lib {
    lib.class("Str", |lib| {
        lib.set("len", atom_str_len)
            .set("upper", atom_str_upper)
            .set("lower", atom_str_lower)
    })
}
