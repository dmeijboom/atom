use std::fmt::Display;

use atom_macros::export;

use crate::{
    gc::{Gc, Handle, Trace},
    runtime::{error::RuntimeError, value::Value},
};

use super::{array::Array, Atom, Lib};

pub struct Str(pub Array<u8>);

impl Str {
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }

    pub fn from_string(gc: &mut Gc, s: String) -> Self {
        Self(Array::from_vec(gc, s.into_bytes()))
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
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
fn str_len(_atom: Atom<'_>, this: Handle<Str>) -> Result<usize, RuntimeError> {
    Ok(this.0.len())
}

macro_rules! map_fn {
    ($func:ident, $rust_fn:ident) => {
        #[export]
        fn $func(atom: Atom<'_>, this: Handle<Str>) -> Result<Handle<Str>, RuntimeError> {
            let rust_string = this.as_str().$rust_fn();
            let str = Str::from_string(atom.gc, rust_string);
            atom.alloc(str)
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
