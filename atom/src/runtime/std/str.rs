use atom_macros::atom_method;

use crate::{
    gc::{Gc, Trace},
    runtime::value::{Type, Value},
};

use super::{array::Array, Context, TypeDescr};

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

#[atom_method(Str.len)]
fn str_len(ctx: Context<'_>, this: &Str) -> Result<usize, RuntimeError> {
    Ok(this.0.len())
}

macro_rules! map_fn {
    ($func:ident, $ty:ident.$method:ident, $rust_fn:ident) => {
        #[atom_method($ty.$method)]
        fn $func(ctx: Context<'_>, this: &Str) -> Result<Handle<Str>, RuntimeError> {
            let rust_string = this.as_str().$rust_fn();
            let str = Str::from_string(ctx.gc, rust_string);
            ctx.gc.alloc(str)
        }
    };
}

map_fn!(str_upper, Str.upper, to_uppercase);
map_fn!(str_lower, Str.lower, to_lowercase);

pub fn descr() -> TypeDescr {
    TypeDescr::new(Type::Str)
        .builder()
        .method(str_len)
        .method(str_upper)
        .method(str_lower)
        .build()
}
