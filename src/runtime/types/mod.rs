pub use atom_ref::{
    make_array, unwrap_or_clone_inner, AtomArray, AtomRef, AtomRefMut, AtomWeakRef,
};
pub use class::{Class, Field};
pub use closure::Closure;
pub use interface::Interface;
pub use method::{Method, Receiver};
pub use object::Object;
pub use r#fn::{ExternalFn, Fn, FnArg, FnKind, Input};
pub use rust_object::RustObject;
pub use string::AtomString;
pub use symbol::Symbol;
pub use value::{Value, ValueType};

#[derive(Debug, Clone, PartialEq)]
pub struct AtomNil;

mod atom_ref;
mod class;
mod closure;
mod r#fn;
mod interface;
mod method;
mod object;
mod rust_object;
mod string;
mod symbol;
mod value;
