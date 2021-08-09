pub use result::{Result, RuntimeError, Trace};
pub use value::{
    with_auto_deref, with_auto_deref_mut, Data, FieldDesc, Method, Object, TypeId, Value, ValueType,
};

pub mod convert;
mod result;
mod value;
