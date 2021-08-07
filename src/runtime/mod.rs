pub use result::{Result, RuntimeError, Trace};
pub use value::{Data, FieldDesc, Method, Object, PointerType, TypeId, Value, ValueType};

pub mod convert;
mod result;
mod value;
