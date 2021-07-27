pub use result::{Result, RuntimeError, Trace};
pub use value::{ClassId, FieldDesc, FuncId, Method, Object, PointerType, Value, ValueType};

pub mod convert;
mod result;
mod value;
