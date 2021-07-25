pub use result::{Result, RuntimeError};
pub use value::{ClassDesc, ClassId, FieldDesc, FuncId, Object, PointerType, Value, ValueType};

pub mod convert;
mod result;
mod value;
