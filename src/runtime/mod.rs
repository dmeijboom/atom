pub use collections::IndexedBTreeMap;
pub use result::{Result, RuntimeError};
pub use value::{ClassDesc, ClassId, FieldDesc, FuncId, Object, PointerType, Value, ValueType};

mod collections;
pub mod convert;
mod result;
mod value;
