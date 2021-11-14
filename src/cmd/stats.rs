use std::mem::size_of;

use crate::runtime::{AtomRef, Object, Value, ValueType};
use crate::utils::Error;
use crate::vm::CallContext;

pub fn command() -> Result<(), Error> {
    println!("size of: ValueType: {} bytes", size_of::<ValueType>());
    println!("size of: Value: {} bytes", size_of::<Value>());
    println!(
        "size of: AtomRef<Value>: {} bytes",
        size_of::<AtomRef<Value>>()
    );
    println!("size of: Object: {} bytes", size_of::<Object>());
    println!("size of: CallContext: {} bytes", size_of::<CallContext>());

    Ok(())
}
