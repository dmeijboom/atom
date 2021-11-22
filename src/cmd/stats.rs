use std::mem::size_of;

use crate::runtime::{AtomRef, Int, Object, Value, ValueType};
use crate::vm::StackFrame;

pub fn command() {
    println!("size of: ValueType: {} bytes", size_of::<ValueType>());
    println!("size of: Value: {} bytes", size_of::<Value>());
    println!(
        "size of: AtomRef<Value>: {} bytes",
        size_of::<AtomRef<Value>>()
    );
    println!("size of: Int: {} bytes", size_of::<Int>());
    println!("size of: Object: {} bytes", size_of::<Object>());
    println!("size of: StackFrame: {} bytes", size_of::<StackFrame>());
}
