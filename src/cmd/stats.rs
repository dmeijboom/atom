use std::mem::size_of;

use crate::runtime::types::{AtomArray, AtomRef, Object, Value, ValueType};
use crate::vm::StackFrame;

pub fn command() {
    println!("size of: ValueType: {} bytes", size_of::<ValueType>());
    println!("size of: Value: {} bytes", size_of::<Value>());
    println!(
        "size of: AtomRef<Value>: {} bytes",
        size_of::<AtomRef<Value>>()
    );
    println!(
        "size of: AtomArray<Value>: {} bytes",
        size_of::<AtomArray<Value>>()
    );
    println!("size of: Object: {} bytes", size_of::<Object>());
    println!("size of: StackFrame: {} bytes", size_of::<StackFrame>());
}
