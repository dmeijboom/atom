use std::mem::size_of;

use crate::runtime::{Data, Object, Value, ValueType};
use crate::utils::Error;

pub fn command() -> Result<(), Error> {
    println!("size of: Data: {} bytes", size_of::<Data>());
    println!("size of: ValueType: {} bytes", size_of::<ValueType>());
    println!("size of: Value: {} bytes", size_of::<Value>());
    println!("size of: Object: {} bytes", size_of::<Object>());

    Ok(())
}
