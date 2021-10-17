use std::convert::TryInto;

use atom_macros::export;
use atom_runtime::{ExternalFn, Int, Result, RuntimeError, Value};

fn to_fixed_array<T: Copy, const N: usize>(items: Vec<T>) -> Result<[T; N]> {
    if items.len() != N {
        return Err(RuntimeError::new("invalid array size".to_string()));
    }

    unsafe { Ok(*(items.as_ptr() as *const [T; N])) }
}

#[export]
fn int_from_bytes(data: Vec<Value>) -> Result<Int> {
    let mut bytes = vec![];

    for item in data.into_iter() {
        bytes.push(item.try_into()?);
    }

    if bytes.len() == 4 {
        return Ok(i32::from_ne_bytes(to_fixed_array::<_, 4>(bytes)?).into());
    }

    Ok(i64::from_ne_bytes(to_fixed_array::<_, 8>(bytes)?).into())
}

#[export]
fn uint_from_bytes(data: Vec<Value>) -> Result<Int> {
    let mut bytes = vec![];

    for item in data.into_iter() {
        bytes.push(item.try_into()?);
    }

    if bytes.len() == 4 {
        return Ok(u32::from_ne_bytes(to_fixed_array::<_, 4>(bytes)?).into());
    }

    Ok(u64::from_ne_bytes(to_fixed_array::<_, 8>(bytes)?).into())
}

pub fn hook(module_name: &str, name: &str, _method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.encoding.binary" {
        match name {
            "int_from_bytes" => return Some(int_from_bytes),
            "uint_from_bytes" => return Some(uint_from_bytes),
            _ => {}
        }
    }

    None
}
