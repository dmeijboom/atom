use crate::runtime::types::{AtomRefMut, AtomString, Input, Value};
use crate::runtime::{Convert, ErrorKind, Result, RuntimeError};

pub mod binary {
    use crate::runtime::macros::wrap_fn;
    use crate::runtime::types::ExternalFn;

    pub const FUNCTIONS: [(&str, ExternalFn); 2] = [
        ("parseInt", wrap_fn!(super::parse_int)),
        ("parseUint", wrap_fn!(super::parse_uint)),
    ];
}

pub mod utf8 {
    use crate::runtime::macros::wrap_fn;
    use crate::runtime::types::ExternalFn;

    pub const FUNCTIONS: [(&str, ExternalFn); 1] = [("decode", wrap_fn!(super::utf8_decode))];
}

fn to_fixed_array<T: Copy, const N: usize>(items: Vec<T>) -> Result<[T; N]> {
    if items.len() != N {
        return Err(RuntimeError::new(
            ErrorKind::FatalError,
            "invalid array size".to_string(),
        ));
    }

    unsafe { Ok(*(items.as_ptr() as *const [T; N])) }
}

pub fn parse_int(mut input: Input<'_>) -> Result<impl Into<Value>> {
    let data: AtomRefMut<Vec<Value>> = input.take_arg()?;
    let mut bytes = vec![];

    for item in data.iter() {
        bytes.push(item.clone().convert()?);
    }

    if bytes.len() == 4 {
        return Ok(i32::from_ne_bytes(to_fixed_array::<_, 4>(bytes)?) as i64);
    }

    Ok(i64::from_ne_bytes(to_fixed_array::<_, 8>(bytes)?))
}

pub fn parse_uint(mut input: Input<'_>) -> Result<impl Into<Value>> {
    let data: AtomRefMut<Vec<Value>> = input.take_arg()?;
    let mut bytes = vec![];

    for item in data.iter() {
        bytes.push(item.clone().convert()?);
    }

    if bytes.len() == 4 {
        return Ok(u32::from_ne_bytes(to_fixed_array::<_, 4>(bytes)?) as u64);
    }

    Ok(u64::from_ne_bytes(to_fixed_array::<_, 8>(bytes)?))
}

pub fn utf8_decode(mut input: Input<'_>) -> Result<impl Into<Value>> {
    let data: AtomRefMut<Vec<Value>> = input.take_arg()?;
    let mut bytes = vec![];

    for item in data.iter() {
        bytes.push(item.clone().convert()?);
    }

    let s = String::from_utf8(bytes)
        .map_err(|e| RuntimeError::new(ErrorKind::FatalError, format!("{}", e)))?;

    Ok(AtomString::new(s))
}
