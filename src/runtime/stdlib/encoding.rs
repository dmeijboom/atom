use crate::runtime::{
    AtomRefMut, Convert, ErrorKind, Input, Int, Output, Result, RuntimeError, Value,
};

pub mod binary {
    use crate::runtime::ExternalFn;

    pub const FUNCTIONS: [(&str, ExternalFn); 2] = [
        ("parseInt", super::parse_int),
        ("parseUint", super::parse_uint),
    ];
}

pub mod utf8 {
    use crate::runtime::ExternalFn;

    pub const FUNCTIONS: [(&str, ExternalFn); 1] = [("decode", super::utf8_decode)];
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

pub fn parse_int(input: Input<'_>) -> Result<Output> {
    let data: AtomRefMut<Vec<Value>> = input.single()?;
    let mut bytes = vec![];

    for item in data.iter() {
        bytes.push(item.clone().convert()?);
    }

    if bytes.len() == 4 {
        return Ok(Output::new(Int::from(i32::from_ne_bytes(
            to_fixed_array::<_, 4>(bytes)?,
        ))));
    }

    Ok(Output::new(Int::from(i64::from_ne_bytes(
        to_fixed_array::<_, 8>(bytes)?,
    ))))
}

pub fn parse_uint(input: Input<'_>) -> Result<Output> {
    let data: AtomRefMut<Vec<Value>> = input.single()?;
    let mut bytes = vec![];

    for item in data.iter() {
        bytes.push(item.clone().convert()?);
    }

    if bytes.len() == 4 {
        return Ok(Output::new(Int::from(u32::from_ne_bytes(
            to_fixed_array::<_, 4>(bytes)?,
        ))));
    }

    Ok(Output::new(Int::from(u64::from_ne_bytes(
        to_fixed_array::<_, 8>(bytes)?,
    ))))
}

pub fn utf8_decode(input: Input<'_>) -> Result<Output> {
    let data: AtomRefMut<Vec<Value>> = input.single()?;
    let mut bytes = vec![];

    for item in data.iter() {
        bytes.push(item.clone().convert()?);
    }

    Ok(Output::new(String::from_utf8(bytes).map_err(|e| {
        RuntimeError::new(ErrorKind::FatalError, format!("{}", e))
    })?))
}
