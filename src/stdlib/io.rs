use std::fs::File;
use std::io;
use std::io::Read;

use crate::runtime::{
    AtomRef, Convert, ExternalFn, Input, Int, Object, Result, RuntimeError, RustObject, Value,
};

pub const FUNCTIONS: [(&str, ExternalFn); 1] = [("openFileHandle", open_file_handle)];
pub const METHODS: [(&str, &str, ExternalFn); 2] =
    [("File", "size", file_size), ("File", "read", file_read)];

fn map_io_err(e: io::Error) -> RuntimeError {
    RuntimeError::new(format!("{}", e)).with_kind("IOError".to_string())
}

pub fn open_file_handle(input: Input<'_>) -> Result<Option<Value>> {
    let filename: String = input.single()?;
    let file = File::open(filename).map_err(map_io_err)?;

    Ok(Some(Value::RustObject(RustObject::new(file))))
}

pub fn file_size(mut input: Input<'_>) -> Result<Option<Value>> {
    let object: AtomRef<Object> = input.take_receiver()?;
    let fd = object
        .get_field(0)
        .ok_or_else(|| RuntimeError::new("missing 'fd' field for std.io.File".to_string()))?;
    let rust_object: &RustObject = fd.convert()?;
    let file: &File = rust_object.try_as_ref()?;
    let meta = file.metadata().map_err(map_io_err)?;

    Ok(Some(Value::Int(Int::Uint64(meta.len()))))
}

pub fn file_read(mut input: Input<'_>) -> Result<Option<Value>> {
    let mut object: AtomRef<Object> = input.take_receiver()?;
    let fd = object
        .as_mut()
        .get_field_mut(0)
        .ok_or_else(|| RuntimeError::new("missing 'fd' field for std.io.File".to_string()))?;
    let rust_object: &mut RustObject = fd.convert()?;
    let file: &mut File = rust_object.try_as_mut()?;

    let buff_size: usize = input.single()?;
    let mut buffer = vec![0; buff_size];

    let max_size = file.read(&mut buffer).map_err(map_io_err)?;

    Ok(Some(Value::Array(AtomRef::new(
        buffer.into_iter().map(Value::Byte).take(max_size).collect(),
    ))))
}
