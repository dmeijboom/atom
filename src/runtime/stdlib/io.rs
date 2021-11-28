use std::fs::File;
use std::io;
use std::io::Read;

use crate::runtime::{
    AtomRef, AtomRefMut, AtomString, Convert, ErrorKind, ExternalFn, Input, Int, Object, Output,
    Result, RuntimeError, RustObject, Value,
};

pub const FUNCTIONS: [(&str, ExternalFn); 1] = [("openFileHandle", open_file_handle)];
pub const METHODS: [(&str, &str, ExternalFn); 2] =
    [("File", "size", file_size), ("File", "read", file_read)];

fn map_io_err(e: io::Error) -> RuntimeError {
    RuntimeError::new(ErrorKind::IOError, format!("{}", e))
}

pub fn open_file_handle(input: Input<'_>) -> Result<Output> {
    let filename: AtomString = input.single()?;
    let file = File::open(filename.as_str()).map_err(map_io_err)?;

    Ok(Output::new(RustObject::new(file)))
}

pub fn file_size(mut input: Input<'_>) -> Result<Output> {
    let object: AtomRefMut<Object> = input.take_receiver()?;
    let fd = object.fields.get(0).ok_or_else(|| {
        RuntimeError::new(
            ErrorKind::FatalError,
            "missing 'fd' field for std.io.File".to_string(),
        )
    })?;
    let rust_object: &RustObject = fd.convert()?;
    let file: &File = rust_object.try_as_ref()?;
    let meta = file.metadata().map_err(map_io_err)?;

    Ok(Output::new(Int::from(meta.len())))
}

pub fn file_read(mut input: Input<'_>) -> Result<Output> {
    let mut object: AtomRefMut<Object> = input.take_receiver()?;
    let fd = &mut object.as_mut().fields.as_mut()[0];
    let rust_object: &mut RustObject = fd.convert()?;
    let file: &mut File = rust_object.try_as_mut()?;

    let buff_size: usize = input.single()?;
    let mut buffer = vec![0; buff_size];

    let max_size = file.read(&mut buffer).map_err(map_io_err)?;

    Ok(Output::new(AtomRefMut::new(AtomRef::new(
        buffer
            .into_iter()
            .map(Value::Byte)
            .take(max_size)
            .collect::<Vec<_>>(),
    ))))
}
