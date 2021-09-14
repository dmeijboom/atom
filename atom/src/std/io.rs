use std::fs::File;
use std::io;
use std::io::Read;

use atom_macros::export;
use atom_runtime::{AtomRef, Extern, ExternalFn, Object, Result, RuntimeError, Value};

fn try_as_file(object: &Object) -> Result<&File> {
    if object.class.origin.module_name == "std.io" && object.class.name == "File" {
        if let Some(Value::Extern(any)) = object.get_field(0) {
            if let Some(file) = any.0.downcast_ref::<File>() {
                return Ok(file);
            }
        }
    }

    Err(
        RuntimeError::new("invalid type, expected: std.io.File".to_string())
            .with_kind("TypeError".to_string()),
    )
}

fn try_as_file_mut(object: &mut Object) -> Result<&mut File> {
    if object.class.origin.module_name == "std.io" && object.class.name == "File" {
        if let Some(Value::Extern(any)) = object.get_field_mut(0) {
            if let Some(file) = any.0.as_mut().downcast_mut::<File>() {
                return Ok(file);
            }
        }
    }

    Err(
        RuntimeError::new("invalid type, expected: std.io.File".to_string())
            .with_kind("TypeError".to_string()),
    )
}

fn map_io_err(e: io::Error) -> RuntimeError {
    RuntimeError::new(format!("{}", e)).with_kind("IOError".to_string())
}

#[export]
fn open_file_handle(filename: String) -> Result<Extern> {
    let file = File::open(filename).map_err(map_io_err)?;

    Ok(Extern(AtomRef::new(Box::new(file))))
}

#[export]
fn file_read(this: &mut Object, max: usize) -> Result<Vec<Value>> {
    let file = try_as_file_mut(this)?;
    let mut buff = vec![];

    buff.reserve(max as usize);

    unsafe {
        buff.set_len(max as usize);
    }

    let bytes_read = file
        .read(&mut buff)
        .map_err(|e| RuntimeError::new(format!("IOError: {}", e)))?;

    Ok(buff.into_iter().take(bytes_read).map(Value::Byte).collect())
}

#[export]
fn file_size(this: &Object) -> Result<i64> {
    let file = try_as_file(this)?;

    file.metadata()
        .map(|meta| meta.len() as i64)
        .map_err(map_io_err)
}

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.io" {
        if name == "File" {
            if let Some(method_name) = method_name {
                return match method_name {
                    "size" => Some(file_size),
                    "read" => Some(file_read),
                    _ => None,
                };
            }
        }

        if name == "openFileHandle" && method_name.is_none() {
            return Some(open_file_handle);
        }
    }

    None

    //module.register_external_method("File", "read", |vm, mut values| {
    //    let max = parse_args!(values => Int);

    //    use_file(vm, |file| {
    //        let mut buff = vec![];

    //        buff.reserve(max as usize);

    //        unsafe {
    //            buff.set_len(max as usize);
    //        }

    //        let bytes_read = file
    //            .read(&mut buff)
    //            .map_err(|e| RuntimeError::new(format!("IOError: {}", e)))?;

    //        let values = buff.into_iter().take(bytes_read).map(Value::Byte).collect();

    //        Ok(Some(Value::Array(AtomRef::new(values))))
    //    })
    //})?;
}
