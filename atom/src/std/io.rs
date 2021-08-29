use std::fs::File;

use atom_macros::export;
use atom_runtime::{AtomRef, Extern, ExternalFn, Object, Result, RuntimeError, Value};

#[export]
fn file_size(this: &Object) -> Result<i64> {
    if let Some(Value::Extern(any)) = this.get_field(0) {
        if let Some(file) = any.0.downcast_ref::<File>() {
            return file
                .metadata()
                .map(|meta| meta.len() as i64)
                .map_err(|e| RuntimeError::new(format!("{}", e)).with_kind("IOError".to_string()));
        }
    }

    Err(RuntimeError::new(
        "invalid type '{}' expected: std.io.File".to_string(),
    ))
}

#[export]
fn open_file_handle(filename: String) -> Result<Extern> {
    let file = File::open(filename)
        .map_err(|e| RuntimeError::new(format!("{}", e)).with_kind("IOError".to_string()))?;

    Ok(Extern(AtomRef::new(Box::new(file))))
}

pub fn hook(module_name: &str, name: &str, method_name: Option<&str>) -> Option<ExternalFn> {
    if module_name == "std.io" {
        if name == "File" {
            if let Some(method_name) = method_name {
                return match method_name {
                    "size" => Some(file_size),
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
