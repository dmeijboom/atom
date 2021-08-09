use std::cell::{RefCell, RefMut};
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use smallvec::smallvec;

use crate::parse_args;
use crate::runtime::{Data, Object, Result, RuntimeError, TypeId, Value};
use crate::vm::{Module, VM};

fn use_file(
    vm: &mut VM,
    handler: impl FnOnce(RefMut<File>) -> Result<Option<Value>>,
) -> Result<Option<Value>> {
    let mut value = vm.get_local_mut("this").unwrap();
    let type_val = value.get_type();

    if let Value::Object(object) = &mut *value {
        if let Some(Data::File(file)) = &object.data {
            return handler(Rc::clone(file).borrow_mut());
        }
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected File",
        type_val.name(),
    )))
}

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("openFile", |_, mut values| {
        let filename = parse_args!(values => String);
        let file =
            File::open(filename).map_err(|e| RuntimeError::new(format!("IOError: {}", e)))?;
        let object = Object::new(TypeId::new("std.io", "File"), smallvec![])
            .with_data(Data::File(Rc::new(RefCell::new(file))));

        Ok(Some(Value::Object(object.into())))
    });

    module.register_external_method("File", "read", |vm, mut values| {
        let max = parse_args!(values => Int);

        use_file(vm, |mut file| {
            let mut buff = vec![];

            buff.reserve(max as usize);

            unsafe {
                buff.set_len(max as usize);
            }

            let bytes_read = file
                .read(&mut buff)
                .map_err(|e| RuntimeError::new(format!("IOError: {}", e)))?;

            let values = buff
                .into_iter()
                .take(bytes_read)
                .map(|b| Value::Byte(b))
                .collect();

            Ok(Some(Value::Array(values)))
        })
    })?;

    Ok(())
}
