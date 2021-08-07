use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

use crate::parse_args;
use crate::runtime::{Data, Object, Result, RuntimeError, TypeId, Value};
use crate::vm::{Module, VM};

fn use_file(
    vm: &VM,
    handler: impl FnOnce(&mut File) -> Result<Option<Value>>,
) -> Result<Option<Value>> {
    let value = vm.get_local("this").unwrap();

    if let Value::Object(object) = &value {
        let mut object = object.borrow_mut();

        if let Some(Data::File(file)) = object.data.get_mut(0) {
            return handler(file);
        }
    }

    Err(RuntimeError::new(format!(
        "invalid type '{}', expected File",
        value.get_type().name()
    )))
}

pub fn register(module: &mut Module) -> Result<()> {
    module.register_external_fn("openFile", |_, mut values| {
        let filename = parse_args!(values => String);
        let file =
            File::open(filename).map_err(|e| RuntimeError::new(format!("IOError: {}", e)))?;
        let object = Object {
            class: TypeId {
                name: "File".to_string(),
                module: "std.io".to_string(),
            },
            fields: vec![],
            data: vec![Data::File(file)],
        };

        Ok(Some(Value::Object(Rc::new(RefCell::new(object)))))
    });

    module.register_external_method("File", "read", |vm, mut values| {
        let max = parse_args!(values => Int);

        use_file(vm, |file| {
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

            Ok(Some(Value::Array(Rc::new(RefCell::new(values)))))
        })
    })?;

    Ok(())
}
