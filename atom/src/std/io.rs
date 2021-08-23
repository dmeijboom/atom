use atom_runtime::Result;

use crate::vm::Module;

//fn use_file(
//    vm: &mut VM,
//    handler: impl FnOnce(&mut File) -> Result<Option<Value>>,
//) -> Result<Option<Value>> {
//    let value = vm.get_fn_self_mut()?;
//
//    if let Value::Object(object) = value {
//        if let Some(Data::File(file)) = &object.as_ref().data {
//            return handler(&mut Rc::clone(file).borrow_mut());
//        }
//    }
//
//    Err(RuntimeError::new(format!(
//        "invalid type '{}', expected File",
//        value.get_type().name(),
//    )))
//}

pub fn register(_: &mut Module) -> Result<()> {
    //module.register_external_fn("openFile", |vm, mut values| {
    //    let filename = parse_args!(values => String);
    //    let file = File::open(filename.as_ref())
    //        .map_err(|e| RuntimeError::new(format!("IOError: {}", e)))?;
    //    let object = Object::new(vm.get_type_id("std.io", "File")?, vec![])
    //        .with_data(Data::File(Rc::new(RefCell::new(file))));

    //    Ok(Some(Value::Object(AtomRef::new(object))))
    //});

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

    Ok(())
}
