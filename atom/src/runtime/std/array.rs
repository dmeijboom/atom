use atom_macros::atom_method;

use crate::{
    gc::Gc,
    runtime::{
        error::Error,
        value::{Type, Value},
    },
};

use super::TypeDescr;

#[atom_method(push)]
fn array_push(gc: &mut Gc, array: Value, item: Value) -> Result<(), Error> {
    let vec = gc.get_mut(array.array());
    vec.push(item);

    Ok(())
}

#[atom_method(len)]
fn array_len(gc: &mut Gc, array: Value) -> Result<Value, Error> {
    let vec = gc.get(array.array());
    Ok(vec.len())
}

pub fn descr() -> TypeDescr {
    TypeDescr::new(Type::Array)
        .builder()
        .method(array_push)
        .method(array_len)
        .build()
}
