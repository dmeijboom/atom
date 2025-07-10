/// Contains generic functions for working with ArrayLike types in the VM
use crate::runtime::{errors::ErrorKind, ArrayLike, Gc, Handle, Trace, Value};

use super::Error;

fn resolve_index(elem: Value, len: usize) -> usize {
    let i = elem.as_bigint();

    match i.as_i64() {
        n if n < 0 => (len as i64 + n) as usize,
        _ => i.as_usize(),
    }
}

pub fn concat<'gc, A, T>(gc: &mut Gc<'gc>, lhs: &A, rhs: &A) -> Result<Value<'gc>, Error>
where
    A: ArrayLike<'gc, Item = T> + Trace + 'gc,
    T: Clone + Trace,
    Value<'gc>: From<Handle<'gc, A>>,
{
    let new_array = lhs.concat(gc, rhs)?;
    Ok(gc.alloc(new_array)?.into())
}

pub fn slice<'gc, T, A>(
    gc: &mut Gc<'gc>,
    array: &A,
    from: Option<Value<'gc>>,
    to: Option<Value<'gc>>,
) -> Result<Value<'gc>, Error>
where
    A: ArrayLike<'gc, Item = T> + Default + Trace + 'gc,
    T: Trace,
    Value<'gc>: From<Handle<'gc, A>>,
{
    let from = from.map(|v| resolve_index(v, array.len())).unwrap_or(0);
    let mut to = to
        .map(|v| resolve_index(v, array.len()))
        .unwrap_or(array.len());

    if from > array.len() {
        return Ok(gc.alloc(A::default())?.into());
    }

    if to > array.len() {
        to = array.len();
    }

    Ok(gc.alloc(array.slice(from, to))?.into())
}

pub fn get_elem<'gc, A, T>(array: &A, elem: Value<'gc>) -> Result<Value<'gc>, ErrorKind>
where
    A: ArrayLike<'gc, Item = T>,
    T: Clone + Trace,
    Value<'gc>: From<T>,
{
    let idx = resolve_index(elem, array.len());
    let value = array
        .get(idx)
        .cloned()
        .ok_or(ErrorKind::IndexOutOfBounds(idx))?;

    Ok(value.into())
}

pub fn set_elem<'gc, A, T>(
    array: &mut A,
    elem: Value<'gc>,
    value: Value<'gc>,
) -> Result<(), ErrorKind>
where
    A: ArrayLike<'gc, Item = T>,
    T: Trace + From<Value<'gc>>,
{
    let idx = resolve_index(elem, array.len());
    let elem = array.get_mut(idx).ok_or(ErrorKind::IndexOutOfBounds(idx))?;

    *elem = value.into();

    Ok(())
}
