use std::{marker::PhantomData, mem::MaybeUninit};

use atom_macros::atom_method;

use crate::{
    gc::{Gc, Trace},
    runtime::{
        error::Error,
        value::{Type, Value},
    },
};

use super::TypeDescr;

pub struct Iter<'a> {
    idx: usize,
    array: &'a Array,
}

impl<'a> Iter<'a> {
    pub fn new(array: &'a Array) -> Self {
        Self { idx: 0, array }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.array.get(self.idx) {
            Some(item) => {
                self.idx += 1;
                Some(item)
            }
            None => None,
        }
    }
}

pub struct Array {
    data: MaybeUninit<*mut Value>,
    len: usize,
    cap: usize,
    _marker: PhantomData<[Value]>,
}

impl Drop for Array {
    fn drop(&mut self) {
        if self.len == 0 {
            return;
        }

        unsafe {
            self.data.assume_init_drop();
        }
    }
}

impl Default for Array {
    fn default() -> Self {
        Self {
            data: MaybeUninit::uninit(),
            len: 0,
            cap: 0,
            _marker: PhantomData,
        }
    }
}

impl Trace for Array {
    fn trace(&self, gc: &mut Gc) {
        for item in self.iter() {
            item.trace(gc);
        }
    }
}

impl Array {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn get(&self, idx: usize) -> Option<Value> {
        if self.len <= idx {
            return None;
        }

        unsafe { Some(self.data.assume_init_ref().add(idx).read()) }
    }

    pub fn iter(&self) -> Iter<'_> {
        Iter::new(self)
    }

    pub fn concat(&self, other: &Array) -> Array {
        Self::from(self.iter().chain(other.iter()).collect::<Vec<_>>())
    }
}

impl From<Vec<Value>> for Array {
    fn from(data: Vec<Value>) -> Self {
        if data.is_empty() {
            return Array::default();
        }

        let slice = data.into_boxed_slice();
        let len = slice.len();
        let raw = Box::into_raw(slice);

        Array {
            len,
            cap: len,
            data: MaybeUninit::new(raw as *mut Value),
            _marker: PhantomData,
        }
    }
}

#[atom_method(push)]
fn array_push(gc: &mut Gc, this: Value, item: Value) -> Result<(), Error> {
    let array = gc.get_mut(this.array());

    unsafe {
        if array.len == 0 {
            let data = Box::new([item]);
            let raw = Box::into_raw(data);

            array.cap = 1;
            array.len = 1;
            array.data = MaybeUninit::new(raw as *mut Value);
        } else if array.cap > array.len {
            array.data.assume_init_ref().add(array.len).write(item);
            array.len += 1;
        } else {
            let cap = array.cap * 2;
            let mut new_data = vec![Value::NIL; cap].into_boxed_slice();

            for (i, item) in array.iter().enumerate() {
                new_data[i] = item;
            }

            new_data[array.len] = item;

            let raw = Box::into_raw(new_data);

            array.cap = cap;
            array.len += 1;
            array.data = MaybeUninit::new(raw as *mut Value);
        }
    }

    Ok(())
}

#[atom_method(len)]
fn array_len(gc: &mut Gc, this: Value) -> Result<Value, Error> {
    let vec = gc.get(this.array());
    Ok(vec.len)
}

pub fn descr() -> TypeDescr {
    TypeDescr::new(Type::Array)
        .builder()
        .method(array_push)
        .method(array_len)
        .build()
}
