use std::{marker::PhantomData, mem::MaybeUninit};

use atom_macros::atom_method;

use crate::{
    gc::{Gc, Trace},
    lexer::Span,
    runtime::{
        error::{Error, ErrorKind},
        value::{Type, Value},
    },
};

use super::TypeDescr;

pub struct Iter<'a, T> {
    idx: usize,
    array: &'a Array<T>,
}

impl<'a, T> Iter<'a, T> {
    pub fn new(array: &'a Array<T>) -> Self {
        Self { idx: 0, array }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

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

pub struct Array<T> {
    data: MaybeUninit<*mut T>,
    len: usize,
    cap: usize,
    _marker: PhantomData<[T]>,
}

impl<T> Drop for Array<T> {
    fn drop(&mut self) {
        if self.len == 0 {
            return;
        }

        unsafe {
            self.data.assume_init_drop();
        }
    }
}

impl<T> Default for Array<T> {
    fn default() -> Self {
        Self {
            data: MaybeUninit::uninit(),
            len: 0,
            cap: 0,
            _marker: PhantomData,
        }
    }
}

impl<T: Trace> Trace for Array<T> {
    fn trace(&self, gc: &mut Gc) {
        for item in self.iter() {
            item.trace(gc);
        }
    }
}

impl<T> Array<T> {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn as_slice(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.data.assume_init_ref().cast_const(), self.len) }
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        if self.len <= idx {
            return None;
        }

        unsafe {
            let ptr = self.data.assume_init_ref().add(idx);
            Some(&*ptr)
        }
    }

    pub fn get_mut(&self, idx: usize) -> Option<&mut T> {
        if self.len <= idx {
            return None;
        }

        unsafe {
            let ptr = self.data.assume_init_ref().add(idx);
            Some(&mut *ptr)
        }
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter::new(self)
    }
}

impl<T: Copy> Array<T> {
    pub fn concat(&self, other: &Array<T>) -> Array<T> {
        Self::from(self.iter().chain(other.iter()).copied().collect::<Vec<_>>())
    }
}

impl<T> From<Vec<T>> for Array<T> {
    fn from(data: Vec<T>) -> Self {
        if data.is_empty() {
            return Array::default();
        }

        let slice = data.into_boxed_slice();
        let len = slice.len();
        let raw = Box::into_raw(slice);

        Array {
            len,
            cap: len,
            data: MaybeUninit::new(raw as *mut T),
            _marker: PhantomData,
        }
    }
}

#[atom_method(Array.pop)]
fn array_pop(gc: &mut Gc, this: Value) -> Result<Value, Error> {
    let array = gc.get_mut(this.array());

    if array.len == 0 {
        return Err(ErrorKind::IndexOutOfBounds(0).at(Span::default()));
    }

    let item = unsafe { array.data.assume_init_ref().add(array.len - 1).read() };

    array.len -= 1;

    Ok(item)
}

#[atom_method(Array.push)]
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
                new_data[i] = *item;
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

#[atom_method(Array.len)]
fn array_len(gc: &mut Gc, this: Value) -> Result<Value, Error> {
    let array = gc.get(this.array());
    Ok(array.len)
}

pub fn descr() -> TypeDescr {
    TypeDescr::new(Type::Array)
        .builder()
        .method(array_push)
        .method(array_pop)
        .method(array_len)
        .build()
}
