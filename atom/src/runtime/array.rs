use std::{alloc::Layout, marker::PhantomData, mem::MaybeUninit, ptr::NonNull};

use atom_macros::export;

use crate::{
    gc::{Gc, Handle, Trace},
    runtime::{
        error::{ErrorKind, RuntimeError},
        value::Value,
    },
};

use super::{Context, Lib};

pub struct Iter<'a, T: Trace> {
    idx: usize,
    array: &'a Array<T>,
}

impl<'a, T: Trace> Iter<'a, T> {
    pub fn new(array: &'a Array<T>) -> Self {
        Self { idx: 0, array }
    }
}

impl<'a, T: Trace> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.array.get(self.idx).map(|item| {
            self.idx += 1;
            item
        })
    }
}

pub struct Array<T: Trace> {
    data: MaybeUninit<Handle<T>>,
    len: usize,
    cap: usize,
    _marker: PhantomData<[T]>,
}

impl<T: Trace> Default for Array<T> {
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

impl<T: Trace> Array<T> {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn slice(&self, from: usize, to: usize) -> Array<T> {
        if self.len == 0 {
            return Array::default();
        }

        unsafe {
            let ptr = self.data.assume_init_ref().as_ptr().add(from);
            let handle = Handle::new(NonNull::new_unchecked(ptr));

            Array {
                data: MaybeUninit::new(handle),
                cap: self.cap - from,
                len: to - from,
                _marker: PhantomData,
            }
        }
    }

    pub unsafe fn from_raw_parts(handle: Handle<T>, len: usize, cap: usize) -> Self {
        Self {
            data: MaybeUninit::new(handle),
            len,
            cap,
            _marker: PhantomData,
        }
    }

    pub fn as_slice(&self) -> &[T] {
        if self.len == 0 {
            return &[];
        }

        unsafe {
            std::slice::from_raw_parts(self.data.assume_init_ref().as_ptr().cast_const(), self.len)
        }
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx >= self.len {
            return None;
        }

        unsafe {
            let ptr = self.data.assume_init_ref().as_ptr().add(idx);
            Some(&*ptr)
        }
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        if idx >= self.len {
            return None;
        }

        unsafe {
            let ptr = self.data.assume_init_mut().as_ptr().add(idx);
            Some(&mut *ptr)
        }
    }

    pub fn from_vec(gc: &mut Gc, data: Vec<T>) -> Self
    where
        T: 'static,
    {
        if data.is_empty() {
            return Array::default();
        }

        let layout = Layout::array::<T>(data.len()).unwrap();
        let slice = data.into_boxed_slice();
        let len = slice.len();

        unsafe {
            let ptr = Box::into_raw(slice) as *mut T;
            let handle = gc.track(NonNull::new_unchecked(ptr), layout);
            Array::from_raw_parts(handle, len, len)
        }
    }

    pub fn concat(&self, other: &Array<T>) -> Vec<T>
    where
        T: Copy,
    {
        self.iter().chain(other.iter()).copied().collect::<Vec<_>>()
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter::new(self)
    }
}

#[export]
fn array_pop(ctx: Context<'_>, this: Handle<Array<Value>>) -> Result<Value, RuntimeError> {
    let array = ctx.gc.get_mut(this);

    if array.len == 0 {
        return Err(ErrorKind::IndexOutOfBounds(0).at(ctx.span));
    }

    let item = unsafe {
        array
            .data
            .assume_init_ref()
            .as_ptr()
            .add(array.len - 1)
            .read()
    };

    array.len -= 1;

    Ok(item)
}

#[export]
fn array_push(
    ctx: Context<'_>,
    this: Handle<Array<Value>>,
    item: Value,
) -> Result<(), RuntimeError> {
    let array = ctx.gc.get(this.clone());
    let (len, cap) = (array.len, array.cap);

    unsafe {
        if len == 0 {
            let new_handle: Handle<Value> = ctx.gc.alloc_array(1)?;
            new_handle.as_ptr().write(item);
            let cur = ctx.gc.get_mut(this);
            *cur = Array::from_raw_parts(new_handle, 1, 1);
        } else if cap > len {
            let array = ctx.gc.get_mut(this);
            array.data.assume_init_ref().as_ptr().add(len).write(item);
            array.len += 1;
        } else {
            let handle: Handle<Value> = ctx.gc.alloc_array(cap * 2)?;
            let ptr = handle.as_ptr();
            let array = ctx.gc.get(this.clone());

            for (i, item) in array.iter().copied().enumerate() {
                ptr.add(i).write(item);
            }

            ptr.add(len).write(item);

            let new_array = Array::from_raw_parts(handle, len + 1, cap * 2);
            let array = ctx.gc.get_mut(this);

            *array = new_array;
        }
    }

    Ok(())
}

#[export]
fn array_len(ctx: Context<'_>, this: Handle<Array<Value>>) -> Result<usize, RuntimeError> {
    Ok(ctx.gc.get(this).len)
}

#[export]
fn array_cap(ctx: Context<'_>, this: Handle<Array<Value>>) -> Result<usize, RuntimeError> {
    Ok(ctx.gc.get(this).cap)
}

pub fn register(lib: Lib) -> Lib {
    lib.class("Array", |lib| {
        lib.set("pop", atom_array_pop)
            .set("push", atom_array_push)
            .set("len", atom_array_len)
            .set("cap", atom_array_cap)
    })
}

#[cfg(test)]
mod tests {
    use crate::runtime::value::Type;

    use super::*;

    #[test]
    fn array_push() {
        let mut gc = Gc::default();
        let array: Array<Value> = Array::default();

        assert_eq!(array.len(), 0);

        let handle = gc.alloc(array).unwrap();
        let expected = [
            (1, 1),
            (2, 2),
            (3, 4),
            (4, 4),
            (5, 8),
            (6, 8),
            (7, 8),
            (8, 8),
            (9, 16),
        ];

        for (i, (len, cap)) in expected.into_iter().enumerate() {
            atom_array_push(
                Context::new(&mut gc),
                vec![(10 * i).into(), handle.clone().into()],
            )
            .expect("Array.push failed");

            let array = gc.get(handle.clone());

            assert_eq!(array.len() as i64, len);
            assert_eq!(array.cap as i64, cap);
        }

        let array = gc.get(handle);

        for (i, item) in array.iter().copied().enumerate() {
            assert_eq!(Type::Int, item.ty());
            assert_eq!(10 * i, item.int() as usize);
        }
    }

    #[test]
    fn array_as_slice() {
        let empty = Array::<bool>::default();
        assert_eq!(empty.as_slice(), &[]);

        let mut gc = Gc::default();
        let array = Array::from_vec(&mut gc, vec![1, 2, 3]);
        assert_eq!(array.as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn array_iter() {
        let mut gc = Gc::default();
        let array = Array::from_vec(&mut gc, vec![1, 2, 3, 4, 5, 6]);
        let items = array.iter().copied().map(|item| item).collect::<Vec<_>>();
        assert_eq!(items, &[1, 2, 3, 4, 5, 6]);
    }
}
