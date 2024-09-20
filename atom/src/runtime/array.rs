use std::{alloc::Layout, marker::PhantomData, mem::MaybeUninit, ptr::NonNull};

use atom_macros::export;

use crate::{
    gc::{Gc, Handle, Trace},
    runtime::{
        error::{ErrorKind, RuntimeError},
        value::Value,
    },
};

use super::{Atom, Lib};

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
        self.array.get(self.idx).inspect(|_| {
            self.idx += 1;
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

impl<T: Trace + 'static> Trace for Array<T> {
    fn trace(&self, gc: &mut Gc) {
        if self.is_empty() {
            return;
        }

        for item in self.iter() {
            item.trace(gc);
        }

        unsafe { gc.mark(self.data.assume_init_ref().boxed()) }
    }
}

impl<T: Trace> Array<T> {
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn slice(&self, from: usize, to: usize) -> Array<T> {
        if self.is_empty() {
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

    pub fn push(&mut self, gc: &mut Gc, item: T) -> Result<(), RuntimeError>
    where
        T: Copy + 'static,
    {
        let (len, cap) = (self.len, self.cap);

        unsafe {
            if len == 0 {
                let new_handle: Handle<T> = gc.alloc_array(1)?;
                new_handle.as_ptr().write(item);
                *self = Array::from_raw_parts(new_handle, 1, 1);
            } else if cap > len {
                self.data.assume_init_ref().as_ptr().add(len).write(item);
                self.len += 1;
            } else {
                let handle: Handle<T> = gc.alloc_array(cap * 2)?;
                let ptr = handle.as_ptr();

                for (i, item) in self.iter().copied().enumerate() {
                    ptr.add(i).write(item);
                }

                ptr.add(len).write(item);

                let new_array = Array::from_raw_parts(handle, len + 1, cap * 2);
                *self = new_array;
            }
        }

        Ok(())
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }

        let item = unsafe {
            self.data
                .assume_init_ref()
                .as_ptr()
                .add(self.len - 1)
                .read()
        };

        self.len -= 1;

        Some(item)
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter::new(self)
    }
}

#[export]
fn array_pop(atom: Atom<'_>) -> Result<Value, RuntimeError> {
    let mut array = atom.receiver()?.array();
    array
        .pop()
        .map_or_else(|| Err(ErrorKind::IndexOutOfBounds(0).at(atom.span)), Ok)
}

#[export]
fn array_push(atom: Atom<'_>, item: Value) -> Result<(), RuntimeError> {
    let mut array = atom.receiver()?.array();
    array.push(atom.gc(), item)?;

    Ok(())
}

#[export]
fn array_len(atom: Atom<'_>) -> Result<usize, RuntimeError> {
    Ok(atom.receiver()?.array().len)
}

#[export]
fn array_cap(atom: Atom<'_>) -> Result<usize, RuntimeError> {
    Ok(atom.receiver()?.array().cap)
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
    fn array_push_slice() {
        let mut gc = Gc::default();
        let mut array = Array::from_vec(&mut gc, vec![1, 2, 3, 4, 5, 6]);

        assert_eq!(array.as_slice(), &[1, 2, 3, 4, 5, 6]);

        let slice = array.slice(2, 4);

        assert_eq!(slice.as_slice(), &[3, 4]);
        assert_eq!(slice.len(), 2);
        assert_eq!(slice.cap, 4);

        array.push(&mut gc, 7).expect("Array.push failed");

        assert_eq!(array.as_slice(), &[1, 2, 3, 4, 5, 6, 7]);
        assert_eq!(slice.as_slice(), &[3, 4]);
        assert_eq!(slice.len(), 2);
    }

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
                Atom::new(&mut gc).with_receiver(handle.clone().into()),
                vec![(10 * i).try_into().unwrap()],
            )
            .expect("Array.push failed");

            assert_eq!(handle.len() as i64, len);
            assert_eq!(handle.cap as i64, cap);
        }

        for (i, item) in handle.iter().copied().enumerate() {
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
