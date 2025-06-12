use std::{alloc::Layout, marker::PhantomData, mem::MaybeUninit, ptr::NonNull};

use crate::{
    gc::{Gc, Handle, Trace},
    runtime::error::RuntimeError,
};

pub struct Iter<'gc, 'a, T: Trace> {
    idx: usize,
    array: &'a Array<'gc, T>,
}

impl<'gc, 'a, T: Trace> Iter<'gc, 'a, T> {
    pub fn new(array: &'a Array<'gc, T>) -> Self {
        Self { idx: 0, array }
    }
}

impl<'gc, 'a, T: Trace> Iterator for Iter<'gc, 'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.array.get(self.idx).inspect(|_| {
            self.idx += 1;
        })
    }
}

#[repr(C)]
pub struct Array<'gc, T: Trace> {
    pub len: usize,
    pub cap: usize,
    data: MaybeUninit<Handle<'gc, T>>,
    _marker: PhantomData<[T]>,
}

impl<'gc, T: Trace> Default for Array<'gc, T> {
    fn default() -> Self {
        Self {
            len: 0,
            cap: 0,
            data: MaybeUninit::uninit(),
            _marker: PhantomData,
        }
    }
}

impl<'gc, T: Trace> Trace for Array<'gc, T> {
    fn trace(&self, gc: &mut Gc) {
        if self.is_empty() {
            return;
        }

        for item in self.iter() {
            item.trace(gc);
        }

        unsafe { gc.mark(self.data.assume_init_ref()) }
    }
}

impl<'gc, T: Trace> Array<'gc, T> {
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn slice(&self, from: usize, to: usize) -> Array<'gc, T> {
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

    pub fn addr(&self) -> Option<usize> {
        if self.len == 0 {
            return None;
        }

        unsafe { Some(self.data.assume_init_ref().addr()) }
    }

    /// # Safety
    ///
    /// The caller must ensure that the handle is valid.
    pub unsafe fn from_raw_parts(handle: Handle<'gc, T>, len: usize, cap: usize) -> Self {
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

    pub fn from_vec(gc: &mut Gc<'gc>, data: Vec<T>) -> Self {
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

    pub fn push(&mut self, gc: &mut Gc<'gc>, item: T) -> Result<(), RuntimeError>
    where
        T: Copy,
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

    pub fn iter(&self) -> Iter<'gc, '_, T> {
        Iter::new(self)
    }
}

#[cfg(test)]
mod tests {
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
