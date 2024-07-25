use std::{alloc::Layout, marker::PhantomData, mem::MaybeUninit};

use atom_macros::atom_method;

use crate::{
    gc::{alloc, Gc, Trace},
    lexer::Span,
    runtime::{
        error::{RuntimeError, ErrorKind},
        value::{Type, Value},
    },
};

use super::{Context, TypeDescr};

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
        self.array.get(self.idx).map(|item| {
            self.idx += 1;
            item
        })
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

    pub unsafe fn from_raw_parts(data: *mut T, len: usize, cap: usize) -> Self {
        Self {
            data: MaybeUninit::new(data),
            len,
            cap,
            _marker: PhantomData,
        }
    }

    pub fn as_slice(&self) -> &[T] {
        if self.len == 0 {
            return &[];
        }

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

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        if self.len <= idx {
            return None;
        }

        unsafe {
            let ptr = self.data.assume_init_mut().add(idx);
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

unsafe fn alloc_array<T>(cap: usize) -> Result<*mut T, RuntimeError> {
    Layout::array::<T>(cap)
        .map_err(|_| ErrorKind::InvalidMemoryLayout.at(Span::default()))
        .and_then(alloc)
}

impl<T> From<Vec<T>> for Array<T> {
    fn from(data: Vec<T>) -> Self {
        if data.is_empty() {
            return Array::default();
        }

        let slice = data.into_boxed_slice();
        let len = slice.len();

        unsafe {
            let ptr = Box::into_raw(slice) as *mut T;
            Array::from_raw_parts(ptr, len, len)
        }
    }
}

#[atom_method(Array.pop)]
fn array_pop(ctx: Context<'_>, this: &mut Array<Value>) -> Result<Value, RuntimeError> {
    if this.len == 0 {
        return Err(ErrorKind::IndexOutOfBounds(0).at(ctx.span));
    }

    let item = unsafe { this.data.assume_init_ref().add(this.len - 1).read() };
    this.len -= 1;

    Ok(item)
}

#[atom_method(Array.push)]
fn array_push(ctx: Context<'_>, this: &mut Array<Value>, item: Value) -> Result<(), RuntimeError> {
    unsafe {
        if this.len == 0 {
            let ptr = alloc_array::<Value>(1)?;
            ptr.write(item);
            *this = Array::from_raw_parts(ptr, 1, 1);
        } else if this.cap > this.len {
            this.data.assume_init_ref().add(this.len).write(item);
            this.len += 1;
        } else {
            let ptr = alloc_array::<Value>(this.cap * 2)?;

            for (i, item) in this.iter().copied().enumerate() {
                ptr.add(i).write(item);
            }

            ptr.add(this.len).write(item);
            *this = Array::from_raw_parts(ptr, this.len + 1, this.cap * 2);
        }
    }

    Ok(())
}

#[atom_method(Array.len)]
fn array_len(ctx: Context<'_>, this: &Array<Value>) -> Result<usize, RuntimeError> {
    Ok(this.len)
}

#[atom_method(Array.cap)]
fn array_cap(ctx: Context<'_>, this: &Array<Value>) -> Result<usize, RuntimeError> {
    Ok(this.cap)
}

pub fn descr() -> TypeDescr {
    TypeDescr::new(Type::Array)
        .builder()
        .method(array_push)
        .method(array_pop)
        .method(array_len)
        .method(array_cap)
        .build()
}

#[cfg(test)]
mod tests {
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
            __atom_export_array_push(
                Context::new(&mut gc),
                vec![handle.clone().into(), (10 * i).into()],
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

        let array = Array::from(vec![1, 2, 3]);
        assert_eq!(array.as_slice(), &[1, 2, 3]);
    }
}
