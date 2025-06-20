use std::{
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use super::{Gc, Trace};

pub trait DynHandle {
    fn as_ptr(&self) -> *mut u8;
    fn trace(&self, gc: &mut Gc);
}

#[derive(Copy)]
pub struct Handle<'gc, T: ?Sized + Trace> {
    ptr: NonNull<T>,
    _phantom: PhantomData<&'gc T>,
}

impl<'gc, T: Trace + Hash> Hash for Handle<'gc, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<'gc, T: ?Sized + Trace> Handle<'gc, T> {
    pub fn new(ptr: NonNull<T>) -> Self {
        Self {
            ptr,
            _phantom: PhantomData,
        }
    }
}

impl<'gc, T: Trace + PartialEq> PartialEq for Handle<'gc, T> {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl<'gc, T: Trace + Eq> Eq for Handle<'gc, T> {}

impl<'gc, T: Trace + Debug> Debug for Handle<'gc, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<'gc, T: Trace> Clone for Handle<'gc, T> {
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr,
            _phantom: PhantomData,
        }
    }
}

impl<'gc, T: Trace> Handle<'gc, T> {
    pub fn addr(&self) -> usize {
        self.ptr.as_ptr() as usize
    }

    pub fn from_ptr(ptr: *mut T) -> Self {
        NonNull::new(ptr)
            .map(|ptr| Handle {
                ptr,
                _phantom: PhantomData,
            })
            .expect("invalid ptr")
    }

    pub fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }
}

impl<'gc, T: Trace> Deref for Handle<'gc, T> {
    type Target = T;

    fn deref(&self) -> &'gc T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<'gc, T: Trace> DerefMut for Handle<'gc, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }
}

impl<'gc, T: Trace> DynHandle for Handle<'gc, T> {
    fn trace(&self, gc: &mut Gc) {
        unsafe {
            self.ptr.as_ref().trace(gc);
        }
    }

    fn as_ptr(&self) -> *mut u8 {
        self.ptr.as_ptr().cast()
    }
}
