use std::{
    alloc::{alloc_zeroed, dealloc, Layout},
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use wyhash2::WyHash;

use crate::{
    lexer::Span,
    runtime::error::{ErrorKind, RuntimeError},
};

macro_rules! impl_trace {
    ($($ty:ty),+) => {
        $(impl Trace for $ty {
            fn trace(&self, _gc: &mut Gc) {}
        })+
    };
}

pub trait Trace {
    fn trace(&self, gc: &mut Gc);
}

impl_trace!(u8, u16, u32, u64, i8, i16, i32, i64, usize, bool, f32, f64);

impl<T: Trace> Trace for Vec<T> {
    fn trace(&self, gc: &mut Gc) {
        for item in self.iter() {
            item.trace(gc);
        }
    }
}

pub trait AnyHandle {
    fn layout(&self) -> Layout;
    fn as_ptr(&self) -> *mut u8;
    fn trace(&self, gc: &mut Gc);
}

#[derive(Copy)]
pub struct Handle<T: ?Sized + Trace> {
    ptr: NonNull<T>,
}

impl<T: Trace + Hash> Hash for Handle<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.deref().hash(state);
    }
}

impl<T: ?Sized + Trace> Handle<T> {
    pub fn new(ptr: NonNull<T>) -> Self {
        Self { ptr }
    }
}

impl<T: Trace + PartialEq> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl<T: Trace + Eq> Eq for Handle<T> {}

impl<T: Trace + Debug> Debug for Handle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
}

impl<T: Trace> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self { ptr: self.ptr }
    }
}

impl<T: Trace> Handle<T> {
    #[inline(always)]
    pub fn addr(&self) -> usize {
        self.ptr.as_ptr() as usize
    }

    pub fn from_addr(addr: usize) -> Option<Self> {
        NonNull::new(addr as *mut T).map(|ptr| Handle { ptr })
    }

    #[inline(always)]
    pub unsafe fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }
}

impl<T: Trace + 'static> Handle<T> {
    pub fn boxed(&self) -> Box<dyn AnyHandle> {
        Box::new(self.clone())
    }
}

impl<T: Trace> Deref for Handle<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: Trace> DerefMut for Handle<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T: Trace> AnyHandle for Handle<T> {
    fn trace(&self, gc: &mut Gc) {
        unsafe {
            self.ptr.as_ref().trace(gc);
        }
    }

    fn as_ptr(&self) -> *mut u8 {
        self.ptr.as_ptr().cast()
    }

    fn layout(&self) -> Layout {
        Layout::new::<T>()
    }
}

pub fn alloc<T>(layout: Layout) -> Result<*mut T, RuntimeError> {
    unsafe {
        let ptr = std::alloc::alloc(layout);

        if ptr.is_null() {
            return Err(ErrorKind::OutOfMemory.at(Span::default()));
        }

        Ok(ptr.cast())
    }
}

#[derive(Default)]
struct Cycle {
    allocated: usize,
}

#[derive(Default)]
pub struct Gc {
    ready: bool,
    cycle: Cycle,
    marked: HashSet<usize, WyHash>,
    roots: Vec<Box<dyn AnyHandle>>,
    arrays: HashMap<usize, Layout, WyHash>,
}

impl Gc {
    #[inline(always)]
    pub fn ready(&self) -> bool {
        self.ready
    }

    pub unsafe fn track<T: Trace + 'static>(
        &mut self,
        ptr: NonNull<T>,
        layout: Layout,
    ) -> Handle<T> {
        let handle = Handle::new(ptr);

        self.roots.push(Box::new(handle.clone()));
        self.cycle.allocated += layout.size();

        if !self.ready && self.cycle.allocated >= 1_000_000 {
            self.ready = true;
        }

        if layout.size() != std::mem::size_of::<T>() {
            self.arrays.insert(handle.addr(), layout);
        }

        handle
    }

    pub fn alloc<T: Trace + 'static>(&mut self, data: T) -> Result<Handle<T>, RuntimeError> {
        let layout = Layout::new::<T>();

        unsafe {
            let ptr = alloc::<T>(layout)?;
            ptr.write(data);
            Ok(self.track(NonNull::new_unchecked(ptr), layout))
        }
    }

    pub fn alloc_array<T: Trace + 'static>(
        &mut self,
        cap: usize,
    ) -> Result<Handle<T>, RuntimeError> {
        let layout = Layout::array::<T>(cap)
            .map_err(|_| ErrorKind::InvalidMemoryLayout.at(Span::default()))?;

        unsafe {
            let ptr = alloc_zeroed(layout) as *mut T;
            Ok(self.track(NonNull::new_unchecked(ptr), layout))
        }
    }

    pub fn mark(&mut self, handle: impl Into<Box<dyn AnyHandle>>) {
        let handle = handle.into();
        self.marked.insert(handle.as_ptr() as usize);
    }

    pub fn sweep(&mut self) {
        self.cycle.allocated = 0;
        self.roots.retain(|root| {
            let addr = root.as_ptr() as usize;

            if self.marked.contains(&addr) {
                return true;
            }

            unsafe {
                dealloc(
                    root.as_ptr(),
                    self.arrays.remove(&addr).unwrap_or_else(|| root.layout()),
                );
            }

            false
        });

        self.marked.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_allocated() {
        let mut gc = Gc::default();
        let _handle: Handle<Vec<i64>> = gc.alloc_array(100).unwrap();

        assert_eq!(gc.cycle.allocated, 2400);

        gc.sweep();
    }
}
