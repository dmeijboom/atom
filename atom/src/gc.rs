use std::{
    alloc::Layout,
    ffi::c_void,
    fmt::Debug,
    hash::Hash,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use libmimalloc_sys::{mi_free, mi_malloc_aligned, mi_zalloc_aligned};
use nohash_hasher::IntMap;

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

pub trait DynHandle {
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
    pub fn addr(&self) -> usize {
        self.ptr.as_ptr() as usize
    }

    pub fn from_addr(addr: usize) -> Option<Self> {
        NonNull::new(addr as *mut T).map(|ptr| Handle { ptr })
    }

    pub unsafe fn as_ptr(&self) -> *mut T {
        self.ptr.as_ptr()
    }
}

impl<T: Trace> Deref for Handle<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: Trace> DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T: Trace> DynHandle for Handle<T> {
    fn trace(&self, gc: &mut Gc) {
        unsafe {
            self.ptr.as_ref().trace(gc);
        }
    }

    fn as_ptr(&self) -> *mut u8 {
        self.ptr.as_ptr().cast()
    }
}

#[derive(Default)]
struct Cycle {
    allocated: usize,
}

struct Root {
    ptr: *mut u8,
}

impl Root {
    fn new(ptr: *mut u8) -> Self {
        Self { ptr }
    }
}

const CYCLE_TRESHOLD: usize = 1_000_000;

#[derive(Default)]
struct Generation {
    allocated: usize,
    // For some reason a Box<Root> is faster than a Root directly in the lifetime of the GC
    roots: Vec<Box<Root>>,
}

impl Generation {
    fn sweep(&mut self, marked: &IntMap<usize, ()>) {
        self.roots.retain(|root| {
            if marked.contains_key(&(root.ptr as usize)) {
                return true;
            }

            unsafe {
                mi_free(root.ptr as *mut c_void);
            }

            false
        });
    }
}

#[derive(Default)]
pub struct Gc {
    ready: bool,
    cycle: Cycle,
    disabled: bool,
    gen: Generation,
    marked: IntMap<usize, ()>,
}

impl Gc {
    pub fn ready(&self) -> bool {
        self.ready
    }

    #[allow(dead_code)]
    pub fn disable(&mut self) {
        self.disabled = true;
    }

    pub fn track<T: Trace + 'static>(&mut self, ptr: NonNull<T>, layout: Layout) -> Handle<T> {
        let handle = Handle::new(ptr);

        unsafe {
            self.gen
                .roots
                .push(Box::new(Root::new(handle.as_ptr() as *mut u8)));
        }

        self.gen.allocated += layout.size();
        self.cycle.allocated += layout.size();
        self.ready = self.cycle.allocated >= CYCLE_TRESHOLD;

        handle
    }

    pub fn alloc<T: Trace + 'static>(&mut self, data: T) -> Result<Handle<T>, RuntimeError> {
        let layout = Layout::new::<T>();

        unsafe {
            let ptr = mi_malloc_aligned(layout.size(), layout.align()).cast::<T>();
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
            let ptr = mi_zalloc_aligned(layout.size(), layout.align()) as *mut T;
            Ok(self.track(NonNull::new_unchecked(ptr), layout))
        }
    }

    pub fn mark(&mut self, handle: &impl DynHandle) {
        self.marked.insert(handle.as_ptr() as usize, ());
        handle.trace(self);
    }

    pub fn sweep(&mut self) {
        if self.disabled {
            return;
        }

        self.gen.sweep(&self.marked);
        self.cycle.allocated = 0;
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
