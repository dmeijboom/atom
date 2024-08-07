use std::{
    alloc::{alloc_zeroed, dealloc, Layout},
    collections::HashMap,
    ptr::NonNull,
};

use bit_set::BitSet;
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

impl<T: ?Sized + Trace> Handle<T> {
    pub fn new(ptr: NonNull<T>) -> Self {
        Self { ptr }
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
pub struct Gc {
    marked: BitSet<usize>,
    cycle_allocated: usize,
    roots: Vec<Box<dyn AnyHandle>>,
    arrays: HashMap<usize, Layout, WyHash>,
}

impl Gc {
    pub fn should_run(&self) -> bool {
        self.cycle_allocated >= 1_000_000
    }

    pub unsafe fn track<T: Trace + 'static>(
        &mut self,
        ptr: NonNull<T>,
        layout: Layout,
    ) -> Handle<T> {
        let handle = Handle::new(ptr);
        self.roots.push(Box::new(handle.clone()));

        if layout.size() != std::mem::size_of::<T>() {
            self.arrays.insert(handle.addr(), layout);
        }

        handle
    }

    pub fn alloc<T: Trace + 'static>(&mut self, data: T) -> Result<Handle<T>, RuntimeError> {
        let layout = Layout::new::<T>();
        self.cycle_allocated += layout.size();

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
        self.cycle_allocated += layout.size();

        unsafe {
            let ptr = alloc_zeroed(layout) as *mut T;
            Ok(self.track(NonNull::new_unchecked(ptr), layout))
        }
    }

    pub fn mark<H: ?Sized + AnyHandle>(&mut self, mut handle: impl AsMut<H>) {
        let handle = handle.as_mut();
        self.marked.insert(handle.as_ptr() as usize);
        handle.trace(self);
    }

    pub fn get<T: Trace>(&self, handle: Handle<T>) -> &T {
        unsafe { handle.ptr.as_ref() }
    }

    pub fn get_mut<T: Trace>(&mut self, mut handle: Handle<T>) -> &mut T {
        unsafe { handle.ptr.as_mut() }
    }

    pub fn sweep(&mut self) {
        self.cycle_allocated = 0;
        self.roots.retain(|root| {
            let addr = root.as_ptr() as usize;

            if !self.marked.contains(addr) {
                unsafe {
                    let layout = self.arrays.remove(&addr).unwrap_or_else(|| root.layout());
                    dealloc(root.as_ptr(), layout);
                }

                return false;
            }

            true
        });

        self.marked.clear();
    }
}
