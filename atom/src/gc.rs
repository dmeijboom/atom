use std::{
    alloc::{dealloc, Layout},
    ptr::NonNull,
};

use crate::{
    lexer::Span,
    runtime::error::{RuntimeError, ErrorKind},
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
    fn marked(&self) -> bool;
    fn trace(&self, gc: &mut Gc);
    fn set_marked(&mut self, marked: bool);
}

pub struct Ptr<T: ?Sized> {
    marked: bool,
    data: T,
}

impl<T> Ptr<T> {
    pub fn new(data: T) -> Self {
        Self {
            marked: false,
            data,
        }
    }
}

#[derive(Copy)]
pub struct Handle<T: ?Sized + Trace> {
    ptr: NonNull<Ptr<T>>,
}

impl<T: Trace> Handle<T> {
    pub fn new(ptr: NonNull<Ptr<T>>) -> Self {
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
        NonNull::new(addr as *mut Ptr<T>).map(|ptr| Handle { ptr })
    }
}

impl<T: Trace> AnyHandle for Handle<T> {
    fn marked(&self) -> bool {
        unsafe { self.ptr.as_ref().marked }
    }

    fn set_marked(&mut self, marked: bool) {
        unsafe {
            self.ptr.as_mut().marked = marked;
        }
    }

    fn trace(&self, gc: &mut Gc) {
        unsafe {
            self.ptr.as_ref().data.trace(gc);
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
    cycle_allocated: usize,
    roots: Vec<Box<dyn AnyHandle>>,
}

impl Gc {
    pub fn should_run(&self) -> bool {
        self.cycle_allocated >= 1_000_000
    }

    pub fn alloc<T: Trace + 'static>(&mut self, data: T) -> Result<Handle<T>, RuntimeError> {
        let layout = Layout::new::<Ptr<T>>();
        self.cycle_allocated += layout.size();

        unsafe {
            let ptr = alloc::<Ptr<T>>(layout)?;
            ptr.write(Ptr::new(data));

            let handle = Handle::new(NonNull::new_unchecked(ptr));
            self.roots.push(Box::new(handle.clone()));

            Ok(handle)
        }
    }

    pub fn mark<H: ?Sized + AnyHandle>(&mut self, mut handle: impl AsMut<H>) {
        let handle = handle.as_mut();
        handle.set_marked(true);
        handle.trace(self);
    }

    pub fn get<T: Trace>(&self, handle: Handle<T>) -> &T {
        unsafe { &handle.ptr.as_ref().data }
    }

    pub fn get_mut<T: Trace>(&mut self, mut handle: Handle<T>) -> &mut T {
        unsafe { &mut handle.ptr.as_mut().data }
    }

    pub fn sweep(&mut self) {
        self.cycle_allocated = 0;
        self.roots.retain(|root| {
            if !root.marked() {
                unsafe {
                    dealloc(root.as_ptr(), root.layout());
                }

                return false;
            }

            true
        });

        self.roots
            .iter_mut()
            .for_each(|root| root.set_marked(false));
    }
}
