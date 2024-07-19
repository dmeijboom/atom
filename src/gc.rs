use std::{mem, ptr::NonNull};

macro_rules! impl_trace {
    ($($ty:ty),+) => {
        $(impl Trace for $ty {
            fn trace(&self, _gc: &mut Gc) {
            }
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

trait AnyHandle {
    fn marked(&self) -> bool;
    fn dealloc(&self);
    fn set_marked(&mut self, marked: bool);
}

struct Ptr<T> {
    marked: bool,
    data: T,
}

#[derive(Copy)]
pub struct Handle<T: Trace> {
    ptr: NonNull<Ptr<T>>,
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

    fn dealloc(&self) {
        unsafe {
            drop(Box::from_raw(self.ptr.as_ptr()));
        }
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

    pub fn alloc<T: Trace + 'static>(&mut self, data: T) -> Handle<T> {
        self.cycle_allocated += mem::size_of::<Ptr<T>>();

        let ptr = Box::into_raw(Box::new(Ptr {
            marked: false,
            data,
        }));

        unsafe {
            let handle = Handle {
                ptr: NonNull::new_unchecked(ptr),
            };

            self.roots.push(Box::new(handle.clone()));

            handle
        }
    }

    pub fn mark<T: Trace>(&mut self, mut handle: Handle<T>) {
        unsafe {
            let ptr = handle.ptr.as_mut();

            ptr.marked = true;
            ptr.data.trace(self);
        }
    }

    pub fn get<T: Trace>(&self, handle: Handle<T>) -> &T {
        unsafe { &handle.ptr.as_ref().data }
    }

    //pub fn get_mut<T: Trace>(&self, mut handle: Handle<T>) -> &mut T {
    //    unsafe { &mut handle.ptr.as_mut().data }
    //}

    pub fn sweep(&mut self) {
        self.cycle_allocated = 0;
        self.roots.retain(|root| {
            if !root.marked() {
                root.dealloc();
                return false;
            }

            true
        });

        self.roots
            .iter_mut()
            .for_each(|root| root.set_marked(false));
    }
}
