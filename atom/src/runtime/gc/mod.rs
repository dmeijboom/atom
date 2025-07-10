use std::{alloc::Layout, marker::PhantomData, ptr::NonNull};

mod allocator;
mod handle;

pub use handle::{DynHandle, Handle};

use crate::{
    collections::IntSet,
    frontend::Span,
    runtime::errors::{ErrorKind, RuntimeError},
};

macro_rules! impl_trace {
    ($($ty:ty),+) => {
        $(impl Trace for $ty {
            fn trace(&self, _gc: &mut Gc) {}
        })+
    };
}

pub trait Trace {
    fn trace(&self, gc: &mut Gc<'_>);
}

impl_trace!(u8, u16, u32, u64, i8, i16, i32, i64, usize, bool, f32, f64);

impl<T: Trace> Trace for Vec<T> {
    fn trace(&self, gc: &mut Gc) {
        for item in self.iter() {
            item.trace(gc);
        }
    }
}

struct Root {
    ptr: *mut u8,
    size: usize,
}

impl Root {
    fn new(ptr: *mut u8, size: usize) -> Self {
        Self { ptr, size }
    }
}

const GEN0_TRESHOLD: usize = 256_000;
const GEN1_TRESHOLD: usize = 2_000_000;
const GEN2_TRESHOLD: usize = 10_000_000;

#[derive(Default)]
struct Generation {
    allocated: usize,
    roots: Vec<Root>,
}

impl Generation {
    fn sweep(&mut self, marked: &IntSet<usize>) {
        self.roots.retain(|root| {
            if marked.contains(&(root.ptr as usize)) {
                return true;
            }

            self.allocated -= root.size;
            unsafe { allocator::dealloc(root.ptr) };

            false
        });
    }

    fn promote(&mut self, other: &mut Generation) {
        other.roots.append(&mut self.roots);
        other.allocated += self.allocated;
        self.allocated = 0;
    }
}

#[derive(Default, Clone)]
pub struct GcStats {
    pub alloc_count: usize,
}

#[derive(Default)]
pub struct Gc<'gc> {
    pub ready: bool,
    stats: GcStats,
    disabled: bool,
    gen0: Generation,
    gen1: Generation,
    gen2: Generation,
    marked: IntSet<usize>,
    _phantom: PhantomData<&'gc ()>,
}

impl<'gc> Drop for Gc<'gc> {
    fn drop(&mut self) {
        if self.disabled {
            return;
        }

        let noop = IntSet::default();

        self.gen0.sweep(&noop);
        self.gen1.sweep(&noop);
        self.gen2.sweep(&noop);
    }
}

impl<'gc> Gc<'gc> {
    #[allow(dead_code)]
    pub fn stats(&self) -> GcStats {
        self.stats.clone()
    }

    #[allow(dead_code)]
    pub fn disable(&mut self) {
        self.disabled = true;
    }

    pub fn track<T: Trace>(&mut self, ptr: NonNull<T>, layout: Layout) -> Handle<'gc, T> {
        let handle = Handle::new(ptr);

        self.gen0
            .roots
            .push(Root::new(handle.as_ptr().cast(), layout.size()));

        self.gen0.allocated += layout.size();
        self.ready = self.gen0.allocated >= GEN0_TRESHOLD;
        self.stats.alloc_count += 1;

        handle
    }

    pub fn alloc<T: Trace>(&mut self, data: T) -> Result<Handle<'gc, T>, RuntimeError> {
        let layout = Layout::new::<T>();

        unsafe {
            Ok(self.track(
                NonNull::new_unchecked(allocator::alloc(layout, data)),
                layout,
            ))
        }
    }

    pub fn alloc_array<T: Trace>(&mut self, cap: usize) -> Result<Handle<'gc, T>, RuntimeError> {
        let layout = Layout::array::<T>(cap)
            .map_err(|_| ErrorKind::InvalidMemoryLayout.at(Span::default()))?;

        unsafe { Ok(self.track(NonNull::new_unchecked(allocator::zalloc(layout)), layout)) }
    }

    pub fn mark(&mut self, handle: &impl DynHandle) {
        self.marked.insert(handle.as_ptr() as usize);
        handle.trace(self);
    }

    pub fn sweep(&mut self) {
        if self.disabled {
            return;
        }

        if self.gen0.allocated >= GEN0_TRESHOLD {
            self.gen0.sweep(&self.marked);
            self.gen0.promote(&mut self.gen1);
        }

        if self.gen1.allocated >= GEN1_TRESHOLD {
            self.gen1.sweep(&self.marked);
            self.gen1.promote(&mut self.gen2);
        }

        if self.gen2.allocated >= GEN2_TRESHOLD {
            self.gen2.sweep(&self.marked);
        }

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

        assert_eq!(gc.gen0.allocated, 2400);

        gc.sweep();
    }
}
