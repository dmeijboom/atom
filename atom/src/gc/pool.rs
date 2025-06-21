use std::{
    alloc::Layout,
    marker::PhantomData,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use super::{allocator, Trace};

pub struct RefCount<T> {
    inner: T,
    count: usize,
}

impl<T> RefCount<T> {
    fn new(inner: T) -> Self {
        Self { inner, count: 1 }
    }

    #[inline(always)]
    pub fn inc_ref_count(&mut self) {
        self.count += 1;
    }

    #[inline(always)]
    pub fn dec_ref_count(&mut self) {
        debug_assert!(self.count > 0, "decrementing reference count below zero");
        self.count -= 1;
    }

    #[inline]
    pub fn ref_count(&self) -> usize {
        self.count
    }
}

pub enum PoolObjectRef<T> {
    Inline(T),
    Rc(ManuallyDrop<PoolObject<T>>),
}

impl<T> PoolObjectRef<T> {
    pub fn as_object_ref(&self) -> Option<&PoolObject<T>> {
        match self {
            Self::Inline(_) => None,
            Self::Rc(object) => Some(object),
        }
    }
}

impl<T> Deref for PoolObjectRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Inline(inner) => inner,
            Self::Rc(object) => object.deref(),
        }
    }
}

pub struct PoolObject<T> {
    inner: NonNull<RefCount<T>>,
    _phantom: PhantomData<RefCount<T>>,
}

impl<T> PoolObject<T> {
    pub fn new(inner: NonNull<RefCount<T>>) -> Self {
        Self {
            inner,
            _phantom: PhantomData,
        }
    }

    pub fn into_raw(self) -> *mut u8 {
        ManuallyDrop::new(self).inner.as_ptr().cast()
    }

    pub fn from_raw(ptr: *mut u8) -> Self {
        NonNull::new(ptr)
            .map(|ptr| Self {
                inner: ptr.cast(),
                _phantom: PhantomData,
            })
            .expect("invalid ptr")
    }

    pub fn ref_count(&self) -> usize {
        unsafe { self.inner.as_ref().ref_count() }
    }
}

impl<T> Deref for PoolObject<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.inner.as_ref().inner }
    }
}

impl<T> DerefMut for PoolObject<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.inner.as_mut().inner }
    }
}

impl<T> Trace for PoolObject<T> {
    fn trace(&self, _gc: &mut super::Gc<'_>) {}
}

impl<T> Clone for PoolObject<T> {
    fn clone(&self) -> Self {
        let rc = unsafe { &mut *self.inner.as_ptr() };
        rc.inc_ref_count();

        Self {
            inner: self.inner,
            _phantom: PhantomData,
        }
    }
}

impl<T> Drop for PoolObject<T> {
    fn drop(&mut self) {
        unsafe {
            self.inner.as_mut().dec_ref_count();
        }
    }
}

pub struct Pool<T: Default> {
    layout: Layout,
    objects: Vec<*mut u8>,
    _phantom: PhantomData<T>,
}

impl<T: Default> Drop for Pool<T> {
    // @TODO: this doesn't drop the inner data
    fn drop(&mut self) {
        for ptr in self.objects.iter() {
            unsafe {
                allocator::dealloc(*ptr);
            }
        }
    }
}

impl<T: Default> Default for Pool<T> {
    fn default() -> Self {
        Self {
            layout: Layout::new::<RefCount<T>>(),
            objects: vec![],
            _phantom: PhantomData,
        }
    }
}

impl<T: Default> Pool<T> {
    fn reclaimable(&self) -> impl Iterator<Item = *mut u8> + '_ {
        self.objects.iter().copied().filter(|ptr| unsafe {
            let rc = *ptr as *const RefCount<T>;
            (*rc).count == 0
        })
    }

    pub fn acquire(&mut self) -> PoolObject<T> {
        if let Some(ptr) = self.reclaimable().next() {
            let rc = unsafe {
                let mut rc: NonNull<RefCount<T>> = NonNull::new_unchecked(ptr.cast());
                rc.as_mut().count = 1;
                rc
            };

            return PoolObject::new(rc);
        }

        let rc = RefCount::new(T::default());
        let ptr = unsafe { allocator::alloc(self.layout, rc) };

        self.objects.push(ptr.cast());

        unsafe { PoolObject::new(NonNull::new_unchecked(ptr)) }
    }
}
