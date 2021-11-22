use core::mem;
use std::cell::Cell;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::{self, NonNull};

pub struct AtomRefInner<T: ?Sized> {
    ref_count: Cell<usize>,
    data: T,
}

impl<T: ?Sized> AtomRefInner<T> {
    fn incr_ref_count(&self) -> usize {
        let count = self.ref_count.get();

        self.ref_count.set(count + 1);

        count + 1
    }

    fn decr_ref_count(&self) -> usize {
        let count = self.ref_count.get();

        if count == 0 {
            unreachable!("unable to decrease reference count when zero");
        }

        self.ref_count.set(count - 1);

        count - 1
    }

    fn get_ref_count(&self) -> usize {
        self.ref_count.get()
    }
}

pub type AtomRefPtr<T> = NonNull<AtomRefInner<T>>;

pub struct AtomRef<T: ?Sized> {
    ptr: AtomRefPtr<T>,
    phantom: PhantomData<T>,
}

impl<T: PartialEq> PartialEq for AtomRef<T> {
    fn eq(&self, other: &Self) -> bool {
        // If they point to the same allocation, they're equal
        if self.ptr.as_ptr() == other.ptr.as_ptr() {
            return true;
        }

        self.as_ref().eq(other.as_ref())
    }
}

impl<T: ?Sized> AtomRef<T> {
    #[inline(always)]
    fn inner(&self) -> &AtomRefInner<T> {
        unsafe { self.ptr.as_ref() }
    }

    fn from_inner(ptr: AtomRefPtr<T>) -> Self {
        Self {
            ptr,
            phantom: PhantomData,
        }
    }
}

impl<T> AtomRef<T> {
    pub fn new(data: T) -> Self {
        Self {
            ptr: Box::leak(Box::new(AtomRefInner {
                ref_count: Cell::new(1),
                data,
            }))
            .into(),
            phantom: PhantomData,
        }
    }

    pub fn try_unwrap(self) -> Result<T, Self> {
        if self.ref_count() == 1 {
            self.inner().decr_ref_count();

            let value = unsafe { ptr::read(self.as_ref()) };

            mem::forget(self);

            return Ok(value);
        }

        Err(self)
    }

    pub fn ref_count(&self) -> usize {
        self.inner().get_ref_count()
    }
}

impl<T: Clone> AtomRef<T> {
    pub fn clone_inner(&self) -> T {
        self.as_ref().clone()
    }

    pub fn unwrap_or_clone_inner(self) -> T {
        match self.try_unwrap() {
            Ok(value) => value,
            Err(value_ref) => value_ref.clone_inner(),
        }
    }
}

impl<T: ?Sized> Clone for AtomRef<T> {
    fn clone(&self) -> Self {
        self.inner().incr_ref_count();

        Self::from_inner(self.ptr)
    }
}

impl<T: ?Sized> Drop for AtomRef<T> {
    #[inline(always)]
    fn drop(&mut self) {
        if self.inner().decr_ref_count() > 0 {
            // We shouldn't drop the value here as there are still other references
            return;
        }

        unsafe {
            let raw_ptr = &mut (*self.ptr.as_ptr()).data;

            ptr::drop_in_place(raw_ptr);
        }
    }
}

impl<T> AsRef<T> for AtomRef<T> {
    fn as_ref(&self) -> &T {
        &self.inner().data
    }
}

impl<T> AsMut<T> for AtomRef<T> {
    fn as_mut(&mut self) -> &mut T {
        unsafe { &mut (*self.ptr.as_ptr()).data }
    }
}

impl<T> Deref for AtomRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> From<T> for AtomRef<T> {
    fn from(inner: T) -> Self {
        AtomRef::new(inner)
    }
}

impl<T: Debug> Debug for AtomRef<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AtomRef<{:?}>", self.as_ref())
    }
}
