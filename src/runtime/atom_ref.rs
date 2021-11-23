use core::mem;
use std::alloc::{alloc, dealloc, Layout};
use std::cell::Cell;
use std::fmt::{Debug, Formatter};
use std::marker::{PhantomData, Unsize};
use std::ops::{CoerceUnsized, Deref};
use std::panic::{RefUnwindSafe, UnwindSafe};
use std::ptr::{self, NonNull};

#[repr(C)]
pub struct AtomRefInner<T: ?Sized> {
    strong: Cell<usize>,
    weak: Cell<usize>,
    data: T,
}

impl<T: ?Sized> AtomRefInner<T> {
    fn incr_strong(&self) {
        let count = self.strong.get();

        self.strong.set(count + 1);
    }

    #[inline]
    fn decr_strong(&self) {
        let count = self.strong.get();

        if count == 0 {
            unreachable!("unable to decrease reference count when zero");
        }

        self.strong.set(count - 1);
    }

    #[inline]
    fn get_strong(&self) -> usize {
        self.strong.get()
    }

    fn incr_weak(&self) {
        let count = self.weak.get();

        self.weak.set(count + 1);
    }

    #[inline]
    fn decr_weak(&self) {
        let count = self.weak.get();

        if count == 0 {
            unreachable!("unable to decrease reference count when zero");
        }

        self.weak.set(count - 1);
    }

    #[inline]
    fn get_weak(&self) -> usize {
        self.weak.get()
    }
}

pub type AtomRefPtr<T> = NonNull<AtomRefInner<T>>;

pub struct AtomRef<T: ?Sized> {
    ptr: AtomRefPtr<T>,
    phantom: PhantomData<AtomRefInner<T>>,
}

impl<T: RefUnwindSafe + ?Sized> UnwindSafe for AtomRef<T> {}

impl<T: ?Sized + PartialEq> PartialEq for AtomRef<T> {
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
                strong: Cell::new(1),
                weak: Cell::new(1),
                data,
            }))
            .into(),
            phantom: PhantomData,
        }
    }

    pub fn weak(&self) -> WeakRef<T> {
        self.inner().incr_weak();

        WeakRef { ptr: self.ptr }
    }

    pub fn try_unwrap(self) -> Result<T, Self> {
        if self.inner().get_strong() == 1 {
            let value = unsafe { ptr::read(self.ptr.as_ptr()) };

            self.inner().decr_strong();
            mem::forget(self);

            return Ok(value.data);
        }

        Err(self)
    }
}

impl<T> AtomRef<[T]> {
    fn copy_from_slice(data: &[T]) -> AtomRef<[T]> {
        unsafe {
            let array_layout = Layout::array::<T>(data.len()).unwrap();
            let layout = Layout::new::<AtomRefInner<()>>()
                .extend(array_layout)
                .unwrap()
                .0
                .pad_to_align();
            let ptr = alloc(layout);
            let inner =
                ptr::slice_from_raw_parts_mut(ptr as *mut T, data.len()) as *mut AtomRefInner<[T]>;

            ptr::write(&mut (*inner).strong, Cell::new(1));
            ptr::write(&mut (*inner).weak, Cell::new(1));

            ptr::copy_nonoverlapping(
                data.as_ptr(),
                &mut (*inner).data as *mut [T] as *mut T,
                data.len(),
            );

            Self::from_inner(NonNull::new_unchecked(inner))
        }
    }
}

impl<T> From<Vec<T>> for AtomRef<[T]> {
    fn from(mut vec: Vec<T>) -> Self {
        let atom_ref = AtomRef::copy_from_slice(&vec);

        unsafe {
            vec.set_len(0);
        }

        atom_ref
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
        self.inner().incr_strong();

        Self::from_inner(self.ptr)
    }
}

impl<T: ?Sized> Drop for AtomRef<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            self.inner().decr_strong();

            if self.inner().get_strong() > 0 {
                // We shouldn't drop the value here as there are still other references
                return;
            }

            let raw_ptr = &mut (*self.ptr.as_ptr()).data;

            ptr::drop_in_place(raw_ptr);

            self.inner().decr_weak();

            if self.inner().get_weak() == 0 {
                let layout = Layout::for_value(self.ptr.as_ref());

                if layout.size() != 0 {
                    dealloc(self.ptr.cast().as_ptr(), layout);
                }
            }
        }
    }
}

impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<AtomRef<U>> for AtomRef<T> {}

impl<T: ?Sized> AsRef<T> for AtomRef<T> {
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

impl<T: ?Sized + Debug> Debug for AtomRef<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AtomRef<{:?}>", self.as_ref())
    }
}

pub struct WeakRef<T: ?Sized> {
    ptr: AtomRefPtr<T>,
}

impl<T: ?Sized> WeakRef<T> {
    #[inline]
    fn inner(&self) -> WeakRefInner<'_> {
        unsafe {
            let ptr = self.ptr.as_ptr();

            WeakRefInner {
                strong: &(*ptr).strong,
                weak: &(*ptr).weak,
            }
        }
    }

    pub fn upgrade(&self) -> Option<AtomRef<T>> {
        let inner = self.inner();

        if inner.get_strong() == 0 {
            return None;
        }

        inner.incr_strong();

        Some(AtomRef::from_inner(self.ptr))
    }
}

impl<T: ?Sized> Drop for WeakRef<T> {
    fn drop(&mut self) {
        let inner = self.inner();

        inner.decr_weak();

        if inner.get_weak() == 0 {
            unsafe {
                let layout = Layout::for_value(self.ptr.as_ref());

                if layout.size() != 0 {
                    dealloc(self.ptr.cast().as_ptr(), layout);
                }
            }
        }
    }
}

pub struct WeakRefInner<'r> {
    strong: &'r Cell<usize>,
    weak: &'r Cell<usize>,
}

impl WeakRefInner<'_> {
    #[inline]
    fn get_weak(&self) -> usize {
        self.weak.get()
    }

    #[inline]
    fn decr_weak(&self) {
        self.weak.set(self.weak.get() - 1);
    }

    #[inline]
    fn get_strong(&self) -> usize {
        self.strong.get()
    }

    #[inline]
    fn incr_strong(&self) {
        self.strong.set(self.strong.get() + 1);
    }
}
