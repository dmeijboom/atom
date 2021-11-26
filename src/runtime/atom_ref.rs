use std::borrow::Borrow;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::{Rc, Weak};

pub type AtomRef<T> = Rc<T>;
pub type AtomWeakRef<T> = Weak<T>;

pub fn unwrap_or_clone_inner<T>(r: AtomRef<T>) -> T
where
    T: Clone,
{
    AtomRef::try_unwrap(r).unwrap_or_else(|r| {
        let br: &T = r.borrow();

        br.clone()
    })
}

// AtomRefMut is an unsafe abstraction for accessing data inside an AtomRef so the caller has to make sure
// it's safe to mutate/read the data
pub struct AtomRefMut<T: ?Sized>(AtomRef<T>);

impl<T: ?Sized> AtomRefMut<T> {
    pub fn new(r: AtomRef<T>) -> Self {
        Self(r)
    }

    pub fn as_mut(&mut self) -> &mut T {
        unsafe { AtomRef::get_mut_unchecked(&mut self.0) }
    }

    pub fn clone(r: &AtomRefMut<T>) -> AtomRefMut<T> {
        AtomRefMut::new(AtomRef::clone(&r.0))
    }
}

impl<T: ?Sized + Debug> Debug for AtomRefMut<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<T: ?Sized + PartialEq> PartialEq for AtomRefMut<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<T: ?Sized> Deref for AtomRefMut<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
