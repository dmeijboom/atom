use crate::runtime::{errors::RuntimeError, Gc, Trace};

use super::{array::ArrayLike, Array};

#[derive(Default)]
pub struct Blob<'gc>(pub Array<'gc, u8>);

impl<'gc> Trace for Blob<'gc> {
    fn trace(&self, gc: &mut Gc<'_>) {
        self.0.trace(gc);
    }
}

impl<'gc> ArrayLike<'gc> for Blob<'gc> {
    type Item = u8;

    fn len(&self) -> usize {
        self.0.len()
    }

    fn get(&self, idx: usize) -> Option<&Self::Item> {
        self.0.get(idx)
    }

    fn get_mut(&mut self, idx: usize) -> Option<&mut Self::Item> {
        self.0.get_mut(idx)
    }

    fn slice(&self, from: usize, to: usize) -> Self
    where
        Self: Sized,
    {
        Self(self.0.slice(from, to))
    }

    fn concat(&self, gc: &mut Gc<'gc>, other: &Self) -> Result<Self, RuntimeError>
    where
        Self::Item: Clone,
        Self: Sized,
    {
        self.0.concat(gc, &other.0).map(Self)
    }
}

#[allow(clippy::len_without_is_empty)]
impl<'gc> Blob<'gc> {
    pub fn with_capacity(gc: &mut Gc<'gc>, capacity: usize) -> Result<Self, RuntimeError> {
        Ok(Self(Array::with_capacity(gc, capacity)?))
    }

    pub fn len(&self) -> usize {
        self.0.len
    }
}
