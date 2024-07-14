use std::{fmt::Debug, mem};

#[cfg(feature = "tracing")]
use tracing::{instrument, Level};

pub struct Stack<T: Default + Debug, const N: usize> {
    size: usize,
    data: [T; N],
}

impl<T: Default + Debug, const N: usize> Stack<T, N> {
    pub fn back(&self) -> Option<&T> {
        if self.size == 0 {
            return None;
        }

        self.data.get(self.size - 1)
    }

    pub fn back_mut(&mut self) -> Option<&mut T> {
        if self.size == 0 {
            return None;
        }

        self.data.get_mut(self.size - 1)
    }

    #[cfg_attr(feature = "tracing", instrument(level = Level::TRACE, skip_all, fields(size = self.size, ?value), ret))]
    pub fn push(&mut self, value: T) {
        assert!(self.size < N, "stack size exceeded");

        self.data[self.size] = value;
        self.size += 1;
    }

    #[cfg_attr(feature = "tracing", instrument(level = Level::TRACE, skip_all, fields(size = self.size), ret(Debug)))]
    pub fn pop(&mut self) -> Option<T> {
        if self.size == 0 {
            return None;
        }

        let value = mem::take(&mut self.data[self.size - 1]);
        self.size -= 1;

        Some(value)
    }

    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        self.data[..self.size].to_vec()
    }
}

impl<T: Default + Debug, const N: usize> Default for Stack<T, N> {
    fn default() -> Self {
        Self {
            size: 0,
            data: core::array::from_fn(|_| T::default()),
        }
    }
}
