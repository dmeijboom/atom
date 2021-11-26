use std::ops::{Deref, DerefMut};

pub struct RecycleVec<T> {
    items: Vec<T>,
    free: Vec<T>,
    max_free: usize,
}

impl<T> RecycleVec<T> {
    pub fn new() -> Self {
        Self {
            items: vec![],
            free: vec![],
            max_free: 5,
        }
    }

    pub fn recycle(&mut self) -> Option<T> {
        self.free.pop()
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item);
    }

    pub fn remove(&mut self, index: usize) -> T {
        self.items.remove(index)
    }

    #[inline]
    pub fn pop(&mut self) {
        if let Some(item) = self.items.pop() {
            if self.free.len() < self.max_free {
                self.free.push(item);
            }
        }
    }
}

impl<T> Deref for RecycleVec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.items
    }
}

impl<T> DerefMut for RecycleVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.items
    }
}
