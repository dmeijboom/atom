use std::slice::Iter;

pub struct Stack<T: Copy + Default, const N: usize> {
    data: [T; N],
    len: usize,
}

impl<T: Copy + Default, const N: usize> Default for Stack<T, N> {
    fn default() -> Self {
        Self {
            data: [T::default(); N],
            len: 0,
        }
    }
}

impl<T: Copy + Default, const N: usize> Stack<T, N> {
    pub fn pop(&mut self) -> T {
        self.len -= 1;
        self.data[self.len]
    }

    pub fn push(&mut self, item: T) {
        self.data[self.len] = item;
        self.len += 1;
    }

    pub fn is_full(&self) -> bool {
        self.len == N
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.data[..self.len].iter()
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}
