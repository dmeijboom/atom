use std::mem;

pub struct Stack<T: Default, const N: usize> {
    size: usize,
    data: [T; N],
}

impl<T: Default, const N: usize> Stack<T, N> {
    pub fn push(&mut self, value: T) {
        if self.size >= N {
            panic!("stack size exceeded");
        }

        self.data[self.size] = value;
        self.size += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.size == 0 {
            return None;
        }

        let value = mem::take(&mut self.data[self.size - 1]);
        self.size -= 1;

        Some(value)
    }
}

impl<T: Default, const N: usize> Default for Stack<T, N> {
    fn default() -> Self {
        Self {
            size: 0,
            data: core::array::from_fn(|_| T::default()),
        }
    }
}
