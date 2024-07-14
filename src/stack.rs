use std::mem;

pub struct Registers<T: Clone + Default, const N: usize> {
    data: [T; N],
}

impl<T: Clone + Default, const N: usize> Registers<T, N> {
    pub fn get(&self, idx: usize) -> Option<T> {
        if idx >= N {
            return None;
        }

        Some(self.data[idx].clone())
    }

    pub fn set(&mut self, idx: usize, value: T) {
        if idx >= N {
            panic!("index out of bounds");
        }

        self.data[idx] = value;
    }

    pub fn clear(&mut self) {
        for i in 0..N {
            self.data[i] = T::default();
        }
    }
}

impl<T: Default + Clone, const N: usize> Default for Registers<T, N> {
    fn default() -> Self {
        Self {
            data: core::array::from_fn(|_| T::default()),
        }
    }
}

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

    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        self.data[..self.size].to_vec()
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
