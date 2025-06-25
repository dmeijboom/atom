use std::{array, mem, slice::Iter};

pub struct Stack<T: Clone + Default, const N: usize> {
    data: [T; N],
    sp: usize,
}

impl<T: Clone + Default, const N: usize> Default for Stack<T, N> {
    fn default() -> Self {
        Self {
            data: array::from_fn(|_| T::default()),
            sp: 0,
        }
    }
}

impl<T: Clone + Default, const N: usize> Stack<T, N> {
    pub fn slice_to_end(&mut self, n: usize) -> &[T] {
        let slice = &self.data[self.sp - n..self.sp];
        self.sp -= n;
        slice
    }

    pub fn operands(&mut self) -> (T, T) {
        let rhs = mem::take(&mut self.data[self.sp - 1]);
        let lhs = mem::take(&mut self.data[self.sp - 2]);

        self.sp -= 2;

        (lhs, rhs)
    }

    pub fn pop(&mut self) -> T {
        self.sp -= 1;
        mem::take(&mut self.data[self.sp])
    }

    pub fn push(&mut self, item: T) {
        self.data[self.sp] = item;
        self.sp += 1;
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.data[..self.sp].iter()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_slice_to_end() {
        let mut stack = super::Stack::<i32, 10>::default();
        stack.push(1);
        stack.push(2);
        stack.push(3);
        stack.push(4);
        stack.push(5);
        stack.sp -= 2;

        let vec = stack.slice_to_end(2);

        assert_eq!(vec, [2, 3]);
    }
}
