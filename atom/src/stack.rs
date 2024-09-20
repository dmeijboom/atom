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
    pub fn slice_at(&mut self, n: usize) -> Vec<T> {
        let slice = &self.data[self.len - n..self.len];
        self.len -= n;
        slice.to_vec()
    }

    pub fn operands(&mut self) -> (T, T) {
        let rhs = self.data[self.len - 1];
        let lhs = self.data[self.len - 2];

        self.len -= 2;

        (lhs, rhs)
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.data.is_empty() {
            None
        } else {
            self.len -= 1;
            Some(self.data[self.len])
        }
    }

    pub fn push(&mut self, item: T) {
        self.data[self.len] = item;
        self.len += 1;
    }

    pub fn iter(&self) -> Iter<'_, T> {
        self.data[..self.len].iter()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_copy_to() {
        let mut stack = super::Stack::<i32, 10>::default();
        stack.push(1);
        stack.push(2);
        stack.push(3);
        stack.push(4);
        stack.push(5);
        stack.len -= 2;

        let vec = stack.slice_at(2);

        assert_eq!(vec, [2, 3]);
    }
}
