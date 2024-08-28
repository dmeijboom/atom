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
    pub fn try_pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            Some(self.data[self.len])
        }
    }

    pub fn split_to_vec(&mut self, n: usize) -> Vec<T> {
        let vec = self.data[self.len - n..self.len].to_vec();
        self.len -= n;
        vec
    }

    pub fn operands(&mut self) -> (T, T) {
        let rhs = self.data[self.len - 1];
        let lhs = self.data[self.len - 2];

        self.len -= 2;

        (lhs, rhs)
    }

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

        let vec = stack.split_to_vec(2);

        assert_eq!(vec, [2, 3]);
    }
}
