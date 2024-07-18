use std::vec::IntoIter;

#[derive(Default)]
pub struct ReuseVec<T> {
    data: Vec<T>,
    len: usize,
}

impl<T> ReuseVec<T> {
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn push_and_reuse(&mut self) -> Option<&mut T> {
        if self.data.len() > self.len {
            self.len += 1;
            return Some(self.last_mut());
        }

        None
    }

    pub fn push(&mut self, item: T) {
        if self.data.len() > self.len {
            self.data[self.len] = item;
        } else {
            self.data.push(item);
        }

        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<&mut T> {
        if self.len == 0 {
            return None;
        }

        let item = self.data.get_mut(self.len - 1);
        self.len -= 1;
        item
    }

    pub fn last(&self) -> &T {
        &self.data[self.len - 1]
    }

    pub fn last_mut(&mut self) -> &mut T {
        &mut self.data[self.len - 1]
    }

    pub fn into_iter(mut self) -> IntoIter<T> {
        self.data.truncate(self.len);
        self.data.into_iter()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_reuse_vec() {
        let mut vec = super::ReuseVec::default();
        assert_eq!(vec.push_and_reuse(), None);

        vec.push(1);
        vec.push(2);
        vec.push(3);
        vec.push(4);

        vec.pop();

        assert_eq!(vec.push_and_reuse(), Some(&mut 4));
        assert_eq!(vec.push_and_reuse(), None);

        assert_eq!(vec.pop(), Some(&mut 4));
        assert_eq!(vec.pop(), Some(&mut 3));
        assert_eq!(vec.pop(), Some(&mut 2));
        assert_eq!(vec.pop(), Some(&mut 1));
        assert_eq!(vec.pop(), None);
    }
}
