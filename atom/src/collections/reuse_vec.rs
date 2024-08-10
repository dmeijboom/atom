#[derive(Default)]
enum Entry<T> {
    #[default]
    Empty,
    Occupied(T),
}

pub struct ReuseVec<T> {
    data: Vec<Entry<T>>,
    len: usize,
}

impl<T> Default for ReuseVec<T> {
    fn default() -> Self {
        Self {
            data: vec![],
            len: 0,
        }
    }
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
            self.data[self.len] = Entry::Occupied(item);
        } else {
            self.data.push(Entry::Occupied(item));
        }

        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<&mut T> {
        if self.len == 0 {
            return None;
        }

        let item = self.data.get_mut(self.len - 1);
        self.len -= 1;
        item.map(|entry| match entry {
            Entry::Empty => unreachable!(),
            Entry::Occupied(item) => item,
        })
    }

    pub fn last(&self) -> &T {
        match &self.data[self.len - 1] {
            Entry::Empty => unreachable!(),
            Entry::Occupied(item) => item,
        }
    }

    pub fn last_mut(&mut self) -> &mut T {
        match &mut self.data[self.len - 1] {
            Entry::Empty => unreachable!(),
            Entry::Occupied(item) => item,
        }
    }

    pub fn iter(&mut self) -> impl DoubleEndedIterator<Item = &T> {
        self.data[0..self.len]
            .iter()
            .filter_map(|entry| match entry {
                Entry::Empty => None,
                Entry::Occupied(item) => Some(item),
            })
    }

    pub fn into_iter(mut self) -> impl DoubleEndedIterator<Item = T> {
        self.data.truncate(self.len);
        self.data.into_iter().filter_map(|entry| match entry {
            Entry::Empty => None,
            Entry::Occupied(item) => Some(item),
        })
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
