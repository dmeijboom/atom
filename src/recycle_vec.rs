use std::ops::{Index, IndexMut};

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

    #[inline]
    pub fn recycle(&mut self) -> Option<T> {
        self.free.pop()
    }

    #[inline]
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

    #[inline]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.len() == 0
    }

    #[inline]
    pub fn last(&self) -> Option<&T> {
        self.items.last()
    }

    #[inline]
    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.items.last_mut()
    }
}

impl<T> Index<usize> for RecycleVec<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        self.items.index(index)
    }
}

impl<T> IndexMut<usize> for RecycleVec<T> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.items.index_mut(index)
    }
}

#[cfg(test)]
mod tests {
    use super::RecycleVec;

    #[test]
    fn test_empty() {
        let vec = RecycleVec::<usize>::new();

        assert!(vec.is_empty());
        assert_eq!(vec.len(), 0);
    }

    #[test]
    fn test_vec_like() {
        let mut vec = RecycleVec::<usize>::new();

        vec.push(1);
        vec.push(2);
        vec.push(3);

        assert!(!vec.is_empty());
        assert_eq!(vec.len(), 3);
        assert_eq!(vec[0], 1);
        assert_eq!(vec[1], 2);
        assert_eq!(vec[2], 3);
        assert_eq!(vec.last(), Some(&3));

        vec[1] = 100;

        assert_eq!(vec[1], 100);

        vec.pop();
        vec.pop();

        assert_eq!(vec.len(), 1);
        assert_eq!(vec[0], 1);
        assert_eq!(vec.last(), Some(&1));

        if let Some(i) = vec.last_mut() {
            *i = 200;
        }

        assert_eq!(vec[0], 200);
        assert_eq!(vec.remove(0), 200);
        assert_eq!(vec.len(), 0);
        assert!(vec.is_empty());
    }

    #[test]
    fn test_recycle() {
        let mut vec = RecycleVec::<usize>::new();

        vec.push(1);
        vec.push(2);
        vec.push(3);

        vec.pop();

        assert_eq!(vec.recycle(), Some(3));
        assert_eq!(vec.recycle(), None);
    }
}
