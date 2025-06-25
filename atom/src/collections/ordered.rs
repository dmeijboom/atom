use std::{
    borrow::Borrow,
    collections::HashMap,
    hash::Hash,
    ops::{Index, IndexMut},
};

use wyhash2::WyHash;

pub struct OrderedSet<T: Hash + PartialEq> {
    items: Vec<T>,
}

impl<T: Hash + PartialEq> Default for OrderedSet<T> {
    fn default() -> Self {
        Self { items: vec![] }
    }
}

impl<T: Hash + PartialEq> OrderedSet<T> {
    pub fn new(iter: impl IntoIterator<Item = T>) -> Self {
        let mut set = Self::default();
        for item in iter.into_iter() {
            set.insert(item);
        }
        set
    }

    pub fn find(&self, value: &T) -> Option<u32> {
        self.items
            .iter()
            .position(|elem| elem == value)
            .map(|idx| idx as u32)
    }

    pub fn insert(&mut self, value: T) -> u32 {
        if let Some(index) = self.find(&value) {
            index
        } else {
            self.items.push(value);
            (self.items.len() - 1) as u32
        }
    }

    pub fn into_vec(self) -> Vec<T> {
        self.items
    }
}

impl<T: Hash + PartialEq> Index<usize> for OrderedSet<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.items.get(index).expect("index out of bounds")
    }
}

impl<T: Hash + PartialEq> IntoIterator for OrderedSet<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

#[derive(Default)]
pub struct OrderedMap<K: Hash, V> {
    map: HashMap<K, V, WyHash>,
    lookup: HashMap<K, usize, WyHash>,
}

impl<K: Hash + Eq, V> OrderedMap<K, V> {
    pub fn insert(&mut self, key: K, value: V) -> u32
    where
        K: Clone,
    {
        match self.lookup.get(&key) {
            Some(idx) => *idx as u32,
            None => {
                let idx = self.lookup.len();

                self.map.insert(key.clone(), value);
                self.lookup.insert(key, idx);

                idx as u32
            }
        }
    }

    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.lookup.contains_key(key)
    }

    pub fn get<Q>(&self, key: &Q) -> Option<u32>
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.lookup.get(key).map(|idx| *idx as u32)
    }

    pub fn into_vec(mut self) -> Vec<V> {
        let mut elements = self.lookup.into_iter().collect::<Vec<_>>();

        elements.sort_by_key(|(_, idx)| *idx);
        elements
            .into_iter()
            .filter_map(|(key, _)| self.map.remove(&key))
            .collect::<Vec<_>>()
    }
}

impl<K: Hash + Eq, V, Q: ?Sized> Index<&Q> for OrderedMap<K, V>
where
    K: Borrow<Q>,
    Q: Hash + Eq,
{
    type Output = V;

    fn index(&self, idx: &Q) -> &Self::Output {
        &self.map[idx]
    }
}

impl<K: Hash + Eq, V, Q: ?Sized> IndexMut<&Q> for OrderedMap<K, V>
where
    K: Borrow<Q>,
    Q: Hash + Eq,
{
    fn index_mut(&mut self, idx: &Q) -> &mut Self::Output {
        self.map.get_mut(idx).expect("key not found")
    }
}
