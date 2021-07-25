use std::borrow::Borrow;
use std::collections::btree_map::Keys;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexedBTreeMap<K: Ord + Clone + Hash, V> {
    map: BTreeMap<K, V>,
    key_index: HashMap<usize, K>,
    value_index: HashMap<K, usize>,
}

impl<K: Ord + Clone + Hash, V> IndexedBTreeMap<K, V> {
    pub fn new(map: BTreeMap<K, V>) -> Self {
        let mut key_index = HashMap::new();
        let mut value_index = HashMap::new();

        for (entry_idx, (key, _)) in map.iter().enumerate() {
            key_index.insert(entry_idx, key.clone());
            value_index.insert(key.clone(), entry_idx);
        }

        Self {
            map,
            key_index,
            value_index,
        }
    }

    pub fn get_key_value<Q: Ord + ?Sized>(&self, key: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q> + Ord,
    {
        self.map.get_key_value(key)
    }

    pub fn get_key_value_by_index(&self, index: usize) -> Option<(&K, &V)> {
        if let Some(key) = self.key_index.get(&index) {
            return self.get_key_value(key);
        }

        None
    }

    pub fn get_index_by_key(&self, search: &K) -> Option<usize> {
        if let Some(index) = self.value_index.get(search) {
            return Some(*index);
        }

        None
    }

    pub fn keys(&self) -> Keys<'_, K, V> {
        self.map.keys()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }
}
