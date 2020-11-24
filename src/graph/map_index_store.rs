use std::collections::HashMap;
use std::hash::Hash;
use crate::graph::index_store::{Index, Ideable, IndexStore};

pub struct MapIndexStore<N: Index, V: Ideable<N>> {
    map: HashMap<N, V>,
    least_free_index: usize
}

impl<N:Index, V: Ideable<N>> MapIndexStore<N,V> {
    pub fn new() -> MapIndexStore<N, V> {
        MapIndexStore {
            map: HashMap::new(),
            least_free_index: 0
        }
    }
}

impl<N: Index, V: Ideable<N>> IndexStore<N,V> for MapIndexStore<N, V> {
    fn retrieve_index(&mut self, mut item: V) -> N {
        let result = N::from(self.least_free_index);
        item.set_id(result);
        self.map.insert(result, item);

        while self.map.contains_key(&N::from(self.least_free_index)) {
            self.least_free_index += 1;
        }

        return result;
    }

    fn insert_with_index(&mut self, item: V, index: &N) {
        if !self.is_available(index) {
            panic!("index not available");
        }

        self.map.insert(*index, item);

        while self.map.contains_key(&N::from(self.least_free_index)) {
            self.least_free_index += 1;
        }
    }

    fn free_index(&mut self, index: &N) -> Option<V> {
        if self.least_free_index > (*index).into()  {
            self.least_free_index = (*index).into();
        }
        self.map.remove(&index)
    }

    fn peek_index(&self) -> N {
        N::from(self.least_free_index)
    }

    fn is_valid_index(&self, index: &N) -> bool {
        self.map.contains_key(index)
    }

    fn is_available(&self, index: &N) -> bool { !self.map.contains_key(index) }

    fn get(&self, index: &N) -> Option<&V> {
        self.map.get(index)
    }

    fn get_mut(&mut self, index: &N) -> Option<&mut V> {
        self.map.get_mut(index)
    }

    fn get_values<'a>(&'a self) -> Box<dyn Iterator<Item=&V> + 'a> {
        Box::new(self.map.values())
    }

    fn get_keys<'a>(&'a self) -> Box<dyn Iterator<Item=&N> + 'a> {
        Box::new(self.map.keys())
    }

    fn is_empty(&self) -> bool {
        self.map.is_empty()
   }

    fn len(&self) -> usize {
        self.map.len()
    }
}

impl<N: Index+Clone, V: Ideable<N> + Clone> Clone for MapIndexStore<N, V>{
    fn clone(&self) -> Self {
        MapIndexStore {
            map: self.map.clone(),
            least_free_index: self.least_free_index
        }
    }
}