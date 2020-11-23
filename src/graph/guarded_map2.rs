use std::collections::HashMap;
use std::hash::Hash;
use crate::graph::guarded_map::{Index, Ideable};
use std::marker::PhantomData;

pub struct GuardedMap2<N: Index, V: Ideable<N>> {
    indices: Vec<N>,
    data: Vec<Option<V>>,
    least_free_index: usize,
    p: PhantomData<N>
}

impl<N: Index, V: Ideable<N>> GuardedMap2<N, V> {

    pub fn new() -> GuardedMap2<N, V> {
        GuardedMap2 {
            indices: Vec::new(),
            data: Vec::new(),
            least_free_index: 0,
            p: PhantomData::default()
        }
    }

    pub fn clone_with_map<Vv: Ideable<N>>(&self, value_mapping: &dyn Fn(&V) -> Vv) -> GuardedMap2<N, Vv> {
        GuardedMap2 {
            indices: self.indices.clone(),
            data: self.data.iter().map(|v|
                match v {
                    Some(val) => Some(value_mapping(val)),
                    None => None
                }
            ).collect(),
            least_free_index: self.least_free_index,
            p: PhantomData::default()
        }
    }

    pub fn peek_index(&self) -> N {
        N::from(self.least_free_index)
    }

    pub fn retrieve_index(&mut self, mut item: V) -> N {
        let result = N::from(self.least_free_index);
        item.set_id(result);
        if self.data.len() > self.least_free_index {
            self.data[self.least_free_index] = Some(item);
            self.indices.push(N::from(self.least_free_index));
            while self.least_free_index < self.data.len() && self.data[self.least_free_index].is_some() {
                self.least_free_index += 1;
            }
        } else {
            self.data.push(Some(item));
            self.indices.push(N::from(self.data.len()-1));
            self.least_free_index += 1;
        }

        return result;
    }

    pub fn insert_with_index(&mut self, item: V, index: &N) {
        if !self.is_available(index) {
            panic!("index not available");
        }
        let index = (*index).into();

        if index < self.data.len() {
            self.data[index] = Some(item);
            self.indices.push(N::from(index));
            while self.least_free_index < self.data.len() && self.data[self.least_free_index].is_some() {
                self.least_free_index += 1;
            }
        } else {
            while self.data.len() < index {
                self.data.push(None);
            }
            self.data.push(Some(item));
            self.indices.push(N::from(self.data.len() - 1));
        }
    }

    /*fn reindex(&mut self, old_index: &N, new_index: &N) -> bool {
        if !self.is_available(new_index) {
            return false;
        }

        if let Some(thing) = self.map.remove(old_index) {
            self.map.insert(*new_index, thing);

            if self.least_free_index > (*old_index).into()  {
                self.least_free_index = (*old_index).into();
            } else {
                while self.map.contains_key(&N::from(self.least_free_index)) {
                    self.least_free_index += 1;
                }
            }

            true
        } else {
            false
        }
    }*/

    pub fn free_index(&mut self, index: &N) -> Option<V> {
        let idx: usize = (*index).into();

        if self.least_free_index > idx  {
            self.least_free_index = idx;
        }
        if idx >= self.data.len() {
            return None;
        }
        let i = self.indices.iter().position(|i|i==index).unwrap();
        self.indices.remove(i);
        self.data[idx].take()
    }

    pub fn is_valid_index(&self, index: &N) -> bool {
        let index = (*index).into();
        index < self.data.len() && self.data[index].is_some()
    }

    pub fn is_available(&self, index: &N) -> bool {
        let index: usize = (*index).into();
        index >= self.data.len() || self.data[index].is_none()
    }

    //pub fn get_map(&self) -> &HashMap<N, V> {
        //&self.map
    //}

    /*fn is_empty(&self) -> bool {
        self.map.is_empty()
    }*/

    /*fn len(&self) -> usize {
        self.map.len()
    }*/

    pub fn get(&self, index: &N) -> Option<&V> {
        match self.data.get((*index).into()) {
            Some(Some(x)) => Some(x),
            _ => None
        }
    }

    pub fn any_index(&self) -> Option<N> {
        self.indices.first().map(|v| *v)
    }

    pub fn get_mut(&mut self, index: &N) -> Option<&mut V> {
        let index = (*index).into();
        match self.data.get_mut(index)  {
            Some(Some(x)) => Some(x),
            _ => None
        }
    }

    pub fn get_values(&self) -> impl Iterator<Item=&V> {
        self.data.iter().filter_map(|o| o.as_ref())
    }

    pub fn get_keys(&self) -> impl Iterator<Item=&N> {
        self.indices.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.indices.is_empty()
    }

    pub fn len(&self) -> usize {
        self.indices.len()
    }

}

impl<N: Index+Clone, V: Ideable<N> + Clone> Clone for GuardedMap2<N, V>{
    fn clone(&self) -> Self {
        GuardedMap2 {
            indices: self.indices.clone(),
            data: self.data.clone(),
            least_free_index: self.least_free_index,
            p: PhantomData::default()
        }
    }
}