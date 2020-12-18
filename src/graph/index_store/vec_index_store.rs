use crate::graph::error::IndexAccessError;
use crate::graph::index_store::{Ideable, Index, IndexStore};
use std::marker::PhantomData;

pub struct VecIndexStore<N: Index, V: Ideable<N>> {
    indices: Vec<N>,
    data: Vec<Option<V>>,
    least_free_index: usize,
    p: PhantomData<N>,
}

impl<N: Index, V: Ideable<N>> VecIndexStore<N, V> {
    pub fn new() -> VecIndexStore<N, V> {
        VecIndexStore {
            indices: Vec::new(),
            data: Vec::new(),
            least_free_index: 0,
            p: PhantomData::default(),
        }
    }
}

impl<N: Index, V: Ideable<N>> IndexStore<N, V> for VecIndexStore<N, V> {
    fn push(&mut self, mut item: V) -> N {
        let result = N::from(self.least_free_index);
        item.set_id(result);
        if self.data.len() > self.least_free_index {
            self.data[self.least_free_index] = Some(item);
            self.indices.push(N::from(self.least_free_index));
            while self.least_free_index < self.data.len()
                && self.data[self.least_free_index].is_some()
            {
                self.least_free_index += 1;
            }
        } else {
            self.data.push(Some(item));
            self.indices.push(N::from(self.data.len() - 1));
            self.least_free_index += 1;
        }

        return result;
    }

    fn insert(&mut self, item: V, index: &N) -> Result<(), IndexAccessError<N>> {
        let i = (*index).into();

        if i < self.data.len() {
            if let Some(_) = self.data[i] {
                return Err(IndexAccessError::new(*index));
            } else {
                self.data[i] = Some(item);
                self.indices.push(*index);
            }
        } else {
            while self.data.len() < i {
                self.data.push(None);
            }
            self.data.push(Some(item));
            self.indices.push(N::from(self.data.len() - 1));
        }
        while self.least_free_index < self.data.len() && self.data[self.least_free_index].is_some()
        {
            self.least_free_index += 1;
        }
        Ok(())
    }

    fn remove(&mut self, index: &N) -> Option<V> {
        let idx: usize = (*index).into();

        if self.least_free_index > idx {
            self.least_free_index = idx;
        }
        if idx >= self.data.len() {
            return None;
        }
        let i = self.indices.iter().position(|i| i == index).unwrap();
        self.indices.remove(i);
        self.data[idx].take()
    }

    fn get(&self, index: &N) -> Option<&V> {
        match self.data.get((*index).into()) {
            Some(Some(x)) => Some(x),
            _ => None,
        }
    }

    fn get_mut(&mut self, index: &N) -> Option<&mut V> {
        let index = (*index).into();
        match self.data.get_mut(index) {
            Some(Some(x)) => Some(x),
            _ => None,
        }
    }

    fn get_values<'a>(&'a self) -> Box<dyn Iterator<Item = &V> + 'a> {
        Box::new(self.data.iter().filter_map(|o| o.as_ref()))
    }

    fn get_indices<'a>(&'a self) -> Box<dyn Iterator<Item = &N> + 'a> {
        Box::new(self.indices.iter())
    }

    fn next_index(&self) -> N {
        N::from(self.least_free_index)
    }

    fn is_valid_index(&self, index: &N) -> bool {
        let index = (*index).into();
        index < self.data.len() && self.data[index].is_some()
    }

    fn is_available(&self, index: &N) -> bool {
        let index: usize = (*index).into();
        index >= self.data.len() || self.data[index].is_none()
    }

    fn is_empty(&self) -> bool {
        self.indices.is_empty()
    }

    fn len(&self) -> usize {
        self.indices.len()
    }
}

impl<N: Index + Clone, V: Ideable<N> + Clone> Clone for VecIndexStore<N, V> {
    fn clone(&self) -> Self {
        VecIndexStore {
            indices: self.indices.clone(),
            data: self.data.clone(),
            least_free_index: self.least_free_index,
            p: PhantomData::default(),
        }
    }
}
