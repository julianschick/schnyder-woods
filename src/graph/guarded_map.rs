use std::collections::HashMap;
use std::hash::Hash;

pub trait Index: From<usize> + Into<usize> + Copy + Clone + Eq + Hash { }

pub trait Ideable<I> {
    fn get_id(&self) -> I;
    fn set_id(&mut self, id: I);
}

pub struct GuardedMap<N: Index, V: Ideable<N>> {
    map: HashMap<N, V>,
    least_free_index: usize
}

impl<N: Index, V: Ideable<N>> GuardedMap<N, V> {

    pub fn new() -> GuardedMap<N, V> {
        GuardedMap {
            map: HashMap::new(),
            least_free_index: 0
        }
    }

    pub fn clone_with_map<Vv: Ideable<N>>(&self, value_mapping: &Fn(&V) -> Vv) -> GuardedMap<N, Vv> {
        GuardedMap {
            map: self.map.iter().map(|(k,v)| (*k, value_mapping(&v))).collect(),
            least_free_index: self.least_free_index,
        }
    }

    pub fn peek_index(&self) -> N {
        N::from(self.least_free_index)
    }

    pub fn retrieve_index(&mut self, mut item: V) -> N {
        let result = N::from(self.least_free_index);
        item.set_id(result);
        self.map.insert(result, item);

        while self.map.contains_key(&N::from(self.least_free_index)) {
            self.least_free_index += 1;
        }

        return result;
    }

    pub fn insert_with_index(&mut self, mut item: V, index: &N) {
        if !self.is_available(index) {
            panic!("index not available");
        }

        self.map.insert(*index, item);

        while self.map.contains_key(&N::from(self.least_free_index)) {
            self.least_free_index += 1;
        }
    }

    pub fn reindex(&mut self, old_index: &N, new_index: &N) -> bool {
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
    }

    pub fn free_index(&mut self, index: &N) -> Option<V> {
        if self.least_free_index > (*index).into()  {
            self.least_free_index = (*index).into();
        }
        self.map.remove(&index)
    }

    pub fn is_valid_index(&self, index: &N) -> bool {
        self.map.contains_key(index)
    }

    pub fn is_available(&self, index: &N) -> bool { !self.map.contains_key(index) }

    pub fn get_map(&self) -> &HashMap<N, V> {
        &self.map
    }

    fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    fn len(&self) -> usize {
        self.map.len()
    }

    pub fn get(&self, index: &N) -> &V {
        self.map.get(index).unwrap()
    }

    pub fn any_index(&self) -> Option<N> {
        if self.map.is_empty() {
            return None;
        }
        return Some(N::from(self.least_free_index - 1));
    }

    pub fn get_mut(&mut self, index: &N) -> &mut V {
        self.map.get_mut(index).unwrap()
    }

}

impl<N: Index+Clone, V: Ideable<N> + Clone> Clone for GuardedMap<N, V>{
    fn clone(&self) -> Self {
        GuardedMap {
            map: self.map.clone(),
            least_free_index: self.least_free_index
        }
    }
}