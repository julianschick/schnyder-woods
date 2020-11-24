use std::hash::Hash;

pub trait Index: From<usize> + Into<usize> + Copy + Clone + Eq + Hash { }

pub trait Ideable<I> {
    fn get_id(&self) -> I;
    fn set_id(&mut self, id: I);
}

pub trait IndexStore<N: Index, V: Ideable<N>> {

    fn push(&mut self, item: V) -> N;
    fn insert(&mut self, item: V, index: &N);
    fn remove(&mut self, index: &N) -> Option<V>;

    fn get(&self, index: &N) -> Option<&V>;
    fn get_mut(&mut self, index: &N) -> Option<&mut V>;
    fn get_values<'a>(&'a self) -> Box<dyn Iterator<Item=&V> + 'a>;
    fn get_indices<'a>(&'a self) -> Box<dyn Iterator<Item=&N> + 'a>;

    fn next_index(&self) -> N;
    fn is_valid_index(&self, index: &N) -> bool;
    fn is_available(&self, index: &N) -> bool;
    fn is_empty(&self) -> bool;
    fn len(&self) -> usize;
}