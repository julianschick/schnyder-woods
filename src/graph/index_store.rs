use std::hash::Hash;
use regex::internal::Input;

pub trait Index: From<usize> + Into<usize> + Copy + Clone + Eq + Hash { }

pub trait Ideable<I> {
    fn get_id(&self) -> I;
    fn set_id(&mut self, id: I);
}

pub trait IndexStore<N: Index, V: Ideable<N>> {

    fn retrieve_index(&mut self, item: V) -> N;
    fn insert_with_index(&mut self, item: V, index: &N);
    fn free_index(&mut self, index: &N) -> Option<V>;

    fn peek_index(&self) -> N;
    fn is_valid_index(&self, index: &N) -> bool;
    fn is_available(&self, index: &N) -> bool;
    fn get(&self, index: &N) -> Option<&V>;
    fn get_mut(&mut self, index: &N) -> Option<&mut V>;
    fn get_values<'a>(&'a self) -> Box<dyn Iterator<Item=&V> + 'a>;
    fn get_keys<'a>(&'a self) -> Box<dyn Iterator<Item=&N> + 'a>;
    fn is_empty(&self) -> bool;
    fn len(&self) -> usize;
}