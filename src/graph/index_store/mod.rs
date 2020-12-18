use crate::graph::error::IndexAccessError;
use std::fmt::Display;
use std::hash::Hash;

pub mod map_index_store;
pub mod vec_index_store;

pub trait Index: From<usize> + Into<usize> + Copy + Clone + Eq + Hash + Display {}

/// Thing that can hold an id.
pub trait Ideable<I> {
    fn get_id(&self) -> I;
    fn set_id(&mut self, id: I);
}

/// Container for items of arbitrary kind which issues a stable index for every item that is added.
/// The index is guaranteed not to change as long as the item remains in the container.
/// After an item has been removed, its index can be reused.
pub trait IndexStore<N: Index, V: Ideable<N>> {
    /// Pushes a new item into the store and returns the assigned index.
    fn push(&mut self, item: V) -> N;
    /// Insert an item with the given index.
    /// # Result
    /// If the index is not available, an IndexAccessError is returned.
    fn insert(&mut self, item: V, index: &N) -> Result<(), IndexAccessError<N>>;
    /// Removes the item with the given index and returns it. Returns None if the index is invalid.
    fn remove(&mut self, index: &N) -> Option<V>;

    /// Reference to the item with the given index or None, if the index is invalid.
    fn get(&self, index: &N) -> Option<&V>;
    /// Mutable reference to the item with the given index or None, if the index is invalid.
    fn get_mut(&mut self, index: &N) -> Option<&mut V>;
    /// Iterator over all items.
    fn get_values<'a>(&'a self) -> Box<dyn Iterator<Item = &V> + 'a>;
    /// Iterator over all indices.
    fn get_indices<'a>(&'a self) -> Box<dyn Iterator<Item = &N> + 'a>;

    /// Index the next added item would get.
    fn next_index(&self) -> N;
    fn is_valid_index(&self, index: &N) -> bool;
    fn is_available(&self, index: &N) -> bool;
    fn is_empty(&self) -> bool;
    fn len(&self) -> usize;
}
