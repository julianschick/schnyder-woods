use crate::graph::indices::{VertexI};
use crate::graph::guarded_map::Index;
use std::fmt::Display;

#[derive(Debug)]
pub struct IndexAccessError<T: Index + Display> {
    failed_index: T,
    internal: bool
}

impl<T: Index + Display> IndexAccessError<T> {
    pub fn new(failed_index: T) -> IndexAccessError<T> {
        IndexAccessError { failed_index, internal: false }
    }

    pub fn new_internal(failed_index: T) -> IndexAccessError<T> {
        IndexAccessError { failed_index, internal: true }
    }

    pub fn get_index(&self) -> T { self.failed_index }
}

#[derive(Debug)]
pub struct NoSuchEdgeError {
    vertices: (VertexI, VertexI)
}

impl NoSuchEdgeError {
    pub fn new(v1: VertexI, v2: VertexI) -> NoSuchEdgeError {
        NoSuchEdgeError { vertices: (v1, v2) }
    }

    pub fn get_vertices(&self) -> (VertexI, VertexI) { self.vertices }
}