use crate::graph::index_store::Index;
use core::fmt;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct VertexI(pub usize);

impl From<usize> for VertexI {
    fn from(n: usize) -> Self {
        VertexI(n)
    }
}
impl Into<usize> for VertexI {
    fn into(self) -> usize {
        self.0
    }
}
impl Index for VertexI {}
impl fmt::Display for VertexI {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v_{}", self.0)
    }
}
impl fmt::Debug for VertexI {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v_{}", self.0)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct EdgeI(pub usize);

impl From<usize> for EdgeI {
    fn from(n: usize) -> Self {
        EdgeI(n)
    }
}
impl Into<usize> for EdgeI {
    fn into(self) -> usize {
        self.0
    }
}
impl Index for EdgeI {}
impl fmt::Display for EdgeI {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "e_{}", self.0)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct FaceI(pub usize);

impl From<usize> for FaceI {
    fn from(n: usize) -> Self {
        FaceI(n)
    }
}
impl Into<usize> for FaceI {
    fn into(self) -> usize {
        self.0
    }
}
impl Index for FaceI {}
impl fmt::Display for FaceI {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "f_{}", self.0)
    }
}
