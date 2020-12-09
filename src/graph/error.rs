use crate::graph::index_store::Index;
use crate::graph::indices::VertexI;
use serde::export::fmt::Debug;
use std::fmt::{Display, Formatter};

pub type GraphResult<T> = Result<T, GraphErr>;

pub struct GraphErr {
    problem: String,
}

impl GraphErr {
    pub fn new(problem: &str) -> Self {
        let mut problem = problem.to_string();
        if !(problem.ends_with(".") || problem.ends_with("!") || problem.ends_with("?")) {
            problem.push('.');
        }

        GraphErr { problem }
    }

    pub fn new_err<T>(problem: &str) -> Result<T, Self> {
        return Err(GraphErr::new(problem));
    }

    pub fn get_message(&self) -> &str {
        return &self.problem;
    }
}

impl Debug for GraphErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("GraphErr: {}", self.problem))
    }
}

impl Display for GraphErr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.problem))
    }
}

impl<T: Index + Display> From<IndexAccessError<T>> for GraphErr {
    fn from(cause: IndexAccessError<T>) -> Self {
        return GraphErr::new(&format!(
            "Access to invalid index {} occurred.",
            cause.get_index()
        ));
    }
}

impl From<NoSuchEdgeError> for GraphErr {
    fn from(cause: NoSuchEdgeError) -> Self {
        return GraphErr::new(&format!(
            "No edge between {} and {}.",
            cause.get_vertices().0,
            cause.get_vertices().1
        ));
    }
}

#[derive(Debug)]
pub struct IndexAccessError<T: Index + Display> {
    failed_index: T,
    internal: bool,
}

impl<T: Index + Display> IndexAccessError<T> {
    pub fn new(failed_index: T) -> IndexAccessError<T> {
        IndexAccessError {
            failed_index,
            internal: false,
        }
    }

    pub fn new_internal(failed_index: T) -> IndexAccessError<T> {
        IndexAccessError {
            failed_index,
            internal: true,
        }
    }

    pub fn mark_internal(&mut self) {
        self.internal = true;
    }

    pub fn get_index(&self) -> T {
        self.failed_index
    }
}

impl<T: Index + Display> Display for IndexAccessError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid index {} accessed", self.failed_index)
    }
}

#[derive(Debug)]
pub struct NoSuchEdgeError {
    vertices: (VertexI, VertexI),
}

impl NoSuchEdgeError {
    pub fn new(v1: VertexI, v2: VertexI) -> NoSuchEdgeError {
        NoSuchEdgeError { vertices: (v1, v2) }
    }

    pub fn get_vertices(&self) -> (VertexI, VertexI) {
        self.vertices
    }
}
