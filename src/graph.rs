use std::rc::Rc;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::cmp::min;
use std::hash::Hash;

#[derive(Copy, Clone)]
pub struct VertexIndex(pub usize);
#[derive(Copy, Clone)]
pub struct EdgeIndex(pub usize);

pub struct PlanarMap<N, E> {
    vertices: usize,
    edges: usize,
    //
    vertices_by_edges: Vec<(VertexIndex, VertexIndex)>,
    edges_by_vertices: Vec<Vec<EdgeIndex>>,
    //
    vertex_weights: Vec<N>,
    edge_weights: Vec<E>
    //
    // embedding
}

impl<N, E> PlanarMap<N, E> {

    pub fn new() -> PlanarMap<N, E> {
        PlanarMap {
            vertices: 0,
            edges: 0,
            vertices_by_edges: Vec::new(),
            edges_by_vertices: Vec::new(),
            vertex_weights: Vec::new(),
            edge_weights: Vec::new()
        }
    }

    pub fn add_vertex(&mut self, weight: N) -> VertexIndex {
        self.vertices += 1;
        self.vertex_weights.push(weight);
        self.edges_by_vertices.push(Vec::new());
        return VertexIndex(self.vertices - 1);
    }

    pub fn add_edge(&mut self, v1: VertexIndex, v2: VertexIndex, weight: E) -> usize {
        self.edges += 1;
        let e = self.edges - 1;
        self.vertices_by_edges.push((v1, v2));
        self.edges_by_vertices[v1.0].push(EdgeIndex(e));
        self.edges_by_vertices[v2.0].push(EdgeIndex(e));
        self.edge_weights.push(weight);
        return e;
    }

    pub fn get_edge(&self, e: EdgeIndex) -> (VertexIndex, VertexIndex, &E) {
        let (v1, v2) = self.vertices_by_edges[e.0];
        (v1, v2, &self.edge_weights[e.0])
    }

}