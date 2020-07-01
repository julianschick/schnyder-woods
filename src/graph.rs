use std::rc::Rc;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::cmp::min;
use std::hash::Hash;
use itertools::Itertools;
use std::borrow::Borrow;

#[derive(Debug)]
pub enum SchnyderColor {
    Red, Green, Blue
}

#[derive(Debug)]
pub enum SchnyderEdge {
    Black,
    Unicolored(SchnyderColor),
    Bicolored(SchnyderColor, SchnyderColor)
}

trait SchnyderEdgeTrait {}
impl SchnyderEdgeTrait for SchnyderEdge {}


trait SchnyderNodeTrait {}
impl SchnyderNodeTrait for SchnyderNode {}

#[derive(Debug)]
pub enum SchnyderNode {
    Normal(usize),
    Suspension(SchnyderColor)
}


#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct VertexIndex(pub usize);
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct EdgeIndex(pub usize);

pub struct PlanarMap<N, E> {
    vertices: usize,
    edges: usize,
    //
    vertices_by_edges: Vec<(VertexIndex, VertexIndex)>,
    edges_by_vertices: Vec<Vec<EdgeIndex>>,
    neighbors: Vec<Vec<VertexIndex>>,
    //
    vertex_weights: Vec<N>,
    edge_weights: Vec<E>,
    //
    // enforce properties
    enforce_simple: bool
}

impl<N, E> PlanarMap<N, E> {

    pub fn new() -> PlanarMap<N, E> {
        PlanarMap {
            vertices: 0,
            edges: 0,
            vertices_by_edges: Vec::new(),
            edges_by_vertices: Vec::new(),
            neighbors: Vec::new(),
            vertex_weights: Vec::new(),
            edge_weights: Vec::new(),
            enforce_simple: true
        }
    }

    pub fn add_vertex(&mut self, weight: N) -> VertexIndex {
        self.vertices += 1;
        self.vertex_weights.push(weight);
        self.edges_by_vertices.push(Vec::new());
        self.neighbors.push(Vec::new());
        return VertexIndex(self.vertices - 1);
    }

    pub fn add_edge(&mut self, v1: VertexIndex, v2: VertexIndex, weight: E) -> Result<EdgeIndex, &str> {
        if self.enforce_simple && (v1 == v2 || self.vertices_by_edges.contains(&(v1, v2))) {
            return Err("With this edge, the graph would not be simple any longer ('enforce_simple').");
        }

        self.edges += 1;
        let e = self.edges - 1;
        self.vertices_by_edges.push((v1, v2));
        self.edges_by_vertices[v1.0].push(EdgeIndex(e));
        self.edges_by_vertices[v2.0].push(EdgeIndex(e));
        self.neighbors[v1.0].push(v2);
        self.neighbors[v2.0].push(v1);
        self.edge_weights.push(weight);
        return Ok(EdgeIndex(e));
    }

    pub fn is_valid_vertex(&self, v: &VertexIndex) -> bool {
        return v.0 < self.vertices;
    }

    pub fn is_valid_edge(&self, e: &EdgeIndex) -> bool {
        return e.0 < self.edges;
    }

    pub fn has_edge(&self, e: (VertexIndex, VertexIndex)) -> bool {
        return self.neighbors[(e.0).0].contains(&e.1);
    }

    pub fn get_edge(&self, e: EdgeIndex) -> (VertexIndex, VertexIndex, &E) {
        let (v1, v2) = self.vertices_by_edges[e.0];
        (v1, v2, &self.edge_weights[e.0])
    }

    pub fn is_simple(&self) -> bool {
        let loops = self.vertices_by_edges.iter().any(|(a,b)| a.0 == b.0);
        let unique_count = self.vertices_by_edges.iter().unique().count();

        unique_count == self.edges && !loops
    }

    /*fn vec_equal_as_sets<I: Eq+Hash>(vec1: &Vec<I>, vec2: &Vec<I>) -> bool {
        HashSet::from_iter::<I>(vec1.iter()) == HashSet::from_iter::<I>(vec2.iter())
    }*/

    /// Gives the graph an embedding into the sphere (i.e. no outer face is selected). The argument
    /// `faces` is a vector of all face cycles. The face cycles are vectors of vertex indices,
    /// specifying the order of vertices around the face in counterclockwise direction.
    pub fn set_embedding(&mut self, faces: Vec<Vec<VertexIndex>>) -> Result<(), String> {
        let v: Vec<_> = faces.iter().flat_map(|fc|fc.iter()).filter(|v| !self.is_valid_vertex(v)).collect();

        if !v.is_empty() {
            return Err(String::from(format!("Invalid vertex index found, {} for instance.", v[0].0)));
        }

        if self.vertices as isize - self.edges as isize + faces.len() as isize != 2 {
            return Err(format!("The number of faces for a full embedding should be {}, you have given {}.", 2 - self.vertices as isize + self.edges as isize, faces.len()));
        }

        for face_cycle_vec in &faces {
            for i in 0..face_cycle_vec.len() {
                let e = (VertexIndex(i), VertexIndex((i + 1) % face_cycle_vec.len()));
                if !self.has_edge(e) {
                    return Err(format!("The edge ({}, {}) is contained in a face cycle but does not exist in the graph.", (e.0).0, (e.1).0))
                }
            }
        }

        let mut angles = Vec::new();
        for i in  0..self.vertices { angles.push(HashMap::new()) }

        for face_cycle_vec in &faces {
            for i in 0..face_cycle_vec.len() {
                // abc-ccw-knee (c directly follows a ccw in b's neighborhood)
                let a = face_cycle_vec[i];
                let b = face_cycle_vec[(i+1) % face_cycle_vec.len()];
                let c = face_cycle_vec[(i+2) % face_cycle_vec.len()];

                angles[b.0].insert(a, c);
            }
        }

        // check if each edge occurs exactly twice, one time as (a,b), one time as (b,a)
        // TODO

        // check if all vertices have a well-defined total order on their neighbors now
        for i in 0..self.vertices {
            let vertex_angles = &angles[i];

            if vertex_angles.len() != self.neighbors[i].len() {
                return Err(String::from("total order fail (count)"));
            }

            {
                let neighbors: HashSet<&VertexIndex> = self.neighbors[i].iter().collect();
                let angle_entering: HashSet<&VertexIndex> = vertex_angles.iter().map(|(u, v)| u).collect();
                let angle_leaving: HashSet<&VertexIndex> = vertex_angles.iter().map(|(u, v)| v).collect();

                if neighbors != angle_entering {
                    return Err(String::from("total order fail (entering)"));
                }

                if neighbors != angle_leaving {
                    return Err(String::from("total order fail (leaving)"));
                }
            }

            let begin = self.neighbors[i][0];
            let len = self.neighbors[i].len();
            let mut cur = &begin;
            self.neighbors[i].clear();

            for _ in 0..len {
                let next = vertex_angles.get(&cur).unwrap();
                self.neighbors[i].push(*cur);
                cur = next;
            }
        }

        Ok(())
    }

}

impl<N: SchnyderNodeTrait, E: SchnyderEdgeTrait> PlanarMap<N, E> {

    pub fn check_wood(&self) -> bool {
        let v: Vec<_> = self.vertex_weights.into_iter()
            .filter(|v| match v {
                SchnyderNode::Suspension(_) => true,
                _ => false
            })
            .map(|v| {  match v {
                SchnyderNode::Suspension(c) => c,
                _ => SchnyderColor::Red
            }
        }).collect();

        if !(v.len() == 3 &&
        v.contains(&SchnyderColor::Red) &&
        v.contains(&SchnyderColor::Green) &&
        v.contains(&SchnyderColor::Blue)) {
            return false;
        }

        return true;
    }
}