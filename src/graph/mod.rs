use std::rc::Rc;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::cmp::min;
use std::hash::Hash;
use itertools::Itertools;
use std::borrow::Borrow;
use crate::graph::EdgeEnd::{Head, Tail};

pub mod schnyder;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct VertexI(pub usize);
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct EdgeI(pub usize);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
enum EdgeEnd {
    Tail, Head
}

struct Edge<E> {
    id: EdgeI,
    tail: VertexI,
    head: VertexI,
    weight: E
}

struct Vertex<N> {
    id: VertexI,
    neighbors: Vec<NbVertex>,
    weight: N
}

struct NbVertex {
    other: VertexI,
    edge: EdgeI,
    end: EdgeEnd
}


pub struct PlanarMap<N, E> {
    vertices: usize,
    edges: usize,
    //
    vertices_by_edges: Vec<(VertexI, VertexI)>,
    neighbors: Vec<Vec<(EdgeI, VertexI, EdgeEnd)>>,
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
            neighbors: Vec::new(),
            vertex_weights: Vec::new(),
            edge_weights: Vec::new(),
            enforce_simple: true
        }
    }

    pub fn add_vertex(&mut self, weight: N) -> VertexI {
        self.vertices += 1;
        self.vertex_weights.push(weight);
        self.neighbors.push(Vec::new());
        return VertexI(self.vertices - 1);
    }

    pub fn add_edge(&mut self, v1: VertexI, v2: VertexI, weight: E) -> Result<EdgeI, &str> {
        if self.enforce_simple && (v1 == v2 || self.vertices_by_edges.contains(&(v1, v2))) {
            return Err("With this edge, the graph would not be simple any longer ('enforce_simple').");
        }

        self.edges += 1;
        let e = self.edges - 1;

        // v1 --> v2 (v1 = tail, v2 = head)
        self.vertices_by_edges.push((v1, v2));
        self.neighbors[v1.0].push((EdgeI(e), v2, Tail));
        self.neighbors[v2.0].push((EdgeI(e), v1, Head));
        self.edge_weights.push(weight);
        return Ok(EdgeI(e));
    }

    pub fn is_valid_vertex(&self, v: &VertexI) -> bool {
        return v.0 < self.vertices;
    }

    pub fn is_valid_edge(&self, e: &EdgeI) -> bool {
        return e.0 < self.edges;
    }

    pub fn has_edge(&self, e: (&VertexI, &VertexI)) -> bool {
        return self.neighbors[(e.0).0].iter().any(|p| match p { (_, v, _) => v == e.1 })
    }

    pub fn get_edge_(&self, e: (&VertexI, &VertexI)) -> Option<EdgeI> {
        self.neighbors[(e.0).0].iter()
            .filter(|p| match p { (_, v, _) => v == e.1 })
            .map(|p| p.0)
            .find(|p| true)
    }

    pub fn get_edge(&self, e: EdgeI) -> (VertexI, VertexI, &E) {
        let (v1, v2) = self.vertices_by_edges[e.0];
        (v1, v2, &self.edge_weights[e.0])
    }

    pub fn is_empty(&self) -> bool { self.vertices == 0 }
    pub fn is_singleton(&self) -> bool { self.vertices == 1 }

    pub fn is_simple(&self) -> bool {
        let loops = self.vertices_by_edges.iter().any(|(a,b)| a.0 == b.0);
        let unique_count = self.vertices_by_edges.iter().unique().count();

        unique_count == self.edges && !loops
    }

    pub fn is_connected(&self) -> bool {
        if self.is_empty() || self.is_singleton() {
            return true;
        }

        let mut visited = vec![false; self.vertices];
        let mut to_visit = vec![&VertexI(0)];

        while !to_visit.is_empty() {
            let v = to_visit.remove(to_visit.len() - 1);
            visited[v.0] = true;
            for (_, nb, _) in &self.neighbors[v.0] {
                if !visited[nb.0] {
                    to_visit.push(nb);
                }
            }
        }

        !visited.contains(&false)
    }

    fn is_forward(&self, e: EdgeI, t: (VertexI, VertexI)) -> Result<bool, &str> {
        let (v1, v2) = self.vertices_by_edges[e.0];
        if v1 == t.0 && v2 == t.1 {
            Ok(true)
        } else if v1 == t.1 && v2 == t.0 {
            Ok(false)
        } else {
            Err("Forward check invalid")
        }
    }

    /*fn vec_equal_as_sets<I: Eq+Hash>(vec1: &Vec<I>, vec2: &Vec<I>) -> bool {
        HashSet::from_iter::<I>(vec1.iter()) == HashSet::from_iter::<I>(vec2.iter())
    }*/

    /// Gives the graph an embedding into the sphere (i.e. no outer face is selected). The argument
    /// `faces` is a vector of all face cycles. The face cycles are vectors of vertex indices,
    /// specifying the order of vertices around the face in counterclockwise direction.
    pub fn set_embedding(&mut self, faces: Vec<Vec<VertexI>>) -> Result<(), String> {
        if !(self.is_simple() && self.is_connected()) {
            return Err(String::from("Embeddings can only be set if the graph is simple and connected"));
        }


        let v: Vec<_> = faces.iter().flat_map(|fc|fc.iter()).filter(|v| !self.is_valid_vertex(v)).collect();

        if !v.is_empty() {
            return Err(String::from(format!("Invalid vertex index found, {} for instance.", v[0].0)));
        }

        if self.vertices as isize - self.edges as isize + faces.len() as isize != 2 {
            return Err(format!("The number of faces for a full embedding should be {}, you have given {}.", 2 - self.vertices as isize + self.edges as isize, faces.len()));
        }

        let mut angles: Vec<_> = (0..self.vertices).map(|i| HashMap::new()).collect();
        let mut fwd_occurence: Vec<_> = (0..self.edges).map(|i| false).collect();
        let mut backwd_occurence: Vec<_> = (0..self.edges).map(|i| false).collect();

        for face_cycle_vec in &faces {
            let l = face_cycle_vec.len();
            for i in 0..l {
                // abc-ccw-knee (c directly follows a ccw in b's neighborhood)
                let a = face_cycle_vec[i];
                let b = face_cycle_vec[(i+1) % l];
                let c = face_cycle_vec[(i+2) % l];

                angles[b.0].insert(a, c);

                // covered edges
                let e = self.get_edge_((&a, &b));
                if e.is_none() {
                    return Err(format!("The edge ({}, {}) is contained in a face cycle but does not exist in the graph.", a.0, b.0))
                }

                let forward = self.is_forward(e.unwrap(), (a, b))?;
                if forward {
                    fwd_occurence[e.unwrap().0] = true;
                } else {
                    backwd_occurence[e.unwrap().0] = true;
                }
            }
        }

        // check if each edge occurs exactly twice, one time as (a,b), one time as (b,a)
        if fwd_occurence.iter().any(|b| !b) {
            return Err(String::from("It is wrong forwardly"));
        }
        if backwd_occurence.iter().any(|b| !b) {
            return Err(String::from("It is wrong backwardly"));
        }



        // check if all vertices have a well-defined total order on their neighbors now
        for i in 0..self.vertices {
            let vertex_angles = &angles[i];

            if vertex_angles.len() != self.neighbors[i].len() {
                return Err(String::from("total order fail (count)"));
            }

            {
                let neighbors: HashSet<&VertexI> = self.neighbors[i].iter().map(|(_, v, _)| v).collect();
                let angle_entering: HashSet<&VertexI> = vertex_angles.iter().map(|(u, _)| u).collect();
                let angle_leaving: HashSet<&VertexI> = vertex_angles.iter().map(|(_, v)| v).collect();

                if neighbors != angle_entering {
                    return Err(String::from("total order fail (entering)"));
                }

                if neighbors != angle_leaving {
                    return Err(String::from("total order fail (leaving)"));
                }
            }

            let len = self.neighbors[i].len();
            if len > 1 {
                let old_neighbors = self.neighbors[i].clone();
                let begin = self.neighbors[i][0];
                let mut cur = begin;
                self.neighbors[i].clear();

                for _ in 0..len {
                    let next_vector = vertex_angles.get(&cur.1).unwrap();
                    let next_nb_tuble = old_neighbors.iter().find(|(_,v,_)| v == next_vector).unwrap();
                    self.neighbors[i].push(*next_nb_tuble);
                    cur = *next_nb_tuble;
                }
            }
        }

        Ok(())
    }

}

/*impl<N: SchnyderNodeTrait, E: SchnyderEdgeTrait> PlanarMap<N, E> {

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
}*/