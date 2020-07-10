use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use itertools::Itertools;
use array_tool::vec::Intersect;
use std::cell::RefCell;
use crate::graph::EdgeEnd::{Head, Tail};
use crate::graph::Signum::{Forward, Backward};
use crate::graph::guarded_map::{GuardedMap, Index, Ideable};
use crate::util::cyclic_iterator::cyclic_iterator;
pub mod schnyder;
mod guarded_map;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct VertexI(pub usize);

impl From<usize> for VertexI { fn from(n: usize) -> Self { VertexI(n) } }
impl Into<usize> for VertexI { fn into(self) -> usize { self.0 } }
impl Index for VertexI { }

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct EdgeI(pub usize);

impl From<usize> for EdgeI { fn from(n: usize) -> Self { EdgeI(n) } }
impl Into<usize> for EdgeI { fn into(self) -> usize { self.0 } }
impl Index for EdgeI { }


#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct FaceI(pub usize);

impl From<usize> for FaceI { fn from(n: usize) -> Self { FaceI(n) } }
impl Into<usize> for FaceI { fn into(self) -> usize { self.0 } }
impl Index for FaceI { }

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
enum EdgeEnd {
    Tail, Head
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Signum {
    Forward, Backward
}

impl Signum {
    pub fn reversed(&self) -> Self {
        match self {
            Forward => Backward,
            Backward => Forward
        }
    }
}

struct Edge<E> {
    id: EdgeI,
    tail: VertexI,
    head: VertexI,
    right_face: Option<FaceI>,
    left_face: Option<FaceI>,
    weight: E
}

impl<E> Edge<E> {
    pub fn get_other(&self, this: VertexI) -> VertexI {
        match self.get_signum_by_tail(this) {
            Forward => return self.head,
            Backward => return self.tail
        }
    }

    pub fn is_loop(&self) -> bool {
        self.tail == self.head
    }

    pub fn get_signum_by_tail(&self, v1: VertexI) -> Signum {
        if self.is_loop() {
            panic!("signum not defined")
        }

        if v1 == self.tail { return Forward }
        else if v1 == self.head { return Backward }
        else { panic!("assertion failed") }
    }

    pub fn get_signum(&self, v1: VertexI, v2: VertexI) -> Signum {
        if self.is_loop() {
            panic!("signum not defined")
        }

        if v1 == self.tail && v2 == self.head {
            return Forward;
        } else if v1 == self.head && v2 == self.tail {
            return Backward;
        } else {
            panic!("Forward check assertion failed");
        }
    }

}

impl<E> PartialEq for Edge<E> {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.tail == other.tail ||
        self.head == other.tail && self.tail == other.head
    }
}

impl<E> Eq for Edge<E> { }

impl<E> Hash for Edge<E> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.head.0 + self.tail.0).hash(state)
    }
}

impl<N> Ideable<EdgeI> for Edge<N> {
    fn get_id(&self) -> EdgeI { self.id }
    fn set_id(&mut self, id: EdgeI) { self.id = id }
}

struct Vertex<N> {
    id: VertexI,
    neighbors: Vec<NbVertex>,
    weight: N
}

impl<N> Ideable<VertexI> for Vertex<N> {
    fn get_id(&self) -> VertexI { self.id }
    fn set_id(&mut self, id: VertexI) { self.id = id }
}

struct Face<F> {
    id: FaceI,
    angles: Vec<VertexI>,
    weight: F
}

impl<N> Ideable<FaceI> for Face<N> {
    fn get_id(&self) -> FaceI { self.id }
    fn set_id(&mut self, id: FaceI) { self.id = id }
}

impl<N> Vertex<N> {

    fn get_nb(&self, other: VertexI) -> Option<&NbVertex> {
        self.neighbors.iter().find(|nb| nb.other == other)
    }
}

#[derive(Clone, Copy, Debug)]
struct NbVertex {
    index: usize,
    other: VertexI,
    edge: EdgeI,
    end: EdgeEnd
}

pub struct PlanarMap<N, E, F: Clone> {
    vertices: GuardedMap<VertexI, Vertex<N>>,
    edges: GuardedMap<EdgeI, Edge<E>>,
    faces: GuardedMap<FaceI, Face<F>>,
    //
    embedded: bool,
    enforce_simple: bool
}

impl<N, E, F: Clone> PlanarMap<N, E, F> {

    pub fn new() -> PlanarMap<N, E, F> {
        PlanarMap {
            vertices: GuardedMap::new(),
            edges: GuardedMap::new(),
            faces: GuardedMap::new(),
            //
            embedded: false,
            enforce_simple: true
        }
    }

    pub fn is_valid_vertex(&self, v: &VertexI) -> bool {
        self.vertices.is_valid_index(&v)
    }

    pub fn is_valid_edge(&self, e: &EdgeI) -> bool {
        self.edges.is_valid_index(&e)
    }

    pub fn get_edge(&self, v1: VertexI, v2: VertexI) -> Option<EdgeI> {
        let v = self.vertices.get(&v1);
        match v.neighbors.iter().find(|nb| nb.other == v2) {
            Some(nb) => Some(nb.edge),
            None => None
        }
    }

    pub fn is_empty(&self) -> bool { self.vertices.get_map().is_empty() }

    pub fn is_singleton(&self) -> bool { self.vertices.get_map().len() == 1 }

    pub fn is_simple(&self) -> bool {
        let loops = self.edges.get_map().values().any(|e| e.is_loop());
        let double_edges = self.edges.get_map().values().unique().count() < self.edges.get_map().len();

        !loops && !double_edges
    }

    pub fn is_connected(&self) -> bool {
        if self.is_empty() || self.is_singleton() {
            return true;
        }

        let mut visited = HashSet::new();
        let mut to_visit = vec![&VertexI(0)];

        while !to_visit.is_empty() {
            let v = to_visit.remove(to_visit.len() - 1);
            visited.insert(v);

            for nb in &self.vertices.get(v).neighbors {
                let v = &nb.other;
                if !visited.contains(&v) {
                    to_visit.push(v);
                }
            }
        }

        visited.len() == self.vertices.get_map().len()
    }

    pub fn add_vertex(&mut self, weight: N) -> VertexI {
        let v = Vertex {
            id: VertexI(0), neighbors: Vec::new(), weight
        };

        let index = self.vertices.retrieve_index(v);
        self.vertices.get_mut(&index).id = index;
        return index;
    }

    pub fn add_edge(&mut self, v1: VertexI, v2: VertexI, weight: E) -> Result<EdgeI, &str> {
        if self.enforce_simple {
            if v1 == v2 {
                return Err("With this edge, the graph would not be simple any longer ('enforce_simple').");
            }
            if let Some(nb) = self.vertices.get(&v1).get_nb(v2) {
                return Err("With this edge, the graph would not be simple any longer ('enforce_simple').");
            }
        }

        let e = Edge { id : EdgeI(0), tail: v1, head: v2, weight, left_face: None, right_face: None };
        let id = self.edges.retrieve_index(e);

        // v1 --> v2 (v1 = tail, v2 = head)
        let l = self.vertex(v1).neighbors.len();
        self.vertices.get_mut(&v1).neighbors.push(
            NbVertex {
                index: l,
                other: v2,
                edge: id,
                end: Tail
            }
        );

        let l = self.vertex(v2).neighbors.len();
        self.vertices.get_mut(&v2).neighbors.push(
            NbVertex {
                index: l,
                other: v1,
                edge: id,
                end: Head
            }
        );

        return Ok(id);
    }

    pub fn add_embedded_edge(&mut self, v1: VertexI, v2: VertexI, weight: E, face: FaceI) -> EdgeI {
        let f = self.face(face);

        let pos1 = f.angles.iter().position(|v| v == &v1);
        let pos2 = f.angles.iter().position(|v| v == &v2);
        let l = f.angles.len();

        if !(pos1.is_some() && pos2.is_some()) {
            panic!("invalid face for insertion");
        }

        if let Ok(e) = self.add_edge(v1, v2, weight) {

            let old_face = self.faces.free_index(&face).unwrap();

            let mut cycle1 = Vec::new();
            let mut cycle2 = Vec::new();

            let mut i = pos1.unwrap();
            while i != pos2.unwrap() {
                cycle1.push(old_face.angles[i]);
                i = (i+1) % l;
            }
            cycle1.push(old_face.angles[pos2.unwrap()]);

            i = pos2.unwrap();
            while i != pos1.unwrap() {
                cycle2.push(old_face.angles[i]);
                i = (i+1) % l;
            }
            cycle2.push(old_face.angles[pos1.unwrap()]);

            let f1 = Face {
                id: FaceI(0),
                weight: old_face.weight.clone(),
                angles: cycle1
            };

            let f2 = Face {
                id: FaceI(0),
                weight: old_face.weight.clone(),
                angles: cycle2
            };

            let id1 = self.faces.retrieve_index(f1);
            let id2 = self.faces.retrieve_index(f2);

            self.edges.get_mut(&e).right_face = Some(id1);
            self.edges.get_mut(&e).left_face = Some(id2);


            return e;
        } else {
            panic!("edge insertion fail");
        }
    }

    fn remove_edge_(&mut self, v1: VertexI, v2: VertexI) -> Option<Edge<E>> {
        let eid = match self.get_edge(v1, v2) {
            Some(eid) => eid,
            None => return None
        };

        let e = self.edges.free_index(&eid).unwrap();

        {
            let mut v1_vertex = self.vertex_mut(v1);
            v1_vertex.neighbors.retain(|nb| nb.other != v2);
        }
        {
            let mut v2_vertex = self.vertex_mut(v2);
            v2_vertex.neighbors.retain(|nb|nb.other != v1);
        }

        return Some(e);
    }

    pub fn remove_edge(&mut self, v1: VertexI, v2: VertexI) -> Option<EdgeI> {
        self.remove_edge_(v1, v2).map(|e|e.id)
    }

    pub fn remove_embedded_edge(&mut self, v1: VertexI, v2: VertexI, merge_weights: &Fn(F, F) -> F) -> Option<EdgeI> {
        let e = match self.remove_edge_(v1, v2) {
            Some(e) => e,
            None => return None,
        };

        // two faces must be merged
        let left_face = self.faces.free_index(&e.left_face.unwrap()).unwrap();
        let right_face = self.faces.free_index(&e.right_face.unwrap()).unwrap();

        let weight = merge_weights(left_face.weight, right_face.weight);

        let right_tail_pos = right_face.angles.iter().position(|vid|*vid == e.tail).unwrap();
        let left_head_pos = left_face.angles.iter().position(|vid|*vid == e.head).unwrap();

        let i = cyclic_iterator(&right_face.angles, right_tail_pos).unwrap();

        let mut cycle = i.collect();


        let f = Face {
            id: FaceI(0),
            angles: cycle,
            weight
        };


        return Some(e.id);
    }

    /// Gives the graph an embedding into the sphere (i.e. no outer face is selected). The argument
  /// `faces` is a vector of all face cycles. The face cycles are vectors of vertex indices,
  /// specifying the order of vertices around the face in counterclockwise direction.
    pub fn set_embedding(&mut self, faces: Vec<(Vec<VertexI>, F)>) -> Result<(), String> {
        if !(self.is_simple() && self.is_connected()) {
            return Err(String::from("Embeddings can only be set if the graph is simple and connected"));
        }

        if let Some(v) = faces.iter().flat_map(|(fc, w)| fc.iter()).find(|v| !self.is_valid_vertex(v)) {
            return Err(String::from(format!("Invalid vertex index found, {} for instance.", v.0)));
        }

        let n = self.vertices.get_map().len() as isize;
        let m = self.edges.get_map().len() as isize;
        let f = faces.len() as isize;

        if n - m + f != 2 {
            return Err(format!("The number of faces for a full embedding should be {}, you have given {}.", 2 - n + m, f));
        }

        let mut angles = HashMap::new();
        let mut fwd_occurence = HashSet::new();
        let mut backwd_occurence = HashSet::new();

        for (face_cycle_vec, weight) in &faces {
            let l = face_cycle_vec.len();
            for i in 0..l {
                // abc-ccw-knee (c directly follows a ccw in b's neighborhood)
                let a = face_cycle_vec[i];
                let b = face_cycle_vec[(i+1) % l];
                let c = face_cycle_vec[(i+2) % l];

                if !angles.contains_key(&b) { angles.insert(b, HashMap::new()); };
                angles.get_mut(&b).unwrap().insert(a, c);

                // covered edges
                let e = self.get_edge(a, b);
                if e.is_none() {
                    return Err(format!("The edge ({}, {}) is contained in a face cycle but does not exist in the graph.", a.0, b.0))
                }

                match self.get_signum(e.unwrap(), a, b) {
                    Forward => fwd_occurence.insert(e.unwrap()),
                    Backward => backwd_occurence.insert(e.unwrap())
                };
            }
        }

        // check if each edge occurs exactly twice, one time as (a,b), one time as (b,a)
        if fwd_occurence.len() < self.edges.get_map().len() {
            return Err(String::from("It is wrong forwardly"));
        }
        if backwd_occurence.len() < self.edges.get_map().len() {
            return Err(String::from("It is wrong backwardly"));
        }

        // check if all vertices have a well-defined total order on their neighbors now
        for i in self.vertices.get_map().keys().copied().collect_vec() {
            let mut v = self.vertices.get_mut(&i);
            let vertex_angles = angles.get(&v.id).unwrap();

            if vertex_angles.len() != v.neighbors.len() {
                return Err(String::from("total order fail (count)"));
            }

            {
                let neighbors: HashSet<&VertexI> = v.neighbors.iter().map(|nb| &nb.other).collect();
                let angle_entering: HashSet<&VertexI> = vertex_angles.iter().map(|(u, _)| u).collect();
                let angle_leaving: HashSet<&VertexI> = vertex_angles.iter().map(|(_, v)| v).collect();

                if neighbors != angle_entering {
                    return Err(String::from("total order fail (entering)"));
                }

                if neighbors != angle_leaving {
                    return Err(String::from("total order fail (leaving)"));
                }
            }

            let len = v.neighbors.len();

            // for 0 or 1 neighbors no rearranging is necessary
            if len > 1 {
                let mut old_neighbors = v.neighbors.clone();
                let begin = &old_neighbors[0];
                let mut cur = begin;
                v.neighbors.clear();

                for _ in 0..len {
                    let next_vector = vertex_angles.get(&cur.other).unwrap();
                    let mut next_nb = old_neighbors.iter_mut().find(|nb| nb.other == *next_vector).unwrap();
                    next_nb.index = v.neighbors.len();
                    v.neighbors.push(*next_nb);
                    cur = next_nb;
                }
            }
        }

        // create face structs and set left_/right_face of edges
        for (face_cycle_vec, weight) in faces {
            let l = face_cycle_vec.len();
            let f = Face { id: FaceI(0), angles: face_cycle_vec.clone(), weight };
            let id = self.faces.retrieve_index(f);

            for i in 0..l {
                let v1 = face_cycle_vec[i];
                let v2 = face_cycle_vec[(i+1)%l];
                let edge = self.edge_mut(self.get_edge(v1, v2).unwrap());

                match edge.get_signum(v1, v2) {
                    Forward => edge.left_face = Some(id),
                    Backward => edge.right_face = Some(id)
                }
            }
        }


        self.embedded = true;
        Ok(())
    }

    /// Unchecked retrieval of the edge struct for a given edge index
    fn edge(&self, e: EdgeI) -> &Edge<E> {
        self.edges.get(&e)
    }

    fn edge_mut(&mut self, e: EdgeI) -> &mut Edge<E> {
        self.edges.get_mut(&e)
    }

    fn face(&self, f: FaceI) -> &Face<F> {
        self.faces.get(&f)
    }

    /// Unchecked retrieval of the vertex struct for a given vertex index
    fn vertex(&self, v: VertexI) -> &Vertex<N> {
        self.vertices.get(&v)
    }

    fn vertex_mut(&mut self, v: VertexI) -> &mut Vertex<N> {
        self.vertices.get_mut(&v)
    }

    /// Unchecked retrieval of signum
    fn get_signum(&self, e: EdgeI, v1: VertexI, v2: VertexI) -> Signum {
        self.edge(e).get_signum(v1, v2)
    }

    fn get_knee(&self, e1: EdgeI, e2: EdgeI) -> Option<(&Vertex<N>, &NbVertex, &NbVertex)> {
        if !self.embedded {
            panic!("no embedding given");
        }

        let e1 = self.edge(e1);
        let e2 = self.edge(e2);

        let vertex_intersection = vec![e1.tail, e1.head].intersect(vec![e2.tail, e2.head]);
        match vertex_intersection.len() {
            0 => return None,
            1 => {},
            _ => panic!("assertion failed")
        };

        let e1_signum = e1.get_signum_by_tail(vertex_intersection[0]);
        let e2_signum = e2.get_signum_by_tail(vertex_intersection[0]);

        let v = self.vertex(vertex_intersection[0]);

        let nb1 = v.get_nb(e1.get_other(v.id)).unwrap();
        let nb2 = v.get_nb(e2.get_other(v.id)).unwrap();
        let l = v.neighbors.len();

        if l < 3 { panic!("knee not uniquely determined"); }

        if (nb1.index + 1) % l == nb2.index {
            return Some((v, nb1, nb2));
        }

        if (nb2.index + 1) % l == nb1.index {
            return Some((v, nb2, nb1));
        }

        return None;
    }

}
