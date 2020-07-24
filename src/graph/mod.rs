use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use itertools::{Itertools, any, merge};
use array_tool::vec::Intersect;
use std::cell::RefCell;
use crate::graph::EdgeEnd::{Head, Tail};
use crate::graph::Signum::{Forward, Backward};
use crate::graph::guarded_map::{GuardedMap, Index, Ideable};
use crate::util::iterators::cyclic::{CyclicIterable};
use crate::graph::ClockDirection::{CW, CCW};
use std::fmt::{Debug, Formatter};
use core::fmt;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ClockDirection {
    CW, CCW
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

    pub fn get_vertex(&self, end: EdgeEnd) -> VertexI {
        match end {
            Head => self.head,
            Tail => self.tail
        }
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

/*impl<E> Debug for Edge<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "v[{}] ==========> v[{}]", self.tail.0, self.head.0)
    }
}*/

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

    pub fn is_embedded(&self) -> bool { self.embedded }

    pub fn edge_count(&self) -> usize { self.edges.get_map().len() }

    pub fn vertex_count(&self) -> usize { self.vertices.get_map().len() }

    pub fn face_count(&self) -> usize {
        if !self.embedded {
            panic!("not embedded");
        }

        self.faces.get_map().len()
    }

    pub fn add_vertex(&mut self, weight: N) -> VertexI {
        if self.embedded {
            panic!("superplanar operation on embedded graph.");
        }

        let v = Vertex {
            id: VertexI(0), neighbors: Vec::new(), weight
        };

        let index = self.vertices.retrieve_index(v);
        self.vertices.get_mut(&index).id = index;
        return index;
    }

    pub fn add_edge(&mut self, v1: VertexI, v2: VertexI, weight: E) -> Result<EdgeI, &str> {
        if self.embedded {
            panic!("superplanar operation on embedded graph.")
        }

        return self.add_edge_(v1, v2, weight);
    }

    fn add_edge_(&mut self, v1: VertexI, v2: VertexI, weight: E) -> Result<EdgeI, &str> {

        if self.enforce_simple {
            if v1 == v2 {
                return Err("With this edge, the graph would not be simple any longer ('enforce_simple').");
            }
            if let Some(_) = self.vertices.get(&v1).get_nb(v2) {
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
        if !self.embedded {
            panic!("no embedding given");
        }

        let f = self.face(face);

        let (pos1, pos2, nb_index_1, nb_index_2) = {
            let knee1 = match self.get_knee_by_face(face, v1) {
                Some(k) => k,
                None => panic!("invalid face for insertion")
            };
            let knee2 = match self.get_knee_by_face(face, v2) {
                Some(k) => k,
                None => panic!("invalid face for insertion")
            };
            (knee1.1, knee2.1, knee1.3.index, knee2.3.index)
        };

        if let Ok(e) = self.add_edge_(v1, v2, weight) {

            // NbVertex structs were added at the end, which is not correct
            {
                let v1_mut = self.vertex_mut(v1);
                let nb = v1_mut.neighbors.pop().unwrap();
                v1_mut.neighbors.insert(nb_index_1, nb);
                self.restore_nb_indices(v1);

                let v2_mut = self.vertex_mut(v2);
                let nb = v2_mut.neighbors.pop().unwrap();
                v2_mut.neighbors.insert(nb_index_2, nb);
                self.restore_nb_indices(v2);
            }

            let old_face = self.faces.free_index(&face).unwrap();

            let mut f1 = Face {
                id: FaceI(0),
                weight: old_face.weight.clone(),
                angles: Vec::new()
            };

            let mut f2 = Face {
                id: FaceI(0),
                weight: old_face.weight.clone(),
                angles: Vec::new()
            };

            for &vid in old_face.angles.cycle(pos1, false) {
                f1.angles.push(vid);
                if vid == v2 { break }
            }

            for &vid in old_face.angles.cycle(pos2, false) {
                f2.angles.push(vid);
                if vid == v1 { break }
            }

            let fid1 = self.faces.retrieve_index(f1);
            let fid2 = self.faces.retrieve_index(f2);

            //self.edges.get_mut(&e).right_face = Some(id1);
            //self.edges.get_mut(&e).left_face = Some(id2);

            self.restore_face_refs(fid1);
            self.restore_face_refs(fid2);

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
        self.vertex_mut(v1).neighbors.retain(|nb| nb.other != v2);
        self.vertex_mut(v2).neighbors.retain(|nb|nb.other != v1);

        self.restore_nb_indices(v1);
        self.restore_nb_indices(v2);

        return Some(e);
    }

    pub fn remove_edge(&mut self, v1: VertexI, v2: VertexI) -> Option<EdgeI> {
        if self.embedded {
            panic!("superplanar operation on embedded graph.")
        }

        self.remove_edge_(v1, v2).map(|e|e.id)
    }

    pub fn remove_embedded_edge_by_id(&mut self, eid: EdgeI, merge_weights: &Fn(F, F) -> F) -> Option<(EdgeI, FaceI)> {
        let e = self.edge(eid);
        self.remove_embedded_edge(e.tail, e.head, merge_weights)
    }

    pub fn remove_embedded_edge(&mut self, v1: VertexI, v2: VertexI, merge_weights: &Fn(F, F) -> F) -> Option<(EdgeI, FaceI)> {
        if !self.embedded {
            panic!("no embedding given");
        }

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

        let mut cycle: Vec<_> = right_face.angles.cycle(right_tail_pos, false).skip(1).collect();
        cycle.extend(left_face.angles.cycle(left_head_pos, false).skip(1));

        let f = Face {
            id: FaceI(0),
            angles: cycle.into_iter().copied().collect(),
            weight
        };

        let fid = self.faces.retrieve_index(f);
        self.restore_face_refs(fid);

        return Some((e.id, fid));
    }

    pub fn next_edge(&self, vid: VertexI, eid: EdgeI, direction: ClockDirection) -> Option<EdgeI> {
        let v = self.vertex(vid);
        let pos = v.neighbors.iter().find(|nb|nb.edge == eid).unwrap().index;

        match direction {
            CW => v.neighbors.cycle(pos, false).nth(1).map(|nb|nb.edge),
            CCW => v.neighbors.cycle(pos, false).rev().next().map(|nb|nb.edge)
        }
    }

    pub fn contract_embedded_edge(&mut self, eid: EdgeI, merge_weights: &Fn(&E, &E) -> E) {
        unsafe {
            self.contract_embedded_edge_(eid, merge_weights);
        }
    }

    /// The head vertex remains
    unsafe fn contract_embedded_edge_(&mut self, eid: EdgeI, merge_weights: &Fn(&E, &E) -> E) {
        let s: *mut Self = self;

        {
            let e = self.edge(eid);

            let tail_cw = self.next_edge(e.tail, eid, CW);
            let tail_ccw = self.next_edge(e.tail, eid, CCW);
            let head_cw = self.next_edge(e.head, eid, CW);
            let head_ccw = self.next_edge(e.head, eid, CCW);

            if let None = tail_cw.and(tail_ccw).and(head_cw).and(head_ccw) {
                panic!("No neighbour");
            }

            let tail_right = self.edge(tail_cw.unwrap());
            let tail_left = self.edge(tail_ccw.unwrap());
            let head_right = self.edge(head_ccw.unwrap());
            let head_left = self.edge(head_cw.unwrap());

            let right_face_collapse = tail_right.get_other(e.tail) == head_right.get_other(e.head);
            let left_face_collapse = tail_left.get_other(e.tail) == head_left.get_other(e.head);

            let head_single_edge = head_cw == head_ccw;
            let tail_single_edge = tail_cw == tail_ccw;

            let right_face_merge: Box<Fn(_, _) -> _> = match tail_right.get_signum_by_tail(e.tail) {
                Forward => Box::new(|a, b| a),
                Backward => Box::new(|a, b| b)
            };

            let left_face_merge: Box<Fn(_, _) -> _> = match tail_left.get_signum_by_tail(e.tail) {
                Forward => Box::new(|a, b| b),
                Backward => Box::new(|a, b| a)
            };

            if right_face_collapse {
                let removed_edge = tail_right;
                let weight = merge_weights(&removed_edge.weight, &head_right.weight);
                (*s).edge_mut(head_right.id).weight = weight;
                (*s).remove_embedded_edge_by_id(removed_edge.id, &right_face_merge);
            }

            if left_face_collapse && !tail_single_edge {
                (*s).remove_embedded_edge(e.tail, tail_left.get_other(e.tail), &left_face_merge);
            }
        }

        let dropped_edge = self.edges.free_index(&eid).unwrap();
        let dropped_vertex = self.vertices.free_index(&dropped_edge.tail).unwrap();

        // patch edges at the tail of the contracted edge and their adjacent vertices
        for nb in dropped_vertex.neighbors.iter().filter(|nb| nb.other != dropped_edge.head) {
            let patch_e = (*s).edge_mut(nb.edge);
            let patch_v = match nb.end {
                Tail => {
                    patch_e.tail = dropped_edge.head;
                    (*s).vertex_mut(patch_e.head)
                },
                Head => {
                    patch_e.head = dropped_edge.head;
                    (*s).vertex_mut(patch_e.tail)
                }
            };

            for patch_nb in patch_v.neighbors.iter_mut().filter(|nb|nb.other == dropped_edge.tail) {
                patch_nb.other = dropped_edge.head;
            }
        }

        // insert the fan of neighbors previously attached to e.tail now to e.head
        {
            let mut v = self.vertex_mut(dropped_edge.head);
            let index = v.get_nb(dropped_edge.tail).unwrap().index;
            v.neighbors.remove(index);
            v.neighbors.splice(index..index, dropped_vertex.neighbors.into_iter().filter(|nb| nb.other != dropped_edge.head));
        }
        self.restore_nb_indices(dropped_edge.head);

        // remove tail vertex from faces adjacent to the contracted edge
        self.face_mut(dropped_edge.left_face.unwrap()).angles.retain(|v| v != &dropped_edge.tail);
        self.face_mut(dropped_edge.right_face.unwrap()).angles.retain(|v| v != &dropped_edge.tail);

        // special case: remaining degree of tail vertex is 1 (<=> left and right face of dropped edge are equal)
        if dropped_edge.left_face.unwrap() == dropped_edge.right_face.unwrap() {
            let fid = dropped_edge.left_face.unwrap();

            let pos = self.face(fid).angles.iter().position(|&v| v == dropped_edge.head);
            self.face_mut(fid).angles.remove(pos.unwrap());

        // normal case
        } else {

            // patch angles in faces adjacent to the removed tail vertex
            for eid in self.vertex(dropped_edge.head).neighbors.iter().map(|nb| nb.edge).collect_vec() {
                let dir = self.edge(eid).get_signum_by_tail(dropped_edge.head);

                let f = match dir {
                    Forward => self.face_mut(self.edge(eid).left_face.unwrap()),
                    Backward => self.face_mut(self.edge(eid).right_face.unwrap())
                };

                if let Some((i, _)) = f.angles.iter().find_position(|v| v == &&dropped_edge.tail) {
                    f.angles[i] = dropped_edge.head;
                }
            }
        }
    }

    /// Dirty function, can also panic instead of returning false
    pub fn check_referential_integrity(&self) -> bool {

        let mut edge_count = 0;
        let mut half_edge_count = 0;

        for v in self.vertices.get_map().values() {
            for nb in v.neighbors.iter() {
                half_edge_count += 1;
                let e = self.edge(nb.edge);

                match nb.end {
                    Tail => if e.tail != v.id { return false },
                    Head => if e.head != v.id { return false },
                };

                self.vertex(e.get_other(v.id));
            }
        }

        for e in self.edges.get_map().values() {
            edge_count += 1;
            self.vertex(e.tail);
            self.vertex(e.head);
        }

        // handshake lemma
        if half_edge_count != 2 * edge_count {
            return false;
        }

        if self.embedded {

            for e in self.edges.get_map().values() {
                if let Some(f) = e.left_face {
                    self.face(f);
                } else {
                    return false;
                }
                if let Some(f) = e.right_face {
                    self.face(f);
                } else {
                    return false;
                }
            }

            for f in self.faces.get_map().values() {
                for &vid in f.angles.iter() {
                    self.vertex(vid);
                }
            }

        }

        return true;
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

    pub fn get_dual(&self) -> PlanarMap<FaceI, EdgeI, VertexI> {

        let mut dual_map = PlanarMap::new();

        let mut face_to_vertex = HashMap::new();

        for f in self.faces.get_map().values() {
            let dual_vid = dual_map.add_vertex(f.id);
            face_to_vertex.insert(f.id, dual_vid);
        }

        for (vid, fid) in dual_map.vertices.get_map().values().map(|v| (v.id, v.weight)).collect_vec() {
            let f = self.face(fid);
            for (&v1, &v2) in f.angles.cycle(0, true).tuple_windows() {
                let e = self.edge(self.get_edge(v1, v2).unwrap());
                let other_face = if f.id == e.left_face.unwrap() {
                    e.right_face
                } else {
                    e.left_face
                }.unwrap();

                let other_vertex = face_to_vertex.get(&other_face).unwrap();

                dual_map.add_edge(vid, *other_vertex, e.id);
            }
        }

        let mut face_cycles = vec![];

        for v in self.vertices.get_map().values() {
            let cycle = v.neighbors.iter().map(|nb| {
                let f = match nb.end {
                    Tail => self.edge(nb.edge).left_face.unwrap(),
                    Head => self.edge(nb.edge).right_face.unwrap()
                };
                *face_to_vertex.get(&f).unwrap()
            }).collect_vec();

            face_cycles.push((cycle, v.id));
        }

        dual_map.set_embedding(face_cycles);

        return dual_map;
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

    fn face_mut(&mut self, f: FaceI) -> &mut Face<F> {
        self.faces.get_mut(&f)
    }

    /// Unchecked retrieval of the vertex struct for a given vertex index
    fn vertex(&self, v: VertexI) -> &Vertex<N> {
        self.vertices.get(&v)
    }

    fn vertex_mut(&mut self, v: VertexI) -> &mut Vertex<N> {
        self.vertices.get_mut(&v)
    }

    fn restore_nb_indices(&mut self, vid: VertexI) {
        let v = self.vertex_mut(vid);
        for i in 0..v.neighbors.len() {
            v.neighbors[i].index = i;
        }
    }

    fn restore_face_refs(&mut self, fid: FaceI) {
        let face = self.face(fid);

        let boundary_cycle = face.angles.cycle(0, true)
            .tuple_windows()
            .map(|(&v1, &v2)| self.get_edge_with_signum(v1, v2))
            .collect_vec();

        for (eid, signum) in boundary_cycle {
            match signum {
                Forward => self.edge_mut(eid).left_face = Some(fid),
                Backward => self.edge_mut(eid).right_face = Some(fid)
            }
        }
    }

    fn get_edge_with_signum(&self, v1: VertexI, v2: VertexI) -> (EdgeI, Signum) {
        let e = self.get_edge(v1, v2).unwrap();
        let signum = self.get_signum(e, v1, v2);
        (e, signum)
    }

    /// Unchecked retrieval of signum
    fn get_signum(&self, e: EdgeI, v1: VertexI, v2: VertexI) -> Signum {
        self.edge(e).get_signum(v1, v2)
    }

    /// The order of NbVertex is in face definition order (ccw), or seen from the vertex in cw order.
    fn get_knee_by_face(&self, fid: FaceI, vid: VertexI) -> Option<(&Vertex<N>, usize, &NbVertex, &NbVertex)> {
        if !self.embedded {
            panic!("no embedding given");
        };

        if !self.face(fid).angles.contains(&vid) {
            return None;
        }

        let idx = self.face(fid).angles.iter().position(|&v| v == vid).unwrap();
        let v = self.vertex(vid);

        let nbs = v.neighbors.iter()
            .filter(|nb| { let e = self.edge(nb.edge);
                e.right_face.unwrap() == fid || e.left_face.unwrap() == fid
            }).collect_vec();

        if nbs.len() == 1 {
            return Some((v, idx, nbs[0], nbs[0]));
        } else if nbs.len() == 2 {
            return Some((v, idx, nbs[0], nbs[1]));
        } else {
            panic!("unexpected number of edges");
        }
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
