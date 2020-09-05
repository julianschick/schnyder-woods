use core::fmt;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::iter;
use std::iter::FromIterator;

use array_tool::vec::Intersect;
use itertools::{any, Itertools, merge};
use petgraph::algo::has_path_connecting;
use petgraph::{Graph, EdgeType, Undirected};
use petgraph::graph::{IndexType, NodeIndex};

use crate::graph::ClockDirection::{CCW, CW};
use crate::graph::EdgeEnd::{Head, Tail};
use crate::graph::guarded_map::{GuardedMap, Ideable, Index};
use crate::graph::Signum::{Backward, Forward};
use crate::util::iterators::cyclic::CyclicIterable;
use crate::graph::Side::{Left, Right};

pub mod schnyder;
pub mod io;

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
pub enum EdgeEnd {
    Tail, Head
}

impl EdgeEnd {
    pub fn inverted(&self) -> Self {
        match self { Tail => Head, Head => Tail }
    }
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ClockDirection {
    CW, CCW
}

impl ClockDirection {
    pub fn reversed(&self) -> Self {
        match self {
            CW => CCW,
            CCW => CW
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Side {
    Left, Right
}

impl Side {
    pub fn reversed(&self) -> Self {
        match self {
            Left => Right,
            Right => Left
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

    pub fn to_vertex_pair(&self, signum: Signum) -> (VertexI, VertexI) {
        return swap((self.tail, self.head), signum == Backward)
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

impl<E> Debug for Edge<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "e[{}]: v[{}] ==> v[{}] (L = f[{}], R = f[{}])",
               self.id.0,
               self.tail.0,
               self.head.0,
               match self.left_face { Some(fid) => format!("{}", fid.0), None => "?".to_string()},
               match self.right_face { Some(fid) => format!("{}", fid.0), None => "?".to_string()}
        )
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

impl<N> Debug for Vertex<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "v[{}]: ", self.id.0)?;
        for nb in self.neighbors.iter() {
            write!(f, "v[{}] . ", nb.other.0)?;
        }
        Ok(())
    }
}

impl<N> Ideable<VertexI> for Vertex<N> {
    fn get_id(&self) -> VertexI { self.id }
    fn set_id(&mut self, id: VertexI) { self.id = id }
}

#[derive(Clone)]
struct Face<F> {
    id: FaceI,
    angles: Vec<VertexI>,
    weight: F
}

fn swap<T>(pair: (T, T), do_swap: bool) -> (T, T) {
    if do_swap {
        let (a, b) = pair;
        (b, a)
    } else {
        pair
    }
}

impl<N> Ideable<FaceI> for Face<N> {
    fn get_id(&self) -> FaceI { self.id }
    fn set_id(&mut self, id: FaceI) { self.id = id }
}

impl<N> Vertex<N> {

    fn get_nb(&self, other: VertexI) -> Option<&NbVertex> {
        self.neighbors.iter().find(|nb| nb.other == other)
    }

    fn get_nb_mut(&mut self, other: VertexI) -> Option<&mut NbVertex> {
        self.neighbors.iter_mut().find(|nb| nb.other == other)
    }

    /// does not include start index
    fn get_iterator<'a>(&'a self, start_index: usize, direction: ClockDirection) -> Box<dyn Iterator<Item = &NbVertex> + 'a> {
        let mut iter = self.neighbors.cycle(start_index, true);
        match direction {
            CW => Box::new(iter.skip(1)),
            CCW => Box::new(iter.rev().skip(1))
        }
    }

    fn next_nb(&self, other: VertexI, direction: ClockDirection) -> &NbVertex {
        let nb = self.get_nb(other).unwrap();
        self.get_iterator(nb.index, direction).next().unwrap()
    }

    /// does not include start index
    fn cycle_while(&self, start_index: usize, condition_while: &Fn(&&NbVertex) -> bool, direction: ClockDirection) -> Vec<&NbVertex> {
        self.get_iterator(start_index, direction).take_while(condition_while).collect_vec()
    }

    /// does not include the edges to v1 and v2 respectively
    fn sector_between(&self, v1: VertexI, v2: VertexI, direction: ClockDirection) -> Vec<&NbVertex> {
        if let (Some(nb1), Some(nb2)) = (self.get_nb(v1), self.get_nb(v2)) {
            return self.nb_sector_between(nb1, nb2, direction);
        } else {
            panic!("v1/v2 invalid");
        }
    }

    fn nb_sector_between(&self, nb1: &NbVertex, nb2: &NbVertex, direction: ClockDirection) -> Vec<&NbVertex> {
        return self.cycle_while(nb1.index, &|nb| nb.other != nb2.other, direction);
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

impl<N: Clone, E: Clone, F: Clone, Ty: EdgeType, Ix: IndexType> From<Graph<N, E, Ty, Ix>> for PlanarMap<N, E, F> {
    fn from(g: Graph<N, E, Ty, Ix>) -> Self {
        let mut map = PlanarMap::new();

        let mut node_map = HashMap::new();
        for i in g.node_indices() {
            let new_idx = map.add_vertex(g.node_weight(i).unwrap().clone());
            node_map.insert(i, new_idx);
        }

        for i in g.edge_indices() {
            let (a, b) = g.edge_endpoints(i).unwrap();

            let v1 = node_map.get(&a).unwrap();
            let v2 = node_map.get(&b).unwrap();

            map.add_edge(*v1, *v2, g.edge_weight(i).unwrap().clone());
        }

        return map;
    }
}

impl<N: Clone, E: Clone, F: Clone> /*Into<(Graph<N, E, Ty, Ix>, HashMap<VertexI, NodeIndex<Ix>>)> for*/ PlanarMap<N, E, F> {
    fn into_petgraph(&self) -> (Graph<N, E, Undirected, u32>, HashMap<VertexI, NodeIndex<u32>>) {
        let mut g: Graph<N, E, _, u32> = Graph::new_undirected();

        let mut node_map = HashMap::new();
        for v in self.vertices.get_map().values() {
            let new_idx = g.add_node(v.weight.clone());
            node_map.insert(v.id, new_idx);
        }

        for e in self.edges.get_map().values() {

            let v1 = node_map.get(&e.tail).unwrap();
            let v2 = node_map.get(&e.head).unwrap();

            g.add_edge(*v1, *v2, e.weight.clone());
        }

        return (g, node_map);
    }
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

    pub fn clone_with_maps<Nn, Ee, Ff: Clone>(&self, vertex_map: fn(&N) -> Nn, edge_map: fn(&E) -> Ee, face_map: Option<fn(&F) -> Ff>) -> PlanarMap<Nn, Ee, Ff> {
        let mut cloned = PlanarMap::new();

        cloned.vertices = self.vertices.clone_with_map(&|v: &Vertex<N>|
               Vertex {
                   id: v.id,
                   neighbors: v.neighbors.clone(),
                   weight: vertex_map(&v.weight)
               }
        );

        cloned.edges = self.edges.clone_with_map(&|e: &Edge<E>|
            Edge {
                id: e.id,
                weight: edge_map(&e.weight),
                tail: e.tail,
                head: e.head,
                left_face: e.left_face,
                right_face: e.right_face
            }
        );

        if self.is_embedded() {
            if let Some(fmap) = face_map {
                cloned.faces = self.faces.clone_with_map(&|f: &Face<F>|
                    Face {
                        id: f.id,
                        weight: fmap(&f.weight),
                        angles: f.angles.clone()
                    }
                )
            } else {
                panic!("cloning an embedded map needs a face mapping")
            }
        }

        cloned.embedded = self.embedded;
        cloned.enforce_simple = self.enforce_simple;

        return cloned;
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

    pub fn get_left_face(&self, v1: VertexI, v2: VertexI) -> FaceI {
        if !self.is_embedded() {
            panic!("operation allowed only on imbedded graphs");
        }

        let (eid, signum) = self.get_edge_with_signum(v1, v2);
        match signum {
            Forward => self.edge(eid).left_face.unwrap(),
            Backward => self.edge(eid).right_face.unwrap()
        }
    }

    pub fn is_empty(&self) -> bool { self.vertices.get_map().is_empty() }

    pub fn is_singleton(&self) -> bool { self.vertices.get_map().len() == 1 }

    pub fn is_simple(&self) -> bool {
        let loops = self.edges.get_map().values().any(|e| e.is_loop());
        let double_edges = self.edges.get_map().values().unique().count() < self.edges.get_map().len();

        !loops && !double_edges
    }

    pub fn is_embedded(&self) -> bool { self.embedded }

    fn edges(&self) -> impl Iterator<Item=&Edge<E>> {
        self.edges.get_map().values()
    }

    pub fn edge_indices(&self) -> impl Iterator<Item=&EdgeI> {
        self.edges.get_map().keys()
    }

    pub fn edge_count(&self) -> usize { self.edges.get_map().len() }

    fn vertices(&self) -> impl Iterator<Item=&Vertex<N>> {
        self.vertices.get_map().values()
    }

    pub fn vertex_count(&self) -> usize { self.vertices.get_map().len() }

    pub fn face_count(&self) -> usize {
        if !self.embedded {
            panic!("not embedded");
        }

        self.faces.get_map().len()
    }

    pub fn edge_endvertex(&self, edge: &EdgeI, end: EdgeEnd) -> Option<VertexI> {
        let mut e = self.edges.get_map().get(&edge);
        match end {
            Tail => e.map(|e| e.tail),
            Head => e.map(|e| e.head)
        }
    }

    pub fn edge_weight(&self, edge: &EdgeI) -> Option<&E> {
        self.edges.get_map().get(edge).map(|e| &e.weight)
    }

    pub fn vertex_weight(&self, vertex: &VertexI) -> Option<&N> {
        self.vertices.get_map().get(vertex).map(|e| &e.weight)
    }

    pub fn is_connected(&self) -> bool {
        if self.is_empty() || self.is_singleton() {
            return true;
        }

        let connected_component = self.connected_component(&self.vertices.any_index().unwrap(), &HashSet::new());
        return connected_component.len() == self.vertex_count();
    }

    pub fn is_triangulation(&self) -> bool {
        self.faces.get_map().values().all(|f| f.angles.len() == 3)
    }

    pub fn connected_component(&self, vertex: &VertexI, forbidden_edges: &HashSet<EdgeI>) -> Vec<VertexI> {
        if !self.vertices.is_valid_index(vertex) {
            panic!("invalid starting vertex");
        }

        let mut visited = HashSet::new();
        let mut to_visit = vec![vertex];

        while !to_visit.is_empty() {
            let v = to_visit.remove(to_visit.len() - 1);
            visited.insert(*v);

            for nb in &self.vertices.get(v).neighbors {
                if !forbidden_edges.contains(&nb.edge) {
                    let v = &nb.other;
                    if !visited.contains(&v) {
                        to_visit.push(v);
                    }
                }
            }
        }

        return visited.into_iter().collect();
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

    pub fn add_edge(&mut self, v1: VertexI, v2: VertexI, weight: E) -> EdgeI {
        if self.embedded {
            panic!("superplanar operation on embedded graph.")
        }

        return self.add_edge_(v1, v2, None, None, weight);
    }

    fn add_edge_(&mut self, v1: VertexI, v2: VertexI, v1_nb_index: Option<usize>, v2_nb_index: Option<usize>, weight: E) -> EdgeI {

        if self.enforce_simple {
            if v1 == v2 {
                panic!("With this edge, the graph would not be simple any longer ('enforce_simple').");
            }
            if let Some(_) = self.vertices.get(&v1).get_nb(v2) {
                panic!("With this edge, the graph would not be simple any longer ('enforce_simple').");
            }
        }

        let e = Edge { id : EdgeI(0), tail: v1, head: v2, weight, left_face: None, right_face: None };
        let id = self.edges.retrieve_index(e);

        // v1 --> v2 (v1 = tail, v2 = head)
        let index = match v1_nb_index {
            None => self.vertex(v1).neighbors.len(),
            Some(i) => i
        };
        self.vertices.get_mut(&v1).neighbors.push(
            NbVertex {
                index,
                other: v2,
                edge: id,
                end: Tail
            }
        );

        let index = match v2_nb_index {
            None => self.vertex(v2).neighbors.len(),
            Some(i) => i
        };
        self.vertices.get_mut(&v2).neighbors.push(
            NbVertex {
                index,
                other: v1,
                edge: id,
                end: Head
            }
        );

        return id;
    }

    pub fn add_embedded_edge(&mut self, v1: VertexI, v2: VertexI, weight: E, face: FaceI) -> EdgeI {
        if !self.embedded {
            panic!("no embedding given");
        }

        let (pos1, pos2, nb_index_1, nb_index_2) = {
            let knee1 = match self.get_knee_by_face(face, v1) {
                Some(k) => k,
                None => panic!("invalid face for insertion")
            };
            let knee2 = match self.get_knee_by_face(face, v2) {
                Some(k) => k,
                None => panic!("invalid face for insertion")
            };
            (knee1.1, knee2.1, knee1.2.index, knee2.2.index)
        };

        let e = self.add_edge_(v1, v2, None, None, weight);

        // NbVertex structs were added at the end, which is not correct
        {
            let v1_mut = self.vertex_mut(v1);
            let nb = v1_mut.neighbors.pop().unwrap();
            v1_mut.neighbors.insert(nb_index_1 + 1, nb);
            self.restore_nb_indices(v1);

            let v2_mut = self.vertex_mut(v2);
            let nb = v2_mut.neighbors.pop().unwrap();
            v2_mut.neighbors.insert(nb_index_2 + 1, nb);
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

    /// returns deleted vertex and edge index
    pub fn contract_embedded_edge(&mut self, eid: EdgeI, merge_weights: &Fn(&Edge<E>, &Edge<E>) -> E) -> (VertexI, EdgeI) {
        unsafe {
            self.contract_embedded_edge_(eid, merge_weights)
        }
    }

    /// The head vertex remains
    /// Returns deleted vertex index and edge index
    unsafe fn contract_embedded_edge_(&mut self, eid: EdgeI, merge_weights: &Fn(&Edge<E>, &Edge<E>) -> E) -> (VertexI, EdgeI) {
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

            //let head_single_edge = head_cw == head_ccw;
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
                (*s).edge_mut(head_right.id).weight = merge_weights(&removed_edge, &head_right);
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

            for patch_nb in patch_v.neighbors.iter_mut().filter(|nb| nb.other == dropped_edge.tail) {
                patch_nb.other = dropped_edge.head;
            }
        }

        //println!("{} nb = {:?}", dropped_edge.head.0, self.vertex(dropped_edge.head).neighbors.iter().map(|nb| nb.other).collect_vec());
        //println!("{} nb = {:?}", dropped_edge.tail.0, dropped_vertex.neighbors.iter().map(|nb| nb.other).collect_vec());
        // insert the fan of neighbors previously attached to e.tail now to e.head
        {
            let mut v = self.vertex_mut(dropped_edge.head);
            let head_index = v.get_nb(dropped_edge.tail).unwrap().index;
            let tail_index = dropped_vertex.get_nb(dropped_edge.head).unwrap().index;
            v.neighbors.remove(head_index);
            v.neighbors.splice(head_index..head_index, dropped_vertex.neighbors.cycle(tail_index, false).filter(|nb| nb.other != dropped_edge.head).map(|nb| *nb));
        }
        self.restore_nb_indices(dropped_edge.head);
        //println!("{} nb = {:?}", dropped_edge.head.0, self.vertex(dropped_edge.head).neighbors.iter().map(|nb| nb.other).collect_vec());

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

        return (dropped_vertex.id, dropped_edge.id);
    }

    fn split_edge_(&mut self, eid: EdgeI, new_end: EdgeEnd, new_vertex_index: Option<VertexI>, new_edge_index: Option<EdgeI>, new_vertex_weight: N, new_edge_weight: E) -> (VertexI, EdgeI) {
        let pivot_vertex = self.edge(eid).get_vertex(new_end);
        let other_vertex = self.edge(eid).get_vertex(new_end.inverted());

        let new_vertex = Vertex {
            id: VertexI(0),
            weight: new_vertex_weight,
            neighbors: vec![
                NbVertex {
                    index: 0,
                    end: new_end,
                    other: other_vertex,
                    edge: eid
                },
                NbVertex {
                    index: 1,
                    end: new_end.inverted(),
                    other: pivot_vertex,
                    edge: EdgeI(0) //to be replaced by new edge id
                }
            ]
        };

        let new_vertex_index = if let Some(idx) = new_vertex_index {
            if !self.vertices.is_available(&idx) {
                panic!("vertex index not available");
            }
            self.vertices.insert_with_index(new_vertex, &idx);
            idx
        } else {
            self.vertices.retrieve_index(new_vertex)
        };

        let new_edge = Edge {
            id: EdgeI(0),
            tail: if new_end == Tail { pivot_vertex } else { new_vertex_index },
            head: if new_end == Tail { new_vertex_index } else { pivot_vertex },
            left_face: None,
            right_face: None,
            weight: new_edge_weight,
        };

        let new_edge_index = if let Some(idx) = new_edge_index {
            if !self.edges.is_available(&idx) {
                panic!("edge index not available");
            }
            self.edges.insert_with_index(new_edge, &idx);
            idx
        } else {
            self.edges.retrieve_index(new_edge)
        };

        // set indices of new edge and vertex
        self.edge_mut(new_edge_index).id = new_edge_index;
        self.vertex_mut(new_vertex_index).id = new_vertex_index;
        self.vertex_mut(new_vertex_index).neighbors[1].edge = new_edge_index;

        // patch old edge
        match new_end {
            Tail => self.edge_mut(eid).tail = new_vertex_index,
            Head => self.edge_mut(eid).head = new_vertex_index
        }
        self.vertex_mut(other_vertex).get_nb_mut(pivot_vertex).unwrap().other = new_vertex_index;

        // patch pivot vertex
        {
            let mut v = self.vertex_mut(pivot_vertex).get_nb_mut(other_vertex).unwrap();
            v.edge = new_edge_index;
            v.other = new_vertex_index;
        }

        return (new_vertex_index, new_edge_index)
    }

    /// new_end = end of the existing edge that will be replaced by the new edge
    fn split_embedded_edge(&mut self, eid: EdgeI, new_end: EdgeEnd, new_vertex_index: Option<VertexI>, new_edge_index: Option<EdgeI>, new_vertex_weight: N, new_edge_weight: E) -> (VertexI, EdgeI) {
        if !self.embedded {
            panic!("not embedded");
        }

        let (old_tail, old_head) = self.edge(eid).to_vertex_pair(Forward);
        let (new_vid, new_eid) = self.split_edge_(eid, new_end, new_vertex_index, new_edge_index, new_vertex_weight, new_edge_weight);

        self.edge_mut(new_eid).left_face = self.edge(eid).left_face;
        self.edge_mut(new_eid).right_face = self.edge(eid).right_face;

        {
            let f = self.face_mut(self.edge(eid).right_face.unwrap());
            let old_head_index = f.angles.iter().position(|&vid| vid == old_head).unwrap();
            f.angles.insert(old_head_index + 1, new_vid);
        }
        {
            let f = self.face_mut(self.edge(eid).left_face.unwrap());
            let old_tail_index = f.angles.iter().position(|&vid| vid == old_tail).unwrap();
            f.angles.insert(old_tail_index + 1, new_vid);
        }

        {
            let f = self.face_mut(self.edge(eid).right_face.unwrap());
            println!("{:?}", f.angles);
        }
        {
            let f = self.face_mut(self.edge(eid).left_face.unwrap());
            println!("{:?}", f.angles);
        }

        return (new_vid, new_eid);
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

                    if let Some((v, idx, nb1, nb2)) = self.get_knee_by_face(f.id, vid) {
                        let l = v.neighbors.len();

                        if !((nb1.index + 1) % l == nb2.index || (nb2.index + 1) % l == nb1.index) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
            }

        }

        return true;
    }

    /// From edge orders to faces
    fn construct_faces(&mut self, f_weights: fn(FaceI) -> F) {
        if self.embedded {
            panic!("embedding already done");
        }

        for eid in self.edges.get_map().keys().cloned().collect_vec() {
            if self.edge(eid).left_face.is_none() {
                self.build_face_cycle(eid, Forward, f_weights);
            }

            if self.edge(eid).right_face.is_none() {
                self.build_face_cycle(eid, Backward, f_weights);
            }
        }

        self.embedded = true;
        let (n, m, f) = (self.vertex_count() as isize, self.edge_count() as isize, self.face_count() as isize);

        if n - m + f != 2 {
            self.embedded = false;
            panic!("Euler is unhappy");
        }
    }

    /// signum -> right face
    fn build_face_cycle(&mut self, eid: EdgeI, signum: Signum, f_weights: fn(FaceI) -> F) {
        let mut cycle = Vec::new();
        let fid = self.faces.peek_index();

        let mut next = {
            let e = self.edge_mut(eid);
            match signum {
                Forward => {
                    cycle.push(e.tail);
                    e.left_face = Some(fid);
                    e.head
                },
                Backward => {
                    cycle.push(e.head);
                    e.right_face = Some(fid);
                    e.tail
                }
            }
        };

        while next != cycle[0] && cycle.len() <= self.vertex_count() + 1 {
            cycle.push(next);
            let l = cycle.len();
            next = self.vertex(cycle[l-1]).next_nb(cycle[l-2], CW).other;

            let (intermediate_eid, s) = self.get_edge_with_signum(cycle[l-1], next);
            let e = self.edge_mut(intermediate_eid);
            match s {
                Forward => e.left_face = Some(fid),
                Backward => e.right_face = Some(fid)
            }
        }

        if cycle.len() > self.vertex_count() {
            panic!("cycle too long");
        }

        let face = Face { id: FaceI(0), weight: f_weights(fid), angles: cycle };
        self.faces.retrieve_index(face);
    }

    /// Gives the graph an embedding into the sphere (i.e. no outer face is selected). The argument
    /// `faces` is a vector of all face cycles. The face cycles are vectors of vertex indices,
    /// specifying the order of vertices around the face in counterclockwise direction.
    pub fn set_embedding(&mut self, faces: Vec<(Vec<VertexI>, F)>) {
        if !(self.is_simple() && self.is_connected()) {
            panic!("Embeddings can only be set if the graph is simple and connected");
        }

        if let Some(v) = faces.iter().flat_map(|(fc, w)| fc.iter()).find(|v| !self.is_valid_vertex(v)) {
            panic!("Invalid vertex index found, {} for instance.", v.0);
        }

        let n = self.vertices.get_map().len() as isize;
        let m = self.edges.get_map().len() as isize;
        let f = faces.len() as isize;

        if n - m + f != 2 {
            panic!("The number of faces for a full embedding should be {}, you have given {}.", 2 - n + m, f);
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
                    panic!("The edge ({}, {}) is contained in a face cycle but does not exist in the graph.", a.0, b.0);
                }

                match self.get_signum(e.unwrap(), a, b) {
                    Forward => fwd_occurence.insert(e.unwrap()),
                    Backward => backwd_occurence.insert(e.unwrap())
                };
            }
        }

        // check if each edge occurs exactly twice, one time as (a,b), one time as (b,a)
        if fwd_occurence.len() < self.edges.get_map().len() {
            return panic!("It is wrong forwardly");
        }
        if backwd_occurence.len() < self.edges.get_map().len() {
            return panic!("It is wrong backwardly");
        }

        // check if all vertices have a well-defined total order on their neighbors now
        for i in self.vertices.get_map().keys().copied().collect_vec() {
            let mut v = self.vertices.get_mut(&i);
            let vertex_angles = angles.get(&v.id).unwrap();

            if vertex_angles.len() != v.neighbors.len() {
                return panic!("total order fail (count)");
            }

            {
                let neighbors: HashSet<&VertexI> = v.neighbors.iter().map(|nb| &nb.other).collect();
                let angle_entering: HashSet<&VertexI> = vertex_angles.iter().map(|(u, _)| u).collect();
                let angle_leaving: HashSet<&VertexI> = vertex_angles.iter().map(|(_, v)| v).collect();

                if neighbors != angle_entering {
                    return panic!("total order fail (entering)");
                }

                if neighbors != angle_leaving {
                    return panic!("total order fail (leaving)");
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
    }

    pub fn get_dual(&self, enforce_simple: bool) -> (PlanarMap<FaceI, EdgeI, VertexI>, HashMap<VertexI, FaceI>, HashMap<EdgeI, EdgeI>, HashMap<FaceI, VertexI>) {

        let mut dual_map = PlanarMap::new();
        dual_map.enforce_simple = self.enforce_simple;

        let mut primal_vertex_to_dual_face = HashMap::new();
        let mut primal_face_to_dual_vertex = HashMap::new();
        let mut primal_edge_to_dual_edge = HashMap::new();

        for f in self.faces.get_map().values() {
            let dual_vid = dual_map.add_vertex(f.id);
            primal_face_to_dual_vertex.insert(f.id, dual_vid);
        }

        for (vid, fid) in dual_map.vertices.get_map().values().map(|v| (v.id, v.weight)).collect_vec() {
            let f = self.face(fid);
            for (&v1, &v2) in f.angles.cycle(0, true).tuple_windows() {
                let e = self.edge(self.get_edge(v1, v2).unwrap());
                if f.id == e.left_face.unwrap() {
                    let other_vertex = primal_face_to_dual_vertex.get(&e.right_face.unwrap()).unwrap();
                    let new_edge = dual_map.add_edge(vid, *other_vertex, e.id);
                    primal_edge_to_dual_edge.insert(e.id, new_edge);
                }
            }
        }

        let mut face_cycles = vec![];

        for v in self.vertices.get_map().values() {
            let cycle = v.neighbors.iter().map(|nb| {
                let f = match nb.end {
                    Tail => self.edge(nb.edge).left_face.unwrap(),
                    Head => self.edge(nb.edge).right_face.unwrap()
                };
                *primal_face_to_dual_vertex.get(&f).unwrap()
            }).collect_vec();

            face_cycles.push((cycle, v.id));
        }

        dual_map.set_embedding(face_cycles);
        for dual_face in dual_map.faces.get_map().values() {
            primal_vertex_to_dual_face.insert(dual_face.weight, dual_face.id);
        }

        (
            dual_map,
            primal_vertex_to_dual_face,
            primal_edge_to_dual_edge,
            primal_face_to_dual_vertex
        )
    }

    /// Unchecked retrieval of the edge struct for a given edge index
    fn edge(&self, e: EdgeI) -> &Edge<E> {
        self.edges.get(&e)
    }

    fn edge_mut(&mut self, e: EdgeI) -> &mut Edge<E> {
        self.edges.get_mut(&e)
    }

    /// (tail vertex, tail neighbor, edge, head neighbor, head vertex)
    fn edge_with_nb(&self, e: EdgeI) -> (&Vertex<N>, &NbVertex, &Edge<E>, &NbVertex, &Vertex<N>) {
        let e = self.edge(e);
        let tail = self.vertex(e.tail);
        let head = self.vertex(e.head);
        let nb_tail = tail.get_nb(e.head).unwrap();
        let nb_head = head.get_nb(e.tail).unwrap();
        (tail, nb_tail, e, nb_head, head)
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

        let first = v.neighbors.iter()
            .find(|nb| {
                let e = self.edge(nb.edge);
                match nb.end {
                    Head => e.left_face.unwrap() == fid,
                    Tail => e.right_face.unwrap() == fid
                }
            }).unwrap();

        let second = v.neighbors.iter()
            .find(|nb| {
                let e = self.edge(nb.edge);
                match nb.end {
                    Head => e.right_face.unwrap() == fid,
                    Tail => e.left_face.unwrap() == fid
                }
            }).unwrap();

        if first.other == second.other {
            return Some((v, idx, first, first));
        } else {
            return Some((v, idx, first, second));
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
            1 => (),
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

impl<N, E, F: Clone> Debug for PlanarMap<N, E, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for v in self.vertices.get_map().values().sorted_by_key(|v| v.id.0) {
            writeln!(f, "{:?}", v);
        }
        for e in self.edges.get_map().values().sorted_by_key(|e| e.id.0) {
            writeln!(f, "{:?}", e);
        }
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nbvertex_next() {
        let mut g = PlanarMap::new();
        let v1 = g.add_vertex(1);
        let v2 = g.add_vertex(2);
        let e = g.add_edge(v1, v2, 1);

        assert_eq!(g.vertex(v1).next_nb(v2, CW).other, v2);
        assert_eq!(g.vertex(v1).next_nb(v2, CCW).other, v2);

        let v3 = g.add_vertex(3);
        g.add_edge(v1, v3, 2);
        g.add_edge(v2, v3, 2);

        g.set_embedding(vec![
            (vec![v1, v2, v3], 1),
            (vec![v3, v2, v1], 1),
        ]);

        assert_eq!(g.vertex(v1).next_nb(v2, CW).other, v3);
        assert_eq!(g.vertex(v1).next_nb(v2, CCW).other, v3);

        assert_eq!(g.vertex(v3).next_nb(v1, CW).other, v2);
        assert_eq!(g.vertex(v3).next_nb(v1, CCW).other, v2);
    }

    #[test]
    fn test_nbvertex_sector() {
        let mut g = PlanarMap::new();

        let ctr = g.add_vertex(1000);

        let mut outer_rim = Vec::new();
        for i in 0..10 {
            let v = g.add_vertex(i);
            outer_rim.push(v);
            g.add_edge(ctr, v,i);
        }

        let mut the_face = Vec::new();
        for vid in &outer_rim {
            the_face.push(*vid);
            the_face.push(*&ctr);
        }

        g.set_embedding(vec![
            (the_face, 0)
        ]);

        assert_eq!(g.vertex(ctr).sector_between(outer_rim[0], outer_rim[3], CW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[1], outer_rim[2]]);
        assert_eq!(g.vertex(ctr).sector_between(outer_rim[0], outer_rim[7], CCW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[9], outer_rim[8]]);
        assert_eq!(g.vertex(ctr).sector_between(outer_rim[1], outer_rim[0], CCW).iter().map(|nb|nb.other).collect_vec(), vec![]);
        assert_eq!(g.vertex(ctr).sector_between(outer_rim[0], outer_rim[1], CW).iter().map(|nb|nb.other).collect_vec(), vec![]);
    }
}