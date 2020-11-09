use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};

use array_tool::vec::Intersect;
use itertools::{Itertools};

use crate::util::iterators::cyclic::CyclicIterable;
use crate::util::errors::{GraphResult, GraphErr};

use self::ClockDirection::{CCW, CW};
use self::EdgeEnd::{Head, Tail};
use self::guarded_map::{GuardedMap, Ideable};
use self::Signum::{Backward, Forward};
use self::indices::{EdgeI, VertexI, FaceI};
use self::error::{IndexAccessError, NoSuchEdgeError};
use self::Side::{Left, Right};

#[macro_export]
macro_rules! invalid_graph {
    () => { panic!("Assertion failed, referential integrity of graph obstructed.") };
}

#[macro_export]
macro_rules! embedded {
    () => { GraphErr::new_err("This operation can only be applied to graphs that are not embedded.") };
}


#[macro_export]
macro_rules! not_embedded {
    () => { GraphErr::new_err("This operation can only be applied to embedded graphs.") };
}


pub mod indices;
pub mod io;
pub mod error;
pub mod guarded_map;

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
    pub fn rev_if(&self, cond: bool) -> Self {
        if cond {
            self.reversed()
        } else {
            *self
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

pub struct Edge<E> {
    pub id: EdgeI,
    pub tail: VertexI,
    pub head: VertexI,
    pub right_face: Option<FaceI>,
    pub left_face: Option<FaceI>,
    pub weight: E
}

impl<E> Edge<E> {
    pub fn get_other(&self, this: VertexI) -> VertexI {//TODO error handling
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

pub struct Vertex<N> {
    pub id: VertexI,
    pub neighbors: Vec<NbVertex>,
    pub weight: N
}



impl<N> Debug for Vertex<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
pub struct Face<F> {
    pub id: FaceI,
    pub angles: Vec<VertexI>,
    pub weight: F
}

pub fn swap<T>(pair: (T, T), do_swap: bool) -> (T, T) {
    if do_swap {
        let (a, b) = pair;
        (b, a)
    } else {
        pair
    }
}

impl<F> Ideable<FaceI> for Face<F> {
    fn get_id(&self) -> FaceI { self.id }
    fn set_id(&mut self, id: FaceI) { self.id = id }
}

impl<F> Debug for Face<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: ", self.id)?;
        for angle in self.angles.iter() {
            write!(f, "{:?}", angle)?;
        }
        Ok(())
    }
}

impl<N> Vertex<N> {

    fn get_nb(&self, other: VertexI) -> Option<&NbVertex> {
        self.neighbors.iter().find(|nb| nb.other == other)
    }

    fn get_nb_mut(&mut self, other: VertexI) -> Option<&mut NbVertex> {
        self.neighbors.iter_mut().find(|nb| nb.other == other)
    }

    /// does contain start_index at the end (wraps around), if condition is always true
    fn get_iterator<'a>(&'a self, start_index: usize, direction: ClockDirection, include_start_index: bool, wrap: bool) -> Box<dyn Iterator<Item = &NbVertex> + 'a> {
        let iter = self.neighbors.cycle(start_index, wrap);
        let skip = if include_start_index { 0 } else { 1 };
        match direction {
            CW => Box::new(iter.skip(skip)),
            CCW => Box::new(iter.rev().skip(skip))
        }
    }

    pub fn next_nb(&self, other: VertexI, direction: ClockDirection) -> &NbVertex {
        let nb = self.get_nb(other).unwrap();
        self.get_iterator(nb.index, direction, false, true).next().unwrap()
    }

    pub fn next(&self, nb: &NbVertex, direction: ClockDirection) -> &NbVertex {
        self.get_iterator(nb.index, direction, false, true).next().unwrap()
    }

    pub fn cycle_while(&self, start_index: usize, condition_while: &dyn Fn(&&NbVertex) -> bool, direction: ClockDirection, include_start_index: bool) -> Vec<&NbVertex> {
        self.get_iterator(start_index, direction, include_start_index, true).take_while(condition_while).collect_vec()
    }

    /// does not include the edges to v1 and v2 respectively
    pub fn sector_between(&self, v1: VertexI, v2: VertexI, direction: ClockDirection) -> Vec<&NbVertex> {
        if let (Some(nb1), Some(nb2)) = (self.get_nb(v1), self.get_nb(v2)) {
            return self.nb_sector_between(nb1, nb2, direction);
        } else {
            panic!("v1/v2 invalid");
        }
    }

    pub fn nb_sector_between(&self, nb1: &NbVertex, nb2: &NbVertex, direction: ClockDirection) -> Vec<&NbVertex> {
        return self.cycle_while(nb1.index, &|nb| nb.other != nb2.other, direction, false);
    }

    fn _sector_including(&self, v1: VertexI, v2: VertexI, direction: ClockDirection) -> Vec<&NbVertex> {
        if let (Some(nb1), Some(nb2)) = (self.get_nb(v1), self.get_nb(v2)) {
            return self.nb_sector_including(nb1, nb2, direction);
        } else {
            panic!("v1/v2 invalid");
        }
    }

    pub fn nb_sector_including(&self, nb1: &NbVertex, nb2: &NbVertex, direction: ClockDirection) -> Vec<&NbVertex> {
        let mut result = Vec::new();
        let mut start = true;
        for nb in self.get_iterator(nb1.index, direction, true, true) {
            result.push(nb);
            if nb.other == nb2.other && !start {
                break;
            }
            start = false;
        }
        return result;
    }

}

#[derive(Clone, Copy, Debug)]
pub struct NbVertex {
    pub index: usize,
    pub other: VertexI,
    pub edge: EdgeI,
    pub end: EdgeEnd
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

    pub fn get_edge(&self, v1: VertexI, v2: VertexI) -> GraphResult<EdgeI> {
        let v = self.try_vertex(v1)?;
        self.try_vertex(v2)?;

        v.neighbors.iter()
            .find(|nb| nb.other == v2)
            .map(|nb| nb.edge)
            .ok_or(GraphErr::from(NoSuchEdgeError::new(v1, v2)))
    }

    pub fn edge_pair(&self, e: EdgeI, sig: Signum) -> Result<(VertexI, VertexI), IndexAccessError<EdgeI>> {
        let e = self.try_edge(e)?;
        return Ok(match sig {
            Forward => (e.tail, e.head), Backward => (e.head, e.tail)
        });
    }

    pub fn faces(&self) -> impl Iterator<Item = &Face<F>> {
        self.faces.get_map().values()
    }

    pub fn get_face(&self, v1: VertexI, v2: VertexI, side: Side) -> FaceI {
        if !self.is_embedded() {
            panic!("operation allowed only on embedded graphs");
        }

        let (eid, signum) = self.get_edge_with_signum(v1, v2);
        let e = self.edge(eid);
        let (left, right) = (e.left_face.unwrap(), e.right_face.unwrap());

        match signum {
            Forward => match side { Left => left, Right => right },
            Backward => match side { Left => right, Right => left },
        }
    }

    pub fn faces_between(&self, v: &VertexI, v1: &VertexI, v2: &VertexI, direction: ClockDirection) -> Vec<FaceI> {
        let v = self.vertex(*v);
        if let (Some(nb1), Some(nb2)) = (v.get_nb(*v1),  v.get_nb(*v2)) {

            let nbs = v.cycle_while(nb1.index, &|nb| nb.index != nb2.index, CW, true);

            nbs.iter().map(|nb|
               self.get_face(v.id, nb.other,
                match direction {
                        CW => Right,
                        CCW => Left
                    }
               )
            ).collect()
        } else {
            panic!("v1/v2 invalid");
        }
    }

    pub fn is_empty(&self) -> bool { self.vertices.get_map().is_empty() }

    pub fn is_singleton(&self) -> bool { self.vertices.get_map().len() == 1 }

    pub fn is_simple(&self) -> bool {
        let loops = self.edges.get_map().values().any(|e| e.is_loop());
        let double_edges = self.edges.get_map().values().unique().count() < self.edges.get_map().len();

        for e in self.edges() {
            println!("{} - {}", e.tail, e.head);
        }

        !loops && !double_edges
    }

    pub fn is_embedded(&self) -> bool { self.embedded }

    pub fn edges(&self) -> impl Iterator<Item=&Edge<E>> {
        self.edges.get_map().values()
    }

    pub fn edge_indices(&self) -> impl Iterator<Item=&EdgeI> {
        self.edges.get_map().keys()
    }

    pub fn edge_count(&self) -> usize { self.edges.get_map().len() }

    pub fn edge_contains(&self, eid: &EdgeI, vid: &VertexI) -> bool {
        let e = self.edge(*eid);
        return &e.head == vid || &e.tail == vid;
    }

    pub fn next_nb(&self, v: VertexI, nb: VertexI, direction: ClockDirection) -> GraphResult<VertexI> {
        let v = self.try_vertex(v)?;
        if let Some(nb) = v.neighbors.iter().find(|&&n| n.other == nb) {
            Ok(v.get_iterator(nb.index, direction, false, true).next().unwrap().other)
        } else {
            GraphErr::new_err(&format!("{:?} is not a neighbor of {:?}", nb, v))
        }
    }

    pub fn vertex_count(&self) -> usize { self.vertices.get_map().len() }

    pub fn vertex_indices(&self) -> impl Iterator<Item = &VertexI> {
        self.vertices.get_map().keys()
    }

    pub fn vertices(&self) -> impl Iterator<Item = &Vertex<N>> {
        self.vertices.get_map().values()
    }

    pub fn vertex_cycle(&self, vid: VertexI, other: VertexI, wrap: bool, direction: ClockDirection) -> impl Iterator<Item=&NbVertex> {
        if direction == CCW { panic!("unable") } //TODO
        let v = self.vertex(vid);//TODO
        let i = v.neighbors.iter().position(|nb| nb.other == other).unwrap();//TODO
        v.neighbors.cycle(i, wrap)
    }

    pub fn get_neighbors<'a>(&self, v: &VertexI) -> impl Iterator<Item = &VertexI> {
        self.vertex(*v).neighbors.iter().map(|nb| &nb.other)
    }

    pub fn face_count(&self) -> usize {
        if !self.embedded {
            panic!("not embedded");
        }

        self.faces.get_map().len()
    }

    pub fn sector_between(&self, center: VertexI, from: VertexI, to: VertexI, direction: ClockDirection) -> GraphResult<Vec<(EdgeI, VertexI)>> {
        Ok(self.try_vertex(center)?.sector_between(from, to, direction).iter()
            .map(|nb| (nb.edge, nb.other))
            .collect_vec())
    }

    pub fn edge_endvertex(&self, edge: &EdgeI, end: EdgeEnd) -> Option<VertexI> {
        let e = self.edges.get_map().get(&edge);
        match end {
            Tail => e.map(|e| e.tail),
            Head => e.map(|e| e.head)
        }
    }

    pub fn edge_opposite_vertex(&self, e: EdgeI, v: VertexI) -> GraphResult<VertexI> {
        Ok(self.try_edge(e)?.get_other(v))
    }

    pub fn edge_weight(&self, e: EdgeI) -> Result<&E, IndexAccessError<EdgeI>> {
        Ok(&self.try_edge(e)?.weight)
    }

    pub fn set_edge_weight(&mut self, e: EdgeI, weight: E) -> Result<(), IndexAccessError<EdgeI>> {
        let e = self.try_edge_mut(e)?;
        e.weight = weight;
        return Ok(());
    }

    pub fn vertex_weight(&self, v: VertexI) -> Result<&N, IndexAccessError<VertexI>> {
        Ok(&self.try_vertex(v)?.weight)
    }

    pub fn set_vertex_weight(&mut self, v: VertexI, weight: N) -> Result<(), IndexAccessError<VertexI>> {
        let v = self.try_vertex_mut(v)?;
        v.weight = weight;
        return Ok(());
    }

    pub fn is_connected(&self) -> bool {
        if self.is_empty() || self.is_singleton() {
            return true;
        }

        let connected_component = self.connected_component(self.vertices.any_index().unwrap(), &HashSet::new()).unwrap();
        return connected_component.len() == self.vertex_count();
    }

    pub fn is_triangulation(&self) -> bool {
        self.faces.get_map().values().all(|f| f.angles.len() == 3)
    }

    pub fn connected_component(&self, vertex: VertexI, forbidden_edges: &HashSet<EdgeI>) -> GraphResult<Vec<VertexI>> {
        self.try_vertex(vertex)?;

        let mut visited = HashSet::new();
        let mut to_visit = vec![vertex];

        while let Some(v) = to_visit.pop() {
            visited.insert(v);

            for nb in &self.try_vertex(v)?.neighbors {
                if !forbidden_edges.contains(&nb.edge) {
                    let v = nb.other;
                    if !visited.contains(&v) {
                        to_visit.push(v);
                    }
                }
            }
        }

        return Ok(visited.into_iter().collect());
    }

    pub fn shortest_path(&self, from: &VertexI, to: &VertexI, forbidden_vertices: &HashSet<VertexI>) -> Vec<VertexI> {
        let mut dist = HashMap::new();
        let mut pred = HashMap::new();
        let mut visited = HashSet::new();
        let mut unvisited : HashSet<_> = self.vertices.get_map().values().map(|v| v.id).collect();

        dist.insert(*from, 0);
        for forbidden in forbidden_vertices {
            unvisited.remove(forbidden);
        }

        while !unvisited.is_empty() {
            let v = *unvisited.iter()
                .filter(|v| dist.get(v).is_some())
                .min_by_key(|v| (dist.get(v).unwrap(), v.0)).unwrap();

            visited.insert(v);
            unvisited.remove(&v);

            let d = *dist.get(&v).unwrap();

            self.vertex(v).neighbors.iter().map(|nb| nb.other)
                .filter(|&other| unvisited.contains(&other))
                .for_each(|other| {
                    if !dist.contains_key(&other) || *dist.get(&other).unwrap() > d + 1 {
                        dist.insert(other, d + 1);
                        pred.insert(other, v);
                    }
                });
        }

        let mut rev_path = vec![*to];
        let mut v = *to;
        while let Some(predecessor) = pred.get(&v) {
            rev_path.push(*predecessor);
            v = *predecessor;
        }

        return rev_path.into_iter().rev().collect();
    }

    pub fn add_vertex(&mut self, weight: N) -> VertexI {
        if self.embedded {
            panic!("superplanar operation on embedded graph.");
        }

        let v = Vertex {
            id: VertexI(0), neighbors: Vec::new(), weight
        };

        let index = self.vertices.retrieve_index(v);
        self.vertices.get_mut(&index).unwrap().id = index;
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
            if let Some(_) = self.vertex(v1).get_nb(v2) {
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
        self.vertex_mut(v1).neighbors.push(
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
        self.vertex_mut(v2).neighbors.push(
            NbVertex {
                index,
                other: v1,
                edge: id,
                end: Head
            }
        );

        return id;
    }

    /*pub fn relabel_vertex(&mut self, vid: &VertexI, new_vid: VertexI) -> GraphResult<()> {
        unsafe {
            let s: *mut Self = self;
            self.vertices.reindex(vid, &new_vid);
            let mut v = self.vertex_mut(*vid);
            v.id = new_vid;
            for mut nb in &v.neighbors {
                match nb.end {
                    Tail => (*s).edge_mut(nb.edge).tail = new_vid,
                    Head => (*s).edge_mut(nb.edge).head = new_vid,
                }

                let mut other_v = (*s).vertex_mut(nb.other);
                let mut other_nb = other_v.neighbors.iter_mut().find(|nb| nb.other == *vid).unwrap();
                other_nb.other = new_vid;
            }
        }

        Ok(())
    }*/

    pub fn add_embedded_edge(&mut self, v1: VertexI, v2: VertexI, weight: E, face: FaceI) -> GraphResult<EdgeI> {
        if !self.embedded {
            return not_embedded!();
        }

        let (pos1, pos2, nb_index_1, nb_index_2) = {
            let knee1 = self.get_knee_by_face(face, v1)?;
            let knee2 = self.get_knee_by_face(face, v2)?;
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

        return Ok(e);
    }

    fn remove_edge_(&mut self, v1: VertexI, v2: VertexI) -> GraphResult<Edge<E>> {
        let eid = self.get_edge(v1, v2)?;

        let e = self.edges.free_index(&eid).unwrap();
        self.vertex_mut(v1).neighbors.retain(|nb| nb.other != v2);
        self.vertex_mut(v2).neighbors.retain(|nb|nb.other != v1);

        self.restore_nb_indices(v1);
        self.restore_nb_indices(v2);

        return Ok(e);
    }

    pub fn remove_edge(&mut self, v1: VertexI, v2: VertexI) -> GraphResult<EdgeI> {
        if self.embedded {
            panic!("superplanar operation on embedded graph.")
        }

        self.remove_edge_(v1, v2).map(|e|e.id)
    }

    pub fn remove_embedded_edge_by_id(&mut self, e: EdgeI, merge_weights: &dyn Fn(F, F) -> F) -> GraphResult<(EdgeI, FaceI)> {
        let e = self.try_edge(e)?;
        self.remove_embedded_edge(e.tail, e.head, merge_weights)
    }

    pub fn remove_embedded_edge(&mut self, v1: VertexI, v2: VertexI, merge_weights: &dyn Fn(F, F) -> F) -> GraphResult<(EdgeI, FaceI)> {
        if !self.embedded {
            panic!("no embedding given");
        }

        let e = self.remove_edge_(v1, v2)?;

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

        return Ok((e.id, fid));
    }

    /// the nbvectors of from and to are not touched!
    pub fn patch_edge(&mut self, eid: EdgeI, from: VertexI, to: VertexI) {
        let (end, other) = match self.edge(eid).to_vertex_pair(Forward) {
            (tail, head) if tail == from => (Tail, head),
            (tail, head) if head == from => (Head, tail),
            _ => panic!("no") //TODO
        };

        match end {
            Head => { self.edge_mut(eid).head = to; }
            Tail => { self.edge_mut(eid).tail = to; }
        }
        self.vertex_mut(other).get_nb_mut(from).unwrap().other = to;
    }

    /// Does not return the given eid in case there is no other edge
    pub fn next_edge(&self, vid: VertexI, eid: EdgeI, direction: ClockDirection) -> Option<EdgeI> {
        let v = self.vertex(vid);
        let pos = v.neighbors.iter().find(|nb|nb.edge == eid).unwrap().index;

        match direction {
            CW => v.neighbors.cycle(pos, false).nth(1).map(|nb|nb.edge),
            CCW => v.neighbors.cycle(pos, false).rev().next().map(|nb|nb.edge)
        }
    }

    /// returns sustained vertex, deleted vertex and edge index
    pub fn contract_embedded_edge(&mut self, eid: EdgeI, keep: EdgeEnd) -> GraphResult<(VertexI, VertexI, EdgeI)> {
        unsafe {
            self.contract_embedded_edge_(eid, keep)
        }
    }

    /// The specified vertex (by keep) is retained.
    /// Returns retained vertex index, deleted vertex index and deleted edge index
    unsafe fn contract_embedded_edge_(&mut self, contracted_eid: EdgeI, keep: EdgeEnd) -> GraphResult<(VertexI, VertexI, EdgeI)> {
        let s: *mut Self = self;

        let (v_dropped, v_kept) = {
            let (v_dropped, v_kept) = {
                let e = self.edge(contracted_eid);
                match keep {
                    Head => (e.tail, e.head),
                    Tail => (e.head, e.tail)
                }
            };

            let dropped_cw = self.next_edge(v_dropped, contracted_eid, CW);
            let dropped_ccw = self.next_edge(v_dropped, contracted_eid, CCW);
            let kept_cw = self.next_edge(v_kept, contracted_eid, CW);
            let kept_ccw = self.next_edge(v_kept, contracted_eid, CCW);

            if let None = kept_cw.and(kept_ccw).and(dropped_cw).and(dropped_ccw) {
                panic!("No neighbour");
                //TODO!!!
            }

            // right and left regarding to the orientation dropped -> kept
            let dropped_right = self.edge(dropped_cw.unwrap());
            let dropped_left = self.edge(dropped_ccw.unwrap());
            let kept_right = self.edge(kept_ccw.unwrap());
            let kept_left = self.edge(kept_cw.unwrap());

            let right_face_collapse = dropped_right.get_other(v_dropped) == kept_right.get_other(v_kept);
            let left_face_collapse = dropped_left.get_other(v_dropped) == kept_left.get_other(v_kept);

            let dropped_single_edge = dropped_cw == dropped_ccw;

            if right_face_collapse {
                let right_face_merge: Box<dyn Fn(_, _) -> _> = match dropped_right.get_signum_by_tail(v_dropped) {
                    Forward => Box::new(|_, b| b),
                    Backward => Box::new(|a, _| a)
                };

                //(*s).edge_mut(kept_right.id).weight = merge_weights(&removed_edge, &kept_right);
                (*s).remove_embedded_edge_by_id(dropped_right.id, &right_face_merge)?;
            }

            if left_face_collapse && !dropped_single_edge {
                let left_face_merge: Box<dyn Fn(_, _) -> _> = match dropped_left.get_signum_by_tail(v_dropped) {
                    Forward => Box::new(|a, _| a),
                    Backward => Box::new(|_, b| b)
                };

                (*s).remove_embedded_edge_by_id(dropped_left.id, &left_face_merge)?;
            }

            (v_dropped, v_kept)
        };

        let dropped_edge = self.edges.free_index(&contracted_eid).unwrap();
        let dropped_vertex = self.vertices.free_index(&v_dropped).unwrap();

        // patch edges at the dropped and of the contracted edge and their adjacent vertices
        for nb in dropped_vertex.neighbors.iter().filter(|nb| nb.other != v_kept) {
            let patch_e = (*s).edge_mut(nb.edge);
            let patch_v = match nb.end {
                Tail => {
                    patch_e.tail = v_kept;
                    (*s).vertex_mut(patch_e.head)
                },
                Head => {
                    patch_e.head = v_kept;
                    (*s).vertex_mut(patch_e.tail)
                }
            };

            for patch_nb in patch_v.neighbors.iter_mut().filter(|nb| nb.other == v_dropped) {
                patch_nb.other = v_kept;
            }
        }

        //println!("{} nb = {:?}", dropped_edge.head.0, self.vertex(dropped_edge.head).neighbors.iter().map(|nb| nb.other).collect_vec());
        //println!("{} nb = {:?}", dropped_edge.tail.0, dropped_vertex.neighbors.iter().map(|nb| nb.other).collect_vec());

        // insert the fan of neighbors previously attached to v_dropped now to v_kept
        {
            let v = self.vertex_mut(v_kept);
            let kept_index = v.get_nb(v_dropped).unwrap().index;
            let dropped_index = dropped_vertex.get_nb(v_kept).unwrap().index;
            v.neighbors.remove(kept_index);
            v.neighbors.splice(kept_index..kept_index, dropped_vertex.neighbors.cycle(dropped_index, false).filter(|nb| nb.other != v_kept).map(|nb| *nb));
        }
        self.restore_nb_indices(v_kept);
        //println!("{} nb = {:?}", dropped_edge.head.0, self.vertex(dropped_edge.head).neighbors.iter().map(|nb| nb.other).collect_vec());

        // remove dropped vertex from faces adjacent to the contracted edge
        self.face_mut(dropped_edge.left_face.unwrap()).angles.retain(|v| v != &v_dropped);
        self.face_mut(dropped_edge.right_face.unwrap()).angles.retain(|v| v != &v_dropped);

        // special case: remaining degree of dropped vertex is 1 (<=> left and right face of dropped edge are equal)
        if dropped_edge.left_face.unwrap() == dropped_edge.right_face.unwrap() {

            // then angle at kept vertex occurs twice, so remove one occurrence
            let fid = dropped_edge.left_face.unwrap();
            let pos = self.face(fid).angles.iter().position(|&v| v == v_kept);
            self.face_mut(fid).angles.remove(pos.unwrap());

        // normal case
        } else {

            // patch angles in faces adjacent to the dropped vertex
            for eid in self.vertex(v_kept).neighbors.iter().map(|nb| nb.edge).collect_vec() {
                let dir = self.edge(eid).get_signum_by_tail(v_kept);

                let f = match dir {
                    Forward => self.face_mut(self.edge(eid).left_face.unwrap()),
                    Backward => self.face_mut(self.edge(eid).right_face.unwrap())
                };

                if let Some((i, _)) = f.angles.iter().find_position(|v| v == &&v_dropped) {
                    f.angles[i] = v_kept;
                }
            }
        }

        return Ok((v_kept, v_dropped, contracted_eid));
    }

    fn split_edge_(
        &mut self,
        split_eid: EdgeI, new_end: EdgeEnd,
        patch_borders: Option<(VertexI, VertexI)>,
        new_vertex_index: Option<VertexI>, new_edge_index: Option<EdgeI>,
        new_vertex_weight: N, new_edge_weight: E
    ) -> GraphResult<(VertexI, EdgeI)> {
        let pivot_vertex = self.edge(split_eid).get_vertex(new_end);
        let other_vertex = self.edge(split_eid).get_vertex(new_end.inverted());

        let mut patched_sector =
            if let Some((ccw_border, cw_border)) = patch_borders {
                self.vertex(pivot_vertex)
                    .sector_between(ccw_border, cw_border, CW)
                    .into_iter().cloned().collect_vec()
            } else {
                vec![
                    NbVertex {
                        index: 0,
                        end: new_end,
                        other: other_vertex,
                        edge: split_eid
                    }
                ]
            };

        if let None = patched_sector.iter().find(|nb| nb.edge == split_eid) {
            return GraphErr::new_err("Invalid patch sector bounds");
        }

        let patched_vids = patched_sector.iter().map(|nb| nb.other).collect_vec();
        let patched_eids = patched_sector.iter().map(|nb| nb.edge).collect_vec();

        patched_sector.push(
            NbVertex {
                index: 0,
                end: new_end.inverted(),
                other: pivot_vertex,
                edge: EdgeI(0) //to be replaced by new edge id
            }
        );
        for i in 0..patched_sector.len() {
            patched_sector[i].index = i;
        }

        let new_vertex = Vertex {
            id: VertexI(0),
            weight: new_vertex_weight,
            neighbors: patched_sector
        };

        let new_vertex_index = if let Some(idx) = new_vertex_index {
            if !self.vertices.is_available(&idx) {
                return GraphErr::new_err("Desired vertex index is not available");
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
                return GraphErr::new_err("Desired edge index is not available");
            }
            self.edges.insert_with_index(new_edge, &idx);
            idx
        } else {
            self.edges.retrieve_index(new_edge)
        };

        // set indices of new edge and vertex
        self.edge_mut(new_edge_index).id = new_edge_index;
        self.vertex_mut(new_vertex_index).id = new_vertex_index;
        self.vertex_mut(new_vertex_index).neighbors.last_mut().unwrap().edge = new_edge_index;

        // patch old edges
        /*match new_end {
            Tail => self.edge_mut(eid).tail = new_vertex_index,
            Head => self.edge_mut(eid).head = new_vertex_index
        }
        self.vertex_mut(other_vertex).get_nb_mut(pivot_vertex).unwrap().other = new_vertex_index;*/
        for eid in patched_eids {
            self.patch_edge(eid, pivot_vertex, new_vertex_index);
        }

        // patch pivot vertex
        {
            self.vertex_mut(pivot_vertex).neighbors.retain(|nb| !patched_vids.contains(&nb.other) || nb.other == other_vertex);
            let mut nb = self.vertex_mut(pivot_vertex).get_nb_mut(other_vertex).unwrap();
            nb.edge = new_edge_index;
            nb.other = new_vertex_index;
            self.restore_nb_indices(pivot_vertex);
        }

        return Ok((new_vertex_index, new_edge_index))
    }

    pub fn replace_angle(&mut self, fid: &FaceI, from: &VertexI, to: &VertexI) {
        let f = self.face_mut(*fid);
        let idx = f.angles.iter().position(|vid| vid == from).unwrap();
        f.angles[idx] = *to;
    }

    /// new_end = end of the existing edge that will be replaced by the new edge
    pub fn split_embedded_edge(
        &mut self,
        split_eid: EdgeI,
        new_end: EdgeEnd,
        patch_borders: Option<(VertexI, VertexI)>,
        new_vertex_index: Option<VertexI>, new_edge_index: Option<EdgeI>,
        new_vertex_weight: N, new_edge_weight: E
    ) -> GraphResult<(VertexI, EdgeI)> {

        if !self.embedded {
            panic!("This operation is only allowed on embedded graphs");
        }

        let (pivot_vid, _) = self.edge(split_eid).to_vertex_pair(match new_end { Tail => Forward, Head => Backward});
        let (new_vid, new_eid) = self.split_edge_(split_eid, new_end, patch_borders, new_vertex_index, new_edge_index, new_vertex_weight, new_edge_weight)?;

        // patch faces that have been moved from the old to the new vertex
        if self.vertex(new_vid).neighbors.len() > 2 {
            for opposite_vid in self.vertex_cycle(new_vid, pivot_vid, false, CW)
                .skip(2)
                .map(|nb| nb.other)
                .collect_vec()
            {
                let fid = self.get_face(new_vid, opposite_vid, Side::Left);
                self.replace_angle(&fid, &pivot_vid, &new_vid);
            }
        }

        let (ccw_face, cw_face) = {
            if let Some((ccw_border, cw_border)) = patch_borders {
                (
                    self.get_face(pivot_vid, ccw_border, Side::Right),
                    self.get_face(pivot_vid, cw_border, Side::Left)
                )
            } else {
                let e = self.edge(split_eid);
                (
                    match new_end { Tail => e.left_face, Head => e.right_face }.unwrap(),
                    match new_end { Tail => e.right_face, Head => e.left_face }.unwrap(),
                )
            }
        };

        {
            let (right_face, left_face) = swap((cw_face, ccw_face), new_end == Head);
            let e = self.edge_mut(new_eid);
            e.left_face = Some(left_face);
            e.right_face = Some(right_face);
        }

        {
            let f = self.face_mut(ccw_face);
            let pivot_vertex_index = f.angles.iter().position(|&vid| vid == pivot_vid).unwrap();
            f.angles.insert(pivot_vertex_index + 1, new_vid);
        }
        {
            let f = self.face_mut(cw_face);
            let pivot_vertex_index = f.angles.iter().position(|&vid| vid == pivot_vid).unwrap();
            f.angles.insert(pivot_vertex_index, new_vid);
        }

        return Ok((new_vid, new_eid));
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

                    if let Ok((v, _, nb1, nb2)) = self.get_knee_by_face(f.id, vid) {
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
        let (n, m, f) = (self.vertex_count(), self.edge_count(), self.face_count());

        if n + f != 2 + m {
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
    pub fn set_embedding_by_face_cycles(&mut self, faces: Vec<(Vec<VertexI>, F)>) -> GraphResult<()> {
        if !(self.is_simple() && self.is_connected()) {
            return GraphErr::new_err("Embeddings can only be set if the graph is simple and connected");
        }

        for v in faces.iter()
            .flat_map(|(fc, _)| fc.iter()) {
            self.try_vertex(*v)?;
        }

        let n = self.vertices.get_map().len() as isize;
        let m = self.edges.get_map().len() as isize;
        let f = faces.len() as isize;

        if n - m + f != 2 {
            return GraphErr::new_err(&format!("Euler is not happy at all. The number of faces for a full embedding should be {}, you have given {}.", 2 - n + m, f));
        }

        let mut angles = HashMap::new();
        let mut fwd_occurence = HashSet::new();
        let mut backwd_occurence = HashSet::new();

        for (face_cycle_vec, _) in &faces {
            let l = face_cycle_vec.len();
            for i in 0..l {
                // abc-ccw-knee (c directly follows a ccw in b's neighborhood)
                let a = face_cycle_vec[i];
                let b = face_cycle_vec[(i+1) % l];
                let c = face_cycle_vec[(i+2) % l];

                if !angles.contains_key(&b) { angles.insert(b, HashMap::new()); };
                angles.get_mut(&b).unwrap().insert(a, c);

                // covered edges
                let e = self.get_edge(a, b).map_err(|_|
                    GraphErr::new(&format!("The edge ({}, {}) is contained in a face cycle but does not exist in the graph.", a.0, b.0))
                )?;

                match self.get_signum(e, a, b)? {
                    Forward => fwd_occurence.insert(e),
                    Backward => backwd_occurence.insert(e)
                };
            }
        }

        // check if each edge occurs exactly twice, one time as (a,b), one time as (b,a)
        if fwd_occurence.len() < self.edges.get_map().len() {
            return GraphErr::new_err("Not every edge occurs twice in the face cycles");
        }
        if backwd_occurence.len() < self.edges.get_map().len() {
            return GraphErr::new_err("Not every edge occurs twice in the face cycles");
        }

        // check if all vertices have a well-defined total order on their neighbors now
        for i in self.vertices.get_map().keys().copied().collect_vec() {
            let v = self.vertex_mut(i);
            let vertex_angles = angles.get(&v.id).unwrap();

            if vertex_angles.len() != v.neighbors.len() {
                return GraphErr::new_err("Not every vertex has a well-defined total order on their neighbors");
            }

            {
                let neighbors: HashSet<&VertexI> = v.neighbors.iter().map(|nb| &nb.other).collect();
                let angle_entering: HashSet<&VertexI> = vertex_angles.iter().map(|(u, _)| u).collect();
                let angle_leaving: HashSet<&VertexI> = vertex_angles.iter().map(|(_, v)| v).collect();

                if neighbors != angle_entering || neighbors != angle_leaving {
                    return GraphErr::new_err("Not every vertex has a well-defined total order on their neighbors");
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

        self.embedded = true; Ok(())
    }

    pub fn set_embedding_by_vertex_orders(&mut self, neighbor_orders: Vec<(VertexI, Vec<VertexI>)>, f_weights: fn(FaceI) -> F) -> GraphResult<()> {
        if !(self.is_simple() && self.is_connected()) {
            return GraphErr::new_err("Embeddings can only be set if the graph is simple and connected");
        }

        let mut vertices: HashSet<_> = self.vertex_indices().cloned().collect();
        for (v, nb_order) in neighbor_orders {
            if !vertices.contains(&v) {
                return GraphErr::new_err(&format!("Vertex orders are given twice for {}", v));
            }

            let map: HashMap<_,_> = nb_order.iter().enumerate().map(|(i, nb)| (nb, i)).collect();

            let v = self.try_vertex_mut(v)?;
            for nb in &mut v.neighbors {
                nb.index = *map.get(&nb.other).ok_or(GraphErr::new(&format!("Vertex orders are not matching actual neighborhoods for {}", v.id)))?;
            }
            v.neighbors.sort_by_key(|nb| nb.index);
            vertices.remove(&v.id);
        }

        if !vertices.is_empty() {
            return GraphErr::new_err(&format!("No vertex orders given for at least one vertex"));
        }

        self.construct_faces(f_weights); //TODO error handling
        Ok(())
    }

    pub fn get_dual(&self, embedded: bool) -> (PlanarMap<FaceI, EdgeI, VertexI>, HashMap<VertexI, FaceI>, HashMap<EdgeI, EdgeI>, HashMap<FaceI, VertexI>) {

        let mut dual_map = PlanarMap::new();
        dual_map.enforce_simple = false;

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

        if embedded {
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

            dual_map.set_embedding_by_face_cycles(face_cycles).expect("TODO");
            for dual_face in dual_map.faces.get_map().values() {
                primal_vertex_to_dual_face.insert(dual_face.weight, dual_face.id);
            }
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
        self.edges.get(&e).unwrap()
    }

    pub fn try_edge(&self, e: EdgeI) -> Result<&Edge<E>, IndexAccessError<EdgeI>> {
        self.edges.get(&e).ok_or(IndexAccessError::new(e))
    }

    fn edge_mut(&mut self, e: EdgeI) -> &mut Edge<E> {
        self.edges.get_mut(&e).unwrap()
    }

    fn try_edge_mut(&mut self, e: EdgeI) -> Result<&mut Edge<E>, IndexAccessError<EdgeI>> {
        self.edges.get_mut(&e).ok_or(IndexAccessError::new(e))
    }

    /// (tail vertex, tail neighbor, edge, head neighbor, head vertex)
    pub fn edge_with_nb(&self, e: EdgeI) -> (&Vertex<N>, &NbVertex, &Edge<E>, &NbVertex, &Vertex<N>) {
        let e = self.edge(e);
        let tail = self.vertex(e.tail);
        let head = self.vertex(e.head);
        let nb_tail = tail.get_nb(e.head).unwrap();
        let nb_head = head.get_nb(e.tail).unwrap();
        (tail, nb_tail, e, nb_head, head)
    }

    fn face(&self, f: FaceI) -> &Face<F> {
        self.faces.get(&f).unwrap()
    }

    pub fn try_face(&self, f: FaceI) -> Result<&Face<F>, IndexAccessError<FaceI>> {
        self.faces.get(&f).ok_or(IndexAccessError::new(f))
    }

    fn face_mut(&mut self, f: FaceI) -> &mut Face<F> {
        self.faces.get_mut(&f).unwrap()
    }

    /// Unchecked retrieval of the vertex struct for a given vertex index
    fn vertex(&self, v: VertexI) -> &Vertex<N> {
        self.vertices.get(&v).unwrap()
    }

    pub fn try_vertex(&self, v: VertexI) -> Result<&Vertex<N>, IndexAccessError<VertexI>> {
        self.vertices.get(&v).ok_or(IndexAccessError::new(v))
    }

    fn vertex_mut(&mut self, v: VertexI) -> &mut Vertex<N> {
        self.vertices.get_mut(&v).unwrap()
    }

    fn try_vertex_mut(&mut self, v: VertexI) -> Result<&mut Vertex<N>, IndexAccessError<VertexI>> {
        self.vertices.get_mut(&v).ok_or(IndexAccessError::new(v))
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

    //TODO: deprecated
    fn get_edge_with_signum(&self, v1: VertexI, v2: VertexI) -> (EdgeI, Signum) {
        let e = self.get_edge(v1, v2).unwrap();
        (e, self.get_signum(e, v1, v2).unwrap())
    }

    pub fn edge_with_signum(&self, v1: VertexI, v2: VertexI) -> GraphResult<(EdgeI, Signum)> {
        let e = self.get_edge(v1, v2)?;
        Ok((e, self.get_signum(e, v1, v2)?))
    }

    pub fn get_signum(&self, e: EdgeI, v1: VertexI, v2: VertexI) -> GraphResult<Signum> {
        Ok(self.try_edge(e)?.get_signum(v1, v2))
    }

    /// The order of NbVertex is in face definition order (ccw), or seen from the vertex in cw order.
    pub fn get_knee_by_face(&self, fid: FaceI, vid: VertexI) -> GraphResult<(&Vertex<N>, usize, &NbVertex, &NbVertex)> {
        if !self.embedded {
            return not_embedded!();
        };

        let v = self.try_vertex(vid)?;
        if !self.try_face(fid)?.angles.contains(&vid) {
            return GraphErr::new_err(&format!("Given face {} does not have an angle at {}", fid, vid))
        }

        let idx = self.face(fid).angles.iter().position(|&v| v == vid).unwrap();

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
            return Ok((v, idx, first, first));
        } else {
            return Ok((v, idx, first, second));
        }
    }

    pub fn get_knee(&self, e1: EdgeI, e2: EdgeI) ->  GraphResult<(&Vertex<N>, &NbVertex, &NbVertex)> {
        if !self.embedded {
            return GraphErr::new_err("Operation only allowed on embedded graphs.");
        }
        if !self.is_simple() {
            return GraphErr::new_err("Operation only allowed on simple graphs.");
        }

        let e1 = self.try_edge(e1)?;
        let e2 = self.try_edge(e2)?;

        let vertex_intersection = vec![e1.tail, e1.head].intersect(vec![e2.tail, e2.head]);
        match vertex_intersection.len() {
            0 => return GraphErr::new_err("No common vertex, edges do not form a knee"),
            1 => (),
            _ => invalid_graph!()
        };

        //let e1_signum = e1.get_signum_by_tail(vertex_intersection[0]);
        //let e2_signum = e2.get_signum_by_tail(vertex_intersection[0]);

        let v = self.vertex(vertex_intersection[0]);

        let nb1 = v.get_nb(e1.get_other(v.id)).unwrap();
        let nb2 = v.get_nb(e2.get_other(v.id)).unwrap();
        let l = v.neighbors.len();

        assert!(l >= 3);

        if (nb1.index + 1) % l == nb2.index {
            return Ok((v, nb1, nb2));
        }

        if (nb2.index + 1) % l == nb1.index {
            return Ok((v, nb2, nb1));
        }

        GraphErr::new_err("Edges are not next to each other at their common vertex, edges do not form a knee")
    }

}

impl<N, E, F: Clone> Debug for PlanarMap<N, E, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for v in self.vertices.get_map().values().sorted_by_key(|v| v.id.0) {
            writeln!(f, "{:?}", v)?;
        }
        for e in self.edges.get_map().values().sorted_by_key(|e| e.id.0) {
            writeln!(f, "{:?}", e)?;
        }
        if self.embedded {
            for face in self.faces.get_map().values().sorted_by_key(|f| f.id.0) {
                writeln!(f, "{:?}", face)?;
            }
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
        g.add_edge(v1, v2, 1);

        assert_eq!(g.vertex(v1).next_nb(v2, CW).other, v2);
        assert_eq!(g.vertex(v1).next_nb(v2, CCW).other, v2);

        let v3 = g.add_vertex(3);
        g.add_edge(v1, v3, 2);
        g.add_edge(v2, v3, 2);

        g.set_embedding_by_face_cycles(vec![
            (vec![v1, v2, v3], 1),
            (vec![v3, v2, v1], 1),
        ]).expect("ok");

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

        g.set_embedding_by_face_cycles(vec![
            (the_face, 0)
        ]).expect("ok");

        assert_eq!(g.vertex(ctr).sector_between(outer_rim[0], outer_rim[3], CW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[1], outer_rim[2]]);
        assert_eq!(g.vertex(ctr).sector_between(outer_rim[0], outer_rim[7], CCW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[9], outer_rim[8]]);
        assert_eq!(g.vertex(ctr).sector_between(outer_rim[1], outer_rim[0], CCW).iter().map(|nb|nb.other).collect_vec(), vec![]);
        assert_eq!(g.vertex(ctr).sector_between(outer_rim[0], outer_rim[1], CW).iter().map(|nb|nb.other).collect_vec(), vec![]);
        assert_eq!(g.vertex(ctr).sector_between(outer_rim[9], outer_rim[0], CW).iter().map(|nb|nb.other).collect_vec(), vec![]);

        assert_eq!(g.vertex(ctr).sector_between(outer_rim[0], outer_rim[0], CW).iter().map(|nb|nb.other).collect_vec(), outer_rim.iter().cloned().skip(1).collect_vec());
        assert_eq!(g.vertex(ctr).sector_between(outer_rim[0], outer_rim[0], CCW).iter().map(|nb|nb.other).collect_vec(), outer_rim.iter().cloned().skip(1).rev().collect_vec());


        assert_eq!(g.vertex(ctr)._sector_including(outer_rim[0], outer_rim[3], CW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[0], outer_rim[1], outer_rim[2], outer_rim[3]]);
        assert_eq!(g.vertex(ctr)._sector_including(outer_rim[0], outer_rim[7], CCW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[0], outer_rim[9], outer_rim[8], outer_rim[7]]);
        assert_eq!(g.vertex(ctr)._sector_including(outer_rim[1], outer_rim[0], CCW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[1], outer_rim[0]]);
        assert_eq!(g.vertex(ctr)._sector_including(outer_rim[0], outer_rim[1], CW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[0], outer_rim[1]]);
        assert_eq!(g.vertex(ctr)._sector_including(outer_rim[9], outer_rim[0], CW).iter().map(|nb|nb.other).collect_vec(), vec![outer_rim[9], outer_rim[0]]);

        let mut fwd = outer_rim.iter().cloned().collect_vec(); fwd.push(outer_rim[0]);
        let mut bwd = outer_rim.iter().rev().cloned().collect_vec(); bwd.insert(0,outer_rim[0]);

        assert_eq!(g.vertex(ctr)._sector_including(outer_rim[0], outer_rim[0], CW).iter().map(|nb|nb.other).collect_vec(), fwd);
        assert_eq!(g.vertex(ctr)._sector_including(outer_rim[0], outer_rim[0], CCW).iter().map(|nb|nb.other).collect_vec(), bwd);

        let mut h = PlanarMap::new();

        let a = h.add_vertex(0);
        let b = h.add_vertex(1);

        h.add_edge(a, b, 0);

        h.set_embedding_by_face_cycles(vec![
            (vec![a, b], 0)
        ]).expect("test");

        assert_eq!(h.vertex(a).sector_between(b, b, CW).iter().map(|nb|nb.other).collect_vec(), vec![]);
        assert_eq!(h.vertex(a).sector_between(b, b, CCW).iter().map(|nb|nb.other).collect_vec(), vec![]);
        assert_eq!(h.vertex(b).sector_between(a, a, CW).iter().map(|nb|nb.other).collect_vec(), vec![]);
        assert_eq!(h.vertex(b).sector_between(a, a, CCW).iter().map(|nb|nb.other).collect_vec(), vec![]);

        assert_eq!(h.vertex(a)._sector_including(b, b, CW).iter().map(|nb|nb.other).collect_vec(), vec![b, b]);
        assert_eq!(h.vertex(a)._sector_including(b, b, CCW).iter().map(|nb|nb.other).collect_vec(), vec![b, b]);
        assert_eq!(h.vertex(b)._sector_including(a, a, CW).iter().map(|nb|nb.other).collect_vec(), vec![a, a]);
        assert_eq!(h.vertex(b)._sector_including(a, a, CCW).iter().map(|nb|nb.other).collect_vec(), vec![a, a]);

        let mut j = PlanarMap::new();

        let a = j.add_vertex(0);
        let b = j.add_vertex(1);
        let c = j.add_vertex(2);

        j.add_edge(a, b, 0);
        j.add_edge(a, c, 0);

        j.set_embedding_by_face_cycles(vec![
            (vec![a, b, a, c], 0)
        ]).expect("test");

        assert_eq!(j.vertex(a).sector_between(b, b, CW).iter().map(|nb|nb.other).collect_vec(), vec![c]);
        assert_eq!(j.vertex(a).sector_between(b, b, CCW).iter().map(|nb|nb.other).collect_vec(), vec![c]);

        assert_eq!(j.vertex(a)._sector_including(b, b, CW).iter().map(|nb|nb.other).collect_vec(), vec![b, c, b]);
        assert_eq!(j.vertex(a)._sector_including(b, b, CCW).iter().map(|nb|nb.other).collect_vec(), vec![b, c, b]);



    }
}

/*impl<N: Clone, E: Clone, F: Clone, Ty: EdgeType, Ix: IndexType> From<Graph<N, E, Ty, Ix>> for PlanarMap<N, E, F> {
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
    pub fn into_petgraph(&self) -> (Graph<N, E, Undirected, u32>, HashMap<VertexI, NodeIndex<u32>>) {
        let mut g: Graph<N, E, Undirected, u32> = Graph::new_undirected();

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
}*/