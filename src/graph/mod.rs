use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};

use array_tool::vec::Intersect;
use itertools::Itertools;

use self::error::{IndexAccessError, NoSuchEdgeError};
use self::indices::{EdgeI, FaceI, VertexI};
use crate::graph::enums::RevertibleEnum;
use crate::graph::error::GraphResult;
use crate::util;
use crate::util::iterators::cyclic::CyclicIterable;
use data_holders::{Edge, Face, NbVertex, Vertex};
use enums::ClockDirection::{CCW, CW};
use enums::EdgeEnd::{Head, Tail};
use enums::Side::{Left, Right};
use enums::Signum::{Backward, Forward};
use enums::{ClockDirection, EdgeEnd, Side, Signum};
use error::GraphErr;
use index_store::map_index_store::MapIndexStore;
use index_store::vec_index_store::VecIndexStore;
use index_store::IndexStore;

#[macro_export]
macro_rules! invalid_graph {
    () => {
        panic!("Assertion failed, referential integrity of graph obstructed.")
    };
}

macro_rules! internal_err {
    () => {
        format!("PlanarMap internal assertion failed (no closer specification)")
    };
    ($arg: expr) => {
        format!("PlanarMap internal assertion failed: {}", $arg)
    };
}

pub mod data_holders;
pub mod enums;
pub mod error;
mod index_store;
pub mod indices;
pub mod io;

/// Represents a undirected planar map (an embedded graph) with weighted vertices, edges and faces.
/// If one of these items does not need to be weighted, the weights can be of void type '()'.
/// This structure does not necessarily need an the embedding, it can also just hold an abstract
/// graph. In this case, the embedding can be supplemented lateron. By default the graph enforces
/// its own simplicity during construction (i.e. no double edges and loops can be added).
pub struct PlanarMap<N: 'static, E: 'static, F: Clone + 'static> {
    vertices: Box<dyn IndexStore<VertexI, Vertex<N>>>,
    edges: Box<dyn IndexStore<EdgeI, Edge<E>>>,
    faces: Box<dyn IndexStore<FaceI, Face<F>>>,
    //
    embedded: bool,
    enforce_simple: bool,
}

impl<N, E, F: Clone> PlanarMap<N, E, F> {
    /// Creates an empty planar map that enforces its own simplicity (i.e. no double edges and loops
    /// can be added).
    pub fn new() -> PlanarMap<N, E, F> {
        PlanarMap {
            vertices: Box::new(VecIndexStore::new()),
            edges: Box::new(VecIndexStore::new()),
            faces: Box::new(VecIndexStore::new()),
            //
            embedded: false,
            enforce_simple: true,
        }
    }

    /// Clones a planar map. The indices are cloned one to one.
    /// # Arguments
    /// vertex_map - maps the old vertex weights to the new vertex weights
    /// edge_map - maps the old edge weights to the new edge weights
    /// face_map - maps the old face weights to the new face weights
    pub fn clone_with_maps<Nn, Ee, Ff: Clone>(
        &self,
        vertex_map: fn(&N) -> Nn,
        edge_map: fn(&E) -> Ee,
        face_map: Option<fn(&F) -> Ff>,
    ) -> PlanarMap<Nn, Ee, Ff> {
        let mut cloned = PlanarMap::new();

        cloned.vertices = Box::new(MapIndexStore::new());
        for v in self.vertices.get_values() {
            cloned
                .vertices
                .insert(
                    Vertex {
                        id: v.id,
                        neighbors: v.neighbors.clone(),
                        weight: vertex_map(&v.weight),
                    },
                    &v.id,
                )
                .expect("PlanarMap internal error: cloning map with non-unique indices.");
        }

        cloned.edges = Box::new(VecIndexStore::new());
        for e in self.edges.get_values() {
            cloned
                .edges
                .insert(
                    Edge {
                        id: e.id,
                        weight: edge_map(&e.weight),
                        tail: e.tail,
                        head: e.head,
                        left_face: e.left_face,
                        right_face: e.right_face,
                    },
                    &e.id,
                )
                .expect("PlanarMap internal error: cloning map with non-unique indices.");
        }

        if self.is_embedded() {
            if let Some(fmap) = face_map {
                cloned.faces = Box::new(VecIndexStore::new());
                for f in self.faces.get_values() {
                    cloned
                        .faces
                        .insert(
                            Face {
                                id: f.id,
                                weight: fmap(&f.weight),
                                angles: f.angles.clone(),
                            },
                            &f.id,
                        )
                        .expect("PlanarMap internal error: cloning map with non-unique indices.");
                }
            } else {
                panic!("cloning an embedded map needs a face mapping")
            }
        }

        cloned.embedded = self.embedded;
        cloned.enforce_simple = self.enforce_simple;

        return cloned;
    }

    /// Iterator over all vertices.
    pub fn vertices(&self) -> impl Iterator<Item = &Vertex<N>> {
        self.vertices.get_values()
    }

    /// Iterator over all vertex indices.
    pub fn vertex_indices(&self) -> impl Iterator<Item = &VertexI> {
        self.vertices.get_indices()
    }

    /// Returns true if the given vertex index refers to a vertex, i.e. is a valid index.
    pub fn is_valid_vertex(&self, v: &VertexI) -> bool {
        self.vertices.is_valid_index(&v)
    }

    /// Returns the number of vertices.
    pub fn vertex_count(&self) -> usize {
        self.vertices.len()
    }

    /// Returns the vertex weight or an IndexAccessError, if v is an invalid index.
    pub fn vertex_weight(&self, v: VertexI) -> Result<&N, IndexAccessError<VertexI>> {
        Ok(&self.try_vertex(v)?.weight)
    }

    /// Set the vertex weight of vertex v.
    /// # Arguments
    /// v - vertex index of vertex to be weighted
    /// weight - vertex weight to set
    /// # Errors
    /// IndexAccessError if v is an invalid index.
    pub fn set_vertex_weight(
        &mut self,
        v: VertexI,
        weight: N,
    ) -> Result<(), IndexAccessError<VertexI>> {
        let v = self.try_vertex_mut(v)?;
        v.weight = weight;
        return Ok(());
    }

    /// Iterator over all edges.
    pub fn edges(&self) -> impl Iterator<Item = &Edge<E>> {
        self.edges.get_values()
    }

    /// Iterator over all edge indices.
    pub fn edge_indices(&self) -> impl Iterator<Item = &EdgeI> {
        self.edges.get_indices()
    }

    /// Returns true if the given edge index refers to an edge, i.e. is a valid index.
    pub fn is_valid_edge(&self, e: &EdgeI) -> bool {
        self.edges.is_valid_index(&e)
    }

    /// Returns the number of edges.
    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }

    /// Returns the edge weight or an IndexAccessError, if e is an invalid index.
    pub fn edge_weight(&self, e: EdgeI) -> Result<&E, IndexAccessError<EdgeI>> {
        Ok(&self.try_edge(e)?.weight)
    }
    pub fn set_edge_weight(&mut self, e: EdgeI, weight: E) -> Result<(), IndexAccessError<EdgeI>> {
        let e = self.try_edge_mut(e)?;
        e.weight = weight;
        return Ok(());
    }

    /// Iterator over all faces. Does not fail if the graph has no embedding, but yields an
    /// empty iterator then.
    pub fn faces(&self) -> impl Iterator<Item = &Face<F>> {
        self.faces.get_values()
    }

    /// Returns the number of edges or an error if the graph has no embedding.
    pub fn face_count(&self) -> GraphResult<usize> {
        if !self.embedded {
            return GraphErr::embedding_expected();
        }

        Ok(self.faces.len())
    }

    /// Returns true if the given face index refers to a face, i.e. is a valid index.
    /// # Errors
    /// Returns an error if the graph has no embedding.
    pub fn is_valid_face(&self, e: &FaceI) -> GraphResult<bool> {
        if !self.embedded {
            return GraphErr::embedding_expected();
        }
        Ok(self.faces.is_valid_index(&e))
    }

    /// Returns the edge connecting v1 and v2 if it exists, otherwise a NoSuchEdgeError.
    pub fn get_edge(&self, v1: VertexI, v2: VertexI) -> GraphResult<EdgeI> {
        let v = self.try_vertex(v1)?;
        self.try_vertex(v2)?;

        v.neighbors
            .iter()
            .find(|nb| nb.other == v2)
            .map(|nb| nb.edge)
            .ok_or(GraphErr::from(NoSuchEdgeError::new(v1, v2)))
    }

    /// Returns the tuple of vertices connected by an edge.
    /// # Arguments
    /// e - edge index
    /// sig - order of the tuple
    /// # Errors
    /// In case 'e' is an invalid index, an IndexAccessError is returned.
    pub fn edge_pair(
        &self,
        e: EdgeI,
        sig: Signum,
    ) -> Result<(VertexI, VertexI), IndexAccessError<EdgeI>> {
        let e = self.try_edge(e)?;
        return Ok(match sig {
            Forward => (e.tail, e.head),
            Backward => (e.head, e.tail),
        });
    }

    /// Get the face adjacent to an edge. The edge is given by two vertices.
    /// # Arguments
    /// v1 - first vertex (tail vertex)
    /// v2 - second vertex (head vertex)
    /// side - picks the left or right face with respect to an edge from tail to head vertex
    /// # Errors
    /// Fails with an error if the graph is not embedded or if there is no edge between v1 and v2.
    pub fn get_face(&self, v1: VertexI, v2: VertexI, side: Side) -> GraphResult<FaceI> {
        if !self.is_embedded() {
            return GraphErr::embedding_expected();
        }

        let (eid, signum) = self.edge_with_signum(v1, v2)?;
        let e = self.edge(eid);
        let (left, right) = (e.left_face(), e.right_face());

        Ok(match signum {
            Forward => match side {
                Left => left,
                Right => right,
            },
            Backward => match side {
                Left => right,
                Right => left,
            },
        })
    }

    /// Get a section of the fan of faces incident to the central vertex v.
    /// # Arguments
    /// v - central vertex
    /// v1 - starting edge of the fan is given by the edge {v, v1}
    /// v2 - ending edge of the fan is given by the edge {v, v2}
    /// direction - clock direction for choosing the cw or ccw sector between {v, v1} and {v, v2}
    /// # Errors
    /// If any index is invalid or v1, v2 are not in the neighborhood of v an error is returned.
    pub fn faces_between(
        &self,
        v: &VertexI,
        v1: &VertexI,
        v2: &VertexI,
        direction: ClockDirection,
    ) -> GraphResult<Vec<FaceI>> {
        let v = self.vertex(*v);
        if let (Some(nb1), Some(nb2)) = (v.get_nb(*v1), v.get_nb(*v2)) {
            let nbs = v.cycle_while(nb1.index, &|nb| nb.index != nb2.index, CW, true);

            nbs.iter()
                .map(|nb| {
                    self.get_face(
                        v.id,
                        nb.other,
                        match direction {
                            CW => Right,
                            CCW => Left,
                        },
                    )
                })
                .collect()
        } else {
            GraphErr::new_err(&format!("{} or {} is not a neighbor of {}", v1, v2, v))
        }
    }

    /// Checks if the graph has no vertices.
    pub fn is_empty(&self) -> bool {
        self.vertices.is_empty()
    }

    /// Checks if the graph consists of exactly one vertex.
    pub fn is_singleton(&self) -> bool {
        self.vertices.len() == 1
    }

    /// Checks whether or not the graph is simple.
    pub fn is_simple(&self) -> bool {
        let loops = self.edges.get_values().any(|e| e.is_loop());
        let double_edges = self.edges.get_values().unique().count() < self.edges.len();

        !loops && !double_edges
    }

    /// Checks whether or not the graph has an embedding (i.e. is a map).
    pub fn is_embedded(&self) -> bool {
        self.embedded
    }

    /// Returns true if the vertex v is part of the edge e.
    /// # Arguments
    /// e - edge index
    /// v - vertex index
    /// # Errors
    /// Fails with an error in case invalid indices are provided.
    pub fn edge_contains(&self, e: EdgeI, v: VertexI) -> GraphResult<bool> {
        let e = self.try_edge(e)?;
        self.try_vertex(v)?; // check if vertex index is valid
        Ok(e.head == v || e.tail == v)
    }

    /// Returns the next neighbor in the neighbor fan of the central vertex v with respect to
    /// the neighbor nb.
    /// # Arguments
    /// v - central vertex
    /// nb - pivot neighbor
    /// direction - pick the direction
    /// # Errors
    /// Returns an error if there is no edge between v and nb.
    pub fn next_nb(
        &self,
        v: VertexI,
        nb: VertexI,
        direction: ClockDirection,
    ) -> GraphResult<VertexI> {
        let v = self.try_vertex(v)?;
        if let Some(nb) = v.neighbors.iter().find(|&&n| n.other == nb) {
            Ok(v.get_iterator(nb.index, direction, false, true)
                .next()
                .unwrap()
                .other)
        } else {
            GraphErr::new_err(&format!("{:?} is not a neighbor of {:?}", nb, v))
        }
    }

    pub fn vertex_cycle(
        &self,
        vid: VertexI,
        other: VertexI,
        wrap: bool,
        direction: ClockDirection,
    ) -> impl Iterator<Item = &NbVertex> {
        if direction == CCW {
            panic!("unable")
        } //TODO
        let v = self.vertex(vid); //TODO
        let i = v.neighbors.iter().position(|nb| nb.other == other).unwrap(); //TODO
        v.neighbors.cycle(i, wrap)
    }

    /// Iterator over all neighbors of a given vertex v.
    /// # Errors
    /// An invalid vertex index will result in an error.
    pub fn get_neighbors<'a>(&self, v: &VertexI) -> GraphResult<impl Iterator<Item = &VertexI>> {
        Ok(self.try_vertex(*v)?.neighbors.iter().map(|nb| &nb.other))
    }

    pub fn sector_between(
        &self,
        center: VertexI,
        from: VertexI,
        to: VertexI,
        direction: ClockDirection,
    ) -> GraphResult<Vec<(EdgeI, VertexI)>> {
        Ok(self
            .try_vertex(center)?
            .sector_between(from, to, direction)
            .iter()
            .map(|nb| (nb.edge, nb.other))
            .collect_vec())
    }

    /// Returns one endvertex of a given edge.
    /// # Arguments
    /// e - edge index
    /// end - end of the edge to return vertex for
    /// # Errors
    /// IndexAccessError if the edge index is invalid.
    pub fn edge_endvertex(&self, edge: EdgeI, end: EdgeEnd) -> GraphResult<VertexI> {
        let e = self.try_edge(edge)?;
        Ok(match end {
            Tail => e.tail,
            Head => e.head,
        })
    }

    /// Returns the opposite endvertex to vertex v at edge e.
    /// # Errors
    /// Returns an error if one of the indices is invalid, v is not adjacent to e.
    pub fn edge_opposite_vertex(&self, e: EdgeI, v: VertexI) -> GraphResult<VertexI> {
        Ok(self.try_edge(e)?.get_other(v)?)
    }

    /// Returns true if the graph is connected and false otherwise.
    pub fn is_connected(&self) -> bool {
        if self.is_empty() || self.is_singleton() {
            return true;
        }

        let index = self.vertices.get_indices().next().unwrap();
        let connected_component = self.connected_component(*index, &HashSet::new()).unwrap();
        return connected_component.len() == self.vertex_count();
    }

    /// Returns true if all faces have degree three, i.e. the map has either no faces (is not embedded)
    /// or is a triangulation.
    pub fn is_triangulation(&self) -> bool {
        self.faces.get_values().all(|f| f.angles.len() == 3)
    }

    /// Returns the connected component the given vertex v lies in.
    /// A depth first search is conducted to find the connected component.
    /// # Arguments
    /// v - vertex to be contained in the connected component
    /// forbidden_edges - set of edges to be ignored when evaluating the connectivity
    /// # Errors
    /// Returns an IndexAccessError if the vertex index is invalid. Invalid indices in the
    /// collection of forbidden edges are ignored, however.
    pub fn connected_component(
        &self,
        v: VertexI,
        forbidden_edges: &HashSet<EdgeI>,
    ) -> GraphResult<Vec<VertexI>> {
        self.try_vertex(v)?;

        let mut visited = HashSet::new();
        let mut to_visit = vec![v];

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

    /// Finds the shortest path between 'from' and 'to' by the Dijkstra algorithm. Every edge is
    /// counted to have weight one.
    /// # Arguments
    /// from - starting vertex of the path
    /// to - end vertex of the path
    /// forbidden_vertices - vertices that are avoided in the search
    /// # Errors
    /// If from or to are invalid indices, an IndexAccessError is returned.
    pub fn shortest_path(
        &self,
        from: VertexI,
        to: VertexI,
        forbidden_vertices: &HashSet<VertexI>,
    ) -> GraphResult<Option<Vec<VertexI>>> {
        self.try_vertex(from)?;
        self.try_vertex(to)?;

        let mut dist = HashMap::new();
        let mut pred = HashMap::new();
        let mut visited = HashSet::new();
        let mut unvisited: HashSet<_> = self.vertices.get_values().map(|v| v.id).collect();

        dist.insert(from, 0);
        for forbidden in forbidden_vertices {
            unvisited.remove(forbidden);
        }

        while !unvisited.is_empty() {
            let v = *unvisited
                .iter()
                .filter(|v| dist.get(v).is_some())
                .min_by_key(|v| (dist.get(v).unwrap(), v.0))
                .unwrap();

            visited.insert(v);
            unvisited.remove(&v);

            let d = *dist.get(&v).unwrap();

            self.vertex(v)
                .neighbors
                .iter()
                .map(|nb| nb.other)
                .filter(|&other| unvisited.contains(&other))
                .for_each(|other| {
                    if !dist.contains_key(&other) || *dist.get(&other).unwrap() > d + 1 {
                        dist.insert(other, d + 1);
                        pred.insert(other, v);
                    }
                });
        }

        if dist.contains_key(&to) {
            Ok(None)
        } else {
            let mut rev_path = vec![to];
            let mut v = to;
            while let Some(predecessor) = pred.get(&v) {
                rev_path.push(*predecessor);
                v = *predecessor;
            }

            Ok(Some(rev_path.into_iter().rev().collect()))
        }
    }

    pub fn add_vertex(&mut self, weight: N) -> VertexI {
        if self.embedded {
            panic!("superplanar operation on embedded graph.");
        }

        let v = Vertex {
            id: VertexI(0),
            neighbors: Vec::new(),
            weight,
        };

        let index = self.vertices.push(v);
        //self.vertices.get_mut(&index).unwrap().id = index;
        return index;
    }

    pub fn add_edge(&mut self, v1: VertexI, v2: VertexI, weight: E) -> EdgeI {
        if self.embedded {
            panic!("superplanar operation on embedded graph.")
        }

        return self.add_edge_(v1, v2, None, None, weight);
    }

    fn add_edge_(
        &mut self,
        v1: VertexI,
        v2: VertexI,
        v1_nb_index: Option<usize>,
        v2_nb_index: Option<usize>,
        weight: E,
    ) -> EdgeI {
        if self.enforce_simple {
            if v1 == v2 {
                panic!(
                    "With this edge, the graph would not be simple any longer ('enforce_simple')."
                );
            }
            if let Some(_) = self.vertex(v1).get_nb(v2) {
                panic!(
                    "With this edge, the graph would not be simple any longer ('enforce_simple')."
                );
            }
        }

        let e = Edge {
            id: EdgeI(0),
            tail: v1,
            head: v2,
            weight,
            left_face: None,
            right_face: None,
        };
        let id = self.edges.push(e);

        // v1 --> v2 (v1 = tail, v2 = head)
        let index = match v1_nb_index {
            None => self.vertex(v1).neighbors.len(),
            Some(i) => i,
        };
        self.vertex_mut(v1).neighbors.push(NbVertex {
            index,
            other: v2,
            edge: id,
            end: Tail,
        });

        let index = match v2_nb_index {
            None => self.vertex(v2).neighbors.len(),
            Some(i) => i,
        };
        self.vertex_mut(v2).neighbors.push(NbVertex {
            index,
            other: v1,
            edge: id,
            end: Head,
        });

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

    /// Adds an edge to a map.
    /// # Arguments
    /// v1 - tail vertex of the edge
    /// v2 - head vertex of the edge
    /// weight - weight of the edge
    /// face - face that is dissected by the edge
    /// # Errors
    /// Erros can occur if the graph is not embedded, the face is not incident to v1 and v2 or
    /// v1, v2 or face are invalid indices.
    pub fn add_embedded_edge(
        &mut self,
        v1: VertexI,
        v2: VertexI,
        weight: E,
        face: FaceI,
    ) -> GraphResult<EdgeI> {
        if !self.embedded {
            return GraphErr::embedding_expected();
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

        let old_face = self.faces.remove(&face).unwrap();

        let mut f1 = Face {
            id: FaceI(0),
            weight: old_face.weight.clone(),
            angles: Vec::new(),
        };

        let mut f2 = Face {
            id: FaceI(0),
            weight: old_face.weight.clone(),
            angles: Vec::new(),
        };

        for &vid in old_face.angles.cycle(pos1, false) {
            f1.angles.push(vid);
            if vid == v2 {
                break;
            }
        }

        for &vid in old_face.angles.cycle(pos2, false) {
            f2.angles.push(vid);
            if vid == v1 {
                break;
            }
        }

        let fid1 = self.faces.push(f1);
        let fid2 = self.faces.push(f2);

        self.restore_face_refs(fid1);
        self.restore_face_refs(fid2);

        return Ok(e);
    }

    fn remove_edge_(&mut self, v1: VertexI, v2: VertexI) -> GraphResult<Edge<E>> {
        let eid = self.get_edge(v1, v2)?;

        let e = self.edges.remove(&eid).unwrap();
        self.vertex_mut(v1).neighbors.retain(|nb| nb.other != v2);
        self.vertex_mut(v2).neighbors.retain(|nb| nb.other != v1);

        self.restore_nb_indices(v1);
        self.restore_nb_indices(v2);

        return Ok(e);
    }

    /// Removes an edge of an abstract graph
    /// # Arguments
    /// v1 - first endvertex of the edge to remove
    /// v2 - second endvertex of the edge to remove
    /// # Errors
    /// If there is no edge between v1 and v2 (which is also the case if v1 or v2 is an invalid index)
    /// an error is returned.
    pub fn remove_edge(&mut self, v1: VertexI, v2: VertexI) -> GraphResult<EdgeI> {
        if self.embedded {
            return GraphErr::no_embedding_expected();
        }

        self.remove_edge_(v1, v2).map(|e| e.id)
    }

    pub fn remove_embedded_edge_by_index(
        &mut self,
        e: EdgeI,
        merge_weights: &dyn Fn(F, F) -> F,
    ) -> GraphResult<(EdgeI, FaceI)> {
        let (v1, v2) = self.edge_pair(e, Forward)?;
        self.remove_embedded_edge(v1, v2, merge_weights)
    }

    pub fn remove_embedded_edge(
        &mut self,
        v1: VertexI,
        v2: VertexI,
        merge_weights: &dyn Fn(F, F) -> F,
    ) -> GraphResult<(EdgeI, FaceI)> {
        if !self.embedded {
            panic!("no embedding given");
        }

        let e = self.remove_edge_(v1, v2)?;

        // two faces must be merged
        let left_face = self.faces.remove(&e.left_face()).unwrap();
        let right_face = self.faces.remove(&e.right_face()).unwrap();

        let weight = merge_weights(left_face.weight, right_face.weight);

        let right_tail_pos = right_face
            .angles
            .iter()
            .position(|vid| *vid == e.tail)
            .unwrap();
        let left_head_pos = left_face
            .angles
            .iter()
            .position(|vid| *vid == e.head)
            .unwrap();

        let mut cycle: Vec<_> = right_face
            .angles
            .cycle(right_tail_pos, false)
            .skip(1)
            .collect();
        cycle.extend(left_face.angles.cycle(left_head_pos, false).skip(1));

        let f = Face {
            id: FaceI(0),
            angles: cycle.into_iter().copied().collect(),
            weight,
        };

        let fid = self.faces.push(f);
        self.restore_face_refs(fid);

        return Ok((e.id, fid));
    }

    /// the nbvectors of from and to are not touched!
    pub fn patch_edge(&mut self, eid: EdgeI, from: VertexI, to: VertexI) {
        let (end, other) = match self.edge(eid).to_vertex_pair(Forward) {
            (tail, head) if tail == from => (Tail, head),
            (tail, head) if head == from => (Head, tail),
            _ => panic!("no"), //TODO
        };

        match end {
            Head => {
                self.edge_mut(eid).head = to;
            }
            Tail => {
                self.edge_mut(eid).tail = to;
            }
        }
        self.vertex_mut(other).get_nb_mut(from).unwrap().other = to;
    }

    /// Does not return the given eid in case there is no other edge
    pub fn next_edge(&self, vid: VertexI, eid: EdgeI, direction: ClockDirection) -> Option<EdgeI> {
        let v = self.vertex(vid);
        let pos = v.neighbors.iter().find(|nb| nb.edge == eid).unwrap().index;

        match direction {
            CW => v.neighbors.cycle(pos, false).nth(1).map(|nb| nb.edge),
            CCW => v.neighbors.cycle(pos, false).rev().next().map(|nb| nb.edge),
        }
    }

    /// returns sustained vertex, deleted vertex and edge index
    pub fn contract_embedded_edge(
        &mut self,
        eid: EdgeI,
        keep: EdgeEnd,
    ) -> GraphResult<(VertexI, VertexI, EdgeI)> {
        unsafe { self.contract_embedded_edge_(eid, keep) }
    }

    /// The specified vertex (by keep) is retained.
    /// Returns retained vertex index, deleted vertex index and deleted edge index
    unsafe fn contract_embedded_edge_(
        &mut self,
        contracted_eid: EdgeI,
        keep: EdgeEnd,
    ) -> GraphResult<(VertexI, VertexI, EdgeI)> {
        let s: *mut Self = self;

        let (v_dropped, v_kept) = {
            let (v_dropped, v_kept) = {
                let e = self.edge(contracted_eid);
                match keep {
                    Head => (e.tail, e.head),
                    Tail => (e.head, e.tail),
                }
            };

            let dropped_cw = self.next_edge(v_dropped, contracted_eid, CW);
            let dropped_ccw = self.next_edge(v_dropped, contracted_eid, CCW);
            let kept_cw = self.next_edge(v_kept, contracted_eid, CW);
            let kept_ccw = self.next_edge(v_kept, contracted_eid, CCW);

            if let None = kept_cw.and(kept_ccw).and(dropped_cw).and(dropped_ccw) {
                return GraphErr::new_err("Edge contraction is only supported if the contracted edge is adjacent to any other edge.");
            }

            // right and left regarding to the orientation dropped -> kept
            let dropped_right = self.edge(dropped_cw.unwrap());
            let dropped_left = self.edge(dropped_ccw.unwrap());
            let kept_right = self.edge(kept_ccw.unwrap());
            let kept_left = self.edge(kept_cw.unwrap());

            let right_face_collapse = dropped_right.get_other(v_dropped).expect(&internal_err!())
                == kept_right.get_other(v_kept).expect(&internal_err!());

            let left_face_collapse = dropped_left.get_other(v_dropped).expect(&internal_err!())
                == kept_left.get_other(v_kept).expect(&internal_err!());

            let dropped_single_edge = dropped_cw == dropped_ccw;

            if right_face_collapse {
                let right_face_merge: Box<dyn Fn(_, _) -> _> = match dropped_right
                    .get_signum_by_tail(v_dropped)
                    .expect(&internal_err!())
                {
                    Forward => Box::new(|_, b| b),
                    Backward => Box::new(|a, _| a),
                };

                //(*s).edge_mut(kept_right.id).weight = merge_weights(&removed_edge, &kept_right);
                (*s).remove_embedded_edge_by_index(dropped_right.id, &right_face_merge)?;
            }

            if left_face_collapse && !dropped_single_edge {
                let left_face_merge: Box<dyn Fn(_, _) -> _> = match dropped_left
                    .get_signum_by_tail(v_dropped)
                    .expect(&internal_err!())
                {
                    Forward => Box::new(|a, _| a),
                    Backward => Box::new(|_, b| b),
                };

                (*s).remove_embedded_edge_by_index(dropped_left.id, &left_face_merge)?;
            }

            (v_dropped, v_kept)
        };

        let dropped_edge = self.edges.remove(&contracted_eid).unwrap();
        let dropped_vertex = self.vertices.remove(&v_dropped).unwrap();

        // patch edges at the dropped and of the contracted edge and their adjacent vertices
        for nb in dropped_vertex
            .neighbors
            .iter()
            .filter(|nb| nb.other != v_kept)
        {
            let patch_e = (*s).edge_mut(nb.edge);
            let patch_v = match nb.end {
                Tail => {
                    patch_e.tail = v_kept;
                    (*s).vertex_mut(patch_e.head)
                }
                Head => {
                    patch_e.head = v_kept;
                    (*s).vertex_mut(patch_e.tail)
                }
            };

            for patch_nb in patch_v
                .neighbors
                .iter_mut()
                .filter(|nb| nb.other == v_dropped)
            {
                patch_nb.other = v_kept;
            }
        }

        // insert the fan of neighbors previously attached to v_dropped now to v_kept
        {
            let v = self.vertex_mut(v_kept);
            let kept_index = v.get_nb(v_dropped).unwrap().index;
            let dropped_index = dropped_vertex.get_nb(v_kept).unwrap().index;
            v.neighbors.remove(kept_index);
            v.neighbors.splice(
                kept_index..kept_index,
                dropped_vertex
                    .neighbors
                    .cycle(dropped_index, false)
                    .filter(|nb| nb.other != v_kept)
                    .map(|nb| *nb),
            );
        }
        self.restore_nb_indices(v_kept);

        // remove dropped vertex from faces adjacent to the contracted edge
        self.face_mut(dropped_edge.left_face())
            .angles
            .retain(|v| v != &v_dropped);
        self.face_mut(dropped_edge.right_face())
            .angles
            .retain(|v| v != &v_dropped);

        // special case: remaining degree of dropped vertex is 1 (<=> left and right face of dropped edge are equal)
        if dropped_edge.left_face() == dropped_edge.right_face() {
            // then angle at kept vertex occurs twice, so remove one occurrence
            let fid = dropped_edge.left_face();
            let pos = self.face(fid).angles.iter().position(|&v| v == v_kept);
            self.face_mut(fid).angles.remove(pos.unwrap());

        // normal case
        } else {
            // patch angles in faces adjacent to the dropped vertex
            for eid in self
                .vertex(v_kept)
                .neighbors
                .iter()
                .map(|nb| nb.edge)
                .collect_vec()
            {
                let dir = self
                    .edge(eid)
                    .get_signum_by_tail(v_kept)
                    .expect(&internal_err!());

                let f = match dir {
                    Forward => self.face_mut(self.edge(eid).left_face()),
                    Backward => self.face_mut(self.edge(eid).right_face()),
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
        split_eid: EdgeI,
        new_end: EdgeEnd,
        patch_borders: Option<(VertexI, VertexI)>,
        new_vertex_index: Option<VertexI>,
        new_edge_index: Option<EdgeI>,
        new_vertex_weight: N,
        new_edge_weight: E,
    ) -> GraphResult<(VertexI, EdgeI)> {
        let pivot_vertex = self.edge(split_eid).get_vertex(new_end);
        let other_vertex = self.edge(split_eid).get_vertex(new_end.reversed());

        let mut patched_sector = if let Some((ccw_border, cw_border)) = patch_borders {
            self.vertex(pivot_vertex)
                .sector_between(ccw_border, cw_border, CW)
                .into_iter()
                .cloned()
                .collect_vec()
        } else {
            vec![NbVertex {
                index: 0,
                end: new_end,
                other: other_vertex,
                edge: split_eid,
            }]
        };

        if let None = patched_sector.iter().find(|nb| nb.edge == split_eid) {
            return GraphErr::new_err("Invalid patch sector bounds");
        }

        let patched_vids = patched_sector.iter().map(|nb| nb.other).collect_vec();
        let patched_eids = patched_sector.iter().map(|nb| nb.edge).collect_vec();

        patched_sector.push(NbVertex {
            index: 0,
            end: new_end.reversed(),
            other: pivot_vertex,
            edge: EdgeI(0), //to be replaced by new edge id
        });
        for i in 0..patched_sector.len() {
            patched_sector[i].index = i;
        }

        let new_vertex = Vertex {
            id: VertexI(0),
            weight: new_vertex_weight,
            neighbors: patched_sector,
        };

        let new_vertex_index = if let Some(idx) = new_vertex_index {
            if let Err(_) = self.vertices.insert(new_vertex, &idx) {
                return GraphErr::new_err("Desired vertex index is not available.");
            }
            idx
        } else {
            self.vertices.push(new_vertex)
        };

        let new_edge = Edge {
            id: EdgeI(0),
            tail: if new_end == Tail {
                pivot_vertex
            } else {
                new_vertex_index
            },
            head: if new_end == Tail {
                new_vertex_index
            } else {
                pivot_vertex
            },
            left_face: None,
            right_face: None,
            weight: new_edge_weight,
        };

        let new_edge_index = if let Some(idx) = new_edge_index {
            if let Err(_) = self.edges.insert(new_edge, &idx) {
                return GraphErr::new_err("Desired edge index is not available");
            }
            idx
        } else {
            self.edges.push(new_edge)
        };

        // set indices of new edge and vertex
        self.edge_mut(new_edge_index).id = new_edge_index;
        self.vertex_mut(new_vertex_index).id = new_vertex_index;
        self.vertex_mut(new_vertex_index)
            .neighbors
            .last_mut()
            .unwrap()
            .edge = new_edge_index;

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
            self.vertex_mut(pivot_vertex)
                .neighbors
                .retain(|nb| !patched_vids.contains(&nb.other) || nb.other == other_vertex);
            let mut nb = self
                .vertex_mut(pivot_vertex)
                .get_nb_mut(other_vertex)
                .unwrap();
            nb.edge = new_edge_index;
            nb.other = new_vertex_index;
            self.restore_nb_indices(pivot_vertex);
        }

        return Ok((new_vertex_index, new_edge_index));
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
        new_vertex_index: Option<VertexI>,
        new_edge_index: Option<EdgeI>,
        new_vertex_weight: N,
        new_edge_weight: E,
    ) -> GraphResult<(VertexI, EdgeI)> {
        if !self.embedded {
            return GraphErr::embedding_expected();
        }

        let (pivot_vid, _) = self.edge(split_eid).to_vertex_pair(match new_end {
            Tail => Forward,
            Head => Backward,
        });
        let (new_vid, new_eid) = self.split_edge_(
            split_eid,
            new_end,
            patch_borders,
            new_vertex_index,
            new_edge_index,
            new_vertex_weight,
            new_edge_weight,
        )?;

        // patch faces that have been moved from the old to the new vertex
        if self.vertex(new_vid).neighbors.len() > 2 {
            for opposite_vid in self
                .vertex_cycle(new_vid, pivot_vid, false, CW)
                .skip(2)
                .map(|nb| nb.other)
                .collect_vec()
            {
                let fid = self.get_face(new_vid, opposite_vid, Side::Left)?;
                self.replace_angle(&fid, &pivot_vid, &new_vid);
            }
        }

        let (ccw_face, cw_face) = {
            if let Some((ccw_border, cw_border)) = patch_borders {
                (
                    self.get_face(pivot_vid, ccw_border, Side::Right)?,
                    self.get_face(pivot_vid, cw_border, Side::Left)?,
                )
            } else {
                let e = self.edge(split_eid);
                (
                    match new_end {
                        Tail => e.left_face(),
                        Head => e.right_face(),
                    },
                    match new_end {
                        Tail => e.right_face(),
                        Head => e.left_face(),
                    },
                )
            }
        };

        {
            let (right_face, left_face) = util::swap((cw_face, ccw_face), new_end == Head);
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

    /// For debugging purposes. Should always return true.
    #[allow(dead_code)]
    fn check_referential_integrity(&self) -> bool {
        let mut edge_count = 0;
        let mut half_edge_count = 0;

        for v in self.vertices.get_values() {
            for nb in v.neighbors.iter() {
                half_edge_count += 1;

                if let Ok(e) = self.try_edge(nb.edge) {
                    match nb.end {
                        Tail => {
                            if e.tail != v.id {
                                return false;
                            }
                        }
                        Head => {
                            if e.head != v.id {
                                return false;
                            }
                        }
                    }

                    let opposite = e.get_other(v.id);
                    if let Ok(opp) = opposite {
                        if self.try_vertex(opp).is_err() {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }

        for e in self.edges.get_values() {
            edge_count += 1;
            if self.try_vertex(e.head).is_err() || self.try_vertex(e.tail).is_err() {
                return false;
            }
        }

        // handshake lemma
        if half_edge_count != 2 * edge_count {
            return false;
        }

        if self.embedded {
            for e in self.edges.get_values() {
                self.face(e.left_face());
                self.face(e.right_face());
            }

            for f in self.faces.get_values() {
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
    fn construct_faces(&mut self, f_weights: fn(FaceI) -> F) -> GraphResult<()> {
        if self.embedded {
            return GraphErr::no_embedding_expected();
        }

        for eid in self.edges.get_indices().cloned().collect_vec() {
            if self.edge(eid).left_face.is_none() {
                self.build_face_cycle(eid, Left, f_weights)?;
            }

            if self.edge(eid).right_face.is_none() {
                self.build_face_cycle(eid, Right, f_weights)?;
            }
        }

        // check euler's formula
        let (n, m, f) = (self.vertex_count(), self.edge_count(), self.faces.len());
        assert_eq!(n + f, 2 + m);

        self.embedded = true;
        Ok(())
    }

    /// Builds the face cycle (from the incidence orders at the vertices) incident to a given edge.
    /// # Arguments
    /// * eid - the edge
    /// * side - side of the edge the face cycle is located (side in forward direction)
    /// * f_weights - face weight function
    fn build_face_cycle(
        &mut self,
        eid: EdgeI,
        side: Side,
        f_weights: fn(FaceI) -> F,
    ) -> GraphResult<FaceI> {
        let mut cycle = Vec::new();
        let fid = self.faces.next_index();

        let mut next = {
            let e = self.try_edge_mut(eid)?;
            match side {
                Left => {
                    cycle.push(e.tail);
                    e.left_face = Some(fid);
                    e.head
                }
                Right => {
                    cycle.push(e.head);
                    e.right_face = Some(fid);
                    e.tail
                }
            }
        };

        while next != cycle[0] && cycle.len() <= self.vertex_count() + 1 {
            cycle.push(next);
            let l = cycle.len();
            next = self.vertex(cycle[l - 1]).next_by_nb(cycle[l - 2], CW).other;

            let (intermediate_eid, s) = self.edge_with_signum(cycle[l - 1], next)?;
            let e = self.edge_mut(intermediate_eid);
            match s {
                Forward => e.left_face = Some(fid),
                Backward => e.right_face = Some(fid),
            }
        }

        if cycle.len() > self.vertex_count() {
            return GraphErr::new_err(
                "The graph does not seem to be planar or the vertex orders are messed up: a proper embedding can not be constructed.",
            );
        }

        let face = Face {
            id: FaceI(0),
            weight: f_weights(fid),
            angles: cycle,
        };
        Ok(self.faces.push(face))
    }

    /// Gives the graph an embedding into the sphere (i.e. no outer face is selected). The argument
    /// `faces` is a vector of all face cycles. The face cycles are vectors of vertex indices,
    /// specifying the order of vertices around the face in counterclockwise direction.
    pub fn set_embedding_by_face_cycles(
        &mut self,
        faces: Vec<(Vec<VertexI>, F)>,
    ) -> GraphResult<()> {
        if !(self.is_simple() && self.is_connected()) {
            return GraphErr::new_err(
                "Embeddings can only be set if the graph is simple and connected",
            );
        }

        for v in faces.iter().flat_map(|(fc, _)| fc.iter()) {
            self.try_vertex(*v)?;
        }

        let n = self.vertices.len() as isize;
        let m = self.edges.len() as isize;
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
                let b = face_cycle_vec[(i + 1) % l];
                let c = face_cycle_vec[(i + 2) % l];

                if !angles.contains_key(&b) {
                    angles.insert(b, HashMap::new());
                };
                angles.get_mut(&b).unwrap().insert(a, c);

                // covered edges
                let e = self.get_edge(a, b).map_err(|_|
                    GraphErr::new(&format!("The edge ({}, {}) is contained in a face cycle but does not exist in the graph.", a.0, b.0))
                )?;

                match self.get_signum_by_tail(e, a)? {
                    Forward => fwd_occurence.insert(e),
                    Backward => backwd_occurence.insert(e),
                };
            }
        }

        // check if each edge occurs exactly twice, one time as (a,b), one time as (b,a)
        if fwd_occurence.len() < self.edges.len() {
            return GraphErr::new_err("Not every edge occurs twice in the face cycles");
        }
        if backwd_occurence.len() < self.edges.len() {
            return GraphErr::new_err("Not every edge occurs twice in the face cycles");
        }

        // check if all vertices have a well-defined total order on their neighbors now
        for i in self.vertices.get_indices().cloned().collect_vec() {
            let v = self.vertex_mut(i);
            let vertex_angles = angles.get(&v.id).unwrap();

            if vertex_angles.len() != v.neighbors.len() {
                return GraphErr::new_err(
                    "Not every vertex has a well-defined total order on their neighbors",
                );
            }

            {
                let neighbors: HashSet<&VertexI> = v.neighbors.iter().map(|nb| &nb.other).collect();
                let angle_entering: HashSet<&VertexI> =
                    vertex_angles.iter().map(|(u, _)| u).collect();
                let angle_leaving: HashSet<&VertexI> =
                    vertex_angles.iter().map(|(_, v)| v).collect();

                if neighbors != angle_entering || neighbors != angle_leaving {
                    return GraphErr::new_err(
                        "Not every vertex has a well-defined total order on their neighbors",
                    );
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
                    let mut next_nb = old_neighbors
                        .iter_mut()
                        .find(|nb| nb.other == *next_vector)
                        .unwrap();
                    next_nb.index = v.neighbors.len();
                    v.neighbors.push(*next_nb);
                    cur = next_nb;
                }
            }
        }

        // create face structs and set left_/right_face of edges
        for (face_cycle_vec, weight) in faces {
            let l = face_cycle_vec.len();
            let f = Face {
                id: FaceI(0),
                angles: face_cycle_vec.clone(),
                weight,
            };
            let id = self.faces.push(f);

            for i in 0..l {
                let v1 = face_cycle_vec[i];
                let v2 = face_cycle_vec[(i + 1) % l];
                let edge = self.edge_mut(self.get_edge(v1, v2).unwrap());

                match edge.get_signum_by_tail(v1).expect(&internal_err!()) {
                    Forward => edge.left_face = Some(id),
                    Backward => edge.right_face = Some(id),
                }
            }
        }

        self.embedded = true;
        Ok(())
    }

    pub fn set_embedding_by_vertex_orders(
        &mut self,
        neighbor_orders: Vec<(VertexI, Vec<VertexI>)>,
        f_weights: fn(FaceI) -> F,
    ) -> GraphResult<()> {
        if !(self.is_simple() && self.is_connected()) {
            return GraphErr::new_err(
                "Embeddings can only be set if the graph is simple and connected",
            );
        }

        let mut vertices: HashSet<_> = self.vertex_indices().cloned().collect();
        for (v, nb_order) in neighbor_orders {
            if !vertices.contains(&v) {
                return GraphErr::new_err(&format!("Vertex orders are given twice for {}", v));
            }

            let map: HashMap<_, _> = nb_order.iter().enumerate().map(|(i, nb)| (nb, i)).collect();

            let v = self.try_vertex_mut(v)?;
            for nb in &mut v.neighbors {
                nb.index = *map.get(&nb.other).ok_or(GraphErr::new(&format!(
                    "Vertex orders are not matching actual neighborhoods for {}",
                    v.id
                )))?;
            }
            v.neighbors.sort_by_key(|nb| nb.index);
            vertices.remove(&v.id);
        }

        if !vertices.is_empty() {
            return GraphErr::new_err(&format!("No vertex orders given for at least one vertex"));
        }

        self.construct_faces(f_weights)?;
        Ok(())
    }

    pub fn get_dual(
        &self,
        embedded: bool,
    ) -> (
        PlanarMap<FaceI, EdgeI, VertexI>,
        HashMap<VertexI, FaceI>,
        HashMap<EdgeI, EdgeI>,
        HashMap<FaceI, VertexI>,
    ) {
        let mut dual_map = PlanarMap::new();
        dual_map.enforce_simple = false;

        let mut primal_vertex_to_dual_face = HashMap::new();
        let mut primal_face_to_dual_vertex = HashMap::new();
        let mut primal_edge_to_dual_edge = HashMap::new();

        for f in self.faces.get_values() {
            let dual_vid = dual_map.add_vertex(f.id);
            primal_face_to_dual_vertex.insert(f.id, dual_vid);
        }

        for (vid, fid) in dual_map
            .vertices
            .get_values()
            .map(|v| (v.id, v.weight))
            .collect_vec()
        {
            let f = self.face(fid);
            for (&v1, &v2) in f.angles.cycle(0, true).tuple_windows() {
                let e = self.edge(self.get_edge(v1, v2).unwrap());
                if f.id == e.left_face.unwrap() {
                    let other_vertex = primal_face_to_dual_vertex
                        .get(&e.right_face.unwrap())
                        .unwrap();
                    let new_edge = dual_map.add_edge(vid, *other_vertex, e.id);
                    primal_edge_to_dual_edge.insert(e.id, new_edge);
                }
            }
        }

        if embedded {
            let mut face_cycles = vec![];

            for v in self.vertices.get_values() {
                let cycle = v
                    .neighbors
                    .iter()
                    .map(|nb| {
                        let f = match nb.end {
                            Tail => self.edge(nb.edge).left_face.unwrap(),
                            Head => self.edge(nb.edge).right_face.unwrap(),
                        };
                        *primal_face_to_dual_vertex.get(&f).unwrap()
                    })
                    .collect_vec();

                face_cycles.push((cycle, v.id));
            }

            dual_map
                .set_embedding_by_face_cycles(face_cycles)
                .expect("TODO");
            for dual_face in dual_map.faces.get_values() {
                primal_vertex_to_dual_face.insert(dual_face.weight, dual_face.id);
            }
        }

        (
            dual_map,
            primal_vertex_to_dual_face,
            primal_edge_to_dual_edge,
            primal_face_to_dual_vertex,
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
    pub fn edge_with_nb(
        &self,
        e: EdgeI,
    ) -> (&Vertex<N>, &NbVertex, &Edge<E>, &NbVertex, &Vertex<N>) {
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

        let boundary_cycle: Vec<_> = face
            .angles
            .cycle(0, true)
            .tuple_windows()
            .map(|(&v1, &v2)| {
                self.edge_with_signum(v1, v2)
                    .expect(&internal_err!("face cycle contains invalid edges"))
            })
            .collect();

        for (eid, signum) in boundary_cycle {
            match signum {
                Forward => self.edge_mut(eid).left_face = Some(fid),
                Backward => self.edge_mut(eid).right_face = Some(fid),
            }
        }
    }

    pub fn edge_with_signum(&self, v1: VertexI, v2: VertexI) -> GraphResult<(EdgeI, Signum)> {
        let e = self.get_edge(v1, v2)?;
        Ok((e, self.get_signum_by_tail(e, v1)?))
    }

    pub fn get_signum_by_tail(&self, e: EdgeI, v: VertexI) -> GraphResult<Signum> {
        self.try_edge(e)?.get_signum_by_tail(v)
    }

    /// The order of NbVertex is in face definition order (ccw), or seen from the vertex in cw order.
    pub fn get_knee_by_face(
        &self,
        fid: FaceI,
        vid: VertexI,
    ) -> GraphResult<(&Vertex<N>, usize, &NbVertex, &NbVertex)> {
        if !self.embedded {
            return GraphErr::no_embedding_expected();
        };

        let v = self.try_vertex(vid)?;
        if !self.try_face(fid)?.angles.contains(&vid) {
            return GraphErr::new_err(&format!(
                "Given face {} does not have an angle at {}",
                fid, vid
            ));
        }

        let idx = self
            .face(fid)
            .angles
            .iter()
            .position(|&v| v == vid)
            .unwrap();

        let first = v
            .neighbors
            .iter()
            .find(|nb| {
                let e = self.edge(nb.edge);
                match nb.end {
                    Head => e.left_face.unwrap() == fid,
                    Tail => e.right_face.unwrap() == fid,
                }
            })
            .unwrap();

        let second = v
            .neighbors
            .iter()
            .find(|nb| {
                let e = self.edge(nb.edge);
                match nb.end {
                    Head => e.right_face.unwrap() == fid,
                    Tail => e.left_face.unwrap() == fid,
                }
            })
            .unwrap();

        if first.other == second.other {
            return Ok((v, idx, first, first));
        } else {
            return Ok((v, idx, first, second));
        }
    }

    pub fn get_knee(
        &self,
        e1: EdgeI,
        e2: EdgeI,
    ) -> GraphResult<(&Vertex<N>, &NbVertex, &NbVertex)> {
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
            _ => invalid_graph!(),
        };

        //let e1_signum = e1.get_signum_by_tail(vertex_intersection[0]);
        //let e2_signum = e2.get_signum_by_tail(vertex_intersection[0]);

        let v = self.vertex(vertex_intersection[0]);

        let nb1 = v
            .get_nb(e1.get_other(v.id).expect(&internal_err!()))
            .expect(&internal_err!());
        let nb2 = v
            .get_nb(e2.get_other(v.id).expect(&internal_err!()))
            .expect(&internal_err!());
        let l = v.neighbors.len();

        assert!(l >= 3);

        if (nb1.index + 1) % l == nb2.index {
            return Ok((v, nb1, nb2));
        }

        if (nb2.index + 1) % l == nb1.index {
            return Ok((v, nb2, nb1));
        }

        GraphErr::new_err(
            "Edges are not next to each other at their common vertex, edges do not form a knee",
        )
    }
}

impl<N, E, F: Clone> Debug for PlanarMap<N, E, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for v in self.vertices.get_values().sorted_by_key(|v| v.id.0) {
            writeln!(f, "{:?}", v)?;
        }
        for e in self.edges.get_values().sorted_by_key(|e| e.id.0) {
            writeln!(f, "{:?}", e)?;
        }
        if self.embedded {
            for face in self.faces.get_values().sorted_by_key(|f| f.id.0) {
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

        assert_eq!(g.vertex(v1).next_by_nb(v2, CW).other, v2);
        assert_eq!(g.vertex(v1).next_by_nb(v2, CCW).other, v2);

        let v3 = g.add_vertex(3);
        g.add_edge(v1, v3, 2);
        g.add_edge(v2, v3, 2);

        g.set_embedding_by_face_cycles(vec![(vec![v1, v2, v3], 1), (vec![v3, v2, v1], 1)])
            .expect("ok");

        assert_eq!(g.vertex(v1).next_by_nb(v2, CW).other, v3);
        assert_eq!(g.vertex(v1).next_by_nb(v2, CCW).other, v3);

        assert_eq!(g.vertex(v3).next_by_nb(v1, CW).other, v2);
        assert_eq!(g.vertex(v3).next_by_nb(v1, CCW).other, v2);
    }

    #[test]
    fn test_nbvertex_sector() {
        let mut g = PlanarMap::new();

        let ctr = g.add_vertex(1000);

        let mut outer_rim = Vec::new();
        for i in 0..10 {
            let v = g.add_vertex(i);
            outer_rim.push(v);
            g.add_edge(ctr, v, i);
        }

        let mut the_face = Vec::new();
        for vid in &outer_rim {
            the_face.push(*vid);
            the_face.push(*&ctr);
        }

        g.set_embedding_by_face_cycles(vec![(the_face, 0)])
            .expect("ok");

        assert_eq!(
            g.vertex(ctr)
                .sector_between(outer_rim[0], outer_rim[3], CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![outer_rim[1], outer_rim[2]]
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_between(outer_rim[0], outer_rim[7], CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![outer_rim[9], outer_rim[8]]
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_between(outer_rim[1], outer_rim[0], CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![]
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_between(outer_rim[0], outer_rim[1], CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![]
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_between(outer_rim[9], outer_rim[0], CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![]
        );

        assert_eq!(
            g.vertex(ctr)
                .sector_between(outer_rim[0], outer_rim[0], CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            outer_rim.iter().cloned().skip(1).collect_vec()
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_between(outer_rim[0], outer_rim[0], CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            outer_rim.iter().cloned().skip(1).rev().collect_vec()
        );

        assert_eq!(
            g.vertex(ctr)
                .sector_including(outer_rim[0], outer_rim[3], CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![outer_rim[0], outer_rim[1], outer_rim[2], outer_rim[3]]
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_including(outer_rim[0], outer_rim[7], CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![outer_rim[0], outer_rim[9], outer_rim[8], outer_rim[7]]
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_including(outer_rim[1], outer_rim[0], CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![outer_rim[1], outer_rim[0]]
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_including(outer_rim[0], outer_rim[1], CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![outer_rim[0], outer_rim[1]]
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_including(outer_rim[9], outer_rim[0], CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![outer_rim[9], outer_rim[0]]
        );

        let mut fwd = outer_rim.iter().cloned().collect_vec();
        fwd.push(outer_rim[0]);
        let mut bwd = outer_rim.iter().rev().cloned().collect_vec();
        bwd.insert(0, outer_rim[0]);

        assert_eq!(
            g.vertex(ctr)
                .sector_including(outer_rim[0], outer_rim[0], CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            fwd
        );
        assert_eq!(
            g.vertex(ctr)
                .sector_including(outer_rim[0], outer_rim[0], CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            bwd
        );

        let mut h = PlanarMap::new();

        let a = h.add_vertex(0);
        let b = h.add_vertex(1);

        h.add_edge(a, b, 0);

        h.set_embedding_by_face_cycles(vec![(vec![a, b], 0)])
            .expect("test");

        assert_eq!(
            h.vertex(a)
                .sector_between(b, b, CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![]
        );
        assert_eq!(
            h.vertex(a)
                .sector_between(b, b, CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![]
        );
        assert_eq!(
            h.vertex(b)
                .sector_between(a, a, CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![]
        );
        assert_eq!(
            h.vertex(b)
                .sector_between(a, a, CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![]
        );

        assert_eq!(
            h.vertex(a)
                .sector_including(b, b, CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![b, b]
        );
        assert_eq!(
            h.vertex(a)
                .sector_including(b, b, CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![b, b]
        );
        assert_eq!(
            h.vertex(b)
                .sector_including(a, a, CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![a, a]
        );
        assert_eq!(
            h.vertex(b)
                .sector_including(a, a, CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![a, a]
        );

        let mut j = PlanarMap::new();

        let a = j.add_vertex(0);
        let b = j.add_vertex(1);
        let c = j.add_vertex(2);

        j.add_edge(a, b, 0);
        j.add_edge(a, c, 0);

        j.set_embedding_by_face_cycles(vec![(vec![a, b, a, c], 0)])
            .expect("test");

        assert_eq!(
            j.vertex(a)
                .sector_between(b, b, CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![c]
        );
        assert_eq!(
            j.vertex(a)
                .sector_between(b, b, CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![c]
        );

        assert_eq!(
            j.vertex(a)
                .sector_including(b, b, CW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![b, c, b]
        );
        assert_eq!(
            j.vertex(a)
                .sector_including(b, b, CCW)
                .iter()
                .map(|nb| nb.other)
                .collect_vec(),
            vec![b, c, b]
        );
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
