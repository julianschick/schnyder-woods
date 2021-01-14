use crate::graph::enums::Signum::{Backward, Forward};
use crate::graph::enums::{ClockDirection, EdgeEnd, RevertibleEnum, Signum};
use crate::graph::error::{GraphErr, GraphResult};
use crate::graph::index_store::Ideable;
use crate::graph::indices::{EdgeI, FaceI, VertexI};
use crate::util::iterators::cyclic::CyclicIterable;
use crate::util::swap;
use itertools::Itertools;
use serde::export::fmt::Debug;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

type Result = std::fmt::Result;

pub struct Edge<E> {
    pub id: EdgeI,
    pub tail: VertexI,
    pub head: VertexI,
    pub right_face: Option<FaceI>,
    pub left_face: Option<FaceI>,
    pub weight: E,
}

impl<E> Edge<E> {
    pub fn get_other(&self, this: VertexI) -> GraphResult<VertexI> {
        if self.is_loop() {
            return if self.tail == this {
                Ok(this)
            } else {
                GraphErr::new_err(&format!(
                    "The given vertex {} is not adjacent to the edge {}.",
                    this, self.id
                ))
            };
        }

        Ok(match self.get_signum_by_tail(this)? {
            Forward => self.head,
            Backward => self.tail,
        })
    }

    pub(super) fn is_loop(&self) -> bool {
        self.tail == self.head
    }

    pub(super) fn get_vertex(&self, end: EdgeEnd) -> VertexI {
        match end {
            EdgeEnd::Head => self.head,
            EdgeEnd::Tail => self.tail,
        }
    }

    pub fn get_signum_by_tail(&self, v: VertexI) -> GraphResult<Signum> {
        if self.is_loop() {
            return GraphErr::new_err("Cannot determine signum for loop edge.");
        }

        if v == self.tail {
            Ok(Forward)
        } else if v == self.head {
            Ok(Backward)
        } else {
            GraphErr::new_err(&format!(
                "The given vertex {} is not adjacent to the edge {}.",
                v, self.id
            ))
        }
    }

    pub fn get_signum_by_head(&self, v: VertexI) -> GraphResult<Signum> {
        Ok(self.get_signum_by_tail(v)?.reversed())
    }

    pub(super) fn to_vertex_pair(&self, signum: Signum) -> (VertexI, VertexI) {
        return swap((self.tail, self.head), signum == Backward);
    }

    pub(super) fn left_face(&self) -> FaceI {
        self.left_face
            .expect("PlanarMap internal error: face reference expected to be present.")
    }

    pub(super) fn right_face(&self) -> FaceI {
        self.right_face
            .expect("PlanarMap internal error: face reference expected to be present.")
    }
}

impl<E> Eq for Edge<E> {}
impl<E> PartialEq for Edge<E> {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.tail == other.tail
            || self.head == other.tail && self.tail == other.head
    }
}

impl<E> Hash for Edge<E> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.head.0 + self.tail.0).hash(state)
    }
}

impl<E> Ideable<EdgeI> for Edge<E> {
    fn get_id(&self) -> EdgeI {
        self.id
    }
    fn set_id(&mut self, id: EdgeI) {
        self.id = id
    }
}

impl<E> Display for Edge<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.id)
    }
}

impl<E> Debug for Edge<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{:?}: {:?} ==> {:?} (L = {:?}, R = {:?})",
            self.id,
            self.tail,
            self.head,
            match self.left_face {
                Some(fid) => format!("{}", fid),
                None => "?".to_string(),
            },
            match self.right_face {
                Some(fid) => format!("{}", fid),
                None => "?".to_string(),
            }
        )
    }
}

pub struct Vertex<N> {
    pub id: VertexI,
    pub neighbors: Vec<NbVertex>,
    pub weight: N,
}

impl<N> Vertex<N> {
    /// Get reference to the NbVertex describing the link to the vertex 'other' or None if no such link exists.
    pub(super) fn get_nb(&self, other: VertexI) -> Option<&NbVertex> {
        self.neighbors.iter().find(|nb| nb.other == other)
    }

    /// Get mutable reference to the NbVertex describing the link to the vertex 'other' or None if no such link exists.
    pub(super) fn get_nb_mut(&mut self, other: VertexI) -> Option<&mut NbVertex> {
        self.neighbors.iter_mut().find(|nb| nb.other == other)
    }

    /// does contain start_index at the end (wraps around), if condition is always true
    /// Get an iterator over all neighbors.
    /// # Arguments
    /// * start_index - start with the neighbor of this index
    /// * direction - in which direction to cycle
    /// * include_start_index - whether to include the starting neighbor
    /// * wrap - whether the neighbor with the start index is revisited after cycling the whole neighborhood
    pub(super) fn get_iterator<'a>(
        &'a self,
        start_index: usize,
        direction: ClockDirection,
        include_start_index: bool,
        wrap: bool,
    ) -> Box<dyn Iterator<Item = &NbVertex> + 'a> {
        let iter = self.neighbors.cycle(start_index, wrap);
        let skip = if include_start_index { 0 } else { 1 };
        match direction {
            ClockDirection::CW => Box::new(iter.skip(skip)),
            ClockDirection::CCW => Box::new(iter.rev().skip(skip)),
        }
    }

    pub fn next(&self, nb: &NbVertex, direction: ClockDirection) -> &NbVertex {
        self.get_iterator(nb.index, direction, false, true)
            .next()
            .unwrap()
    }

    pub fn next_by_nb(&self, other: VertexI, direction: ClockDirection) -> &NbVertex {
        let nb = self.get_nb(other).unwrap();
        self.get_iterator(nb.index, direction, false, true)
            .next()
            .unwrap()
    }

    /// does not include the edges to v1 and v2 respectively
    pub fn sector_between(
        &self,
        v1: VertexI,
        v2: VertexI,
        direction: ClockDirection,
    ) -> Vec<&NbVertex> {
        if let (Some(nb1), Some(nb2)) = (self.get_nb(v1), self.get_nb(v2)) {
            return self.sector_between_by_nb(nb1, nb2, direction);
        } else {
            panic!("v1/v2 invalid");
        }
    }

    pub fn sector_between_by_nb(
        &self,
        nb1: &NbVertex,
        nb2: &NbVertex,
        direction: ClockDirection,
    ) -> Vec<&NbVertex> {
        return self.cycle_while(nb1.index, &|nb| nb.other != nb2.other, direction, false);
    }

    pub fn sector_including(
        &self,
        v1: VertexI,
        v2: VertexI,
        direction: ClockDirection,
    ) -> Vec<&NbVertex> {
        if let (Some(nb1), Some(nb2)) = (self.get_nb(v1), self.get_nb(v2)) {
            return self.sector_including_by_nb(nb1, nb2, direction);
        } else {
            panic!("v1/v2 invalid");
        }
    }

    pub fn sector_including_by_nb(
        &self,
        nb1: &NbVertex,
        nb2: &NbVertex,
        direction: ClockDirection,
    ) -> Vec<&NbVertex> {
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

    pub fn cycle_while(
        &self,
        start_index: usize,
        condition_while: &dyn Fn(&&NbVertex) -> bool,
        direction: ClockDirection,
        include_start_index: bool,
    ) -> Vec<&NbVertex> {
        self.get_iterator(start_index, direction, include_start_index, true)
            .take_while(condition_while)
            .collect_vec()
    }
}

impl<N> Ideable<VertexI> for Vertex<N> {
    fn get_id(&self) -> VertexI {
        self.id
    }
    fn set_id(&mut self, id: VertexI) {
        self.id = id
    }
}

impl<N> Display for Vertex<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl<N> Debug for Vertex<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        for nb in self.neighbors.iter() {
            write!(f, "{:?} . ", nb.other)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Face<F> {
    pub id: FaceI,
    pub angles: Vec<VertexI>,
    pub weight: F,
}

impl<F> Ideable<FaceI> for Face<F> {
    fn get_id(&self) -> FaceI {
        self.id
    }
    fn set_id(&mut self, id: FaceI) {
        self.id = id
    }
}

impl<F> Display for Face<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
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

#[derive(Clone, Copy, Debug)]
pub struct NbVertex {
    pub index: usize,
    pub other: VertexI,
    pub edge: EdgeI,
    pub end: EdgeEnd,
}
