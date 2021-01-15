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
    /// Gets the opposite vertex of 'v' at this edge.
    /// # Errors
    /// If 'v' is not adjacent to the edge, an error is yielded.
    pub fn get_other(&self, v: VertexI) -> GraphResult<VertexI> {
        if self.is_loop() {
            return if self.tail == v {
                Ok(v)
            } else {
                GraphErr::new_err(&format!(
                    "The given vertex {} is not adjacent to the edge {}.",
                    v, self.id
                ))
            };
        }

        Ok(match self.get_signum_by_tail(v)? {
            Forward => self.head,
            Backward => self.tail,
        })
    }

    /// Returns true if this edge is a loop
    pub(super) fn is_loop(&self) -> bool {
        self.tail == self.head
    }

    /// Returns the specified endvertex of this edge.
    pub(super) fn get_vertex(&self, end: EdgeEnd) -> VertexI {
        match end {
            EdgeEnd::Head => self.head,
            EdgeEnd::Tail => self.tail,
        }
    }

    /// Gets the orientation of the internal storage of this edge, assuming 'v' is the tail vertex.
    /// # Returns
    /// If 'v' is the tail, the result is 'Forward'. Otherwise it is 'Backward'.
    /// # Errors
    /// If 'v' is not adjacent to this edge, it will return an error.
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

    /// Gets the orientation of the internal storage of this edge, assuming 'v' is the head vertex.
    /// # Returns
    /// If 'v' is the head, the result is 'Forward'. Otherwise it is 'Backward'.
    /// # Errors
    /// If 'v' is not adjacent to this edge, it will return an error.
    pub fn get_signum_by_head(&self, v: VertexI) -> GraphResult<Signum> {
        Ok(self.get_signum_by_tail(v)?.reversed())
    }

    /// Returns the vertex pair adjacent to the edge.
    /// # Arguments
    /// signum - Order of the pair with respect to the internal storage orientation of this edge.
    /// If 'Forward' is given, the pair is ordered as internally stored, otherwise it is reversed.
    pub(super) fn to_vertex_pair(&self, signum: Signum) -> (VertexI, VertexI) {
        return swap((self.tail, self.head), signum == Backward);
    }

    /// Returns the left face and panics if the left face is not set. For internal usage.
    pub(super) fn left_face(&self) -> FaceI {
        self.left_face
            .expect("PlanarMap internal error: face reference expected to be present.")
    }

    /// Returns the right face and panics if the left face is not set. For internal usage.
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
    /// Get NbVertex describing the link to the neighbor 'other'.
    /// # Errors
    /// Returns an error, if there is no edge to 'other'.
    pub(super) fn get_nb(&self, other: VertexI) -> GraphResult<&NbVertex> {
        self.neighbors
            .iter()
            .find(|nb| nb.other == other)
            .ok_or(GraphErr::new(&format!(
                "{} is not in the neighborhood of {}",
                other, self
            )))
    }

    /// Get mutable NbVertex describing the link to the neighbor 'other'.
    /// # Errors
    /// Returns an error, if there is no edge to 'other'.
    pub(super) fn get_nb_mut(&mut self, other: VertexI) -> GraphResult<&mut NbVertex> {
        let my_index = self.id;
        self.neighbors
            .iter_mut()
            .find(|nb| nb.other == other)
            .ok_or(GraphErr::new(&format!(
                "{} is not in the neighborhood of {}",
                other, my_index
            )))
    }

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

    /// Returns the next neighbor in the given direction
    /// # Arguments
    /// nb - reference neighbor
    /// direction - direction to march one step from the reference neighbor
    pub fn next(&self, nb: &NbVertex, direction: ClockDirection) -> &NbVertex {
        self.get_iterator(nb.index, direction, false, true)
            .next()
            .unwrap()
    }

    /// Returns the next neighbor in the given direction
    /// # Arguments
    /// other - reference neighbor
    /// direction - direction to march one step from the reference neighbor
    /// # Error
    /// Fails with an error, if there is no edge to 'other'.
    pub fn next_by_nb(&self, other: VertexI, direction: ClockDirection) -> GraphResult<&NbVertex> {
        let nb = self.get_nb(other)?;
        Ok(self
            .get_iterator(nb.index, direction, false, true)
            .next()
            .unwrap())
    }

    /// Returns the sector of the fan between 'v1' and 'v2' exclusively.
    /// # Arguments
    /// v1 - first bound of the sector (exclusive)
    /// v2 - second bound of the sector (exclusive)
    /// direction - picks the clockwise or counterclockwise sector
    /// # Errors
    /// Returns an error if 'v1' or 'v2' are not neighboring.
    pub fn sector_between(
        &self,
        v1: VertexI,
        v2: VertexI,
        direction: ClockDirection,
    ) -> GraphResult<Vec<&NbVertex>> {
        Ok(self.sector_between_by_nb(self.get_nb(v1)?, self.get_nb(v2)?, direction))
    }

    /// Returns the sector of the fan between 'nb1' and 'nb2' exclusively.
    /// # Arguments
    /// nb1 - first bound of the sector (exclusive)
    /// nb2 - second bound of the sector (exclusive)
    /// direction - picks the clockwise or counterclockwise sector
    /// # Errors
    /// Caution: The results are undefined, if 'nb1' or 'nb2' are not in the list of this vertex.
    pub fn sector_between_by_nb(
        &self,
        nb1: &NbVertex,
        nb2: &NbVertex,
        direction: ClockDirection,
    ) -> Vec<&NbVertex> {
        return self.cycle_while(nb1.index, &|nb| nb.other != nb2.other, direction, false);
    }

    /// Returns the sector of the fan between 'v1' and 'v2' inclusively.
    /// # Arguments
    /// v1 - first bound of the sector (inclusive)
    /// v2 - second bound of the sector (inclusive)
    /// direction - picks the clockwise or counterclockwise sector
    /// # Errors
    /// Returns an error if 'v1' or 'v2' are not neighboring.
    pub fn sector_including(
        &self,
        v1: VertexI,
        v2: VertexI,
        direction: ClockDirection,
    ) -> GraphResult<Vec<&NbVertex>> {
        Ok(self.sector_including_by_nb(self.get_nb(v1)?, self.get_nb(v2)?, direction))
    }

    /// Returns the sector of the fan between 'nb1' and 'nb2' inclusively.
    /// # Arguments
    /// nb1 - first bound of the sector (inclusive)
    /// nb2 - second bound of the sector (inclusive)
    /// direction - picks the clockwise or counterclockwise sector
    /// # Errors
    /// Caution: The results are undefined, if 'nb1' or 'nb2' are not in the list of this vertex.
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

    /// Cycles through the neighbors starting with a specified index as long as a condition is true.
    /// However, every neighbor apart from the starting neighbor is visited exactly once. The starting
    /// neighbor is visited once in the end if include_start_index is false and twice, in the end
    /// and in the beginning, otherwise.
    /// # Arguments
    /// start_index - index to start with
    /// condition_while - cycle condition
    /// direction - direction for cycling
    /// include_start_index - whether or not to include the start index in the iterator
    /// # Errors
    /// Panics if the start_index is invalid.
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
