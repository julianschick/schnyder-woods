use crate::graph::indices::{EdgeI, VertexI, FaceI};
use crate::graph::enums::{EdgeEnd, Signum, ClockDirection};
use crate::graph::enums::Signum::{Forward, Backward};
use crate::graph;
use std::hash::{Hash, Hasher};
use serde::export::fmt::Debug;
use std::fmt::{Formatter, Display};
use crate::graph::guarded_map::Ideable;
use crate::util::iterators::cyclic::CyclicIterable;
use itertools::Itertools;

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
            EdgeEnd::Head => self.head,
            EdgeEnd::Tail => self.tail
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
        return graph::swap((self.tail, self.head), signum == Backward)
    }
}

impl<E> Eq for Edge<E> { }
impl<E> PartialEq for Edge<E> {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.tail == other.tail ||
        self.head == other.tail && self.tail == other.head
    }
}

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

impl<E> Display for Edge<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl<E> Ideable<EdgeI> for Edge<E> {
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

impl<N> Display for Vertex<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl<N> Ideable<VertexI> for Vertex<N> {
    fn get_id(&self) -> VertexI { self.id }
    fn set_id(&mut self, id: VertexI) { self.id = id }
}

impl<N> Vertex<N> {

    pub(super) fn get_nb(&self, other: VertexI) -> Option<&NbVertex> {
        self.neighbors.iter().find(|nb| nb.other == other)
    }

    pub(super) fn get_nb_mut(&mut self, other: VertexI) -> Option<&mut NbVertex> {
        self.neighbors.iter_mut().find(|nb| nb.other == other)
    }

    /// does contain start_index at the end (wraps around), if condition is always true
    pub(super) fn get_iterator<'a>(&'a self, start_index: usize, direction: ClockDirection, include_start_index: bool, wrap: bool) -> Box<dyn Iterator<Item = &NbVertex> + 'a> {
        let iter = self.neighbors.cycle(start_index, wrap);
        let skip = if include_start_index { 0 } else { 1 };
        match direction {
            ClockDirection::CW => Box::new(iter.skip(skip)),
            ClockDirection::CCW => Box::new(iter.rev().skip(skip))
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

    pub(super) fn _sector_including(&self, v1: VertexI, v2: VertexI, direction: ClockDirection) -> Vec<&NbVertex> {
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


#[derive(Clone)]
pub struct Face<F> {
    pub id: FaceI,
    pub angles: Vec<VertexI>,
    pub weight: F
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

impl<F> Display for Face<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NbVertex {
    pub index: usize,
    pub other: VertexI,
    pub edge: EdgeI,
    pub end: EdgeEnd
}
