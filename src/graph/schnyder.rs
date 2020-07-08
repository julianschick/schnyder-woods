use crate::graph::{PlanarMap, NbVertex, Edge, Signum, VertexI, Vertex, EdgeI};
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use itertools::Itertools;
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::SchnyderEdgeDirection::{UnicoloredBackward, UnicoloredForward, Bicolored, Black};
use crate::graph::EdgeEnd::{Tail, Head};
use crate::graph::Signum::{Forward, Backward};
use std::collections::HashSet;
use array_tool::vec::Intersect;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SchnyderColor {
    Red, Green, Blue
}

impl SchnyderColor {

    pub fn next(&self) -> Self {
        match self {
            Red => Green, Green => Blue, Blue => Red
        }
    }

    pub fn prev(&self) -> Self {
        match self {
            Red => Blue, Blue => Green, Green => Red
        }
    }
}

pub enum SplitDirection {
    CW, CCW
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SchnyderVertexType {
    Normal(usize),
    Suspension(SchnyderColor)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SchnyderEdgeDirection {
    Black,
    UnicoloredForward(SchnyderColor),
    UnicoloredBackward(SchnyderColor),
    Bicolored(SchnyderColor, SchnyderColor)
}

pub trait SchnyderVertex {
    fn get_type(&self) -> SchnyderVertexType;
    fn set_type(&mut self, t: SchnyderVertexType);
}

pub trait SchnyderEdge {
    fn get_direction(&self) -> SchnyderEdgeDirection;
    fn set_direction(&mut self, d: SchnyderEdgeDirection);
}

impl<N: SchnyderVertex, E: SchnyderEdge, F> PlanarMap<N, E, F> {

    pub fn color(&self, e: &Edge<E>, sig: Signum) -> Option<SchnyderColor> {
        return match e.weight.get_direction() {
            UnicoloredForward(c) => match sig { Forward => Some(c), Backward => None},
            UnicoloredBackward(c) => match sig { Forward => None, Backward => Some(c)},
            Bicolored(fwd_c, bwd_c) => match sig { Forward => Some(fwd_c), Backward => Some(bwd_c)},
            Black => None
        }
    }

    fn find_outgoing(&self, v: &Vertex<N>, c: SchnyderColor) -> Option<usize> {
        v.neighbors.iter()
            .find_position(|nb| match self.outgoing_color(nb) { Some(cc) if cc == c => true, _ => false })
            .map(|(pos, _)| pos)
    }

    pub fn outgoing_color(&self, nb: &NbVertex) -> Option<SchnyderColor> {
        self.color(self.edge(nb.edge), match nb.end { Tail => Forward, Head => Backward})
    }

    pub fn incoming_color(&self, nb: &NbVertex) -> Option<SchnyderColor> {
        self.color(self.edge(nb.edge), match nb.end { Tail => Backward, Head => Forward})
    }

    pub fn mergeable(&self, source: EdgeI, target: EdgeI) -> Option<SplitDirection> {
        let source = self.edge(source);
        let target = self.edge(target);

        let vertex_intersection = vec![source.tail, source.head].intersect(vec![target.tail, target.head]);
        match vertex_intersection.len() {
            0 => return None,
            1 => {},
            _ => panic!("assertion failed")
        };

        let source_signum = source.get_signum_by_tail(vertex_intersection[0]);
        let target_signum = target.get_signum_by_tail(vertex_intersection[0]);

        return None;
    }

    pub fn merge(&mut self, source: EdgeI, target: EdgeI) {

    }

    pub fn splittable(&self, e: EdgeI) -> bool {
        return match self.edge(e).weight.get_direction() {
            Bicolored(_, _) => true,
            _ => false
        }
    }

    pub fn split(&self, e: EdgeI, direction: SplitDirection) {

    }

    pub fn check_wood(&self) -> bool {

        // check for 3 suspension vertices
        let colors: Vec<_> = self.vertices.values().filter_map(|v| {
            match v.weight.get_type() {
                Suspension(c) => Some(c),
                _ => None
            }
        }).collect();

        if !(colors.len() == 3 && colors.iter().dedup().count() == 3) {
            return false;
        }

        // check if every bicolored edge has two distinct colors
        if self.edges.values().any(|nb| match nb.weight.get_direction() { Bicolored(a, b) if a == b => true, _ => false}) {
            return false;
        }

        // check no black edges
        if self.edges.values().any(|nb|match nb.weight.get_direction() { Black => true, _ => false}) {
            return false;
        }

        // check vertex rule
        for v in self.vertices.values() {
            let l = v.neighbors.len();

            let begin_sector = match v.weight.get_type() {
                Normal(id) => Blue,
                Suspension(c) => c
            };

            match v.weight.get_type() {
                Normal(id) => if l < 3 { return false },
                Suspension(c) => if l < 2 { return false }
            }

            let begin = self.find_outgoing(v, begin_sector.next());
            if begin.is_none() {
                return false
            }

            let mut i = (begin.unwrap() + 1) % l;
            let mut state = begin_sector; // inbound sector
            let mut last_outgoing = None;

            while i != begin.unwrap() {
                let nb = v.neighbors.get(i).unwrap();
                println!("\t(out = {:?}, in = {:?})", self.outgoing_color(nb), self.incoming_color(nb));
                last_outgoing = self.outgoing_color(nb);

                match last_outgoing {
                    Some(c) => if c != state.prev() {
                        return false
                    } else {
                        state = state.next();
                    },
                    _ => match self.incoming_color(nb) {
                        Some(c) => if c != state {
                            return false
                        },
                        None => panic!("assertion failed")
                    }
                }

                i = (i + 1) % l;
            }

            match v.weight.get_type() {
                Normal(id) => if state != begin_sector.prev() { return false },
                Suspension(c) => if state != begin_sector.next() || last_outgoing != Some(c.prev()) { return false }
            }
        }

        // check face cycle rule
        for f in self.faces.values() {
            let l = f.angles.len();
            let mut fwd_colors = HashSet::new();
            let mut bwd_colors = HashSet::new();

            for i in 0..l {
                let v1 = f.angles[i];
                let v2 = f.angles[(i+1)%l];

                let e = self.edge(self.get_edge(v1, v2).unwrap());
                let signum = e.get_signum(v1, v2);

                fwd_colors.insert(self.color(e, signum));
                bwd_colors.insert(self.color(e, signum.reversed()));
            }

            if fwd_colors.len() == 1 || bwd_colors.len() == 1 {
                return false;
            }
        }

        return true;
    }
}