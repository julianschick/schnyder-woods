use crate::graph::{PlanarMap, NbVertex, Edge, Signum, VertexI, Vertex, EdgeI, ClockDirection};
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use itertools::{Itertools, Merge};
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::SchnyderEdgeDirection::{Unicolored, Bicolored, Black};
use crate::graph::EdgeEnd::{Tail, Head};
use crate::graph::Signum::{Forward, Backward};
use std::collections::HashSet;
use array_tool::vec::Intersect;
use crate::graph::ClockDirection::{CCW, CW};
use std::io::{Split, empty};
use std::ops::Deref;
use std::fmt::{Debug, Formatter};
use core::fmt;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SchnyderVertexType {
    Normal(usize),
    Suspension(SchnyderColor)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SchnyderEdgeDirection {
    Black,
    Unicolored(SchnyderColor, Signum),
    Bicolored(SchnyderColor, SchnyderColor)
}

impl SchnyderEdgeDirection {
    pub fn is_unicolored(&self) -> bool {
        match self {
            Unicolored(_, _) => true,
            _ => false
        }
    }

    pub fn is_bicolored(&self) -> bool {
        match self {
            Bicolored(_, _) => true,
            _ => false
        }
    }
}

pub trait SchnyderVertex {
    fn get_type(&self) -> SchnyderVertexType;
    fn set_type(&mut self, t: SchnyderVertexType);
}

pub trait SchnyderEdge {
    fn get_direction(&self) -> SchnyderEdgeDirection;
    fn set_direction(&mut self, d: SchnyderEdgeDirection);
}

impl<N: SchnyderVertex> Debug for Vertex<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "v[{}]: ", self.id.0)?;
        for nb in self.neighbors.iter() {
            write!(f, "v[{}] . ", nb.other.0)?;
        }
        Ok(())
    }
}

impl<E: SchnyderEdge> Debug for Edge<E> {

    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let tail_color = match self.weight.get_direction() {
            Black => None,
            Unicolored(c, Forward) => Some(c),
            Bicolored(c, _) => Some(c),
            _ => None
        };
        let head_color = match self.weight.get_direction() {
            Black => None,
            Unicolored(c, Backward) => Some(c),
            Bicolored(_, c) => Some(c),
            _ => None
        };

        write!(f, "e[{}]: v[{}] ={:=<5}======={:=>5}=> v[{}] (R = f[{}], L = f[{}])",
           self.id.0,
           self.tail.0,
           match tail_color {
               Some(c) => format!("{:?}", c),
               _ => String::new()
           },
           match head_color {
               Some(c) => format!("{:?}", c),
               _ => String::new()
           },
           self.head.0,
           self.right_face.unwrap().0,
           self.left_face.unwrap().0
        )
    }
}

struct MergeData<'a, N, E> {
    v: &'a Vertex<N>,
    source_edge: &'a Edge<E>,
    source_nb: &'a NbVertex,
    source_color: SchnyderColor,
    target_edge: &'a Edge<E>,
    target_nb: &'a NbVertex,
    target_color: SchnyderColor,
    direction: ClockDirection
}

impl<N: SchnyderVertex, E: SchnyderEdge, F: Clone> PlanarMap<N, E, F> {

    pub fn debug(&self) {
        for v in self.vertices.get_map().values().sorted_by_key(|v| v.id.0) {
            println!("{:?}", v);
        }

        for e in self.edges.get_map().values().sorted_by_key(|e| e.id.0) {
            println!("{:?}", e);
        }

        /*for f in self.vertices.get_map().values() {
            println!("{:?}", f);
        }*/
    }

    fn color(&self, e: &Edge<E>, sig: Signum) -> Option<SchnyderColor> {
        return match e.weight.get_direction() {
            Unicolored(c, s) => if sig == s { Some(c) } else { None }
            Bicolored(fwd_c, bwd_c) => match sig { Forward => Some(fwd_c), Backward => Some(bwd_c)},
            Black => None
        }
    }

    fn replace_color_nb(&self, nb: &NbVertex, color: SchnyderColor, sig: Signum) -> SchnyderEdgeDirection {
        let effective_signum = match nb.end {
            Tail => sig,
            Head => sig.reversed()
        };

        self.replace_color(self.edge(nb.edge), color, effective_signum)
    }

    fn replace_color(&self, e: &Edge<E>, color: SchnyderColor, sig: Signum) -> SchnyderEdgeDirection {
        match e.weight.get_direction() {
            Black => Unicolored(color, sig),
            Unicolored(c, s) => if s == sig {
                Unicolored(color, sig)
            } else {
                match sig {
                    Forward => Bicolored(color, c),
                    Backward => Bicolored(c, color)
                }
            },
            Bicolored(fwd_c, bwd_c) => {
                match sig {
                    Forward => Bicolored(color, bwd_c),
                    Backward => Bicolored(fwd_c, color)
                }
            }
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

    fn assemble_merge_data(&self, source: EdgeI, target: EdgeI) -> Option<MergeData<N, E>> {
        let knee = self.get_knee(source, target);

        if let Some((v, nb1, nb2)) = knee {

            let source_edge = self.edge(source);
            let target_edge = self.edge(target);
            let direction = if source == nb1.edge { CW } else { CCW };
            let (source_nb, target_nb) = match direction {
                CW => (nb1, nb2),
                CCW => (nb2, nb1)
            };

            // both edges involved must be unicolored
            if !source_edge.weight.get_direction().is_unicolored() || !target_edge.weight.get_direction().is_unicolored() {
                return None;
            }

            let source_color = match self.outgoing_color(source_nb) {
                None => return None,
                Some(c) => c
            };
            let target_color = match self.incoming_color(target_nb) {
                None => return None,
                Some(c) => c
            };

            return Some(MergeData {
                v, source_edge, source_nb, source_color, target_edge, target_nb, target_color, direction
            });
        } else {
            return None;
        }
    }

    pub fn mergeable(&self, source: EdgeI, target: EdgeI) -> Option<ClockDirection> {
        self.assemble_merge_data(source, target).map(|d| d.direction)
    }

    pub fn merge(&mut self, source: EdgeI, target: EdgeI) -> bool {
        let s: *mut Self = self;
        let data = match self.assemble_merge_data(source, target) {
            Some(d) => d,
            None => return false
        };

        let dir = self.replace_color_nb(data.target_nb, data.source_color, Forward);

        unsafe {
            (*s).edge_mut(data.target_edge.id).weight.set_direction(dir);
            (*s).remove_embedded_edge_by_id(data.source_edge.id, &(|a, b| a));
        }
        return true;
    }

    pub fn splittable(&self, e: EdgeI) -> bool {
        return match self.edge(e).weight.get_direction() {
            Bicolored(_, _) => true,
            _ => false
        }
    }

    pub fn split(&mut self, eid: EdgeI, direction: ClockDirection, target_vid: VertexI, mut empty_weight: E) {

        if let Bicolored(tail_color, head_color) = self.edge(eid).weight.get_direction() {
            let tail_split_dir = if tail_color.next() == head_color {
                CW
            } else if tail_color.prev() == head_color {
                CCW
            } else {
                panic!("invalid bicolored edge");
            };

            let fid = match tail_split_dir {
                CW => self.edge(eid).right_face,
                CCW => self.edge(eid).left_face
            };

            let split_side = if tail_split_dir == direction { Tail } else { Head };
            let (split_color, constant_color) = match split_side {
                Tail => (tail_color, head_color),
                Head => (head_color, tail_color)
            };

            self.edge_mut(eid).weight.set_direction(Unicolored(constant_color, match split_side {
                Tail => Backward,
                Head => Forward
            }));

            let hinge_vid = self.edge(eid).get_vertex(split_side);
            if let Some((_, _, nb1, nb2)) = self.get_knee_by_face(fid.unwrap(), target_vid) {
                assert!(nb1.index <= nb2.index);

                let cond_r1 = if let Some(c) = self.incoming_color(nb1) { c == split_color } else {false};
                let cond_r2 = if let Some(c) = self.outgoing_color(nb1) { c == split_color.next() } else {false};
                let cond_l1 = if let Some(c) = self.incoming_color(nb2) { c == split_color } else {false};
                let cond_l2 = if let Some(c) = self.outgoing_color(nb2)  { c == split_color.prev() } else {false};

                if (cond_r1 || cond_r2) && (cond_l1 || cond_l2) {

                    empty_weight.set_direction(Unicolored(split_color, Forward));
                    self.add_embedded_edge(hinge_vid, target_vid, empty_weight, fid.unwrap());

                } else {
                    panic!("vertex is not a good parking lot");
                }
            }
        }
    }

    pub fn check_wood(&self) -> bool {

        // check for 3 suspension vertices
        let colors: Vec<_> = self.vertices.get_map().values().filter_map(|v| {
            match v.weight.get_type() {
                Suspension(c) => Some(c),
                _ => None
            }
        }).collect();

        if !(colors.len() == 3 && colors.iter().dedup().count() == 3) {
            return false;
        }

        // check if every bicolored edge has two distinct colors
        if self.edges.get_map().values()
            .any(|nb| match nb.weight.get_direction()
                { Bicolored(a, b) if a == b => true, _ => false}) {
            return false;
        }

        // check no black edges
        if self.edges.get_map().values().any(|nb|match nb.weight.get_direction() { Black => true, _ => false}) {
            return false;
        }

        // check vertex rule
        for v in self.vertices.get_map().values() {
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
        for f in self.faces.get_map().values() {
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