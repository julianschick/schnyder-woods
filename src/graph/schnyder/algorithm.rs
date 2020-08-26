use itertools::Itertools;
use chrono::{NaiveDateTime, Utc};
use std::time::Instant;
use std::collections::HashMap;
use array_tool::vec::Intersect;

use crate::graph::{ClockDirection, VertexI, EdgeI, Signum, swap, Vertex, NbVertex};
use crate::graph::schnyder::{SchnyderMap, SchnyderVertexType, SchnyderColor};
use crate::graph::schnyder::SchnyderVertexType::Suspension;
use crate::graph::schnyder::SchnyderEdgeDirection::Unicolored;
use crate::graph::Signum::{Backward, Forward};
use crate::util::iterators::cyclic::CyclicIterable;
use crate::graph::schnyder::IndexedEnum;
use crate::graph::ClockDirection::{CW, CCW};
use crate::graph::schnyder::algorithm::OpType::{Merge, Split};
use crate::DEBUG;

#[derive(Debug)]
pub enum OpType {
    Split, Merge
}

#[derive(Debug)]
pub struct Operation {
    hinge_vertex: VertexI,
    source_vertex: VertexI,
    target_vertex: VertexI,
    operation_type: OpType
}

impl Operation {

    /*pub fn split(edge: (VertexI, VertexI), direction: ClockDirection, target_vertex: VertexI) -> Operation {
        Operation {
            split: Some(Split {
                edge, direction, target_vertex
            }),
            merge: None
        }
    }*/

    pub fn merge(merged_edge: (VertexI, VertexI), target_edge: (VertexI, VertexI)) -> Operation {
        let m = vec![merged_edge.0, merged_edge.1];
        let t = vec![target_edge.0, target_edge.1];

        if let Some(&hinge) = m.intersect(t).first() {
            Operation {
                hinge_vertex: hinge,
                source_vertex: *m.iter().find(|&&vid| vid != hinge).unwrap(),
                target_vertex: *vec![target_edge.0, target_edge.1].iter().find(|&&vid| vid != hinge).unwrap(),
                operation_type: Merge
            }
        } else {
            panic!("invalid merge");
        }
    }

    pub fn inverted(&self) -> Self {
        Operation {
            hinge_vertex: self.hinge_vertex,
            source_vertex: self.target_vertex,
            target_vertex: self.source_vertex,
            operation_type: match &self.operation_type { Merge => Split, Split => Merge}
        }
    }

}

fn flip_over_to_triangle<F: Clone>(wood: &mut SchnyderMap<F>, mut flip_edges: Vec<EdgeI>, debug: bool, face_counts: &HashMap<VertexI, (usize, usize, usize)>) -> Vec<Operation> {
    let mut result = Vec::new();
    while flip_edges.len() >= 2 {
        let source = flip_edges[flip_edges.len() - 1];
        let target = flip_edges[flip_edges.len() - 2];

        result.push(Operation::merge(wood.map.edge(source).to_vertex_pair(Forward), wood.map.edge(target).to_vertex_pair(Forward)));
        let dir = wood.merge(source, target);

        DEBUG.write().unwrap().output(wood, Some("merge"), face_counts);

        let vid = wood.split_to_any(target, dir);
        //result.push(Operation::split(wood.map.edge(target).to_vertex_pair(Forward), dir,vid));
        DEBUG.write().unwrap().output(wood, Some("split"), face_counts);
        flip_edges.pop();
    }
    return result;
}

fn nb_cycle<F:Clone>(v: &Vertex<SchnyderVertexType>, wood: &SchnyderMap<F>, start_index: usize, in_color: SchnyderColor, out_color: SchnyderColor, direction: ClockDirection) -> Vec<EdgeI> {
    v.cycle_while(start_index, &|nb| wood.incoming_color(nb) == Some(in_color) || wood.outgoing_color(nb) == Some(out_color), direction)
        .iter().map(|nb| nb.edge).collect()
}

pub fn make_contractable<F: Clone>(wood: &mut SchnyderMap<F>, eid: EdgeI) -> Vec<Operation> {

    let (tail, tail_nb, e, head_nb, head) = wood.map.edge_with_nb(eid);

    for &v in &[tail, head] {
        if let Suspension(_) = v.weight {
            panic!("only inner edges can be made contractable");
        }
    }

    if !wood.map.is_triangulation() {
        panic!("needs to be a triangulation")
    }

    let yummi = &wood.calculate_face_counts();
    DEBUG.write().unwrap().output(wood, Some("Start"), yummi);
    let mut result = Vec::new();

    if let Unicolored(color, signum) = e.weight {

        // normalize tail and head according to color orientation
        let (tail, head) = swap((tail, head), signum == Backward);
        let (tail_nb, head_nb) = swap((tail_nb, head_nb), signum == Backward);

        // figure out part of the fan that forms the merge-split sequence
        let right_tail_section = nb_cycle(tail, wood, tail_nb.index, color.prev(), color.next(), CW);
        let right_head_section = nb_cycle(head, wood, head_nb.index, color, color.next(), CCW);
        let left_tail_section = nb_cycle(tail, wood, tail_nb.index, color.next(), color.prev(), CCW);
        let left_head_section = nb_cycle(head, wood, head_nb.index, color, color.prev(), CW);

        // do the actual operations
        result.extend(flip_over_to_triangle(wood, right_head_section, true, yummi));
        result.extend(flip_over_to_triangle(wood, right_tail_section, true, yummi));
        result.extend(flip_over_to_triangle(wood, left_head_section, true, yummi));
        result.extend(flip_over_to_triangle(wood, left_tail_section, true, yummi));

    } else {
        panic!("assertion failed - triangulation should only consist of unicolored inner edges.");
    }

    DEBUG.write().unwrap().output(wood, Some("Finish"), yummi);
    DEBUG.write().unwrap().output(wood, Some("Finish - with new positions"), &wood.calculate_face_counts());

    return result;
}