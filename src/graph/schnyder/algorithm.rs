use std::collections::HashMap;
use std::time::Instant;

use array_tool::vec::Intersect;
use chrono::{NaiveDateTime, Utc};
use itertools::Itertools;

use crate::DEBUG;
use crate::graph::{ClockDirection, EdgeI, NbVertex, Signum, swap, Vertex, VertexI, Side};
use crate::graph::ClockDirection::{CCW, CW};
use crate::graph::schnyder::{SchnyderColor, SchnyderMap, SchnyderVertexType};
use crate::graph::schnyder::algorithm::OpType::{Merge, Split};
use crate::graph::schnyder::IndexedEnum;
use crate::graph::schnyder::SchnyderEdgeDirection::Unicolored;
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use crate::graph::Signum::{Backward, Forward};
use crate::util::iterators::cyclic::CyclicIterable;
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::EdgeEnd::{Tail, Head};
use crate::util::errors::GraphErr;

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

    pub fn split(hinge_vertex: VertexI, source_vertex: VertexI, target_vertex: VertexI) -> Operation {
        Operation {
            hinge_vertex, source_vertex, target_vertex,
            operation_type: Split
        }
    }

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

impl<F: Clone> SchnyderMap<F> {

    pub fn do_operation(&mut self, op: &Operation) {
        match op.operation_type {
            Merge => {
                let src = self.map.get_edge(op.hinge_vertex, op.source_vertex);
                let tgt = self.map.get_edge(op.hinge_vertex, op.target_vertex);

                if let (Some(source_edge), Some(target_edge)) = (src, tgt) {
                    self.merge(source_edge, target_edge);
                } else {
                    panic!("invalid edges given!");
                }
            }
            Split => {
                let e = self.map.get_edge(op.hinge_vertex, op.source_vertex);

                if let Some(e) = e {
                    self.split(e, op.hinge_vertex, op.target_vertex);
                } else {
                    panic!("invalid edge given");
                }

            }
        }
    }

}

fn flip_over_to_triangle<F: Clone>(wood: &mut SchnyderMap<F>, mut flip_edges: Vec<EdgeI>, debug: bool, face_counts: &HashMap<VertexI, (usize, usize, usize)>) -> Vec<Operation> {
    let mut result = Vec::new();
    while flip_edges.len() >= 2 {
        let merge_source_edge = flip_edges[flip_edges.len() - 1];
        let merge_target_edge = flip_edges[flip_edges.len() - 2];

        result.push(Operation::merge(wood.map.edge(merge_source_edge).to_vertex_pair(Forward), wood.map.edge(merge_target_edge).to_vertex_pair(Forward)));
        let merge_hinge = wood.merge(merge_source_edge, merge_target_edge);
        let split_hinge = wood.map.edge(merge_target_edge).get_other(merge_hinge);
        let split_edge = merge_target_edge;

        DEBUG.write().unwrap().output(wood, Some("merge"), face_counts);

        let split_target_vertex = wood.split_to_any(merge_target_edge, split_hinge);
        result.push(Operation::split(split_hinge, wood.map.edge(split_edge).get_other(split_hinge), split_target_vertex));
        DEBUG.write().unwrap().output(wood, Some("split"), face_counts);
        flip_edges.pop();
    }
    return result;
}

fn cycle_while_color<F:Clone>(v: &Vertex<SchnyderVertexType>, wood: &SchnyderMap<F>, start_index: usize, in_color: SchnyderColor, out_color: SchnyderColor, direction: ClockDirection) -> Vec<EdgeI> {
    v.cycle_while(start_index, &|nb| wood.incoming_color(nb) == Some(in_color) || wood.outgoing_color(nb) == Some(out_color), direction)
        .iter().map(|nb| nb.edge).collect()
}

fn check_triangle<F: Clone>(wood: &SchnyderMap<F>, eid: EdgeI, side: Side) -> Result<(), GraphErr> {

    let (v1, v2, apex1, apex2) = {
        let (tail, head) = {
            let e = wood.map.edge(eid);
            (wood.map.vertex(e.tail), wood.map.vertex(e.head))
        };

        let (tail_apex, head_apex) = match side {
            Side::Right=> (tail.next_nb(head.id, CW), head.next_nb(tail.id, CCW)),
            Side::Left => (tail.next_nb(head.id, CCW), head.next_nb(tail.id, CW))
        };

        (tail.id, head.id, tail_apex.other, head_apex.other)
    };


    if apex1 == apex2 {
        let apex = apex1;
        match wood.get_color(v1, apex1) {
            Unicolored(c1, Forward) => match wood.get_color(v2, apex2) {
                Unicolored(c2, Forward) if c1 == c2 => Ok(()),
                _ => Err(GraphErr::new("Edge in triangle is not unicolored").with_operation("Check triangle for schnyder contractibility"))
            },
            _ => Err(GraphErr::new("Edge in triangle is not unicolored").with_operation("Check triangle for schnyder contractibility"))
        }
    } else {
        return Err(GraphErr::new("The face is not triangular").with_operation("Check triangle for schnyder contractibility"));
    }
}

pub fn schnyder_contractible<F: Clone>(wood: &SchnyderMap<F>, eid: EdgeI) -> Result<(), GraphErr> {
    if !wood.is_inner_edge(&eid) {
        return Err(
            GraphErr::new("Only inner edges can be schnyder contractible.")
                .with_operation("Check schnyder contractibility")
                .with_edge(eid)
        );
    }

    return match wood.map.edge_weight(&eid) {
        Some(Unicolored(c, _)) => {
            check_triangle(wood, eid, Side::Left)?;
            check_triangle(wood, eid, Side::Right)?;
            Ok(())
        },
        Some(_) => Err(GraphErr::new("Bicolored edges are not schnyder contractible").with_operation("Check schnyder contractibility").with_edge(eid)),
        None => Err(GraphErr::invalid_edge_index(eid).with_operation("Check schnyder contractibility")),
    }
}

pub fn make_contractible<F: Clone>(wood: &mut SchnyderMap<F>, eid: EdgeI) -> Vec<Operation> {

    if !wood.is_inner_edge(&eid) {
        panic!("only inner edges can be made contractible");
    }
    if !wood.map.is_triangulation() {
        panic!("needs to be a triangulation")
    }

    let (tail, tail_nb, e, head_nb, head) = wood.map.edge_with_nb(eid);
    let yummi = &wood.calculate_face_counts();
    let mut result = Vec::new();

    if let Unicolored(color, signum) = e.weight {

        // normalize tail and head according to color orientation
        let (tail, head) = swap((tail, head), signum == Backward);
        let (tail_nb, head_nb) = swap((tail_nb, head_nb), signum == Backward);

        // figure out part of the fan that forms the merge-split sequence
        let right_tail_section = cycle_while_color(tail, wood, tail_nb.index, color.prev(), color.next(), CW);
        let right_head_section = cycle_while_color(head, wood, head_nb.index, color, color.next(), CCW);
        let left_tail_section = cycle_while_color(tail, wood, tail_nb.index, color.next(), color.prev(), CCW);
        let left_head_section = cycle_while_color(head, wood, head_nb.index, color, color.prev(), CW);

        // do the actual operations
        result.extend(flip_over_to_triangle(wood, right_head_section, true, yummi));
        result.extend(flip_over_to_triangle(wood, right_tail_section, true, yummi));
        result.extend(flip_over_to_triangle(wood, left_head_section, true, yummi));
        result.extend(flip_over_to_triangle(wood, left_tail_section, true, yummi));

    } else {
        panic!("assertion failed - triangulation should only consist of unicolored inner edges.");
    }

    DEBUG.write().unwrap().output(wood, Some("Finish"), yummi);
    DEBUG.write().unwrap().output(wood, Some("Finish w/ Updated Vertex Positions"), &wood.calculate_face_counts());

    /*DEBUG.write().unwrap().output(wood, Some("Pre-Revert"), yummi);
    for op in result.iter().rev().map(|op| op.inverted()) {
        wood.do_operation(&op);
        DEBUG.write().unwrap().output(wood, Some("Revert"), yummi);
    }*/

    return result;
}

pub fn make_inner_edge<F: Clone>(wood: &mut SchnyderMap<F>, color: SchnyderColor) -> (EdgeI, Vec<Operation>) {
    if !wood.map.is_triangulation() {
        panic!("needs to be a triangulation");
    }
    if wood.map.vertex_count() < 4 {
        panic!("vertex count is too small");
    }

    if let Some(e) = wood.map.edge_indices()
        .filter(|e| wood.is_inner_edge(e))
        .find(|e| match wood.map.edge_weight(e) {
            Some(Unicolored(c, _)) if *c == color  => true,
            _ => false
        }) {
        return (*e, vec![]);
    }

    let v = wood.map.vertex(wood.get_suspension_vertex(color));
    let nbs = v.nb_sector_between(wood.get_suspension_vertex(color.prev()), wood.get_suspension_vertex(color.next()), CCW);

    if nbs.len() < 2 {
        panic!("assertion failed! (vertex count >= 4 and non presence of inner edge should guarantee at least 2 nbs");
    }

    let mut result = Vec::new();

    let opposite_edge = wood.map.get_edge(nbs[0].other, nbs[1].other).unwrap();
    let merged_edge = match wood.get_color(nbs[0].other, nbs[1].other) {
        Unicolored(_, Forward) => nbs[1].edge,
        Unicolored(_, Backward) => nbs[0].edge,
        _ => panic!("bicolored edge found")
    };

    result.push(Operation::merge(wood.map.edge(merged_edge).to_vertex_pair(Forward), wood.map.edge(opposite_edge).to_vertex_pair(Forward)));
    let merge_hinge = wood.merge(merged_edge, opposite_edge);
    let split_hinge = wood.map.edge(opposite_edge).get_other(merge_hinge);
    let split_target = wood.split_to_any(opposite_edge, split_hinge);
    result.push(Operation::split(split_hinge, merge_hinge, split_target));

    return (opposite_edge, result);
}
