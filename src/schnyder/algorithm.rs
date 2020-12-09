use array_tool::vec::Intersect;

use crate::graph::data_holders::Vertex;
use crate::graph::enums::ClockDirection::{CCW, CW};
use crate::graph::enums::Side::{Left, Right};
use crate::graph::enums::Signum::{Backward, Forward};
use crate::graph::enums::{ClockDirection, Side, Signum};
use crate::graph::error::{GraphErr, GraphResult};
use crate::graph::indices::{EdgeI, FaceI, VertexI};
use crate::graph::swap;
use crate::schnyder::algorithm::OpType::{ExtMerge, ExtSplit, Merge, Split};
use crate::schnyder::IndexedEnum;
use crate::schnyder::SchnyderEdgeDirection::{Bicolored, Unicolored};
use crate::schnyder::{SchnyderColor, SchnyderMap, SchnyderVertexType};
use crate::util::swapped;
use bimap::BiMap;
use std::fmt::{Debug, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum OpType {
    Split,
    Merge,
    ExtSplit,
    ExtMerge,
}

#[derive(Copy, Clone)]
pub struct Operation {
    pub hinge_vertex: VertexI,
    pub source_vertex: VertexI,
    pub target_vertex: VertexI,
    pub operation_type: OpType,
}

#[derive(Debug)]
pub struct Contraction {
    pub retained_vertex: VertexI,
    pub color: SchnyderColor,
    pub color_orientation: Signum,
    //
    pub dropped_vertex: VertexI,
    pub dropped_edge: EdgeI,
}

impl Operation {
    pub fn split(
        hinge_vertex: VertexI,
        source_vertex: VertexI,
        target_vertex: VertexI,
    ) -> Operation {
        Operation {
            hinge_vertex,
            source_vertex,
            target_vertex,
            operation_type: Split,
        }
    }

    pub fn merge(merged_edge: (VertexI, VertexI), target_edge: (VertexI, VertexI)) -> Operation {
        let m = vec![merged_edge.0, merged_edge.1];
        let t = vec![target_edge.0, target_edge.1];

        if let Some(&hinge) = m.intersect(t).first() {
            Operation {
                hinge_vertex: hinge,
                source_vertex: *m.iter().find(|&&vid| vid != hinge).unwrap(),
                target_vertex: *vec![target_edge.0, target_edge.1]
                    .iter()
                    .find(|&&vid| vid != hinge)
                    .unwrap(),
                operation_type: Merge,
            }
        } else {
            panic!("invalid merge");
        }
    }

    pub fn merge_by_vertices(hinge: VertexI, source: VertexI, target: VertexI) -> Operation {
        Operation {
            hinge_vertex: hinge,
            source_vertex: source,
            target_vertex: target,
            operation_type: Merge,
        }
    }

    pub fn ext_split(hinge1: VertexI, hinge2: VertexI) -> Operation {
        Operation {
            hinge_vertex: hinge1,
            source_vertex: hinge2,
            target_vertex: hinge2,
            operation_type: ExtSplit,
        }
    }

    pub fn ext_merge(hinge1: VertexI, hinge2: VertexI) -> Operation {
        Operation {
            hinge_vertex: hinge1,
            source_vertex: hinge2,
            target_vertex: hinge2,
            operation_type: ExtMerge,
        }
    }

    pub fn inverted(&self) -> Self {
        Operation {
            hinge_vertex: self.hinge_vertex,
            source_vertex: self.target_vertex,
            target_vertex: self.source_vertex,
            operation_type: match &self.operation_type {
                Merge => Split,
                Split => Merge,
                ExtMerge => ExtSplit,
                ExtSplit => ExtMerge,
            },
        }
    }

    pub fn swapped_vertices(&self, a: &VertexI, b: &VertexI) -> Self {
        Operation {
            hinge_vertex: swapped(a, b, &self.hinge_vertex),
            source_vertex: swapped(a, b, &self.source_vertex),
            target_vertex: swapped(a, b, &self.target_vertex),
            operation_type: self.operation_type,
        }
    }

    pub fn mapped_vertices_by_left(&self, map: &BiMap<VertexI, VertexI>) -> Self {
        Operation {
            hinge_vertex: *map.get_by_left(&self.hinge_vertex).unwrap(),
            source_vertex: *map.get_by_left(&self.source_vertex).unwrap(),
            target_vertex: *map.get_by_left(&self.target_vertex).unwrap(),
            operation_type: self.operation_type,
        }
    }

    pub fn mapped_vertices_by_right(&self, map: &BiMap<VertexI, VertexI>) -> Self {
        Operation {
            hinge_vertex: *map.get_by_right(&self.hinge_vertex).unwrap(),
            source_vertex: *map.get_by_right(&self.source_vertex).unwrap(),
            target_vertex: *map.get_by_right(&self.target_vertex).unwrap(),
            operation_type: self.operation_type,
        }
    }

    pub fn is_upwards(&self) -> bool {
        match self.operation_type {
            Split | ExtSplit => true,
            _ => false,
        }
    }

    pub fn is_downwards(&self) -> bool {
        match self.operation_type {
            Merge | ExtMerge => true,
            _ => false,
        }
    }
}

impl Debug for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "{:?} @{}: {} => {}",
            self.operation_type, self.hinge_vertex.0, self.source_vertex.0, self.target_vertex.0
        ))
    }
}

impl SchnyderMap {
    pub fn do_operation(&mut self, op: &Operation) -> GraphResult<()> {
        match op.operation_type {
            Merge => {
                let src = self.map.get_edge(op.hinge_vertex, op.source_vertex)?;
                let tgt = self.map.get_edge(op.hinge_vertex, op.target_vertex)?;
                self.merge(src, tgt)?;
            }
            Split => {
                let e = self.map.get_edge(op.hinge_vertex, op.source_vertex)?;
                self.split(e, op.hinge_vertex, Some(op.target_vertex))?;
            }
            ExtMerge => {
                self.ext_merge(op.hinge_vertex, op.target_vertex)?;
            }
            ExtSplit => {
                self.ext_split(op.hinge_vertex, op.target_vertex)?;
            }
        }
        Ok(())
    }

    pub fn get_operation_direction(&self, op: &Operation) -> GraphResult<ClockDirection> {
        match op.operation_type {
            Split => {
                let (fwd_c, bwd_c) =
                    self.get_bidirected_colors(&op.hinge_vertex, &op.source_vertex);

                if bwd_c.prev() == fwd_c {
                    Ok(CW)
                } else if bwd_c.next() == fwd_c {
                    Ok(CCW)
                } else {
                    panic!("Assertion failed");
                }
            }
            Merge => {
                let target_color = self.get_unidirected_color(&op.target_vertex, &op.hinge_vertex);
                let source_color = self.get_unidirected_color(&op.hinge_vertex, &op.source_vertex);

                if target_color.prev() == source_color {
                    Ok(CCW)
                } else if target_color.next() == source_color {
                    Ok(CW)
                } else {
                    panic!("Assertion failed");
                }
            }
            ExtSplit | ExtMerge => {
                GraphErr::new_err("External merges or splits do not have a direction")
            }
        }
    }

    pub fn get_affected_face(&self, op: &Operation) -> FaceI {
        let dir = self.get_operation_direction(op).unwrap(); //TODO
        self.map.get_face(
            op.hinge_vertex,
            op.source_vertex,
            match dir {
                CW => Right,
                CCW => Left,
            },
        )
    }

    fn get_unidirected_color(&self, v1: &VertexI, v2: &VertexI) -> SchnyderColor {
        match self.get_color(*v1, *v2).unwrap() {
            Unicolored(color, Forward) => color,
            _ => panic!("Other color or direction than expected by assertion"),
        }
    }

    fn get_bidirected_colors(&self, v1: &VertexI, v2: &VertexI) -> (SchnyderColor, SchnyderColor) {
        match self.get_color(*v1, *v2).unwrap() {
            Bicolored(a, b) => (a, b),
            _ => panic!("Other color or direction than expected by assertion"),
        }
    }
}

fn flip_over_to_triangle(
    wood: &mut SchnyderMap,
    mut flip_edges: Vec<EdgeI>,
) -> GraphResult<Vec<Operation>> {
    let mut result = Vec::new();
    while flip_edges.len() >= 2 {
        let merge_source_edge = flip_edges[flip_edges.len() - 1];
        let merge_target_edge = flip_edges[flip_edges.len() - 2];

        result.push(Operation::merge(
            wood.map.edge_pair(merge_source_edge, Forward)?,
            wood.map.edge_pair(merge_target_edge, Forward)?,
        ));
        let merge_op = wood.merge(merge_source_edge, merge_target_edge).unwrap();
        let split_hinge = wood
            .map
            .try_edge(merge_target_edge)?
            .get_other(merge_op.hinge_vertex);

        let split_op = wood.split(merge_target_edge, split_hinge, None).unwrap();
        result.push(split_op);

        flip_edges.pop();
    }
    return Ok(result);
}

fn cycle_while_color(
    v: &Vertex<SchnyderVertexType>,
    wood: &SchnyderMap,
    start_index: usize,
    in_color: SchnyderColor,
    out_color: SchnyderColor,
    direction: ClockDirection,
) -> Vec<EdgeI> {
    v.cycle_while(
        start_index,
        &|nb| {
            wood.incoming_color(nb) == Some(in_color) || wood.outgoing_color(nb) == Some(out_color)
        },
        direction,
        false,
    )
    .iter()
    .map(|nb| nb.edge)
    .collect()
}

pub fn check_triangle(wood: &SchnyderMap, eid: EdgeI, side: Side) -> GraphResult<()> {
    let (v1, v2, apex1, apex2) = {
        let (tail, head) = wood.map.edge_pair(eid, Forward)?;
        let (tail, head) = (wood.map.try_vertex(tail)?, wood.map.try_vertex(head)?);

        let (tail_apex, head_apex) = match side {
            Side::Right => (tail.next_nb(head.id, CW), head.next_nb(tail.id, CCW)),
            Side::Left => (tail.next_nb(head.id, CCW), head.next_nb(tail.id, CW)),
        };

        (tail.id, head.id, tail_apex.other, head_apex.other)
    };

    if apex1 == apex2 {
        //let apex = apex1;
        match wood.get_color(v1, apex1)? {
            Unicolored(c1, Forward) => match wood.get_color(v2, apex2)? {
                Unicolored(c2, Forward) if c1 == c2 => Ok(()),
                _ => GraphErr::new_err("Edge in triangle is not unicolored"),
            },
            _ => GraphErr::new_err("Edge in triangle is not unicolored"),
        }
    } else {
        return GraphErr::new_err("The face is not triangular");
    }
}

pub fn make_contractible(wood: &mut SchnyderMap, eid: EdgeI) -> GraphResult<Vec<Operation>> {
    if !wood.is_inner_edge(eid)? {
        return GraphErr::new_err("Only inner edges can be made contractible");
    }
    if !wood.map.is_triangulation() {
        return GraphErr::new_err("Only in triangulations edges can be made contractible");
    }

    let (tail, tail_nb, e, head_nb, head) = wood.map.edge_with_nb(eid);
    //let yummi = &wood.calculate_face_counts();
    let mut result = Vec::new();

    if let Unicolored(color, signum) = e.weight {
        // normalize tail and head according to color orientation
        let (tail, head) = swap((tail, head), signum == Backward);
        let (tail_nb, head_nb) = swap((tail_nb, head_nb), signum == Backward);

        // figure out part of the fan that forms the merge-split sequence
        let right_tail_section =
            cycle_while_color(tail, wood, tail_nb.index, color.prev(), color.next(), CW);
        let right_head_section =
            cycle_while_color(head, wood, head_nb.index, color, color.next(), CCW);
        let left_tail_section =
            cycle_while_color(tail, wood, tail_nb.index, color.next(), color.prev(), CCW);
        let left_head_section =
            cycle_while_color(head, wood, head_nb.index, color, color.prev(), CW);

        // do the actual operations
        result.extend(flip_over_to_triangle(wood, right_head_section)?);
        result.extend(flip_over_to_triangle(wood, right_tail_section)?);
        result.extend(flip_over_to_triangle(wood, left_head_section)?);
        result.extend(flip_over_to_triangle(wood, left_tail_section)?);
    } else {
        assert!(false);
        //panic!("assertion failed - triangulation should only consist of unicolored inner edges.");
    }

    //DEBUG.write().unwrap().output(wood, Some("Finish"), yummi);
    //DEBUG.write().unwrap().output(wood, Some("Finish w/ Updated Vertex Positions"), &wood.calculate_face_counts());

    /*DEBUG.write().unwrap().output(wood, Some("Pre-Revert"), yummi);
    for op in result.iter().rev().map(|op| op.inverted()) {
        wood.do_operation(&op);
        DEBUG.write().unwrap().output(wood, Some("Revert"), yummi);
    }*/

    return Ok(result);
}

pub fn full_pizza_lemma(
    wood: &mut SchnyderMap,
    v: VertexI,
    color: SchnyderColor,
) -> GraphResult<Vec<Operation>> {
    if !wood.map.is_triangulation() {
        return GraphErr::new_err("Map needs to be triangulation for exploiting the pizza lemma");
    }

    let mut result = Vec::new();
    let mut incoming = wood.get_incoming_sector(v, color, false);
    while incoming.len() > 1 {
        let target_edge = wood.map.get_edge(incoming[0], incoming[1])?;

        match wood.get_color(incoming[0], incoming[1])? {
            Unicolored(_, Forward) => {
                let source_edge = wood.map.get_edge(v, incoming[1])?;
                incoming.remove(1);
                result.extend(wood.merge_and_resplit(source_edge, target_edge, None)?);
            }
            Unicolored(_, Backward) => {
                let source_edge = wood.map.get_edge(v, incoming[0])?;
                incoming.remove(0);
                result.extend(wood.merge_and_resplit(source_edge, target_edge, None)?);
            }
            _ => panic!("Invalid Schnyder wood detected: Bicolored edges in triangulation"),
        }
    }

    return Ok(result);
}

pub fn make_inner_edge(
    wood: &mut SchnyderMap,
    color: SchnyderColor,
) -> GraphResult<(EdgeI, Vec<Operation>)> {
    if !wood.map.is_triangulation() {
        panic!("needs to be a triangulation"); //TODO graph errors
    }
    if wood.map.vertex_count() < 4 {
        panic!("vertex count is too small");
    }

    if let Some(e) = wood
        .map
        .edge_indices()
        .filter(|e| wood.is_inner_edge(**e).unwrap())
        .find(|e| match wood.map.edge_weight(**e).unwrap() {
            Unicolored(c, _) if *c == color => true,
            _ => false,
        })
    {
        return Ok((*e, vec![]));
    }

    let v = wood.map.try_vertex(wood.get_suspension_vertex(color))?;
    let nbs = v.sector_between(
        wood.get_suspension_vertex(color.prev()),
        wood.get_suspension_vertex(color.next()),
        CCW,
    );

    if nbs.len() < 2 {
        panic!("assertion failed! (vertex count >= 4 and non presence of inner edge should guarantee at least 2 nbs");
    }

    let mut result = Vec::new();

    let opposite_edge = wood.map.get_edge(nbs[0].other, nbs[1].other).unwrap();
    let merged_edge = match wood.get_color(nbs[0].other, nbs[1].other)? {
        Unicolored(_, Forward) => nbs[1].edge,
        Unicolored(_, Backward) => nbs[0].edge,
        _ => panic!("bicolored edge found"),
    };

    let merge_op = wood.merge(merged_edge, opposite_edge).unwrap();
    let split_hinge = wood
        .map
        .edge_opposite_vertex(opposite_edge, merge_op.hinge_vertex)?;
    let split_op = wood.split(opposite_edge, split_hinge, None).unwrap();

    result.push(merge_op);
    result.push(split_op);

    return Ok((opposite_edge, result));
}
