use itertools::Itertools;
use std::collections::HashMap;

use crate::graph::indices::{EdgeI, VertexI};
use crate::schnyder::algorithm::OpType::{ExtMerge, ExtSplit, Merge, Split};
use crate::schnyder::algorithm::{
    full_pizza_lemma, make_contractible, make_inner_edge, Contraction, Operation,
};
use crate::schnyder::enums::SchnyderColor::{Blue, Green, Red};
use crate::schnyder::enums::SchnyderEdgeDirection::Unicolored;
use crate::schnyder::enums::{IndexedEnum, SchnyderColor};

use crate::graph::enums::ClockDirection;
use crate::graph::enums::ClockDirection::{CCW, CW};
use crate::graph::error::{GraphErr, GraphResult};
use crate::schnyder::SchnyderMap;
#[cfg(debug_assertions)]
use crate::DEBUG;
use bimap::BiMap;

pub fn find_sequence(
    wood1: &mut SchnyderMap,
    wood2: &mut SchnyderMap,
) -> GraphResult<Vec<Operation>> {
    let tri1 = arbitrary_triangulation(wood1)?;
    let tri2 = arbitrary_triangulation(wood2)?;

    let (vm, seq) = find_sequence_(wood1, wood2, 0)?;
    let mut result = Vec::with_capacity(tri1.len() + tri2.len() + seq.len());

    result.extend(tri1);
    result.extend(seq);
    result.extend(tri2.iter().rev().map(|op| {
        op.inverted()
            .mapped_vertices_by_left(&vm)
            .unwrap_or_else(|e| panic!("Internal assertion failed: {}", e))
    }));

    //println!("outer vertex map = {:?}", vm);
    return Ok(result);
}

pub fn compute_contraction_candidates(wood: &SchnyderMap) -> HashMap<SchnyderColor, (EdgeI, bool)> {
    let tmp = wood
        .map
        .edge_indices()
        .map(|&e| (wood.map.edge_weight(e).unwrap(), e))
        .filter(|(_, e)| wood.is_inner_edge(*e).unwrap())
        .filter_map(|(dir, e)| match dir {
            Unicolored(c, _) => Some((*c, e)),
            _ => None,
        })
        .map(|(c, e)| (c, (e, wood.is_schnyder_contractible(e).is_ok())))
        .sorted_by_key(|(_, (e, contractible))| (!*contractible, e.0))
        .into_group_map();

    [Red, Green, Blue]
        .iter()
        .filter_map(|&c| {
            if let Some(list) = tmp.get(&c) {
                Some((c, list[0]))
            } else {
                None
            }
        })
        .collect()
}

fn compute_color_quality(info: Option<&(EdgeI, bool)>) -> usize {
    match info {
        Some((_, contractible)) => {
            if *contractible {
                2
            } else {
                1
            }
        }
        None => 0,
    }
}

fn prepare_wood(
    wood: &mut SchnyderMap,
    candidates: &HashMap<SchnyderColor, (EdgeI, bool)>,
    color: SchnyderColor,
) -> GraphResult<(Contraction, Vec<Operation>)> {
    let mut seq = Vec::new();
    let contraction = if let Some((e, contractible)) = candidates.get(&color) {
        if !contractible {
            seq.extend(make_contractible(wood, *e)?);
        }

        //DEBUG.write().unwrap().output("std", &wood, Some(&format!("Precontract/mc")), &wood.calculate_face_counts());
        wood.schnyder_contract(*e)?
    } else {
        let (new_edge, sub_seq) = make_inner_edge(wood, color)?;
        seq.extend(sub_seq);

        if !wood.is_schnyder_contractible(new_edge).is_ok() {
            seq.extend(make_contractible(wood, new_edge)?);
        }

        //DEBUG.write().unwrap().output("std", &wood, Some(&format!("Precontract/mi+mc")), &wood.calculate_face_counts());
        wood.schnyder_contract(new_edge)?
    };

    return Ok((contraction, seq));
}

// TODO so many sector functions flying around
fn calc_sector(
    wood: &mut SchnyderMap,
    center: VertexI,
    from: VertexI,
    to: VertexI,
    direction: ClockDirection,
) -> GraphResult<Vec<VertexI>> {
    Ok(wood
        .map
        .sector_between(center, from, to, direction)?
        .iter()
        .map(|(_, v)| *v)
        .collect_vec())
}

#[allow(unused_variables)] //for lvl, which is needed only in debug builds
fn lift_sequence(
    seq: &Vec<Operation>,
    ctr: &Contraction,
    wood: &mut SchnyderMap,
    lvl: usize,
) -> GraphResult<Vec<Operation>> {
    let mut result = Vec::new();
    let vr = ctr.retained_vertex;
    let vd = ctr.dropped_vertex;
    let mut u = wood
        .find_outgoing_endvertex(ctr.retained_vertex, ctr.color.prev())
        .unwrap();
    let mut w = wood
        .find_outgoing_endvertex(ctr.retained_vertex, ctr.color.next())
        .unwrap();
    let mut sector_vertices = calc_sector(wood, vr, u, w, CW).unwrap(); //TODO
    let mut sector_faces = wood.map.faces_between(&vr, &u, &w, CW).unwrap(); //TODO

    #[cfg(debug_assertions)]
    println!(
        "lifting (vd = {}, vr = {}, u = {}, w = {})",
        vd.0, vr.0, u.0, w.0
    );

    for op in seq {
        //let mut recalculate_sector = false;
        let mut replacement_ops = Vec::new();
        let affected_face = wood.get_affected_face(op);

        if op.hinge_vertex == vr
            && ((op.operation_type == Split
                && (op.source_vertex == u
                    || op.source_vertex == w
                    || sector_vertices.contains(&op.source_vertex)))
                || (op.operation_type == Merge
                    && (op.source_vertex == u
                        || op.source_vertex == w
                        || sector_vertices.contains(&op.source_vertex))))
        {
            if op.source_vertex == u || op.source_vertex == w {
                let dir = wood
                    .get_operation_direction(op)
                    .unwrap()
                    .reversed_if(op.source_vertex == w);
                let u_or_w = op.source_vertex;

                match (op.operation_type, dir) {
                    (Split, CW) => {
                        replacement_ops.push(Operation::split(vd, u_or_w, op.target_vertex));
                        replacement_ops.push(Operation::merge((vd, u_or_w), (vr, u_or_w)));
                        replacement_ops.push(*op);
                    }
                    (Split, CCW) => {
                        replacement_ops.push(*op);
                        replacement_ops.push(Operation::merge((vr, u_or_w), (vd, u_or_w)));
                        replacement_ops.push(Operation::split(vd, u_or_w, op.target_vertex));
                    }
                    (Merge, CW) => {
                        replacement_ops
                            .push(Operation::merge((vd, u_or_w), (vd, op.target_vertex)));
                        replacement_ops.push(Operation::split(op.target_vertex, vd, vr));
                        replacement_ops.push(*op);
                    }
                    (Merge, CCW) => {
                        replacement_ops.push(*op);
                        replacement_ops.push(Operation::split(op.target_vertex, vr, vd));
                        replacement_ops
                            .push(Operation::merge((vd, u_or_w), (vd, op.target_vertex)));
                    }
                    (ExtMerge, _) | (ExtSplit, _) => panic!("No external ops allowed here"),
                }

                if u_or_w == u {
                    u = op.target_vertex
                } else {
                    w = op.target_vertex
                };
            //println!("\tu = {}, w = {}", u.0, w.0);
            //recalculate_sector = true;
            } else {
                // sector
                replacement_ops.push(Operation {
                    hinge_vertex: vd,
                    ..*op
                });
            }
        } else if op.target_vertex == vr
            && ((op.operation_type == Split && sector_faces.contains(&affected_face))
                || (op.operation_type == Merge
                    && (op.hinge_vertex == u
                        || op.hinge_vertex == w
                        || sector_vertices.contains(&op.hinge_vertex))))
        {
            match op.operation_type {
                Split => replacement_ops.push(Operation {
                    target_vertex: vd,
                    ..*op
                }),
                Merge => {
                    if op.hinge_vertex == u || op.hinge_vertex == w {
                        let dir = wood
                            .get_operation_direction(op)
                            .unwrap()
                            .reversed_if(op.hinge_vertex == w);
                        match dir {
                            CW => replacement_ops.push(Operation {
                                target_vertex: vd,
                                ..*op
                            }),
                            CCW => replacement_ops.push(*op),
                        }
                    } else {
                        replacement_ops.push(Operation {
                            target_vertex: vd,
                            ..*op
                        });
                    }
                }
                ExtSplit | ExtMerge => panic!("No external ops allowed here"),
            }
        } else if op.source_vertex == vr
            && ((op.operation_type == Split
                && (op.hinge_vertex == u
                    || op.hinge_vertex == w
                    || sector_vertices.contains(&op.hinge_vertex)))
                || (op.operation_type == Merge && sector_vertices.contains(&op.hinge_vertex)))
        {
            match op.operation_type {
                Split => {
                    if op.hinge_vertex == u || op.hinge_vertex == w {
                        let dir = wood
                            .get_operation_direction(op)
                            .unwrap()
                            .reversed_if(op.hinge_vertex == w);
                        match dir {
                            CW => replacement_ops.push(*op),
                            CCW => replacement_ops.push(Operation {
                                source_vertex: vd,
                                ..*op
                            }),
                        }
                    } else {
                        // sector
                        replacement_ops.push(Operation {
                            source_vertex: vd,
                            ..*op
                        })
                    }
                }
                Merge => {
                    // sector
                    replacement_ops.push(Operation {
                        source_vertex: vd,
                        ..*op
                    })
                }
                ExtSplit | ExtMerge => panic!("No external ops allowed here"),
            }
        } else {
            replacement_ops.push(*op);
        }

        /*println!("\t{:?}", op);
        for r_op in &replacement_ops {
            println!("\t\t{:?}", r_op);
        }*/

        result.extend(replacement_ops);
        wood.exec_op(op)?;

        //recalculate_sector = true;
        //if recalculate_sector {
        sector_vertices = calc_sector(wood, vr, u, w, CW).unwrap(); //TODO
        sector_faces = wood.map.faces_between(&vr, &u, &w, CW).unwrap(); //TODO
                                                                         //println!("\tsector_vertices = {:?}", sector_vertices);
                                                                         //println!("\tsector_faces = {:?}", sector_faces);
                                                                         //}

        #[cfg(debug_assertions)]
        DEBUG.write().unwrap().output(
            &format!("level{}", lvl),
            &wood,
            Some(&format!("Step")),
            &wood.calculate_face_counts(),
        );
    }

    //rewind
    for op in seq.iter().rev() {
        wood.exec_op(&op.inverted())?;
    }

    return Ok(result);
}

fn find_sequence_(
    wood1: &mut SchnyderMap,
    wood2: &mut SchnyderMap,
    depth: usize,
) -> GraphResult<(BiMap<VertexI, VertexI>, Vec<Operation>)> {
    if wood1.map.vertex_count() != wood2.map.vertex_count() {
        panic!("vertex count is not equal");
    }
    let n = wood1.map.vertex_count();

    // having checked this, we know that every inner edge is unicolored
    if !wood1.map.is_triangulation() || !wood2.map.is_triangulation() {
        panic!("one is no triangulation");
    }

    if n < 4 {
        panic!("can only handle schnyder woods with 4 or more vertices")
    }

    // induction start
    if n == 4 {
        let mut vertex_map = BiMap::new();
        for &c in &[Red, Green, Blue] {
            vertex_map.insert(
                wood2.get_suspension_vertex(c),
                wood1.get_suspension_vertex(c),
            );
        }
        let inner1 = wood1.get_inner_vertices();
        assert_eq!(inner1.len(), 1);
        let inner2 = wood2.get_inner_vertices();
        assert_eq!(inner2.len(), 1);
        vertex_map.insert(inner2[0], inner1[0]);

        return Ok((vertex_map, vec![]));
    } else {
        let candidates1 = compute_contraction_candidates(&wood1);
        let candidates2 = compute_contraction_candidates(&wood2);

        let best_color = [Red, Green, Blue]
            .iter()
            .max_by_key(|c| {
                compute_color_quality(candidates1.get(c))
                    + compute_color_quality(candidates2.get(c))
            })
            .unwrap();

        //println!("best_color = {:?}; depth = {}", best_color, depth);

        #[cfg(debug_assertions)]
        {
            DEBUG.write().unwrap().output(
                &format!("level{}", depth),
                &wood1,
                Some("Wood1"),
                &wood1.calculate_face_counts(),
            );
            DEBUG.write().unwrap().output(
                &format!("level{}", depth),
                &wood2,
                Some("Wood2"),
                &wood2.calculate_face_counts(),
            );
        }

        let (contraction1, prep_seq1) = prepare_wood(wood1, &candidates1, *best_color).unwrap();
        let (contraction2, prep_seq2) = prepare_wood(wood2, &candidates2, *best_color).unwrap();

        #[cfg(debug_assertions)]
        {
            DEBUG.write().unwrap().output(
                &format!("level{}", depth),
                &wood1,
                Some("Wood1 (prepared, contracted)"),
                &wood1.calculate_face_counts(),
            );
            DEBUG.write().unwrap().output(
                &format!("level{}", depth),
                &wood2,
                Some("Wood2 (prepared, contracted)"),
                &wood2.calculate_face_counts(),
            );
        }

        let (vertex_map, mut lifted_seq) = {
            let (mut vertex_map, mut seq) = find_sequence_(wood1, wood2, depth + 1)?;
            /*println!(
                "level{} vertex map (wood2 -> wood1) = {:?}",
                depth, vertex_map
            );*/
            vertex_map.insert(contraction2.dropped_vertex, contraction1.dropped_vertex);

            // swap
            let retained_vertex = *vertex_map
                .get_by_left(&contraction2.retained_vertex)
                .unwrap(); //TODO
            for op in &seq {
                wood1.exec_op(op)?;
            }
            /*println!(
                "swap {} ~ {}",
                retained_vertex.0, contraction1.retained_vertex.0
            );*/
            let swap_seq = wood1
                .swap(retained_vertex, contraction1.retained_vertex)
                .unwrap(); //TODO
                           //println!("swap seq len = {}", swap_seq.len());

            // reflect swap in vertex_map
            let swap_a = retained_vertex;
            let swap_b = contraction1.retained_vertex;
            if swap_a != swap_b {
                let (a2, a1) = vertex_map.remove_by_right(&swap_a).unwrap();
                let (b2, b1) = vertex_map.remove_by_right(&swap_b).unwrap();
                vertex_map.insert(a2, b1);
                vertex_map.insert(b2, a1);
            }

            // rewind changes
            for op in swap_seq.iter().rev().chain(seq.iter().rev()) {
                wood1.exec_op(&op.inverted())?;
            }

            seq.extend(swap_seq);

            (
                vertex_map,
                lift_sequence(&seq, &contraction1, wood1, depth)?,
            )
        };

        wood1.revert_schnyder_contraction(&contraction1)?;
        #[cfg(debug_assertions)]
        DEBUG.write().unwrap().output(
            &format!("level{}", depth),
            &wood1,
            Some("Wood1 (prepared, uncontracted)"),
            &wood1.calculate_face_counts(),
        );
        for op in prep_seq1.iter().rev() {
            wood1.exec_op(&op.inverted())?;
        }
        #[cfg(debug_assertions)]
        DEBUG.write().unwrap().output(
            &format!("level{}", depth),
            &wood1,
            Some("Wood1 (re-unprepared, uncontracted)"),
            &wood1.calculate_face_counts(),
        );

        wood2.revert_schnyder_contraction(&contraction2)?;

        #[cfg(debug_assertions)]
        DEBUG.write().unwrap().output(
            &format!("level{}", depth),
            &wood2,
            Some("Wood2 (prepared, uncontracted)"),
            &wood2.calculate_face_counts(),
        );
        #[cfg(debug_assertions)]
        let mut i = 1;

        for op in prep_seq2.iter().rev() {
            wood2.exec_op(&op.inverted())?;

            #[cfg(debug_assertions)]
            {
                DEBUG.write().unwrap().output(
                    &format!("level{}", depth),
                    &wood2,
                    Some(&format!("Wood2 unprepare Step {}", i)),
                    &wood2.calculate_face_counts(),
                );
                i += 1;
            }
        }

        #[cfg(debug_assertions)]
        DEBUG.write().unwrap().output(
            &format!("level{}", depth),
            &wood2,
            Some("Wood2 (re-unprepared, uncontracted)"),
            &wood2.calculate_face_counts(),
        );

        // final assembly of sequence
        lifted_seq.splice(0..0, prep_seq1);
        //seq.extend(swap_seq);
        lifted_seq.extend(prep_seq2.iter().rev().map(|op| {
            op.inverted()
                .mapped_vertices_by_left(&vertex_map)
                .unwrap_or_else(|e| panic!("Internal assertion failed: {}", e))
        }));

        return Ok((vertex_map, lifted_seq));
    }
}

//
//
//

pub fn arbitrary_triangulation(wood: &mut SchnyderMap) -> GraphResult<Vec<Operation>> {
    let mut seq = Vec::new();
    while let Some(&op) = wood.get_admissible_ops()?.iter().find(|op| op.is_upwards()) {
        wood.exec_op(&op)?;
        seq.push(op);
    }

    return Ok(seq);
}

pub fn find_sequence_2(
    wood1: &mut SchnyderMap,
    wood2: &mut SchnyderMap,
    color: SchnyderColor,
) -> Vec<Operation> {
    let tri_seq1 = arbitrary_triangulation(wood1).unwrap(); //TODO
    let tri_seq2 = arbitrary_triangulation(wood2).unwrap(); //TODO

    let seq1 = to_simple_stack(wood1, color).unwrap();
    let seq2 = to_simple_stack(wood2, color).unwrap();

    #[cfg(debug_assertions)]
    {
        DEBUG.write().unwrap().output(
            "to_canonical",
            &wood1,
            Some("Canonical1"),
            &wood1.calculate_face_counts(),
        );
        DEBUG.write().unwrap().output(
            "to_canonical",
            &wood2,
            Some("Canonical2"),
            &wood2.calculate_face_counts(),
        );
    }

    let vertex_map = wood1.get_vertex_map(wood2).expect("TODO");

    let mut seq = Vec::with_capacity(tri_seq1.len() + tri_seq2.len() + seq1.len() + seq2.len());
    seq.extend(tri_seq1);
    seq.extend(seq1);

    for op in seq2.iter().rev() {
        seq.push(
            op.mapped_vertices_by_right(&vertex_map)
                .unwrap_or_else(|e| panic!("Internal assertion failed: {}", e))
                .inverted(),
        );
    }

    for op in tri_seq2.iter().rev() {
        seq.push(
            op.mapped_vertices_by_right(&vertex_map)
                .unwrap_or_else(|e| panic!("Internal assertion failed: {}", e))
                .inverted(),
        );
    }

    return seq;
}

pub fn to_simple_stack(
    wood: &mut SchnyderMap,
    color: SchnyderColor,
) -> GraphResult<Vec<Operation>> {
    if !wood.map.is_triangulation() {
        return GraphErr::new_err("Only triangulations can be transformed to a simple stack.");
    }

    let mut pivot = wood.get_suspension_vertex(color);
    let mut seq = Vec::new();

    while wood
        .get_incoming_sector(pivot, color, false)
        .first()
        .is_some()
    {
        seq.extend(full_pizza_lemma(wood, pivot, color).expect("TODO"));

        if let Some(v) = wood.get_incoming_sector(pivot, color, false).first() {
            pivot = *v;
        } else {
            panic!("Invalid Schnyder wood detected.");
        }
    }

    return Ok(seq);
}
