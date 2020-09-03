use std::collections::HashMap;
use itertools::Itertools;

use crate::graph::EdgeEnd::{Tail, Head};
use crate::graph::schnyder::algorithm::{Operation, schnyder_contractible, make_inner_edge, make_contractible};
use crate::graph::schnyder::SchnyderColor::{Blue, Green, Red};
use crate::graph::schnyder::SchnyderEdgeDirection::Unicolored;
use crate::graph::schnyder::{SchnyderMap, SchnyderColor};
use crate::graph::schnyder::SchnyderVertexType::Normal;
use crate::graph::EdgeI;

use crate::DEBUG;

pub fn find_sequence<F: Clone>(wood1: SchnyderMap<F>, wood2: SchnyderMap<F>) -> Vec<Operation> {
    find_sequence_(wood1, wood2)
}

pub fn compute_contraction_candidates<F: Clone>(wood: &SchnyderMap<F>) -> HashMap<SchnyderColor, Vec<(EdgeI, bool)>> {
    wood.map.edge_indices().map(|&e| (wood.map.edge_weight(&e).unwrap(), e))
        .filter(|(_, e)| wood.is_inner_edge(e))
        .filter_map(|(dir,e)|
            match dir {
                Unicolored(c, _) => Some((*c, e)),
                _ => None
            }
        )
        .map(|(c, e)| (c, (e, schnyder_contractible(wood, e).is_ok())))
        .sorted_by_key(|(c, (e, contractible))| !*contractible)
        .into_group_map()
}

fn compute_color_quality(candidates: &HashMap<SchnyderColor, Vec<(EdgeI, bool)>>) -> HashMap<SchnyderColor, usize> {
    [Red, Green, Blue].iter().map(|&c|
        match candidates.get(&c) {
            Some(cand) =>  if cand[0].1 { (c, 2) } else { (c, 1) },
            None => (c, 0usize)
        }
    ).collect()
}

fn prepare_wood<F: Clone>(wood: &mut SchnyderMap<F>, candidates: &HashMap<SchnyderColor, Vec<(EdgeI, bool)>>, color: SchnyderColor) {

    if let Some(cand) = candidates.get(&color) {
        let (e, contractible) = cand[0];
        if !contractible {
            make_contractible(wood, e);
        }
        wood.schnyder_contract(e);
    } else {
        let (new_edge, _) = make_inner_edge(wood, color);

        if !schnyder_contractible(wood, new_edge).is_ok() {
            make_contractible(wood, new_edge);
        }
        wood.schnyder_contract(new_edge);
    }
}

fn find_sequence_<F: Clone>(mut wood1: SchnyderMap<F>, mut wood2: SchnyderMap<F>) -> Vec<Operation> {
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
        return vec![];
    } else {
        let candidates1 = compute_contraction_candidates(&wood1);
        let candidates2 = compute_contraction_candidates(&wood2);

        let color_quality1 = compute_color_quality(&candidates1);
        let color_quality2 = compute_color_quality(&candidates2);
        let best_color = [Red, Green, Blue].iter()
            .max_by_key(|c| color_quality1.get(c).unwrap() + color_quality2.get(c).unwrap()).unwrap();

        eprintln!("best_color = {:?}", best_color);

        prepare_wood(&mut wood1, &candidates1, *best_color);
        prepare_wood(&mut wood2, &candidates2, *best_color);

        DEBUG.write().unwrap().output(&wood1, Some("Wood1 =>"), &wood1.calculate_face_counts());
        DEBUG.write().unwrap().output(&wood2, Some("=> Wood2"), &wood2.calculate_face_counts());

        find_sequence(wood1, wood2);

        return Vec::new();
    }
}