use std::collections::HashMap;
use itertools::Itertools;

use crate::graph::EdgeEnd::{Tail, Head};
use crate::graph::schnyder::algorithm::{Operation, schnyder_contractible};
use crate::graph::schnyder::SchnyderColor::{Blue, Green, Red};
use crate::graph::schnyder::SchnyderEdgeDirection::Unicolored;
use crate::graph::schnyder::{SchnyderMap, SchnyderColor};
use crate::graph::schnyder::SchnyderVertexType::Normal;
use crate::graph::EdgeI;

pub fn find_sequence<F: Clone>(wood1: SchnyderMap<F>, wood2: SchnyderMap<F>) -> Vec<Operation> {
    find_sequence_(wood1, wood2)
}

pub fn compute_contraction_candidates<F: Clone>(wood: &SchnyderMap<F>) -> HashMap<&SchnyderColor, Vec<(&EdgeI, bool)>> {
    wood.map.edge_indices().map(|e| (wood.map.edge_weight(&e).unwrap(), e))
        .filter(|(_, e)| wood.is_inner_edge(e))
        .filter_map(|(dir,e)|
            match dir {
                Unicolored(c, _) => Some((c, e)),
                _ => None
            }
        )
        .map(|(c, e)| (c, (e, schnyder_contractible(wood, *e).is_ok())))
        .sorted_by_key(|(c, (e, contractible))| !*contractible)
        .into_group_map()
}

fn find_sequence_<F: Clone>(wood1: SchnyderMap<F>, wood2: SchnyderMap<F>) -> Vec<Operation> {
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
        let candidates2 = compute_contraction_candidates(&wood2);


        /*let contractibles1 = wood1.map.edge_indices()
            .filter(|e| schnyder_contractible(&wood1, **e).is_ok())
            .map(|e| (wood1.map.edge_weight(&e).unwrap(), e))
            .filter_map(|(dir,e)| match dir { Unicolored(c, _) => Some((c, e)), _ => None })
            .into_group_map();
        let contractibles2 = wood2.map.edge_indices()
            .filter(|e| schnyder_contractible(&wood2, **e).is_ok())
            .map(|e| (wood2.map.edge_weight(&e).unwrap(), e))
            .filter_map(|(dir,e)| match dir { Unicolored(c, _) => Some((c, e)), _ => None })
            .into_group_map();

        for c in &[Red, Green, Blue] {
            if let Some(cc1) = contractibles1.get(c) {
                if let Some(cc2) = contractibles2.get(c) {
                    if !cc1.is_empty() && !cc2.is_empty() {
                        (cc1[0], cc2[0])
                    }
                }
            }
        }


        let contr_candidates1 = compute_contraction_candidates(&wood1);
        let contr_candidates2 = compute_contraction_candidates(&wood2);*/

        return Vec::new();
    }
}