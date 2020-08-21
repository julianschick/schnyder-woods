use crate::graph::{ClockDirection, VertexI, EdgeI, Signum, swap};
use crate::graph::schnyder::SchnyderMap;
use crate::graph::schnyder::SchnyderVertexType::Suspension;
use crate::graph::schnyder::SchnyderEdgeDirection::Unicolored;
use crate::graph::Signum::{Backward, Forward};
use crate::util::iterators::cyclic::CyclicIterable;
use crate::graph::schnyder::IndexedEnum;
use itertools::Itertools;
use crate::graph::io::debug_output;

struct Split {
    edge: (VertexI, VertexI),
    direction: ClockDirection,
    target_vertex: VertexI
}

struct Merge {
    merged_edge: (VertexI, VertexI),
    target_edge: (VertexI, VertexI)
}

pub struct Operation {
    split: Option<Split>,
    merge: Option<Merge>
}

impl Operation {

    pub fn split(edge: (VertexI, VertexI), direction: ClockDirection, target_vertex: VertexI) -> Operation {
        Operation {
            split: Some(Split {
                edge, direction, target_vertex
            }),
            merge: None
        }
    }

    pub fn merge(merged_edge: (VertexI, VertexI), target_edge: (VertexI, VertexI)) -> Operation {
        Operation {
            merge: Some(Merge {
                merged_edge, target_edge
            }),
            split: None
        }
    }
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

    debug_output(wood, "start", Some("Start"), &wood.calculate_face_counts());

    if let Unicolored(color, signum) = e.weight {

        // normalize tail and head according to color orientation
        let (tail, head) = swap((tail, head), signum == Backward);
        let (tail_nb, head_nb) = swap((tail_nb, head_nb), signum == Backward);

        let mut tail_section = tail.neighbors.cycle(tail_nb.index, false).skip(1)
            .take_while(|nb| wood.incoming_color(nb) == Some(color.prev()) || wood.outgoing_color(nb) == Some(color.next()))
            .map(|nb| nb.edge)
            .collect_vec();

        let mut head_section = head.neighbors.cycle(head_nb.index, false).rev()
            .take_while(|nb| wood.incoming_color(nb) == Some(color) || wood.outgoing_color(nb) == Some(color.next()))
            .map(|nb| nb.edge)
            .collect_vec();

        while tail_section.len() > 1 {
            let source = tail_section[tail_section.len() - 1];
            let target = tail_section[tail_section.len() - 2];

            let dir = wood.merge(source, target);
            wood.split_to_any(target, dir);
            tail_section.pop();
        }

        let mut i = 0;
        while head_section.len() > 1 {
            let source = head_section[head_section.len() - 1];
            let target = head_section[head_section.len() - 2];

            let dir = wood.merge(source, target);
            println!("merge refint = {}", wood.map.check_referential_integrity());
            debug_output(wood, &format!("merge {}", i), Some(&format!("merge {}", i)), &wood.calculate_face_counts());

            wood.split_to_any(target, dir);
            println!("split refint = {}", wood.map.check_referential_integrity());
            debug_output(wood, &format!("split {}", i), Some(&format!("split {}", i)), &wood.calculate_face_counts());
            head_section.pop();
            i += 1;
        }


    } else {
        panic!("assertion failed!");
    }

    debug_output(wood, "finish", Some("Finish"), &wood.calculate_face_counts());


    return Vec::new();
}