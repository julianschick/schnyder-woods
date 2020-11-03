use petgraph::{Graph, Undirected};
use std::sync::{Arc, Mutex};
use std::collections::{HashMap, VecDeque};
use crate::graph::schnyder::SchnyderColor;
use crate::graph::ClockDirection;
use crate::graph::ClockDirection::{CW, CCW};
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::SchnyderMap;
use std::time::{Instant, Duration};
use std::thread;
use rand::thread_rng;
use itertools::Itertools;
use std::thread::sleep;
use std::sync::atomic::AtomicBool;
use std::cmp::Ordering;
use std::sync::mpsc::channel;

#[derive(Copy, Clone)]
pub enum SymmetryBreaking {
    None, BreakOrientation, BreakColourRotation, BreakAll
}

impl SymmetryBreaking {
    pub fn expand<'a>(&self, color: &'a SchnyderColor, direction: &'a ClockDirection) -> Vec<(&'a SchnyderColor, &'a ClockDirection)> {
        match self {
            SymmetryBreaking::None => vec![(color, direction)],
            SymmetryBreaking::BreakOrientation => vec![(color, &CW), (color, &CCW)],
            SymmetryBreaking::BreakColourRotation => vec![(&Red, direction), (&Green, direction), (&Blue, direction)],
            SymmetryBreaking::BreakAll => {
                let mut result = Vec::with_capacity(6);
                for color in &[Red, Green, Blue] {
                    for dir in &[CW, CCW] {
                        result.push((color, dir));
                    }
                }
                return result;
            }
        }
    }
}

pub fn build_flipgraph(n: usize, symmetry_breaking: SymmetryBreaking, thread_count: usize) -> Graph<usize, f32, Undirected> {

    let mut g = Arc::new(Mutex::new(Graph::new_undirected()));
    let mut known = Arc::new(Mutex::new(HashMap::new()));
    let mut stack = Arc::new(Mutex::new(Vec::new()));

    {
        let mut g = g.lock().unwrap();
        let mut known = known.lock().unwrap();
        let mut stack = stack.lock().unwrap();

        let mut wood1 = SchnyderMap::build_apollonian_path(n, Red).expect("TODO");
        let root_node_index1 = g.add_node(wood1.map.edge_count());

        known.insert(wood1.compute_standard_identification_vector(), root_node_index1);
        stack.push(wood1);
    }

    let mut handles = Vec::new();
    let (tx, rx) = channel();

    println!("Traversing the flipgraph for n = {}, using {} threads.", n, thread_count);

    for i in 0..thread_count {
        let stack = Arc::clone(&stack);
        let g = Arc::clone(&g);
        let known = Arc::clone(&known);

        let tx = tx.clone();
        let mut informed_others = false;

        let mut last_print = Instant::now();
        let mut nodes_checked = 0usize;
        let mut last_nodes_checked = 0usize;
        let mut nodes_added = 0usize;
        let mut last_nodes_added = 0usize;

        let handle = thread::spawn(move || {

            loop {
                let mut stack_ = stack.lock().unwrap();

                let len = stack_.len();
                let current_optional = stack_.pop();
                drop(stack_);

                if current_optional.is_none() {
                    if i == 0 && !informed_others {
                        tx.send(true).expect("Multithreading did not work as expected");
                    }
                    break;
                }
                let current = current_optional.unwrap();

                nodes_checked += 1;

                if i == 0 && !informed_others && len > thread_count*2 {
                    tx.send(true);
                }

                let admissible_ops = current.get_admissible_ops().expect("TODO");

                let neighbors = admissible_ops.iter().map(|op| {
                    let mut neighbor = current.clone();
                    neighbor.do_operation(&op);
                    let nb_ids: VecDeque<_> = symmetry_breaking.expand(&Red, &CW)
                        .iter().map(|(c, d)| neighbor.compute_identification_vector(**c, **d)).collect();
                    return (nb_ids, neighbor);
                }).collect_vec();

                for (mut nb_ids, neighbor) in neighbors {
                    let mut known = known.lock().unwrap();
                    let mut g = g.lock().unwrap();

                    let current_node_index = *known.get(&current.compute_standard_identification_vector()).expect("TODO");

                    //println!("{} - {:?}", current_node_index.index(), current.compute_standard_identification_vector());

                    let mut nb_node_index = None;
                    for nb_id in &nb_ids {
                        if let Some(index) = known.get(nb_id) {
                            nb_node_index = Some(index); break;
                        }
                    }

                    if let Some(nb_node_index) = nb_node_index {
                        if !g.contains_edge(current_node_index, *nb_node_index) {
                            g.add_edge(current_node_index, *nb_node_index, 1.0);
                        }
                    } else {
                        let nb_node_index = g.add_node(neighbor.map.edge_count());
                        nodes_added += 1;
                        g.add_edge(current_node_index, nb_node_index, 1.0f32);

                        known.insert(nb_ids.pop_front().unwrap(), nb_node_index);
                        stack.lock().unwrap().push(neighbor);
                    }

                }

                if last_print.elapsed().as_secs() > 5 {

                    if i == 0 {
                        println!("{:<8}|{:>14}|{:>14}|", "TOTAL", "NODES", "STACKLEN");
                        println!("{:<8}|{:>14}|{:>14}|", ">>>", g.lock().unwrap().node_count(), stack.lock().unwrap().len());
                        println!("{:<8}|{:<14}|{:<14}|{:<14}|{:<14}|{:<14}|", "THREAD", "N_CHECK", "N_CHECK/s", "N_ADD", "N_ADD/s", "ADD/CHECK");

                    }

                    let ratio = (nodes_added - last_nodes_added) as f32 /  (nodes_checked - last_nodes_checked) as f32;
                    println!("{:<8}|{:>14}|{:>14}|{:>14}|{:>14}|{:>14.4}|", i, nodes_checked, (nodes_checked - last_nodes_checked) / 5, nodes_added, (nodes_added - last_nodes_added) / 5, ratio);
                    last_nodes_added = nodes_added;
                    last_nodes_checked = nodes_checked;

                    last_print = Instant::now();
                }
            }
        });
        handles.push(handle);

        if i == 0 {
            rx.recv().expect("Multithreading did not work as expected");
        }
    }

    for handle in handles {
        handle.join().unwrap();
    }

    return Arc::try_unwrap(g).unwrap().into_inner().unwrap();
}