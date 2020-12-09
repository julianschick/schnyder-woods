use bimap::BiMap;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::io::Write;
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Instant;

use crate::graph::enums::ClockDirection;
use crate::graph::enums::ClockDirection::{CCW, CW};
use crate::schnyder::enums::SchnyderColor;
use crate::schnyder::enums::SchnyderColor::{Blue, Green, Red};
use crate::schnyder::SchnyderMap;

pub mod io;
pub mod random_walk;
pub mod stats;

#[derive(Copy, Clone)]
pub enum SymmetryBreaking {
    None,
    BreakOrientation,
    BreakColourRotation,
    BreakAll,
}

impl SymmetryBreaking {
    pub fn expand<'a>(
        &self,
        color: &'a SchnyderColor,
        direction: &'a ClockDirection,
    ) -> Vec<(&'a SchnyderColor, &'a ClockDirection)> {
        match self {
            SymmetryBreaking::None => vec![(color, direction)],
            SymmetryBreaking::BreakOrientation => vec![(color, &CW), (color, &CCW)],
            SymmetryBreaking::BreakColourRotation => {
                vec![(&Red, direction), (&Green, direction), (&Blue, direction)]
            }
            SymmetryBreaking::BreakAll => {
                let mut result = Vec::with_capacity(6);
                for color in &[Red, Green, Blue] {
                    for dir in &[CW, CCW] {
                        result.push((color, dir));
                    }
                }
                result
            }
        }
    }
}

pub fn build_flipgraph(
    n: usize,
    min_level: Option<usize>,
    max_level: Option<usize>,
    symmetry_breaking: SymmetryBreaking,
    thread_count: usize,
) -> Flipgraph {
    if let (Some(min), Some(max)) = (min_level, max_level) {
        if min > max {
            panic!("Invalid min/max levels given.");
        }
    }

    if n < 3 {
        panic!("n must be at least 3.");
    }

    let g = Arc::new(Mutex::new(Flipgraph::new(n)));
    let stack = Arc::new(Mutex::new(Vec::new()));

    {
        let mut g = g.lock().unwrap();
        let mut stack = stack.lock().unwrap();

        let seed = match max_level {
            Some(max_level) if max_level < 3 * n - 6 => {
                let mut seed = SchnyderMap::build_min_edge_wood(n, Red).expect("TODO");
                while seed.map.edge_count() < max_level + 1 && seed.map.edge_count() < 3 * n - 6 {
                    seed.split_any();
                }
                seed
            }
            _ => SchnyderMap::build_simple_stack(n, Red).expect("TODO"),
        };

        let code = seed.compute_3tree_code();
        let index = g.add_node(code, seed.map.edge_count() as u8);
        stack.push(index);
    }

    let mut handles = Vec::new();
    let (tx, rx) = channel();

    println!(
        "Traversing the flipgraph for n = {}, using {} threads.",
        n, thread_count
    );
    if let Some(min_level) = min_level {
        println!("CAUTION: minimal level set to {}", min_level);
    }
    if let Some(max_level) = max_level {
        println!("CAUTION: maximal level set to {}", max_level);
    }

    for i in 0..thread_count {
        let stack = Arc::clone(&stack);
        let g = Arc::clone(&g);

        let tx = tx.clone();
        let informed_others = false;

        let mut last_print = Instant::now();
        let mut nodes_checked = 0usize;
        let mut last_nodes_checked = 0usize;
        let mut nodes_added = 0usize;
        let mut last_nodes_added = 0usize;

        let handle = thread::spawn(move || loop {
            let mut stack_ = stack.lock().unwrap();

            let len = stack_.len();
            let current_optional = stack_.pop();
            drop(stack_);

            if current_optional.is_none() {
                if i == 0 && !informed_others {
                    tx.send(true)
                        .expect("Multithreading did not work as expected");
                }
                break;
            }
            let current_index = current_optional.unwrap();
            let current = {
                let g = g.lock().unwrap();
                let current_code = g.get_code(current_index);
                SchnyderMap::build_from_3tree_code(&current_code).unwrap()
            };

            nodes_checked += 1;

            if i == 0 && !informed_others && len > thread_count * 2 {
                tx.send(true)
                    .expect("Multithreading did not work as expected");
            }

            let current_level = current.map.edge_count();

            let admissible_ops = current
                .get_admissible_ops()
                .expect("Admissible operations could not be listed")
                .into_iter()
                .filter(|op| {
                    if op.is_upwards() {
                        if let Some(max_level) = max_level {
                            if max_level < 3 * n - 6 {
                                current_level == max_level
                            } else {
                                current_level == 3 * n - 7
                            }
                        } else {
                            current_level == 3 * n - 7
                        }
                    } else {
                        if let Some(min_level) = min_level {
                            current_level >= min_level
                        } else {
                            true
                        }
                    }
                })
                .collect_vec();

            let neighbors = admissible_ops
                .iter()
                .map(|op| {
                    let mut neighbor = current.clone();
                    neighbor
                        .do_operation(&op)
                        .expect("Admissible operation not successful");
                    let nb_ids: VecDeque<_> = symmetry_breaking
                        .expand(&Red, &CW)
                        .iter()
                        .map(|(c, d)| neighbor.compute_3tree_code_with_rotation(**c, **d))
                        .collect();
                    return (nb_ids, neighbor);
                })
                .collect_vec();

            for (mut nb_ids, neighbor) in neighbors {
                let mut g = g.lock().unwrap();

                let current_node_index = *g.contains_node(&g.get_code(current_index)).unwrap();

                let mut nb_node_index = None;
                for nb_id in &nb_ids {
                    if let Some(&index) = g.contains_node(nb_id) {
                        nb_node_index = Some(index);
                        break;
                    }
                }

                if let Some(nb_node_index) = nb_node_index {
                    if !g.contains_edge(current_node_index, nb_node_index) {
                        g.add_edge(current_node_index, nb_node_index);
                    }
                } else {
                    let code = nb_ids.pop_front().unwrap();
                    let nb_node_index = g.add_node(code, neighbor.map.edge_count() as u8);
                    nodes_added += 1;
                    g.add_edge(current_node_index, nb_node_index);

                    stack.lock().unwrap().push(nb_node_index);
                }
            }

            if last_print.elapsed().as_millis() >= 5000 {
                if i == 0 {
                    println!("{:<8}|{:>14}|{:>14}|", "TOTAL", "NODES", "STACKLEN");
                    println!(
                        "{:<8}|{:>14}|{:>14}|",
                        ">>>",
                        g.lock().unwrap().node_count(),
                        stack.lock().unwrap().len()
                    );
                    println!(
                        "{:<8}|{:<14}|{:<14}|{:<14}|{:<14}|{:<14}|",
                        "THREAD", "N_CHECK", "N_CHECK/s", "N_ADD", "N_ADD/s", "ADD/CHECK"
                    );
                }

                let ratio = (nodes_added - last_nodes_added) as f32
                    / (nodes_checked - last_nodes_checked) as f32;
                println!(
                    "{:<8}|{:>14}|{:>14}|{:>14}|{:>14}|{:>14.4}|",
                    i,
                    nodes_checked,
                    (nodes_checked - last_nodes_checked) / 5,
                    nodes_added,
                    (nodes_added - last_nodes_added) / 5,
                    ratio
                );
                last_nodes_added = nodes_added;
                last_nodes_checked = nodes_checked;

                last_print = Instant::now();
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

    Arc::try_unwrap(g).unwrap().into_inner().unwrap()
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Flipgraph {
    expected_average_degree: usize,
    adjacencies: Vec<Vec<usize>>,
    nodes_by_treecode: BiMap<Vec<u8>, usize>,
    levels: Vec<u8>,
}

//Frozen for ma-auxiliary/computations
/*(#[derive(Debug, Serialize, Deserialize)]
pub struct Flipgraph {
    expected_average_degree: usize,
    adjacencies: Vec<Vec<usize>>,
    nodes_by_treecode: BiMap<Vec<u8>, usize>,
    levels: Vec<u8>
}*/

impl Flipgraph {
    pub fn new(expected_average_degree: usize) -> Self {
        Flipgraph {
            expected_average_degree,
            adjacencies: Vec::new(),
            nodes_by_treecode: BiMap::new(),
            levels: Vec::new(),
        }
    }

    pub fn node_count(&self) -> usize {
        self.adjacencies.len()
    }

    pub fn edge_count(&self) -> usize {
        self.adjacencies
            .iter()
            .enumerate()
            .map(|(i, adj)| adj.iter().filter(|&&j| i < j).count())
            .sum()
    }

    pub fn get_n(&self) -> u8 {
        if let Some((code, _)) = self.nodes_by_treecode.iter().next() {
            (code.len() / 3) as u8
        } else {
            return 0;
        }
    }

    pub fn add_node(&mut self, code: Vec<u8>, level: u8) -> usize {
        self.adjacencies.push(Vec::with_capacity(
            (self.expected_average_degree as f32 * 0.6).ceil() as usize,
        ));
        let index = self.adjacencies.len() - 1;
        self.nodes_by_treecode.insert(code, index);
        self.levels.push(level);
        return index;
    }

    pub fn get_code(&self, index: usize) -> &Vec<u8> {
        self.nodes_by_treecode.get_by_right(&index).unwrap()
    }

    pub fn contains_node(&self, code: &Vec<u8>) -> Option<&usize> {
        self.nodes_by_treecode.get_by_left(code)
    }

    pub fn add_edge(&mut self, v1: usize, v2: usize) {
        //let (v1, v2) = if v1 < v2 { (v1, v2) } else { (v2, v1) };
        self.adjacencies[v1].push(v2);
        self.adjacencies[v2].push(v1);
    }

    pub fn contains_edge(&self, v1: usize, v2: usize) -> bool {
        //let (v1, v2) = if v1 < v2 { (v1, v2) } else { (v2, v1) };
        self.adjacencies[v1].contains(&v2)
    }

    pub fn get_level(&self, v: usize) -> u8 {
        return self.levels[v];
    }

    pub fn get_neighbors(&self, v: usize) -> impl Iterator<Item = &usize> {
        self.adjacencies[v].iter()
    }

    pub fn get_degree(&self, v: usize) -> usize {
        self.adjacencies[v].len()
    }

    pub fn get_updegree(&self, v: usize) -> usize {
        let level = self.get_level(v);
        self.adjacencies[v]
            .iter()
            .filter(|&&nb| self.get_level(nb) > level)
            .count()
    }

    pub fn get_downdegree(&self, v: usize) -> usize {
        let level = self.get_level(v);
        self.adjacencies[v]
            .iter()
            .filter(|&&nb| self.get_level(nb) < level)
            .count()
    }

    pub fn get_levels(&self) -> HashMap<u8, Vec<usize>> {
        self.levels
            .iter()
            .enumerate()
            .map(|(a, &b)| (b, a))
            .into_group_map()
    }

    pub fn to_edge_list(&self, writer: &mut dyn Write) -> std::io::Result<()> {
        let n = self.edge_count();
        writer.write_all(&(n as u64).to_le_bytes())?;

        for v1 in 0..self.adjacencies.len() {
            for &v2 in &self.adjacencies[v1] {
                if v1 < v2 {
                    writer.write_all(&(v1 as u64).to_le_bytes())?;
                    writer.write_all(&(v2 as u64).to_le_bytes())?;
                }
            }
        }

        Ok(())
    }

    pub fn to_code_list(&self, writer: &mut dyn Write) -> std::io::Result<()> {
        if let Some((code, _)) = self.nodes_by_treecode.iter().next() {
            writer.write_all(&(code.len() as u64).to_le_bytes())?;

            for v in 0..self.adjacencies.len() {
                writer.write_all(&self.nodes_by_treecode.get_by_right(&v).unwrap())?;
            }
        }

        Ok(())
    }

    pub fn to_level_list(&self, writer: &mut dyn Write) -> std::io::Result<()> {
        writer.write_all(&self.levels)
    }
}
