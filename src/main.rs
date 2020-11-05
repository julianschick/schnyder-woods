use std::collections::{HashSet, HashMap, BTreeMap};
use std::cmp::Ordering;
use itertools::Itertools;

use crate::graph::{PlanarMap, VertexI, Side};
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use crate::graph::schnyder::SchnyderEdgeDirection::{Bicolored, Unicolored};
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::{SchnyderVertexType, SchnyderEdgeDirection, SchnyderMap, SchnyderEdge, SchnyderColor};
use crate::graph::Signum::Forward;
use crate::util::iterators::cyclic::CyclicIterable;
use crate::util::debug::Debug;
use crate::graph::ClockDirection::{CCW, CW};
use std::fs::File;
use std::sync::{RwLock, Arc, Mutex};
use std::io::Write;
use std::io::Read;
use std::process::Command;
use crate::graph::schnyder::SchnyderBuildMode::{LeftMost, RightMost, Random};
use crate::graph::io::{read_plantri_planar_code};
use crate::graph::schnyder::algorithm::{make_contractible, make_inner_edge, full_pizza_lemma, OpType};
use crate::algorithm::{compute_contraction_candidates, find_sequence, to_canonical_form, find_sequence_2};
use crate::graph::EdgeEnd::{Tail, Head};

use petgraph::algo::{is_isomorphic, bellman_ford};
use petgraph::prelude::*;
use petgraph::Graph;
use crate::petgraph_ext::{to_sparse6, to_edge_list, to_level_list};
use std::time::{Instant, Duration};
use std::thread;
use std::thread::sleep;
use rand::{thread_rng, Rng};
use crate::flipgraph::{build_flipgraph, SymmetryBreaking, Flipgraph};
use crate::arraytree::{ArrayTree, WalkAroundIterator, WalkAroundDirection};
use clap::App;
use std::path::Path;

#[macro_use]
extern crate lazy_static;

mod graph;
mod util;
mod algorithm;
mod petgraph_ext;
mod flipgraph;
mod arraytree;

lazy_static! {
    static ref DEBUG: RwLock<Debug> = RwLock::new(Debug::new("/tmp/schnyder", "/tmp/schnyder/output"));
}

fn main() {
    let matches = App::new("schnyderflip")
        .version("0.9.1")
        .author("Julian Schick <julian.schick@posteo.de>")
        .about("Algorithms for manipulating Schnyder woods")
        .subcommand(
            App::new("build-flipgraph")
                .arg("<N> 'Number of vertices'")
                .arg("<OUTPUT> 'Output file'")
                .arg("-t --threads [t] 'Number of threads to start'")
                .arg("-c --break-color-symmetry 'Interpret Schnyder woods that differ only in color rotation as the same node of the flip graph'")
                .arg("-o --break-orientation-symmetry 'Interpret Schnyder woods that differ only in orientation as the same node of the flip graph'")
        )
        .get_matches();

    if let Some(build_matches) = matches.subcommand_matches("build-flipgraph") {

    }

    match matches.subcommand() {
        Some(("build-flipgraph", matches))=> {
            let n = str::parse::<usize>(matches.value_of("N").unwrap_or("3"))
                .unwrap_or_else(|_| { println!("{}", "Bittaschön"); return 3 });
            let num_threads= str::parse::<usize>(matches.value_of("threads").unwrap_or("1"))
                .unwrap_or_else(|_| { println!("{}", "Bittaschön!"); return 1});

            let brk_orientation = matches.is_present("break-orientation-symmetry");
            let brk_color = matches.is_present("break-color-symmetry");

            let output = Path::new(matches.value_of("OUTPUT").unwrap());

            if output.is_file() {
                println!("{}", "already present");
                return;
            }
            if output.is_dir() {
                println!("{}", "directory given");
                return;
            }
            if let Some(parent) = output.parent() {
                if !parent.is_dir() {
                    println!("{}", "can't write there");
                    return;
                }
            }


            let symmetry_breaking = match (brk_orientation, brk_color) {
                (false, false) => SymmetryBreaking::None,
                (true, false) => SymmetryBreaking::BreakOrientation,
                (false, true) => SymmetryBreaking::BreakColourRotation,
                (true, true) => SymmetryBreaking::BreakAll
            };

            if n < 3 {
                println!("{}", "n must be at least 3");
                return;
            }

            main7(n, num_threads, symmetry_breaking, output);
        },
        _ => {
            println!("{}", "No valid command specified.");
        }
    }
}

fn main8() {

    let mut file = File::open("/tmp/test.bincode").unwrap();
    let g : Flipgraph = bincode::deserialize_from(file).expect("TODO");

    let levels = g.get_levels();

    print_header();
    print_statistics("ALL", (0..g.node_count()).collect_vec(), &g);
    for (level, indices) in levels.into_iter().sorted_by_key(|(level, _)| *level) {
        print_statistics(&format!("Level {}", level), indices, &g);
    }

    eprintln!("NODES = {:?}", g.node_count());
    eprintln!("EDGES = {:?}", g.edge_count());
}

fn main7(n: usize, thread_count: usize, symmetry_breaking: SymmetryBreaking, output_file: &Path) {
    let g = build_flipgraph(n, symmetry_breaking, thread_count);

    let levels = g.get_levels();

    print_header();
    print_statistics("ALL", (0..g.node_count()).collect_vec(), &g);
    for (level, indices) in levels.into_iter().sorted_by_key(|(level, _)| *level) {
        print_statistics(&format!("Level {}", level), indices, &g);
    }

    eprintln!("NODES = {:?}", g.node_count());
    eprintln!("EDGES = {:?}", g.edge_count());

    /*println!("{}", "Writing edge list...");
    {
        let mut file = File::create("/tmp/test.edges").unwrap();
        g.to_edge_list(&mut file);
    }

    println!("{}", "Writing level list...");
    {
        let mut file = File::create("/tmp/test.levels").unwrap();
        g.to_level_list(&mut file);
    }

    println!("{}", "Writing codes list...");
    {
        let mut file = File::create("/tmp/test.codes").unwrap();
        g.to_code_list(&mut file);
    }*/

    println!("{}", "Writing CBOR...");
    {
        let mut file = File::create(output_file).expect("TODO");
        serde_cbor::to_writer(file, &g).expect("TODO");
    }

    /*println!("{}", "Writing Bincode...");
    {
        let mut file = File::create("/tmp/test.bincode").expect("TODO");
        bincode::serialize_into(file, &g).expect("TODO");
    }*/
}

fn main6() {
    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let mut g = Arc::new(Mutex::new(Graph::new_undirected()));
    let mut known = Arc::new(Mutex::new(BTreeMap::new()));
    let mut stack = Arc::new(Mutex::new(Vec::new()));

    //let maps = read_plantri_planar_code(&data, Some(78), |i| i.0, |i| i.0, |i| i.0);
    let n = 6;
    DEBUG.write().unwrap().activate();

    {
        let mut g = g.lock().unwrap();
        let mut known = known.lock().unwrap();
        let mut stack = stack.lock().unwrap();

        /*for map in maps {

            let mut wood1 = SchnyderMap::build_on_triangulation(&map, map.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();
            let mut wood2 = wood1.clone();

            loop {
                let admissible = wood2.get_admissible_ops().unwrap().into_iter().filter(|op| match op.operation_type { OpType::Merge | OpType::ExtMerge => true, _ => false}).collect_vec();
                if admissible.is_empty() { break };
                wood2.do_operation(&admissible[0]);
            }

            let root_node_index1 = g.add_node(wood1.map.edge_count());
            known.insert(wood1.compute_identification_vector(Red), root_node_index1);
            debug_wood(&wood1, root_node_index1);
            stack.push(wood1);

            let id2 = wood2.compute_identification_vector(Red);
            if !known.contains_key(&id2) {
                let root_node_index2 = g.add_node(wood2.map.edge_count());
                known.insert(id2, root_node_index2);
                debug_wood(&wood2, root_node_index2);
                stack.push(wood2);
            }
        }*/

        let mut wood1 = SchnyderMap::build_apollonian_path(n, Red).expect("TODO");
        let root_node_index1 = g.add_node(wood1.map.edge_count());
        for color in &[Red, Green, Blue] {
            for dir in &[CW, CCW] {
                known.insert(wood1.compute_3tree_code_with_rotation(*color, *dir), root_node_index1);
            }
        }
        debug_wood(&wood1, root_node_index1);
        stack.push(wood1);

    }

    let mut handles = Vec::new();

    let num_threads = if let Some(Ok(num)) = std::env::args().nth(1).map(|str| str.parse()) {
        num
    } else { 1 };

    println!("Using {} threads.", num_threads);

    for i in 0..num_threads {
        let stack = Arc::clone(&stack);
        let g = Arc::clone(&g);
        let known = Arc::clone(&known);
        //let inspected = Arc::clone(&inspected);

        let mut last_print = Instant::now();
        let mut nr_inspected = 0;

        let handle = thread::spawn(move || {
            let mut rnd = thread_rng();

            loop {
                let mut stack_ = stack.lock().unwrap();

                let len = stack_.len();
                let current_optional = stack_.pop();
                drop(stack_);

                if current_optional.is_none() {
                    break;
                }
                let current = current_optional.unwrap();
                nr_inspected += 1;

                let admissible_ops = current.get_admissible_ops().expect("TODO");

                let neighbors = admissible_ops.iter().map(|op| {
                    let mut neighbor = current.clone();
                    neighbor.do_operation(&op);
                    let nb_id = (
                        neighbor.compute_3tree_code_with_rotation(Red, CW),
                        neighbor.compute_3tree_code_with_rotation(Green, CW),
                        neighbor.compute_3tree_code_with_rotation(Blue, CW),
                        neighbor.compute_3tree_code_with_rotation(Red, CCW),
                        neighbor.compute_3tree_code_with_rotation(Green, CCW),
                        neighbor.compute_3tree_code_with_rotation(Blue, CCW)
                    );

                    return (nb_id, neighbor);
                }).collect_vec();

                for (nb_id, neighbor) in neighbors {
                    let mut known = known.lock().unwrap();
                    let mut g = g.lock().unwrap();

                    let current_node_index = *known.get(&current.compute_3tree_code()).expect("TODO");

                    //println!("{} - {:?}", current_node_index.index(), current.compute_standard_identification_vector());

                    if let Some(nb_node_index) = known.get(&nb_id.0) {
                        if !g.contains_edge(current_node_index, *nb_node_index) {
                            g.add_edge(current_node_index, *nb_node_index, 1.0);
                        }
                    } else {
                        let nb_node_index = g.add_node(neighbor.map.edge_count());
                        g.add_edge(current_node_index, nb_node_index, 1.0f32);

                        //let cond1 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::ExtSplit => true, _ => false}).count() == 1;
                        //let cond2 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::Split => true, _ => false}).count() == 0;

                        //if cond1 && cond2 {
                        debug_wood(&neighbor, nb_node_index);
                        //}

                        known.insert(nb_id.0.clone(), nb_node_index);
                        known.insert(nb_id.1.clone(), nb_node_index);
                        known.insert(nb_id.2.clone(), nb_node_index);
                        known.insert(nb_id.3.clone(), nb_node_index);
                        known.insert(nb_id.4.clone(), nb_node_index);
                        known.insert(nb_id.5.clone(), nb_node_index);
                        stack.lock().unwrap().push(neighbor.clone());
                    }

                    if let Some(nb_node_index) = known.get(&nb_id.1) {
                        if !g.contains_edge(current_node_index, *nb_node_index) {
                            g.add_edge(current_node_index, *nb_node_index, 1.0);
                        }
                    } else {
                        let nb_node_index = g.add_node(neighbor.map.edge_count());
                        g.add_edge(current_node_index, nb_node_index, 1.0f32);

                        //let cond1 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::ExtSplit => true, _ => false}).count() == 1;
                        //let cond2 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::Split => true, _ => false}).count() == 0;

                        //if cond1 && cond2 {
                        debug_wood(&neighbor, nb_node_index);
                        //}

                        known.insert(nb_id.0.clone(), nb_node_index);
                        known.insert(nb_id.1.clone(), nb_node_index);
                        known.insert(nb_id.2.clone(), nb_node_index);
                        known.insert(nb_id.3.clone(), nb_node_index);
                        known.insert(nb_id.4.clone(), nb_node_index);
                        known.insert(nb_id.5.clone(), nb_node_index);
                        stack.lock().unwrap().push(neighbor.clone());
                    }

                    if let Some(nb_node_index) = known.get(&nb_id.2) {
                        if !g.contains_edge(current_node_index, *nb_node_index) {
                            g.add_edge(current_node_index, *nb_node_index, 1.0);
                        }
                    } else {
                        let nb_node_index = g.add_node(neighbor.map.edge_count());
                        g.add_edge(current_node_index, nb_node_index, 1.0f32);

                        //let cond1 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::ExtSplit => true, _ => false}).count() == 1;
                        //let cond2 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::Split => true, _ => false}).count() == 0;

                        //if cond1 && cond2 {
                        debug_wood(&neighbor, nb_node_index);
                        //}

                        known.insert(nb_id.0.clone(), nb_node_index);
                        known.insert(nb_id.1.clone(), nb_node_index);
                        known.insert(nb_id.2.clone(), nb_node_index);
                        known.insert(nb_id.3.clone(), nb_node_index);
                        known.insert(nb_id.4.clone(), nb_node_index);
                        known.insert(nb_id.5.clone(), nb_node_index);
                        stack.lock().unwrap().push(neighbor.clone());
                    }

                    if let Some(nb_node_index) = known.get(&nb_id.3) {
                        if !g.contains_edge(current_node_index, *nb_node_index) {
                            g.add_edge(current_node_index, *nb_node_index, 1.0);
                        }
                    } else {
                        let nb_node_index = g.add_node(neighbor.map.edge_count());
                        g.add_edge(current_node_index, nb_node_index, 1.0f32);

                        //let cond1 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::ExtSplit => true, _ => false}).count() == 1;
                        //let cond2 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::Split => true, _ => false}).count() == 0;

                        //if cond1 && cond2 {
                        debug_wood(&neighbor, nb_node_index);
                        //}

                        known.insert(nb_id.0.clone(), nb_node_index);
                        known.insert(nb_id.1.clone(), nb_node_index);
                        known.insert(nb_id.2.clone(), nb_node_index);
                        known.insert(nb_id.3.clone(), nb_node_index);
                        known.insert(nb_id.4.clone(), nb_node_index);
                        known.insert(nb_id.5.clone(), nb_node_index);
                        stack.lock().unwrap().push(neighbor.clone());
                    }

                    if let Some(nb_node_index) = known.get(&nb_id.4) {
                        if !g.contains_edge(current_node_index, *nb_node_index) {
                            g.add_edge(current_node_index, *nb_node_index, 1.0);
                        }
                    } else {
                        let nb_node_index = g.add_node(neighbor.map.edge_count());
                        g.add_edge(current_node_index, nb_node_index, 1.0f32);

                        //let cond1 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::ExtSplit => true, _ => false}).count() == 1;
                        //let cond2 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::Split => true, _ => false}).count() == 0;

                        //if cond1 && cond2 {
                        debug_wood(&neighbor, nb_node_index);
                        //}

                        known.insert(nb_id.0.clone(), nb_node_index);
                        known.insert(nb_id.1.clone(), nb_node_index);
                        known.insert(nb_id.2.clone(), nb_node_index);
                        known.insert(nb_id.3.clone(), nb_node_index);
                        known.insert(nb_id.4.clone(), nb_node_index);
                        known.insert(nb_id.5.clone(), nb_node_index);
                        stack.lock().unwrap().push(neighbor.clone());
                    }

                    if let Some(nb_node_index) = known.get(&nb_id.5) {
                        if !g.contains_edge(current_node_index, *nb_node_index) {
                            g.add_edge(current_node_index, *nb_node_index, 1.0);
                        }
                    } else {
                        let nb_node_index = g.add_node(neighbor.map.edge_count());
                        g.add_edge(current_node_index, nb_node_index, 1.0f32);

                        //let cond1 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::ExtSplit => true, _ => false}).count() == 1;
                        //let cond2 = neighbor.get_admissible_ops().unwrap().iter().filter(|op| match op.operation_type { OpType::Split => true, _ => false}).count() == 0;

                        //if cond1 && cond2 {
                        debug_wood(&neighbor, nb_node_index);
                        //}

                        known.insert(nb_id.0.clone(), nb_node_index);
                        known.insert(nb_id.1.clone(), nb_node_index);
                        known.insert(nb_id.2.clone(), nb_node_index);
                        known.insert(nb_id.3.clone(), nb_node_index);
                        known.insert(nb_id.4.clone(), nb_node_index);
                        known.insert(nb_id.5.clone(), nb_node_index);
                        stack.lock().unwrap().push(neighbor.clone());
                    }

                }

                //inspected.lock().unwrap().insert(current_node_index);

                if last_print.elapsed().as_secs() > 5 {
                    /*let number_of_keys = known.lock().unwrap().keys().len();
                    let size_of_key = known.lock().unwrap().keys().next().unwrap().len();
                    let memory = (number_of_keys * size_of_key) as f64 / (1024f64 * 1024f64);
                    println!("{} Inspected nodes = {}, Memory = {:.2} MiB, stack size = {}", i, inspected.lock().unwrap().len(), memory, stack.lock().unwrap().len());*/

                    println!("{}: inspected {}", i, nr_inspected);
                    last_print = Instant::now();
                }
            }
        });
        handles.push(handle);
        //sleep(Duration::new(0, 300000));
    }

    for handle in handles {
        handle.join().unwrap();
    }

    let g = g.lock().unwrap();

    let levels = g.node_indices().map(|idx| (g.node_weight(idx).unwrap(), idx)).into_group_map();

    /*print_header();
    print_statistics("ALL", g.node_indices().collect(), &*g);
    for (level, indices) in levels.into_iter().sorted_by_key(|(level, _)| **level) {
        print_statistics(&format!("Level {}", level), indices, &*g);
    }*/

    eprintln!("NODES = {:?}", g.node_count());
    eprintln!("EDGES = {:?}", g.edge_count());

    /*let sparse6 = to_sparse6(&*g);
    {
        let mut file = File::create("/tmp/test.g6").expect("TODO");
        file.write_all(&sparse6).expect("TODO");
    }*/
    let edge_list = to_edge_list(&g);
    {
        let mut file = File::create("/tmp/test.els").expect("TODO");
        file.write_all(&edge_list).expect("TODO");
    }
    let level_list = to_level_list(&g);
    {
        let mut file = File::create("/tmp/test.lls").expect("TODO");
        file.write_all(&level_list).expect("TODO");
    }

    let json = serde_json::to_string(&*g).unwrap();
    {
        let mut file = File::create("/tmp/test.json").expect("TODO");
        file.write_all(json.as_bytes());
    }

    {
        let mut file = File::create("/tmp/test.cbor").expect("TODO");
        serde_cbor::to_writer(file, &*g).expect("TODO");
    }
}

fn debug_wood(wood: &SchnyderMap, index: NodeIndex) {

    if (DEBUG.read().unwrap().is_active()) {
        let minimum = !wood.get_admissible_ops().unwrap().iter().any(|op| match op.operation_type {
            OpType::Merge | OpType::ExtMerge => true,
            _ => false
        });
        DEBUG.write().unwrap().output("nodes", &wood, Some(&format!("{} (level {}, minimum = {})", index.index(), wood.map.edge_count(), minimum)), &wood.calculate_face_counts());
    }
}

fn print_statistics(name: &str, nodes: Vec<usize>, g: &Flipgraph) {

    let degrees = nodes.iter().map(|idx| g.get_neighbors(*idx).count()).collect_vec();
    let down_degrees = nodes.iter().map(|idx| g.get_neighbors(*idx).filter(|&&nb| g.get_level(nb) < g.get_level(*idx)).count()).collect_vec();
    let up_degrees = nodes.iter().map(|idx| g.get_neighbors(*idx).filter(|&&nb| g.get_level(nb) > g.get_level(*idx)).count()).collect_vec();

    let min_degree = degrees.iter().min().unwrap();
    let max_degree = degrees.iter().max().unwrap();

    let minimums = nodes.iter().filter(|idx| g.get_neighbors(**idx).filter(|&&nb| g.get_level(nb) < g.get_level(**idx)).count() == 0).collect_vec();

    let min_up_degree = up_degrees.iter().min().unwrap();
    let max_up_degree = up_degrees.iter().max().unwrap();
    let min_down_degree = down_degrees.iter().min().unwrap();
    let max_down_degree = down_degrees.iter().max().unwrap();
    let avg_degree = degrees.iter().sum::<usize>() as f64 / degrees.len() as f64;

    println!("{:<10} {:>10} {:>10} {:>10.2} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}", name, nodes.len(), min_degree, avg_degree, max_degree, min_up_degree, max_up_degree, min_down_degree, max_down_degree, minimums.len());
}

fn print_header() {
    println!("{:<10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10} {:>10}", "Subset", "Card", "MinDeg", "AvgDeg", "MaxDeg", "Min🠕Deg", "Max🠕Deg", "Min🠗Deg", "Max🠗Deg", "Minima");
}

fn main5() {
    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(78), |i| i.0, |i| i.0, |i| ());
    let map = &maps[0];
    let mut wood = SchnyderMap::build_on_triangulation(map, map.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();

    let n = wood.map.vertex_count();

    // now explore and log
    let mut g = Graph::new_undirected();

    let root_node_index = g.add_node(0);
    //DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std", &wood, Some(&format!("{}", root_node_index.index())), &wood.calculate_face_counts());

    let mut known = HashMap::new();
    let mut inspected = HashSet::new();
    known.insert(wood.compute_3tree_code(), root_node_index);

    let mut stack = vec![wood];
    let mut last_print = Instant::now();

    while let Some(current) = stack.pop() {

        let current_node_index = *known.get(&current.compute_3tree_code()).expect("TODO");

        let admissible_ops = current.get_admissible_ops().expect("TODO");

        /*let min = admissible_ops.iter().filter(|op| match op.operation_type {
            OpType::Merge | OpType::ExtMerge => true,
            _ => false
        }).count() == 0;*/

        /*if min {
            println!("min with {} edges", current.map.edge_count());
            DEBUG.write().unwrap().output("std", &current, Some(&format!("{} (edges = {})", current_node_index.index(), current.map.edge_count())), &current.calculate_face_counts());
        }*/

        for op in admissible_ops {

            let mut neighbor = current.clone();
            neighbor.do_operation(&op);
            let nb_id = neighbor.compute_3tree_code();

            if let Some(nb_node_index) = known.get(&nb_id) {
                if !inspected.contains(nb_node_index) {
                    g.add_edge(current_node_index, *nb_node_index, 1.0);
                }
            } else {
                let nb_node_index = g.add_node(neighbor.map.edge_count());
                DEBUG.write().unwrap().output("std", &neighbor, Some(&format!("{} (edges = {})", nb_node_index.index(), neighbor.map.edge_count())), &neighbor.calculate_face_counts());

                g.add_edge(current_node_index, nb_node_index, 1.0f32);
                known.insert(nb_id, nb_node_index);

                stack.push(neighbor);
            }
        }

        inspected.insert(current_node_index);

        if last_print.elapsed().as_secs() > 5 {
            let number_of_keys = known.keys().len();
            let size_of_key = known.keys().next().unwrap().len();
            let memory = (number_of_keys * size_of_key) as f64 / (1024f64 * 1024f64);
            println!("Inspected nodes = {}, Memory = {:.2} MiB, stack size = {}", inspected.len(), memory, stack.len());
            last_print = Instant::now();
        }

    }

    let degrees = g.node_indices().map(|idx| g.neighbors(idx).count()).collect_vec();
    let min_degree = degrees.iter().min().unwrap();
    let max_degree = degrees.iter().max().unwrap();
    let avg_degree = degrees.iter().sum::<usize>() as f64 / degrees.len() as f64;

    eprintln!("g.node_count() = {:?}", g.node_count());
    eprintln!("g.edge_count() = {:?}", g.edge_count());

    eprintln!("lower bound = {:?}", (3*n - 9) as f64 /2.0);
    eprintln!("upper bound = {:?}", n*n - 4*n + 4);

    eprintln!("min_degree = {:?}", min_degree);
    eprintln!("max_degree = {:?}", max_degree);
    eprintln!("avg_degree = {:?}", avg_degree);

    let sparse6 = to_sparse6(&g);
    {
        let mut file = File::create("/tmp/test.g6").expect("TODO");
        file.write_all(&sparse6).expect("TODO");
    }

}

fn main4() {
    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(1001), |i| i.0, |i| i.0, |_| ());
    let map = &maps[0];


    let mut wood = SchnyderMap::build_on_triangulation(map, map.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();
    //let fc = &wood.calculate_face_counts();
    //wood.ext_merge(VertexI(3), VertexI(0)).expect("?");

    DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std", &wood, Some("Wood"), &wood.calculate_face_counts());

    eprintln!("wood.compute_identification_vector() = {:?}", wood.compute_3tree_code());

    loop {
        let admissible_ops = wood.get_admissible_ops().expect("?");

        //for op in admissible_ops {
        //    println!("{:?}", op);
        //}

        if let Some(op) = admissible_ops.iter().find(|op| match op.operation_type {
            OpType::Merge | OpType::ExtMerge => true,
            _ => false
        }) {
            let (before,_) = wood.map.into_petgraph();

            eprintln!(">>.compute_identification_vector() = {:?}", wood.compute_3tree_code());
            wood.do_operation(op);
            eprintln!(">>.compute_identification_vector() = {:?}", wood.compute_3tree_code());
            DEBUG.write().unwrap().output("std", &wood, Some("Wood"), &wood.calculate_face_counts());

            let (afterwards, _) = wood.map.into_petgraph();

            eprintln!("is_isomorphic(&before, &afterwards); = {:?}", is_isomorphic(&before, &afterwards));
        } else {
            break;
        }
    }




}

fn main3() {
    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(1001), |i| i.0, |i| i.0, |_| ());

    let map1 = &maps[256];
    let map2 = &maps[42];
    let mut wood1 = SchnyderMap::build_on_triangulation(map1, map1.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();
    let mut wood2 = SchnyderMap::build_on_triangulation(map2, map2.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();

    DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std", &wood1, Some("Wood1"), &wood1.calculate_face_counts());
    DEBUG.write().unwrap().output("std", &wood2, Some("Wood2"), &wood2.calculate_face_counts());

    let ops = find_sequence_2(&mut wood1, &mut wood2, Blue);

    println!("{} ops", ops.len());

    let mut wood = SchnyderMap::build_on_triangulation(map1, map1.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();
    DEBUG.write().unwrap().output("ops", &wood, Some("Start"), &wood.calculate_face_counts());
    for op in ops {
        eprintln!("op = {:?}", op);
        wood.do_operation(&op).expect("TODO");
        DEBUG.write().unwrap().output("ops", &wood, Some("Step"), &wood.calculate_face_counts());
    }

    /*wood.ext_merge(VertexI(1), VertexI(7));
    wood.ext_merge(VertexI(0), VertexI(1));
    wood.ext_merge(VertexI(7), VertexI(0));
    wood.ext_merge(VertexI(2), VertexI(1));
    wood.ext_merge(VertexI(6), VertexI(0)).expect("hä?");

    DEBUG.write().unwrap().output("std", &wood, Some("Merged"), &wood.calculate_face_counts());

    wood.ext_split(VertexI(1), VertexI(7));

    DEBUG.write().unwrap().output("std", &wood, Some("Resplit"), &wood.calculate_face_counts());*/
}

fn main2() {
    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(1001), |i| i.0, |i| i.0, |_| ());

    let map1 = &maps[4];
    let map2 = &maps[8];

    let mut wood1 = SchnyderMap::build_on_triangulation(map1, map1.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();
    let mut wood2 = SchnyderMap::build_on_triangulation(map2, map2.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();

    //DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std",&wood1, Some("Wood1 (-1)"), &wood1.calculate_face_counts());
    DEBUG.write().unwrap().output("std",&wood2, Some("Wood2 (-1)"), &wood2.calculate_face_counts());
    DEBUG.write().unwrap().deactivate();

    let seq = find_sequence(&mut wood1, &mut wood2);

    DEBUG.write().unwrap().output("std",&wood1, Some("Final1 (-1)"), &wood1.calculate_face_counts());
    DEBUG.write().unwrap().output("std",&wood2, Some("Final2 (-1)"), &wood2.calculate_face_counts());

    /*let mut wood1 = SchnyderMap::build_on_triangulation(map1, map1.get_left_face(VertexI(0), VertexI(1)), LeftMost).unwrap();

    DEBUG.write().unwrap().output(&wood1, Some("Wood1 POST"), &wood1.calculate_face_counts());
    for op in seq {
        wood1.do_operation(&op);
        DEBUG.write().unwrap().output(&wood1, Some("Wood1 POST"), &wood1.calculate_face_counts());
    }*/

    let mut i = 0;
    DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std",&wood1, Some(&format!("Step {}",i)), &wood1.calculate_face_counts());
    println!("{} operations", seq.len());
    for op in &seq {
        i += 1;
        wood1.do_operation(op).expect(&format!("first level operation {:?} execution failed!", op));
        DEBUG.write().unwrap().output("std",&wood1, Some(&format!("Step {}",i)), &wood1.calculate_face_counts());
    }

}

fn main1() {

    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(1001), |i| i.0, |i| i.0, |_| ());

    //let mut i = 0;
    //for m in maps {

        let mut map = &maps[123];

        //println!("{:?}", map);

        let (nr_a, nr_b) = (8, 9);
        let a = VertexI(nr_a);
        let b = VertexI(nr_b);

        let mut wood = SchnyderMap::build_on_triangulation(map, map.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();
        DEBUG.write().unwrap().output("std",&wood, Some("The Wood"), &wood.calculate_face_counts());

        let edge = wood.map.get_edge(a, b).unwrap();
        let seq = make_contractible(&mut wood, edge);

        println!("pre-contracted refint = {}", wood.map.check_referential_integrity());
        DEBUG.write().unwrap().output("std",&wood, Some("Uncontracted"), &wood.calculate_face_counts());
        let fc = wood.calculate_face_counts();

        let contraction = wood.schnyder_contract(wood.map.get_edge(a, b).unwrap()).unwrap();
        println!("contraction = {:#?}", contraction);

        println!("contracted refint = {}", wood.map.check_referential_integrity());
        DEBUG.write().unwrap().output("std", &wood, Some("Contracted"), &fc);
        //DEBUG.write().unwrap().output(&wood, Some("Contracted w/ Updated Vertex Positions"), &wood.calculate_face_counts());

        //let edge_to_discontract = wood.find_outgoing_edge(contraction.retained_vertex, contraction.color).unwrap(); // should fail sometimes!!!
        wood.revert_schnyder_contraction(&contraction);

        DEBUG.write().unwrap().output("std", &wood, Some("Discontracted"), &fc);
        println!("discontracted refint = {}", wood.map.check_referential_integrity());

        println!("{:?}", seq);

        //debug_output(wood, &format!("foo{}", i), Some(&format!("Number {}", i)));

        /*let mut map = PlanarMap::<SchnyderVertexType, SchnyderEdgeDirection, _>::new();

        let r = map.add_vertex(Suspension(Red));
        let g = map.add_vertex(Suspension(Green));
        let b = map.add_vertex(Suspension(Blue));
        let c1 = map.add_vertex(Normal(0));
        let c2 = map.add_vertex(Normal(1));

        map.add_edge(r, g, Bicolored(Green, Red));
        map.add_edge(r, b, Bicolored(Blue, Red));
        map.add_edge(b, g, Bicolored(Green, Blue));

        map.add_edge(c1, r, Unicolored(Red, Forward));
        map.add_edge(c1, b, Unicolored(Blue, Forward));
        let trg = map.add_edge(c1, c2, Unicolored(Green, Forward));

        map.add_edge(c2, r, Unicolored(Red, Forward));
        map.add_edge(c2, g, Unicolored(Green, Forward));
        let src = map.add_edge(c2, b, Unicolored(Blue, Forward));

        println!("is_simple() = {}", map.is_simple());
        println!("is_connected() = {}", map.is_connected());

        map.set_embedding(vec![
            (vec![r,g,b], 1),
            (vec![r,b,c1], 2),
            (vec![b,g,c2], 3),
            (vec![r,c1,c2], 4),
            (vec![c1,b,c2], 5),
            (vec![r,c2,g], 5)
        ]);*/
        //map.contract_embedded_edge(trg, &(|e1, e2| *e1));

        /*let (dual, ..) = wood.map.get_dual(true);

        let mut schnyder_map = SchnyderMap::from(map);
        schnyder_map.debug();
        schnyder_map.merge(src, trg);
        //schnyder_map.split(trg, CW, g);

        println!("-----");
        println!("{:?}", dual);*/

      //  i += 1;
    //}


    // println!("ref_integrity = {}, check_wood = {}, edge_count = {}", map.check_referential_integrity(), map.check_wood(), map.edge_count(), );
    // println!("{:?}", map.calculate_face_counts(c1));
    // println!("{:?}", map.calculate_face_counts(c2));
    //
    // map.calculate_face_counts(c2);
    // //map.debug();
    //
    // //map.contract_embedded_edge(trg, &(|e1, e2| *e1));
    //
    // println!("ref_integrity = {}, check_wood = {}, edge_count = {}", map.check_referential_integrity(), map.check_wood(), map.edge_count(), );
    //
    //
    // //map.merge(src, trg);
    // //println!("-------");
    // //map.debug();
    // //println!("{}, {}", map.check_wood(), map.edge_count());
    //
    // //map.split(trg, CCW, b, Black));
    // //println!("-------");
    // //map.debug();
    // //println!("{}, {}", map.check_wood(), map.edge_count());
    //
    // let dual = map.get_dual();
    // println!("ref_integrity = {}, check_wood = {}, edge_count = {}", dual.check_referential_integrity(), false, dual.edge_count(), );
    //
    // let v:Vec<usize> = vec![1,2,3];
    //
    // let v2:Vec<(_,_)> = v.cycle(0, true).tuple_windows().collect_vec();
    //
    // println!("{:?}", v2);



}
