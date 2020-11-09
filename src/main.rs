use std::fs::File;
use std::sync::RwLock;
use std::path::Path;

use itertools::Itertools;
use clap::App;

use crate::schnyder::SchnyderColor::{Red};
use crate::schnyder::{SchnyderMap};
use crate::util::debug::Debug;
use crate::flipgraph::{build_flipgraph, SymmetryBreaking, Flipgraph};
use crate::algorithm::{find_sequence_2, find_sequence};

#[macro_use]
extern crate lazy_static;

pub mod flipgraph;
pub mod graph;
pub mod schnyder;

mod util;
mod algorithm;
//mod petgraph_ext;
mod arraytree;

lazy_static! {
    static ref DEBUG: RwLock<Debug> = RwLock::new(Debug::new("/tmp/schnyder"));
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
        .subcommand(
    App::new("explore")
                .arg("<GRAPH> 'Flipgraph file'")
        )
        .subcommand(
            App::new("test")
        )
        .get_matches();

    match matches.subcommand() {
        Some(("build-flipgraph", matches))=> {
            let n = str::parse::<usize>(matches.value_of("N").unwrap_or("3"))
                .unwrap_or_else(|_| { println!("Bittaschön"); return 3 });
            let num_threads= str::parse::<usize>(matches.value_of("threads").unwrap_or("1"))
                .unwrap_or_else(|_| { println!("Bittaschön!"); return 1});

            let brk_orientation = matches.is_present("break-orientation-symmetry");
            let brk_color = matches.is_present("break-color-symmetry");

            let output = Path::new(matches.value_of("OUTPUT").unwrap());

            if output.is_file() {
                println!("already present");
                return;
            }
            if output.is_dir() {
                println!("directory given");
                return;
            }
            if let Some(parent) = output.parent() {
                if !parent.is_dir() {
                    println!("can't write there");
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
                println!("n must be at least 3");
                return;
            }

            main7(n, num_threads, symmetry_breaking, output);
        },
        Some(("explore", matches)) => {
            let flipgraph_file = matches.value_of("GRAPH").unwrap();

            println!("Reading CBOR...");
            {
                let file = File::open(flipgraph_file).expect("TODO");
                let g: Flipgraph = serde_cbor::from_reader(file).expect("TODO");

                let mut wood1 = SchnyderMap::build_from_3tree_code(g.get_code(42)).expect("TODO");
                let mut wood2 = SchnyderMap::build_from_3tree_code(g.get_code(142)).expect("TODO");

                let mut wood = wood1.clone();

                DEBUG.write().unwrap().activate();
                DEBUG.write().unwrap().output("std", &wood1, Some("From"), &wood1.calculate_face_counts().unwrap());
                DEBUG.write().unwrap().output("std", &wood2, Some("To"), &wood2.calculate_face_counts().unwrap());

                let seq1 = find_sequence(&mut wood1, &mut wood2).unwrap();
                let _seq2 = find_sequence_2(&mut wood1, &mut wood2, Red);

                DEBUG.write().unwrap().output("ops", &wood, Some("Intermediate"), &wood.calculate_face_counts().unwrap());
                for op in &seq1 {
                    wood.do_operation(op).unwrap();//TODO
                    DEBUG.write().unwrap().output("ops", &wood, Some("Intermediate"), &wood.calculate_face_counts().unwrap());
                }
            }
        }
        Some(("test", _)) => {
            test();
        }
        _ => {
            println!("No valid command specified.");
        }
    }
}

fn test() {
    let n = 5u8;
    let mut code = Vec::with_capacity(n as usize * 3);
    code.extend(&[0, 0, 0]);
    code.extend((3..5).map(|x| match x { 3 => 0, 4 => 2, _ => 0}));
    code.extend((5..n).map(|x|x-1));
    code.extend((0..n).map(|_| 1));
    code.extend((0..n).map(|_| if n == 3 { 2 } else { 3 }));

    let code = SchnyderMap::build_apollonian_path(5, Red).unwrap().compute_3tree_code();

    let wood = SchnyderMap::build_from_3tree_code(&code).unwrap();
    DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std", &wood, Some("Wood"), &wood.calculate_face_counts().unwrap());
}

#[allow(dead_code)]
fn main8() {

    let file = File::open("/tmp/test.cbor").unwrap();
    let g : Flipgraph = serde_cbor::from_reader(file).expect("TODO");

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

    println!("Writing CBOR...");
    {
        let file = File::create(output_file).expect("TODO");
        serde_cbor::to_writer(file, &g).expect("TODO");
    }

    /*println!("{}", "Writing Bincode...");
    {
        let mut file = File::create("/tmp/test.bincode").expect("TODO");
        bincode::serialize_into(file, &g).expect("TODO");
    }*/
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
