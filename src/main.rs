use std::fs::File;
use std::sync::RwLock;

use clap::{App, ArgMatches};

use crate::schnyder::SchnyderColor::{Red};
use crate::schnyder::{SchnyderMap};
use crate::util::debug::Debug;
use crate::algorithm::{find_sequence_2, find_sequence};
use crate::subcommands::{build, explore, convert_to_tikz};
use crate::flipgraph::Flipgraph;

#[macro_use]
extern crate lazy_static;

pub mod flipgraph;
pub mod graph;
pub mod schnyder;

mod util;
mod algorithm;
//mod petgraph_ext;
mod arraytree;
mod repl;
mod subcommands;

lazy_static! {
    static ref DEBUG: RwLock<Debug> = RwLock::new(Debug::new("/tmp/schnyder"));
}

fn main() {
    let matches = App::new("schnyderflip")
        .version("1.0.0")
        .author("Julian Schick <julian.schick@posteo.de>")
        .about("Algorithms for manipulating Schnyder woods")
        .subcommand(
            App::new("build")
                .arg("<N> 'Number of vertices (at least 3 and at most 64)'")
                .arg("<OUTPUT> 'Output file'")
                .arg("-t --threads [t] 'Number of threads to start'")
                .arg("-c --break-color-symmetry 'Interpret Schnyder woods that differ only in color rotation as the same node of the flip graph'")
                .arg("-o --break-orientation-symmetry 'Interpret Schnyder woods that differ only in orientation as the same node of the flip graph'")
        )
        .subcommand(
            App::new("explore")
                .arg("<GRAPH> 'Flipgraph file'")
        ).subcommand(
            App::new("tikz")
                .arg("<3CODE> 'ASCII 3 tree code file'")
                .arg("-o, --output [OUTPUT] 'Output file to be written, otherwise output goes to STDOUT'")
                .arg("-a, --anchor [ANCHOR] 'Tikz node the drawing is to be drawn relative to'")
                .arg("-e, --env 'Print tizpicture environment'")
                .arg("-d, --doc 'Print standalone document (only effective, if -e/--env is specified)'")
                .arg("-s, --styles 'Print style definitions'")
                .arg("-i, --slanted 'Print Schnyder wood slanted, such that the top suspension node is centered'")
        )
        .subcommand(
            App::new("test")
                .arg("<GRAPH> 'Flipgraph file'")
        )
        .get_matches();

    match matches.subcommand() {
        Some(("build", matches))=> {
            build(matches);
        },
        Some(("explore", matches)) => {
            explore(matches);
        },
        Some(("tikz", matches)) => {
            convert_to_tikz(matches);
        }
        Some(("test", matches)) => {
            test(matches);
        },
        _ => {
            println!("No valid command specified. Type 'schnyderflip help' for a list of valid commands.");
        }
    }
}

// Simple stack from 3code
/*fn test() {
    let n = 5u8;
    let mut code = Vec::with_capacity(n as usize * 3);
    code.extend(&[0, 0, 0]);
    code.extend((3..5).map(|x| match x { 3 => 0, 4 => 2, _ => 0}));
    code.extend((5..n).map(|x|x-1));
    code.extend((0..n).map(|_| 1));
    code.extend((0..n).map(|_| if n == 3 { 2 } else { 3 }));

    let code = SchnyderMap::build_simple_stack(5, Red).unwrap().compute_3tree_code();

    let wood = SchnyderMap::build_from_3tree_code(&code).unwrap();
    DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std", &wood, Some("Wood"), &wood.calculate_face_counts());
}*/

fn test(matches: &ArgMatches) {
    let flipgraph_file = matches.value_of("GRAPH").unwrap();

    println!("Reading CBOR...");
    {
        let file = File::open(flipgraph_file).expect("TODO");
        let g: Flipgraph = serde_cbor::from_reader(file).expect("TODO");

        let mut wood1 = SchnyderMap::build_from_3tree_code(g.get_code(42)).expect("TODO");
        let mut wood2 = SchnyderMap::build_from_3tree_code(g.get_code(142)).expect("TODO");

        let mut wood = wood1.clone();

        DEBUG.write().unwrap().activate();
        DEBUG.write().unwrap().output("std", &wood1, Some("From"), &wood1.calculate_face_counts());
        DEBUG.write().unwrap().output("std", &wood2, Some("To"), &wood2.calculate_face_counts());

        let seq1 = find_sequence(&mut wood1, &mut wood2).unwrap();
        let _seq1 = find_sequence_2(&mut wood1, &mut wood2, Red);

        DEBUG.write().unwrap().output("ops", &wood, Some("Intermediate"), &wood.calculate_face_counts());
        for op in &seq1 {
            wood.do_operation(op).unwrap();//TODO
            DEBUG.write().unwrap().output("ops", &wood, Some("Intermediate"), &wood.calculate_face_counts());
        }
    }
}
