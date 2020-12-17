use crate::subcommands::{build, explore, path, tikz};
use crate::subcommands::{random_walk, replay};
use clap::{App, ArgMatches};
use std::fs::File;

use crate::schnyder::enums::SchnyderColor;
use crate::schnyder::SchnyderMap;
#[cfg(debug_assertions)]
use crate::util::debug::Debug;
#[cfg(debug_assertions)]
use std::sync::RwLock;

#[cfg(debug_assertions)]
#[macro_use]
extern crate lazy_static;

pub mod flipgraph;
pub mod graph;
pub mod schnyder;

mod algorithm;
mod util;
//mod petgraph_ext;
mod arraytree;
mod repl;
mod subcommands;

#[cfg(debug_assertions)]
lazy_static! {
    static ref DEBUG: RwLock<Debug> = RwLock::new(Debug::new("/tmp/schnyder"));
}

fn main() {
    let matches = App::new("schnyderflip")
        .version("1.0.1")
        .author("Julian Schick <julian.schick@posteo.de>")
        .about("Algorithms for manipulating Schnyder woods with split and merge.")
        .subcommand(
            App::new("build")
                .about("Builds a flipgraph and stores it to a file.")
                .arg("<N> 'Number of vertices (at least 3 and at most 64).'")
                .arg("<OUTPUT> 'Output file'")
                .arg("-t --threads [t] 'Number of threads use for the traversal.'")
                .arg("-c --break-color-symmetry 'Interpret Schnyder woods that differ only in color rotation as the same node of the flip graph.'")
                .arg("-o --break-orientation-symmetry 'Interpret Schnyder woods that differ only in orientation as the same node of the flip graph.'")
                .arg("--min-level [LEVEL] 'Do not include levels below the given level.'")
                .arg("--max-level [LEVEL] 'Do not include levels above the given level.'")
        )
        .subcommand(
            App::new("explore")
                .about("Loads a flipgraph into memory for exploration.")
                .arg("<GRAPH> 'Flipgraph file'")
        ).subcommand(
            App::new("tikz")
                .about("Reads a Schnyder wood from an ascii or binary 3treecode file and generates LaTeX/TikZ directives.")
                .arg("<FILE> '3treecode file to be read.'")
                .arg("-o, --output [FILE] 'Output file to be written, otherwise output goes to STDOUT.'")
                .arg("-a, --anchor [ANCHOR] 'Tikz node the drawing is to be drawn relative to.'")
                .arg("-c, --central 'With this flag the anchor is placed centrally in the wood and not at the wood's origin.'")
                .arg("-e, --env 'Print tikzpicture environment.'")
                .arg("-d, --doc 'Wrap picture environment in standalone document (only effective, if -e/--env is specified).'")
                .arg("-s, --styles 'Print style definitions.'")
                .arg("-i, --equilateral 'Shear the drawing in x-direction so that the outer triangle is equilateral.'")
                .arg("-f, --stats 'Include number of edges, degree, down-degree and up-degree as text.'")
        ).subcommand(
            App::new("path")
                .about("Generates a split and merge sequence from one Schnyder wood to the other.")
                .arg("<FROM> '3treecode file to be read as starting Schnyder wood.'")
                .arg("<TO> '3treecode file to be read as ending Schnyder wood.'")
                .arg("<ALGO> 'Algorithm to use, can be \'simplestack\' or \'contraction\'.'")
                .arg("-o, --output [FILE] 'Output file containing the operation sequence to be written, otherwise output goes to STDOUT.'")
                .arg("-s, --steps [DIR] 'Output directory for all intermediate Schnyder woods.'")
                .arg("-c, --color [COLOR] 'In case of the simple stack algorithm being used, choose the color of the simple stack. Can be red, green or blue. Default is red.'")
        )
        .subcommand(
            App::new("replay")
                .about("Replays a split and merge sequence from a sequence file.")
                .arg("<FROM> '3treecode file to be read as starting Schnyder wood.'")
                .arg("<SEQ> 'File containing the sequence.'")
                .arg("-s, --steps [DIR] 'Output directory for all intermediate Schnyder woods.'")
        )
        .subcommand(
            App::new("random-walk")
                .arg("<N> 'Number of vertices (at least 3 and at most 200).'")
        )
        .subcommand(
            App::new("test")
        )
        .get_matches();

    match matches.subcommand() {
        Some(("build", matches)) => build(matches),
        Some(("explore", matches)) => explore(matches),
        Some(("tikz", matches)) => tikz(matches),
        Some(("random-walk", matches)) => random_walk(matches),
        Some(("path", matches)) => path(matches),
        Some(("replay", matches)) => replay(matches),
        Some(("test", matches)) => test(matches),
        _ => {
            println!("No valid command specified. Type 'schnyderflip help' for a list of valid commands.");
        }
    }
}

fn test(_matches: &ArgMatches) {
    let wood = SchnyderMap::build_min_edge_wood(42, SchnyderColor::Red).unwrap();
    //wood.write_tikz(&mut stdout(), &TikzOptions::default());
    let mut f = File::create("/tmp/test.out").unwrap();
    wood.write_ascii_3treecode(&mut f).expect("TODO");
    drop(f);
    let wood_r = SchnyderMap::read_3treecode(&mut File::open("/tmp/test.out").unwrap()).unwrap();
    eprintln!("eq = {:?}", wood == wood_r);

    /*let flipgraph_file = matches.value_of("GRAPH").unwrap();

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
    }*/
}
