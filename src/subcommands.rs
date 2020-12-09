use crate::algorithm::{find_sequence, find_sequence_2};
use crate::flipgraph::io::{write_flipgraph, FlipgraphOutputFormat};
use crate::flipgraph::{build_flipgraph, SymmetryBreaking};
use crate::repl::Repl;
use crate::schnyder::algorithm::Operation;
use crate::schnyder::enums::SchnyderColor;
use crate::schnyder::tikz::TikzOptions;
use crate::schnyder::SchnyderMap;
use clap::ArgMatches;
use itertools::Itertools;
use std::fs::{read_to_string, File};
use std::io::{stdout, Write};
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;

pub fn convert_to_tikz(matches: &ArgMatches) {
    let input_filename = matches.value_of("FILE").unwrap();

    if let Ok(mut input_file) = File::open(input_filename) {
        match SchnyderMap::read_3treecode(&mut input_file) {
            Ok(wood) => {
                let mut opts = TikzOptions::default();
                opts.anchor = matches.value_of("anchor");
                opts.print_document = matches.is_present("doc");
                opts.print_environment = matches.is_present("env");
                opts.print_styles = matches.is_present("styles");
                opts.slanted = matches.is_present("slanted");

                let ops = wood.get_admissible_ops().expect("TODO");
                let title = &format!(
                    "|E| = {}, deg = {}, updeg = {}, downdeg = {}",
                    wood.map.edge_count(),
                    ops.len(),
                    ops.iter().filter(|op| op.is_upwards()).count(),
                    ops.iter().filter(|op| op.is_downwards()).count()
                );
                opts.title = if matches.is_present("stats") {
                    Some(title)
                } else {
                    None
                };

                if let Some(output_filename) = matches.value_of("output") {
                    if let Ok(mut output_file) = File::create(output_filename) {
                        if let Err(e) = wood.write_tikz(&mut output_file, &opts) {
                            println!("Output file could not be written: {}", e);
                        }
                    } else {
                        println!(
                            "Output file '{}' could not be created or opened for writing.",
                            output_filename
                        );
                    }
                } else {
                    if let Err(e) = wood.write_tikz(&mut std::io::stdout(), &opts) {
                        println!("Output could not be written to STDOUT: {}", e);
                    }
                }
            }
            Err(e) => println!("{}", e),
        }
    } else {
        println!(
            "Input file '{}' could not be opened for reading.",
            input_filename
        );
    }
}

pub fn build(matches: &ArgMatches) {
    let n = match retrieve_n(matches, 64) {
        Some(n) => n,
        None => return,
    };

    let min_level = match matches
        .value_of("min-level")
        .map(|m| str::parse::<usize>(m))
    {
        Some(Ok(k)) => Some(k),
        None => None,
        _ => {
            println!("Level should be a positive number.");
            return;
        }
    };

    let max_level = match matches
        .value_of("max-level")
        .map(|m| str::parse::<usize>(m))
    {
        Some(Ok(k)) => Some(k),
        None => None,
        _ => {
            println!("Level should be a positive number.");
            return;
        }
    };

    if let (Some(min), Some(max)) = (min_level, max_level) {
        if min > max {
            println!("Minimal level has to be less than or equal to maximal level.");
            return;
        }
    }

    let num_threads = match str::parse::<usize>(matches.value_of("threads").unwrap_or("1")) {
        Ok(n) if n >= 1 && n <= 64 => n,
        Ok(_) => {
            println!("The number of threads should be at least 1 and at most 64");
            return;
        }
        _ => {
            println!("The number of threads should be a positive number.");
            return;
        }
    };

    let brk_orientation = matches.is_present("break-orientation-symmetry");
    let brk_color = matches.is_present("break-color-symmetry");

    let output_arg = matches.value_of("OUTPUT").unwrap();
    let output_path = Path::new(output_arg);

    if output_path.is_dir() {
        println!("The given output path is a directory.");
        return;
    }
    if let Some(parent) = output_path.parent() {
        if !parent.as_os_str().is_empty() && !parent.is_dir() {
            println!("The given output path is not in an existing and writeable directory.");
            return;
        }
    }

    let symmetry_breaking = match (brk_orientation, brk_color) {
        (false, false) => SymmetryBreaking::None,
        (true, false) => SymmetryBreaking::BreakOrientation,
        (false, true) => SymmetryBreaking::BreakColourRotation,
        (true, true) => SymmetryBreaking::BreakAll,
    };

    if n < 3 {
        println!("n must be at least 3");
        return;
    }

    let g = build_flipgraph(n, min_level, max_level, symmetry_breaking, num_threads);
    write_flipgraph(&g, &mut stdout(), FlipgraphOutputFormat::TabbedTable, false).unwrap();

    println!("\nWriting Flipgraph to file '{}'...", output_arg);
    {
        match File::create(output_path) {
            Ok(file) => match serde_cbor::to_writer(file, &g) {
                Err(e) => println!("Flipgraph could not be written: {}", e),
                _ => println!("...done."),
            },
            Err(e) => println!(
                "File '{}' could not be opened for writing: {}",
                output_arg, e
            ),
        }
    }
}

pub fn explore(matches: &ArgMatches) {
    let flipgraph_file = matches.value_of("GRAPH").unwrap();

    if let Ok(file) = File::open(flipgraph_file) {
        //println!("Reading flipgraph...");
        if let Ok(g) = serde_cbor::from_reader(file) {
            let mut repl = Repl::new(g);
            repl.main_loop();
        } else {
            println!("The specified file does not seem to contain a flipgraph.");
        }
    } else {
        println!("File '{}' could not be opened for reading.", flipgraph_file);
    }
}

enum PathAlgo {
    SimpleStack,
    Contraction,
}

pub fn path(matches: &ArgMatches) {
    let paths = [
        matches.value_of("FROM").unwrap(),
        matches.value_of("TO").unwrap(),
    ];
    let algo = match matches.value_of("ALGO").unwrap().to_lowercase().as_str() {
        "simplestack" => PathAlgo::SimpleStack,
        "contraction" => PathAlgo::Contraction,
        str => {
            println!("Algorithm '{}' not known.", str);
            return;
        }
    };
    let step_path = matches.value_of("steps");
    let file_path = matches.value_of("output");
    let color = matches
        .value_of("color")
        .map(|c| match SchnyderColor::from_str(c) {
            Ok(c) => c,
            Err(_) => SchnyderColor::Red,
        })
        .unwrap_or(SchnyderColor::Red);

    let mut woods = Vec::with_capacity(2);
    for &path in &paths {
        if let Ok(mut file) = File::open(path) {
            if let Ok(wood) = SchnyderMap::read_3treecode(&mut file) {
                woods.push(wood);
            } else {
                println!("File '{}' does not seem to contain a Schnyder wood.", path);
                return;
            }
        } else {
            println!("File '{}' could not be opened for reading.", path);
            return;
        }
    }

    let woods = woods; //in order to make immutable
    let mut wood_from = woods[0].clone();
    let mut wood_to = woods[1].clone();

    let seq = match algo {
        PathAlgo::SimpleStack => find_sequence_2(&mut wood_from, &mut wood_to, color),
        PathAlgo::Contraction => find_sequence(&mut wood_from, &mut wood_to).expect("TODO"),
    };

    let mut cur = woods[0].clone();

    if let Some(step_path) = step_path {
        if !Path::new(step_path).is_dir() {
            println!(
                "The given directory '{}' for the intermediate woods is not a directory.",
                step_path
            );
            return;
        }
    }

    write_out(&woods[0], step_path, "a_from");
    write_out(&woods[1], step_path, "a_to");

    let mut i = 0;
    let mut canonical_seq = Vec::new();
    write_out(&cur, step_path, &format!("step{}", i));

    for op in &seq {
        canonical_seq.push(
            op.canonicalize(&cur.get_canonical_vertex_indices())
                .unwrap_or_else(|e| panic!("Internal error: {}", e)),
        );
        cur.exec_op(op)
            .unwrap_or_else(|e| panic!("Internal error: {}", e));

        i += 1;
        write_out(&cur, step_path, &format!("step{}", i));
    }

    if let Some(file_path) = file_path {
        if let Ok(mut file) = File::create(file_path) {
            for canonical_op in canonical_seq {
                let mut str = canonical_op.to_string();
                str.push('\n');
                file.write_all(str.as_bytes())
                    .unwrap_or_else(|e| println!("Error writing to output file: {}", e));
            }
        }
    } else {
        for canonical_op in canonical_seq {
            println!("{}", canonical_op.to_string());
        }
    }
}

pub fn replay(matches: &ArgMatches) {
    let wood_file_path = matches.value_of("FROM").unwrap();
    let seq_file_path = matches.value_of("SEQ").unwrap();
    let step_path = matches.value_of("steps");

    if let Some(step_path) = step_path {
        if !Path::new(step_path).is_dir() {
            println!(
                "The given directory '{}' for the intermediate woods is not a directory.",
                step_path
            );
            return;
        }
    }

    let mut wood = if let Ok(mut wood_file) = File::open(wood_file_path) {
        if let Ok(wood) = SchnyderMap::read_3treecode(&mut wood_file) {
            wood
        } else {
            println!(
                "File '{}' does not seem to contain a Schnyder wood.",
                wood_file_path
            );
            return;
        }
    } else {
        println!("File '{}' could not be opened for reading.", wood_file_path);
        return;
    };

    let mut seq = Vec::new();
    if let Ok(str) = read_to_string(seq_file_path) {
        let lines = str.split('\n').collect_vec();
        for line in lines {
            if !line.trim().is_empty() {
                if let Ok(op) = Operation::from_str(line) {
                    seq.push(op);
                } else {
                    println!("Misformatted operation found in sequence file.");
                    return;
                }
            }
        }
    } else {
        println!("File '{}' could not be opened for reading.", seq_file_path);
    }

    write_out(&wood, step_path, "step0");
    let mut i = 0;
    for op in seq {
        let result = wood.exec_op_canonical(&op);
        i += 1;
        write_out(&wood, step_path, &format!("step{}", i));
        if let Err(e) = result {
            println!("An error occurred when executing the operations: {} Maybe the sequence does not fit to the starting Schnyder wood?", e);
            return;
        }
    }
}

pub fn random_walk(matches: &ArgMatches) {
    let n = match retrieve_n(matches, 200) {
        Some(n) => n,
        None => return,
    };

    crate::flipgraph::random_walk::random_walk(n, 4, Some(Duration::from_secs(60 * 60)), None);
}

fn write_out(wood: &SchnyderMap, path: Option<&str>, name: &str) {
    if let Some(path) = path {
        let file_path = &format!("{}/{}.b3t", path, name);
        let result = wood.write_binary_3treecode_to_file(file_path);
        if let Err(e) = result {
            println!("Error writing 3treecode file: {}", e);
        }
    }
}

fn retrieve_n(matches: &ArgMatches, upper_bound: usize) -> Option<usize> {
    match str::parse::<usize>(matches.value_of("N").unwrap()) {
        Ok(n) if n >= 3 && n <= upper_bound => Some(n),
        Ok(_) => {
            println!("N should be at least 3 and at most {}", upper_bound);
            None
        }
        _ => {
            println!("N should be a positive number.");
            None
        }
    }
}
