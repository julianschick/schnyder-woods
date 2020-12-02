use std::fs::File;
use crate::flipgraph::{build_flipgraph, SymmetryBreaking};
use crate::repl::Repl;
use clap::ArgMatches;
use std::path::Path;
use std::io::stdout;
use crate::schnyder::tikz::TikzOptions;
use crate::schnyder::SchnyderMap;
use crate::flipgraph::io::{write_flipgraph, FlipgraphOutputFormat};

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

                if let Some(output_filename) = matches.value_of("OUTPUT") {
                    if let Ok(mut output_file) = File::create(output_filename) {
                        if let Err(e) = wood.write_tikz(&mut output_file, &opts) {
                            println!("Output file could not be written: {}", e);
                        }
                    } else {
                        println!("Output file '{}' could not be created or opened for writing.", output_filename);
                    }
                } else {
                    if let Err(e) = wood.write_tikz(&mut std::io::stdout(), &opts) {
                        println!("Output could not be written to STDOUT: {}", e);
                    }
                }
            },
            Err(e) => println!("{}", e)
        }
    } else {
        println!("Input file '{}' could not be opened for reading.", input_filename);
    }


}

pub fn build(matches: &ArgMatches) {
    let n = match str::parse::<usize>(matches.value_of("N").unwrap()) {
        Ok(n) if n >= 3 && n <= 64 => n,
        Ok(_) => { println!("N should be at least 3 and at most 64"); return; }
        _ => { println!("N should be a positive number."); return; }
    };

    let min_level = match matches.value_of("min-level").map(|m|  str::parse::<usize>(m)) {
        Some(Ok(k)) => Some(k),
        None => None,
        _ => { println!("Level should be a positive number."); return; }
    };

    let max_level = match matches.value_of("max-level").map(|m|  str::parse::<usize>(m)) {
        Some(Ok(k)) => Some(k),
        None => None,
        _ => { println!("Level should be a positive number."); return; }
    };

    if let (Some(min), Some(max)) = (min_level, max_level) {
        if min > max {
            println!("Minimal level has to be less than or equal to maximal level."); return;
        }
    }

    let num_threads = match str::parse::<usize>(matches.value_of("threads").unwrap_or("1")) {
        Ok(n) if n >= 1 && n <= 64 => n,
        Ok(_) => { println!("The number of threads should be at least 1 and at most 64"); return; }
        _ => { println!("The number of threads should be a positive number."); return; }
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
        (true, true) => SymmetryBreaking::BreakAll
    };

    if n < 3 {
        println!("n must be at least 3");
        return;
    }

    let g = build_flipgraph(n, min_level, max_level, symmetry_breaking, num_threads);
    write_flipgraph(&g, &mut stdout(), FlipgraphOutputFormat::TabbedTable).unwrap();

    println!("\nWriting Flipgraph to file '{}'...", output_arg);
    {
        match File::create(output_path) {
            Ok(file) => match serde_cbor::to_writer(file, &g) {
                Err(e) => println!("Flipgraph could not be written: {}", e),
                _ => println!("...done.")
            },
            Err(e) => println!("File '{}' could not be opened for writing: {}", output_arg, e)
        }
    }
}

pub fn explore(matches: &ArgMatches) {
    let flipgraph_file = matches.value_of("GRAPH").unwrap();

    if let Ok(file) = File::open(flipgraph_file) {
        println!("Reading flipgraph...");
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