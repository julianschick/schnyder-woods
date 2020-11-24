use std::fs::File;
use crate::flipgraph::{build_flipgraph, SymmetryBreaking};
use crate::print_flipgraph;
use crate::repl::Repl;
use clap::ArgMatches;
use std::path::Path;
use std::io::Read;
use std::convert::TryFrom;
use crate::schnyder::io::TikzOptions;
use crate::schnyder::SchnyderMap;

pub fn convert_to_tikz(matches: &ArgMatches) {

    let input_filename = matches.value_of("3CODE").unwrap();
    let mut code = Vec::new();

    if let Ok(mut input_file) = File::open(input_filename) {
        let mut str = String::new();
        if let Ok(_) = input_file.read_to_string(&mut str) {

            let mut comment = false;
            let mut buf = String::new();
            for c in str.chars() {
                if !comment {
                    if c.is_numeric() {
                        buf.push(c);
                    } else {
                        if !buf.is_empty() {
                            if let Ok(nr) = u8::try_from(buf.parse::<usize>().unwrap()) {
                                code.push(nr);
                                buf.clear();
                            } else {
                                println!("Input file contained numbers above 255, this cannot be a valid 3tree code.");
                                return;
                            }
                        }
                    }
                }
                if c == '\n' {
                    comment = false;
                }
                if c == '#' {
                    comment = true;
                }
            }

        } else {
            println!("Input file '{}' could not be read as text file.", input_filename);
        }
    } else {
        println!("Input file '{}' could not be opened for reading.", input_filename);
        return;
    }

    println!("{:?}", code);

    let mut opts = TikzOptions::default();
    opts.anchor = matches.value_of("anchor");
    opts.print_document = matches.is_present("doc");
    opts.print_environment = matches.is_present("env");
    opts.print_styles = matches.is_present("styles");
    opts.slanted = matches.is_present("slanted");

    match SchnyderMap::build_from_3tree_code(&code) {
        Ok(wood) => {
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
        }
        Err(e) => println!("Input could not be interpreted: {}", e.get_message())
    }

}

pub fn build(matches: &ArgMatches) {
    let n = match str::parse::<usize>(matches.value_of("N").unwrap()) {
        Ok(n) if n >= 3 && n <= 64 => n,
        Ok(_) => { println!("N should be at least 3 and at most 64"); return; }
        _ => { println!("N should be a positive number."); return; }
    };

    let num_threads = match str::parse::<usize>(matches.value_of("threads").unwrap_or("1")) {
        Ok(n) if n >= 1 && n <= 64 => n,
        Ok(_) => { println!("The number of threads should be at least 1 and at most 64"); return; }
        _ => { println!("The number of threads should be a positive number."); return; }
    };


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

    let g = build_flipgraph(n, symmetry_breaking, num_threads);
    print_flipgraph(&g);

    println!("Writing Flipgraph to file '{}'...", output.to_str().unwrap());
    {
        let file = File::create(output).expect("TODO");
        serde_cbor::to_writer(file, &g).expect("TODO");
    }
    println!("...done.");

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