use std::fs::File;
use std::str::FromStr;
use std::path::Path;
use std::io::stdout;
use regex::Regex;
use rustyline::Editor;
use clap::{App, AppSettings, ArgMatches};
use shellwords::split;

use crate::flipgraph::Flipgraph;
use crate::schnyder::io::{write_as_ascii_representation, write_as_binary_representation};
use crate::flipgraph::io::{write_flipgraph, FlipgraphOutputFormat};

pub struct Repl {
    g: Flipgraph
}

#[derive(Debug)]
enum Property {
    Level, Degree, Updegree, Downdegree
}

#[derive(Debug)]
enum Comparator {
    Eq, Gt, Ge, Lt, Le
}

#[derive(Debug)]
struct Condition {
    prop: Property,
    comp: Comparator,
    num: usize
}

impl Condition {
    fn new(prop: Property, comp: Comparator, num: usize) -> Condition {
        Condition { prop, comp, num }
    }

    fn apply(&self, g: &Flipgraph, node: usize) -> bool {
        let prop_value = match &self.prop {
            Property::Level => g.get_level(node) as usize,
            Property::Degree => g.get_degree(node),
            Property::Updegree => g.get_updegree(node),
            Property::Downdegree => g.get_downdegree(node)
        };

        match &self.comp {
            Comparator::Eq => prop_value == self.num,
            Comparator::Lt => prop_value < self.num,
            Comparator::Le => prop_value <= self.num,
            Comparator::Gt => prop_value > self.num,
            Comparator::Ge => prop_value >= self.num,
        }
    }
}

impl Repl {

    pub fn new(g: Flipgraph) -> Repl {
        Repl { g }
    }

    #[allow(dead_code)]
    pub fn main_loop(&mut self) {
        let mut rl = Editor::<()>::new();

        loop {
            let line = rl.readline("Δ.> ");
            match line {
                Ok(l) => {
                    rl.add_history_entry(&l);
                    if !self.parse_line(l.trim()) {
                        return;
                    }
                }
                _ => ()
            }

        }
    }

    fn parse_line(&mut self, line: &str) -> bool {

        if line.trim().is_empty() {
            return true;
        }

        if line.trim() == "execute order 66" {
            println!("The order of the galaxy will be restored.");
            return true;
        }

        if line.trim() == "execute order 42" {
            println!("You're confusing some things.");
            return true;
        }

        if let Ok(tokens) = split(line) {
            let matches = App::new("")
                .setting(AppSettings::NoBinaryName)
                .subcommand(
                    App::new("save")
                        .about("Write flip graph to file(s) in data interchange formats.")
                        .arg("-e, --edges 'Write a file containing the graph in EDGE64 format. The suffix \'.edges\' is appended to the base name.")
                        .arg("-l, --levels 'Write a file containing the vertex levels in LEVEL8 format. The suffix \'.levels\' is appended to the base name.")
                        .arg("-w, --woods 'Write a file containing the graph in L3TREECODE8 format. The suffix \'.woods\' is appended to the base name.")
                        .arg("<FILE> 'Base name for the file(s) to be written.'")
                )
                .subcommand(
                    App::new("select")
                        .about("Selects nodes from the flipgraph that fulfill a set of conditions. If no output flags are set, just the count of matches is printed.")
                        .arg("-d, --dir [DIR] 'Output directory. Without the output directory given, the other output flags do not have any effect.'")
                        .arg("-a, --ascii 'Write woods in ASCII 3TREECODE to the file \'<NODE NR>.a3t\'.'")
                        .arg("-b, --binary 'Write woods in binary 3TREECODE8 to the file \'<NODE NR>.b3t\'.'")
                        .arg("[CONDITION]... 'Conditions for selection of woods. The conditions must be formatted as follows: (level|deg|updeg|downdeg)(<|<=|=|=>|>)(NUMBER). If multiple conditions are given, they are joined by a logical \'AND\'.'")
                )
                .subcommand(
                    App::new("load")
                        .about("Loads a new flipgraph to explore from a file. Replaces the previously loaded one.")
                        .arg("[FILE] 'Source file.'")
                )
                .subcommand(
                    App::new("stats")
                        .about("Print statistics about the currently loaded flipgraph")
                        .arg("-c, --csv 'Prints the statistics in CSV format rather than in human readable format.'")
                        .arg("-d, --check 'Checks the stats against the proven formulas. The program is terminated with an assertion failure if any formula fails.'")
                )
                .subcommand(
                    App::new("exit")
                        .about("Exits the application (if a large flipgraph is loaded, memory cleanup will take a bit of time).")
                )
                .try_get_matches_from(tokens);

            if let Err(e) = matches {
                println!("{}", e);
                return true;
            }

            match matches.unwrap().subcommand() {
                Some(("save", matches)) => self.cmd_save(matches),
                Some(("stats", matches)) => self.cmd_stats(matches),
                Some(("select", matches)) => self.cmd_select(matches),
                Some(("load", matches)) => self.cmd_load(matches),
                Some(("exit", _)) => return false,
                _ => println!("No valid command specified. Type 'schnyderflip help' for a list of valid commands.")
            }
        } else {
            println!("You got your quotes all messed up. Ignoring command.");
        }

        return true;
    }

    fn cmd_select(&self, matches: &ArgMatches) {
        let cond_strings = match matches.values_of_lossy("CONDITION") {
            Some(c) => c,
            None => Vec::new()
        };

        let mut conditions = Vec::with_capacity(cond_strings.len());
        for cond_str in cond_strings {
            let condition = self.extract_condition(&cond_str);
            if let Some(c) = condition {
                conditions.push(c);
            } else {
                println!("Condition '{}' could not be interpreted. Aborting command.", cond_str);
                return;
            }
        }

        let output_folder = matches.value_of("dir").map(|s| Path::new(s));

        let mut count = 0;
        for v in 0..self.g.node_count() {
            let select = conditions.iter().fold(true, |acc, c| acc && c.apply(&self.g, v));

            if select {
                count += 1;

                if let Some(output_folder) = output_folder {
                    if matches.is_present("ascii") {
                        self.select_ascii_out(v, output_folder);
                    }
                    if matches.is_present("binary") {
                        self.select_binary_out(v, output_folder);
                    }
                }
            };
        }

        println!("{} woods matched.", count);
    }

    fn select_ascii_out(&self, v: usize, folder: &Path) {
        match File::create(folder.join( &format!("{}.a3t", v))) {
            Ok(mut file) => {
                if let Err(e) = write_as_ascii_representation(&mut file, self.g.get_code(v)) {
                    println!("Error writing file: {}", e);
                }
            },
            Err(e) => println!("File could not be opened for writing: {}", e)
        }
    }

    fn select_binary_out(&self, v: usize, folder: &Path) {
        match File::create(folder.join( &format!("{}.b3t", v))) {
            Ok(mut file) => {
                if let Err(e) =  write_as_binary_representation(&mut file, self.g.get_code(v)) {
                    println!("Error writing file: {}", e);
                }
            },
            Err(e) => println!("File could not be opened for writing: {}", e)
        }
    }

    fn extract_condition(&self, str: &str) -> Option<Condition> {
        let r = Regex::new(r"^\s*(level|updeg|downdeg|deg)\s*(<|>|<=|>=|=)\s*([0-9]+)\s*$").unwrap();

        if let Some(cap) = r.captures(str) {
            let prop = match &cap[1] {
                "level" => Property::Level, "updeg" => Property::Updegree,
                "downdeg" => Property::Downdegree, "deg" => Property::Degree,
                _ => return None
            };

            let comp = match &cap[2] {
                "<" => Comparator::Lt, "<=" => Comparator::Le,
                "=" => Comparator::Eq,
                ">" => Comparator::Gt, ">=" => Comparator::Ge,
                _ => return None
            };

            let num = usize::from_str(&cap[3]).unwrap();
            return Some(Condition::new(prop, comp, num));
        } else {
            return None;
        }
    }

    fn cmd_stats(&self, matches: &ArgMatches) {
        let with_check = matches.is_present("check");

        if matches.is_present("csv") {
            write_flipgraph(&self.g, &mut stdout(),FlipgraphOutputFormat::CSV, with_check).unwrap();
        } else {
            write_flipgraph(&self.g, &mut stdout(),FlipgraphOutputFormat::TabbedTable, with_check).unwrap();
        }
    }

    fn cmd_load(&mut self, matches: &ArgMatches) {

        let filename = matches.value_of("FILE").unwrap();

        if let Ok(file) = File::open(filename) {
            if let Ok(g) = serde_cbor::from_reader(file) {
                self.g = g;
            } else {
                println!("The specified file does not seem to contain a flipgraph.");
            }
        } else {
            println!("File '{}' could not be opened for reading.", filename);
        }
    }

    fn cmd_save(&self, matches: &ArgMatches) {
        let base_name = matches.value_of("FILE").unwrap();
        let targets = ["edges", "levels", "woods"];

        for target in &targets {
            if matches.is_present(target) {
                let file_name = format!("{}.{}", base_name, target);

                match File::create(file_name) {
                    Ok(mut file) => {
                        let result = match *target {
                            "edges" => self.g.to_edge_list(&mut file),
                            "levels" => self.g.to_level_list(&mut file),
                            "woods" => self.g.to_code_list(&mut file),
                            _ => panic!("Internal assertion failed.")
                        };

                        if let Err(e) = result {
                            println!("Error writing file: {}", e);
                        }
                    },
                    Err(e) => {
                        println!("File could not be opened for writing: {}", e);
                    }
                }
            }
        }
    }

}

