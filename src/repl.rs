use rustyline::Editor;
use crate::flipgraph::Flipgraph;
use std::fs::File;
use std::str::FromStr;
use regex::Regex;
use crate::schnyder::SchnyderMap;
use std::path::Path;
use crate::schnyder::io::TikzOptions;
use crate::flipgraph::io::{write_flipgraph, FlipgraphOutputFormat};
use std::io::stdout;

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

        /*let m = App::new("test")
            .setting(AppSettings::NoBinaryName)
            .subcommand( App::new("execute"))
            .get_matches_from(split(line).unwrap());

        println!("{:?}", m);*/

        let mut iter = line.split_whitespace();

        if let Some(command) = iter.next() {
            match command {
                "exit" => return false,
                "execute" => self.cmd_execute_order_66(&mut iter),
                "write-edges" => self.cmd_write_edges(&mut iter),
                "write-levels" => self.cmd_write_levels(&mut iter),
                "write-codes" => self.cmd_write_codes(&mut iter),
                "print-trees" => self.cmd_print_trees(&mut iter),
                "count-trees" => self.cmd_count_trees(&mut iter),
                "load" => self.cmd_load(&mut iter),
                "stats" => self.cmd_statistics(),
                "csv" => self.cmd_csv(),
                _ => println!("Invalid command {}", command)
            }
        }

        return true;
    }

    fn extract_condition(&self, str: &str) -> Option<Condition> {
        let r = Regex::new(r"^(level|updeg|downdeg|deg)(<|>|<=|>=|=)([0-9]+)$").unwrap();

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

    fn cmd_count_trees(&self, args: &mut dyn Iterator<Item=&str>) {
        let mut conditions = Vec::new();
        for arg in args {
            if let Some(cond) = self.extract_condition(arg) {
                conditions.push(cond);
            } else {
                println!("Usage: count-trees [PATH] [level|deg|updeg|downdeg][<|<=|=|=>|>][NUMBER] ...");
                println!("If multiple conditions are given, they are joined with a logical 'and'.");
                return;
            }
        }

        let count = (0..self.g.node_count()).filter(
            |&v| conditions.iter().fold(true, |acc, c| acc && c.apply(&self.g, v))
        ).count();

        println!("{}", count);
    }

    fn cmd_print_trees(&self, args: &mut dyn Iterator<Item=&str>) {
        let mut conditions = Vec::new();

        let path = {
            if let Some(p) = args.next() {
                Path::new(p)
            } else {
                println!("Usage: print-trees [PATH] [level|deg|updeg|downdeg][<|<=|=|=>|>][NUMBER] ...");
                println!("If multiple conditions are given, they are joined with a logical 'and'.");
                return;
            }
        };

        for arg in args {
            if let Some(cond) = self.extract_condition(arg) {
                conditions.push(cond);
            } else {
                println!("Usage: print-trees [PATH] [level|deg|updeg|downdeg][<|<=|=|=>|>][NUMBER] ...");
                println!("If multiple conditions are given, they are joined with a logical 'and'.");
                return;
            }
        }

        let mut printed = 0;
        for v in 0..self.g.node_count() {
            let print = conditions.iter().fold(true, |acc, c| acc && c.apply(&self.g, v));
            if print {
                let wood = SchnyderMap::build_from_3tree_code(self.g.get_code(v)).unwrap();
                let title = &format!("|E| = {}, deg = {}, updeg = {}, downdeg = {}",
                    self.g.get_level(v),
                    self.g.get_degree(v),
                    self.g.get_updegree(v),
                    self.g.get_downdegree(v)
                );
                match File::create(path.join(&format!("{}.tex", v))) {
                    Ok(mut file) => {
                        let mut opts = TikzOptions::default();
                        opts.title = Some(title);

                        match wood.write_tikz(&mut file, &opts) {
                            Ok(_) => printed += 1,
                            Err(e) => println!("Error writing file: {}", e)
                        }
                    }
                    Err(e) => {
                        println!("File could not be opened for writing: {}", e);
                        return;
                    }
                }
            }
        }

        println!("{} trees printed.", printed);
    }

    fn cmd_statistics(&self) {
        write_flipgraph(&self.g, &mut stdout(),FlipgraphOutputFormat::TabbedTable).unwrap();
    }

    fn cmd_csv(&self) {
        write_flipgraph(&self.g, &mut stdout(),FlipgraphOutputFormat::CSV).unwrap();
    }

    fn cmd_load(&mut self, args: &mut dyn Iterator<Item=&str>) {
        if let Some(filename) = args.next() {
            if let Ok(file) = File::open(filename) {
                if let Ok(g) = serde_cbor::from_reader(file) {
                    self.g = g;
                } else {
                    println!("The specified file does not seem to contain a flipgraph.");
                }
            } else {
                println!("File '{}' could not be opened for reading.", filename);
            }
        } else {
            println!("Usage: load [FILENAME]");
        }
    }

    fn cmd_execute_order_66(&self, args: &mut dyn Iterator<Item=&str>) {
        if let (Some(arg1), Some(arg2)) = (args.next(), args.next()) {
            if arg1 == "order" && arg2 == "66" {
                println!("The order of the galaxy will be restored.");
            } else {
                println!("Execution failed.")
            }
        } else {
            println!("Execution failed.")
        }
    }

    fn cmd_write_edges(&self, args: &mut dyn Iterator<Item=&str>) {
        if let Some(arg) = args.next() {
            match File::create(arg) {
                Ok(mut file) => {
                    if let Err(e) = self.g.to_edge_list(&mut file) {
                        println!("Error writing file: {}", e);
                    }
                },
                Err(e) => {
                    println!("File could not be opened for writing: {}", e);
                }
            }
        } else {
            println!("Usage: write-edges [FILENAME]");
        }
    }

    fn cmd_write_codes(&self, args: &mut dyn Iterator<Item=&str>) {
        if let Some(arg) = args.next() {
            match File::create(arg) {
                Ok(mut file) => {
                    if let Err(e) = self.g.to_code_list(&mut file) {
                        println!("Error writing file: {}", e);
                    }
                },
                Err(e) => {
                    println!("File could not be opened for writing: {}", e);
                }
            }
        } else {
            println!("Usage: write-codes [FILENAME]");
        }
    }

    fn cmd_write_levels(&self, args: &mut dyn Iterator<Item=&str>) {
        if let Some(arg) = args.next() {
            match File::create(arg) {
                Ok(mut file) => {
                    if let Err(e) = self.g.to_level_list(&mut file) {
                        println!("Error writing file: {}", e);
                    }
                },
                Err(e) => {
                    println!("File could not be opened for writing: {}", e);
                }
            }
        } else {
            println!("Usage: write-levels [FILENAME]");
        }
    }

}

