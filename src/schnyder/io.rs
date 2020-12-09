use crate::graph::error::{GraphErr, GraphResult};
use crate::schnyder::SchnyderMap;
use itertools::Itertools;
use pad::{Alignment, PadStr};
use std::convert::TryFrom;
use std::fs::File;
use std::io::Result;
use std::io::{Read, Write};

impl SchnyderMap {
    pub fn write_binary_3treecode_to_file(&self, path: &str) -> Result<()> {
        self.write_binary_3treecode(&mut File::create(path)?)
    }

    pub fn write_binary_3treecode(&self, w: &mut dyn Write) -> Result<()> {
        return write_as_binary_representation(w, &self.compute_3tree_code());
    }

    pub fn write_ascii_3treecode_to_file(&self, path: &str) -> Result<()> {
        self.write_ascii_3treecode(&mut File::create(path)?)
    }

    pub fn write_ascii_3treecode(&self, w: &mut dyn Write) -> Result<()> {
        return write_as_ascii_representation(w, &self.compute_3tree_code());
    }

    pub fn read_3treecode(r: &mut dyn Read) -> GraphResult<SchnyderMap> {
        let mut data = Vec::new();
        r.read_to_end(&mut data)
            .map_err(|e| GraphErr::new(&format!("Problem reading from stream: {}", e)))?;

        if let Some(s) = data.get(0..13) {
            if let Ok(s) = String::from_utf8(s.to_vec()) {
                if s == "<3TREECODE:8>" {
                    return SchnyderMap::read_binary_3treecode(&mut &data[..]);
                }
            }
        }

        if let Some(s) = data.get(0..18) {
            if let Ok(s) = String::from_utf8(s.to_vec()) {
                if s == "#<3TREECODE:ASCII>" {
                    return SchnyderMap::read_ascii_3treecode(&mut &data[..]);
                }
            }
        }

        GraphErr::new_err("Could not be recognized as 3-treecode.")
    }

    pub fn read_ascii_3treecode(r: &mut dyn Read) -> GraphResult<SchnyderMap> {
        let mut str = String::new();
        r.read_to_string(&mut str)
            .map_err(|e| GraphErr::new(&format!("Problem reading from stream: {}", e)))?;

        if !str.starts_with("#<3TREECODE:ASCII>") {
            return GraphErr::new_err("Not an ASCII 3-treecode.");
        }

        let mut code = Vec::new();
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
                            return GraphErr::new_err("Input file contained numbers above 255, this cannot be a valid 3tree code.");
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

        return SchnyderMap::build_from_3tree_code(&code);
    }

    pub fn read_binary_3treecode(r: &mut dyn Read) -> GraphResult<SchnyderMap> {
        let mut buf = [0u8; 13];
        r.read(&mut buf)
            .map_err(|e| GraphErr::new(&format!("Problem reading from stream: {}", e)))?;

        return match String::from_utf8(buf.to_vec()) {
            Ok(s) if s == "<3TREECODE:8>" => {
                let mut code = Vec::new();
                r.read_to_end(&mut code)
                    .map_err(|e| GraphErr::new(&format!("Problem reading from stream: {}", e)))?;
                SchnyderMap::build_from_3tree_code(&code)
            }
            Err(e) => GraphErr::new_err(&format!("Problem reading from stream: {}", e)),
            _ => GraphErr::new_err("Not a binary 3-treecode."),
        };
    }
}

pub fn write_as_binary_representation(w: &mut dyn Write, code: &Vec<u8>) -> Result<()> {
    write!(w, "<3TREECODE:8>")?;
    w.write(code)?;
    Ok(())
}

pub fn write_as_ascii_representation(w: &mut dyn Write, code: &Vec<u8>) -> Result<()> {
    let n = code.len() / 3;
    let pad = ((n - 1) as f64).log(10.0) as usize + 1;

    writeln!(w, "#<3TREECODE:ASCII>")?;
    let heading = (0..n)
        .map(|k| k.to_string().pad(pad, ' ', Alignment::Right, false))
        .join(" ");
    writeln!(w, "# {}", heading)?;

    for c in 0..3 {
        let str = code
            .get(c * n..(c + 1) * n)
            .unwrap()
            .iter()
            .map(|k| k.to_string().pad(pad, ' ', Alignment::Right, false))
            .join(" ");
        writeln!(w, "  {}", str)?;
    }

    Ok(())
}
