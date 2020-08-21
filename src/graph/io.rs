use super::{VertexI, EdgeI, FaceI, PlanarMap};
use std::collections::{HashSet, HashMap};
use itertools::Itertools;
use std::fs::{create_dir, File};
use std::process::Command;
use crate::graph::schnyder::SchnyderMap;
use std::io::Write;
use std::path::Path;

pub fn read_plantri_planar_code<N, E, F: Clone>(data: &Vec<u8>, v_weights: fn(VertexI) -> N, e_weights: fn(EdgeI) -> E, f_weights: fn(FaceI) -> F) -> Vec<PlanarMap<N, E, F>> {
    if data.len() < 15 {
        panic!("not a valid planar code file");
    }

    match std::str::from_utf8(&data[0..15]) {
        Err(_) => panic!("utf8 error"),
        Ok(str) => if str != ">>planar_code<<" {
            panic!("no planar code data");
        }
    }

    let mut result = Vec::new();
    let mut iter = data.iter().skip(15).peekable();

    while let Some(_) = iter.peek() {
        let map = PlanarMap::from_plantri_planar_code(&mut iter, v_weights, e_weights, f_weights);
        result.push(map);
    }

    return result;

}

pub fn debug_output<F: Clone>(wood: &SchnyderMap<F>, name: &str, title: Option<&str>, face_counts: &HashMap<VertexI, (usize, usize, usize)>) {
    let tikz_string = wood.generate_tikz(title, face_counts);

    let basedir = "/tmp/schnyder";
    if !Path::new(&basedir).is_dir() {
        create_dir(basedir).expect("Unable to create temporary output dir");
    }
    let outputdir = format!("{}/output", basedir);
    if !Path::new(&outputdir).is_dir() {
        create_dir(&outputdir).expect("Unable to create output dir");
    }

    let mut f = File::create(format!("{}/{}.tex", basedir, name)).expect("Unable to create file");
    f.write_all(tikz_string.as_bytes()).expect("Unable to write data");

    Command::new("xelatex").current_dir(basedir).arg(format!("{}.tex", name)).output();
    Command::new("pdftoppm").current_dir(basedir)
        .arg(format!("{}.pdf", name))
        .arg(format!("{}/{}", outputdir, name))
        .arg("-png").arg("-singlefile").output();
}

impl<N,E,F: Clone> PlanarMap<N,E,F> {
    pub fn from_plantri_planar_code(data: &mut Iterator<Item=&u8>, v_weights: fn(VertexI) -> N, e_weights: fn(EdgeI) -> E, f_weights: fn(FaceI) -> F) -> PlanarMap<N, E, F> {

        if let Some(n) = data.next() {
            let mut result = PlanarMap::new();
            let mut neighbors = Vec::new();
            let mut index_list = Vec::new();

            // iterate byte stream to identify number of vertices and their neighbors
            // neighbors are given in clockwise order, so that the embedding is implied
            for i in 1..n+1 {
                let weight = v_weights(result.vertices.peek_index());
                index_list.push(result.add_vertex(weight));

                let mut nb = Vec::new();
                let mut other = if let Some(&byte) = data.next() {
                    byte
                } else {
                    panic!("invalid input data");
                };

                while other > 0 {
                    nb.push(other);
                    if let Some(&byte) = data.next() {
                        other = byte;
                    } else {
                        panic!("invalid input data");
                    }

                }

                let set: HashSet<_> = nb.iter().collect();
                if set.len() != nb.len() {
                    panic!("double edges are not allowed");
                }
                if set.contains(&i) {
                    panic!("loops are not allowed");
                }

                neighbors.push(nb);
            }

            // construct edges in graph from the adjacency list
            for i in 1..(*n as usize)+1 {
                for &j in neighbors[i-1].iter().filter(|&&j| j as usize > i) {
                    let (v1, v2) = (index_list[i-1], index_list[j as usize-1]);

                    let pos1 = neighbors[i-1].iter().position(|&k| k == j);
                    let pos2 = neighbors[j as usize -1].iter().position(|&k| k as usize == i);

                    let weight = e_weights(result.edges.peek_index());

                    if let None = pos1.and(pos2) {
                        panic!("invalid data");
                    }

                    result.add_edge_(v1, v2, pos1, pos2, weight);
                }
            }

            // transfer neighbor order from adjacency list into graph struct
            for vid in result.vertices.get_map().keys().cloned().collect_vec() {
                let v = result.vertex_mut(vid);
                v.neighbors.sort_by_key(|nb| nb.index);

                for i in 0..v.neighbors.len() {
                    if v.neighbors[i].index != i {
                        panic!("error 252");
                    }
                }
            }

            // construct faces from neighbor order (now the graph struct knows about its faces
            // and is embedded)
            result.construct_faces(f_weights);
            return result;
        } else {
            panic!("empty input data");
        }
    }
}