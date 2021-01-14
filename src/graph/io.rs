use super::{EdgeI, FaceI, PlanarMap, VertexI};
use crate::graph::error::{GraphErr, GraphResult};
use itertools::Itertools;
use std::collections::HashSet;

impl<N, E, F: Clone> PlanarMap<N, E, F> {
    /// Reads a plantri planar code file (which usually contains a whole set of embedded planar graphs) and
    /// returns a list of PlanarMaps.
    /// # Arguments
    /// * data - a vector of the file bytes
    /// * max_count - if some value k is given, only the first k graphs from the file will be read
    /// * v_weights - weight generator function for the vertices (simplest would be '|_| ()')
    /// * e_weights - weight generator function for the edges (simplest would be '|_| ()')
    /// * f_weights - weight generator function for the faces (simplest would be '|_| ()')
    /// # Returns
    /// Various error states are possible and are triggered by invalid input data. All errors
    /// are returned, no panics are to be expected.
    pub fn read_plantri_planar_code(
        data: &Vec<u8>,
        max_count: Option<usize>,
        v_weights: fn(VertexI) -> N,
        e_weights: fn(EdgeI) -> E,
        f_weights: fn(FaceI) -> F,
    ) -> GraphResult<Vec<PlanarMap<N, E, F>>> {
        if data.len() < 15 {
            return GraphErr::new_err("Invalid planar code file, not enough bytes.");
        }

        match std::str::from_utf8(&data[0..15]) {
            Err(_) => {
                return GraphErr::new_err("Invalid planar code file, header not utf8 encoded.")
            }
            Ok(str) => {
                if str != ">>planar_code<<" {
                    return GraphErr::new_err("Invalid planar code file, header not found.");
                }
            }
        }

        let mut result = Vec::new();
        let mut iter = data.iter().skip(15).peekable();

        let mut count = 0;
        while let Some(_) = iter.peek() {
            let map =
                PlanarMap::from_plantri_planar_code(&mut iter, v_weights, e_weights, f_weights)?;
            result.push(map);
            count += 1;
            if let Some(max) = max_count {
                if max <= count {
                    return Ok(result);
                }
            }
        }

        return Ok(result);
    }

    /// Reads one embedded graph from a plantri planar code data stream given as iterator and
    /// returns it as PlanarMap.
    /// # Arguments
    /// * data - iterator to the data stream, data will be read from its current position on
    /// * max_count - if some value k is given, only the first k graphs from the file will be read
    /// * v_weights - weight generator function for the vertices (simplest would be '|_| ()')
    /// * e_weights - weight generator function for the edges (simplest would be '|_| ()')
    /// * f_weights - weight generator function for the faces (simplest would be '|_| ()')
    /// # Returns
    /// Various error states are possible and are triggered by invalid input data, All errors
    /// are returned, no panics are to be expected.
    pub fn from_plantri_planar_code(
        data: &mut dyn Iterator<Item = &u8>,
        v_weights: fn(VertexI) -> N,
        e_weights: fn(EdgeI) -> E,
        f_weights: fn(FaceI) -> F,
    ) -> GraphResult<PlanarMap<N, E, F>> {
        if let Some(n) = data.next() {
            let mut result = PlanarMap::new();
            let mut neighbors = Vec::new();
            let mut index_list = Vec::new();

            // iterate byte stream to identify number of vertices and their neighbors
            // neighbors are given in clockwise order, so that the embedding is implied
            for i in 1..n + 1 {
                let weight = v_weights(result.vertices.next_index());
                index_list.push(result.add_vertex(weight));

                let mut nb = Vec::new();
                let mut other = if let Some(&byte) = data.next() {
                    byte
                } else {
                    return GraphErr::new_err("Preliminary end of planar code data.");
                };

                while other > 0 {
                    nb.push(other);
                    if let Some(&byte) = data.next() {
                        other = byte;
                    } else {
                        return GraphErr::new_err("Preliminary end of planar code data.");
                    }
                }

                let set: HashSet<_> = nb.iter().collect();
                if set.len() != nb.len() {
                    return GraphErr::new_err(
                        "Double edges are not allowed when reading planar code.",
                    );
                }
                if set.contains(&i) {
                    return GraphErr::new_err("Loops are not allowed when reading planar code.");
                }

                neighbors.push(nb);
            }

            // construct edges in graph from the adjacency list
            for i in 1..(*n as usize) + 1 {
                for &j in neighbors[i - 1].iter().filter(|&&j| j as usize > i) {
                    let (v1, v2) = (index_list[i - 1], index_list[j as usize - 1]);

                    let pos1 = neighbors[i - 1].iter().position(|&k| k == j);
                    let pos2 = neighbors[j as usize - 1]
                        .iter()
                        .position(|&k| k as usize == i);

                    let weight = e_weights(result.edges.next_index());

                    if let None = pos1.and(pos2) {
                        return GraphErr::new_err("Referential integrity of planar code broken.");
                    }

                    result.add_edge_(v1, v2, pos1, pos2, weight);
                }
            }

            // transfer neighbor order from adjacency list into graph struct
            for vid in result.vertices.get_indices().cloned().collect_vec() {
                let v = result.vertex_mut(vid);
                v.neighbors.sort_by_key(|nb| nb.index);

                for i in 0..v.neighbors.len() {
                    if v.neighbors[i].index != i {
                        return GraphErr::new_err("Referential integrity of planar code broken.");
                    }
                }
            }

            // construct faces from neighbor order (now the graph struct knows about its faces
            // and is embedded)
            result.construct_faces(f_weights)?;
            return Ok(result);
        } else {
            return GraphErr::new_err("Empty input data.");
        }
    }
}
