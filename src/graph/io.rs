use super::{VertexI, EdgeI, FaceI, PlanarMap};

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