use bitvec::prelude::*;
use petgraph::graph::IndexType;
use petgraph::visit::{IntoNeighbors, IntoNodeIdentifiers, NodeCount};
use petgraph::{EdgeType, Graph};
use std::collections::HashMap;

pub fn to_edge_list<N, E, Ty: EdgeType, Ix: IndexType>(g: &Graph<N, E, Ty, Ix>) -> Vec<u8> {
    let mut result = Vec::with_capacity((g.edge_count() + 1) * 8 * 2);
    let n = g.edge_count();
    result.extend(&n.to_le_bytes());
    let map: HashMap<_, _> = g
        .node_indices()
        .enumerate()
        .map(|(i, index)| (index, i))
        .collect();

    for edge in g.edge_indices() {
        let (a, b) = g.edge_endpoints(edge).unwrap();

        result.extend(&map.get(&a).unwrap().to_le_bytes());
        result.extend(&map.get(&b).unwrap().to_le_bytes());
    }

    return result;
}

pub fn to_level_list<E, Ty: EdgeType, Ix: IndexType>(g: &Graph<usize, E, Ty, Ix>) -> Vec<u8> {
    let mut result = Vec::with_capacity(g.node_count());

    for node in g.node_indices() {
        result.push(*g.node_weight(node).unwrap() as u8);
    }

    return result;
}

pub fn to_sparse6<G: IntoNodeIdentifiers + IntoNeighbors + NodeCount>(g: G) -> Vec<u8> {
    let n = g.node_count();
    let mut bits = BitVec::<Msb0, u8>::with_capacity((n * (n - 1)) / 2);
    let nodes: Vec<_> = g.node_identifiers().collect();

    for i in 0..nodes.len() {
        let neighbors: Vec<_> = g.neighbors(nodes[i]).collect();

        for j in 0..i {
            bits.push(neighbors.contains(&nodes[j]))
        }
    }

    let mut ascii = sparse6_int(n);
    ascii.extend(sparse6_bitvec(&bits));
    return ascii;
}

fn sparse6_bitvec(bits: &BitVec<Msb0, u8>) -> Vec<u8> {
    let len = (bits.len() as f32 / 6 as f32).ceil() as usize;
    let mut result = Vec::with_capacity(len);

    for i in 0..len {
        let mut value = 0u8;
        for j in 0..6 {
            let bit = bits.get(i * 6 + j).unwrap_or(&false);
            value = (value << 1) | if *bit { 1 } else { 0 };
        }
        result.push(value + 63);
    }

    return result;
}

fn sparse6_int(i: usize) -> Vec<u8> {
    if i <= 63 {
        return vec![i as u8 + 63];
    } else if i <= 258047 {
        let mut result = vec![126, 0, 0, 0];

        for j in 0..3 {
            result[3 - j] = (((i & (0x3F << j * 6)) >> j * 6) + 63) as u8
        }

        return result;
    } else if i <= 68719476735 {
        let mut result = vec![126, 126, 0, 0, 0, 0, 0, 0];

        for j in 0..6 {
            result[7 - j] = (((i & (0x3F << j * 6)) >> j * 6) + 63) as u8
        }

        return result;
    }

    panic!["number too high"];
}

/*impl<N: Clone, E: Clone, F: Clone, Ty: EdgeType, Ix: IndexType> From<Graph<N, E, Ty, Ix>> for PlanarMap<N, E, F> {
    fn from(g: Graph<N, E, Ty, Ix>) -> Self {
        let mut map = PlanarMap::new();

        let mut node_map = HashMap::new();
        for i in g.node_indices() {
            let new_idx = map.add_vertex(g.node_weight(i).unwrap().clone());
            node_map.insert(i, new_idx);
        }

        for i in g.edge_indices() {
            let (a, b) = g.edge_endpoints(i).unwrap();

            let v1 = node_map.get(&a).unwrap();
            let v2 = node_map.get(&b).unwrap();

            map.add_edge(*v1, *v2, g.edge_weight(i).unwrap().clone());
        }

        return map;
    }
}

impl<N: Clone, E: Clone, F: Clone> /*Into<(Graph<N, E, Ty, Ix>, HashMap<VertexI, NodeIndex<Ix>>)> for*/ PlanarMap<N, E, F> {
    pub fn into_petgraph(&self) -> (Graph<N, E, Undirected, u32>, HashMap<VertexI, NodeIndex<u32>>) {
        let mut g: Graph<N, E, Undirected, u32> = Graph::new_undirected();

        let mut node_map = HashMap::new();
        for v in self.vertices.get_map().values() {
            let new_idx = g.add_node(v.weight.clone());
            node_map.insert(v.id, new_idx);
        }

        for e in self.edges.get_map().values() {

            let v1 = node_map.get(&e.tail).unwrap();
            let v2 = node_map.get(&e.head).unwrap();

            g.add_edge(*v1, *v2, e.weight.clone());
        }

        return (g, node_map);
    }
}*/
