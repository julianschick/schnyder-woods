use petgraph::visit::{IntoNodeIdentifiers, IntoNeighbors, NodeCount, IntoNodeReferences, IntoEdgeReferences};
use bitvec::prelude::*;
use std::collections::{HashSet, HashMap};
use itertools::Itertools;
use petgraph::{Graph, EdgeType};
use petgraph::graph::IndexType;
use petgraph::visit::NodeRef;

pub fn to_edge_list<N, E, Ty: EdgeType, Ix: IndexType>(g: &Graph<N, E, Ty, Ix>) -> Vec<u8> {
    let mut result = Vec::with_capacity((g.edge_count()+1) * 8 * 2);
    let n = g.edge_count();
    result.extend(&n.to_le_bytes());
    let map :HashMap<_, _> =  g.node_indices().enumerate().map(|(i, index)| (index, i)).collect();

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
    let mut bits = BitVec::<Msb0,u8>::with_capacity((n*(n-1))/2);
    let nodes: Vec<_> = g.node_identifiers().collect();

    for i in 0..nodes.len() {
        let neighbors : Vec<_> = g.neighbors(nodes[i]).collect();

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
            let bit = bits.get(i*6 + j).unwrap_or(&false);
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
            result[3-j] = (((i & (0x3F << j*6)) >> j*6) + 63) as u8
        }

        return result;
    } else if i <= 68719476735 {
        let mut result = vec![126, 126, 0, 0, 0, 0, 0, 0];

        for j in 0..6 {
            result[7-j] = (((i & (0x3F << j*6)) >> j*6) + 63) as u8
        }

        return result;
    }

    panic!["number too high"];
}
