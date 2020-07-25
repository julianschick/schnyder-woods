use std::rc::Rc;
use std::collections::HashSet;
use std::cmp::Ordering;
use itertools::Itertools;

use crate::graph::{PlanarMap, VertexI};
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use crate::graph::schnyder::SchnyderEdgeDirection::{Bicolored, Unicolored, Black};
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::{SchnyderVertexType, SchnyderEdgeDirection, SchnyderMap};
use crate::graph::Signum::Forward;
use crate::util::iterators::cyclic::CyclicIterable;
use crate::graph::ClockDirection::{CCW, CW};

mod graph;
mod util;

struct SchnyderVertex {
    pub kind: SchnyderVertexType
}

#[derive(Copy, Clone)]
struct SchnyderEdge {
    pub direction: SchnyderEdgeDirection
}

impl SchnyderVertex {
    fn new(kind: SchnyderVertexType) -> SchnyderVertex {
        SchnyderVertex { kind }
    }
}

impl SchnyderEdge {
    fn new(direction: SchnyderEdgeDirection) -> SchnyderEdge {
        SchnyderEdge { direction }
    }
}

impl crate::graph::schnyder::SchnyderVertex for SchnyderVertex {
    fn get_type(&self) -> SchnyderVertexType { self.kind }
    fn set_type(&mut self, t: SchnyderVertexType) { self.kind = t; }
}

impl crate::graph::schnyder::SchnyderEdge for SchnyderEdge {
    fn get_direction(&self) -> SchnyderEdgeDirection { self.direction }
    fn set_direction(&mut self, d: SchnyderEdgeDirection) { self.direction = d }
}


fn main() {

    let mut map = PlanarMap::<SchnyderVertexType, SchnyderEdgeDirection, _>::new();

    let r = map.add_vertex(Suspension(Red));
    let g = map.add_vertex(Suspension(Green));
    let b = map.add_vertex(Suspension(Blue));
    let c1 = map.add_vertex(Normal(0));
    let c2 = map.add_vertex(Normal(1));

    map.add_edge(r, g, Bicolored(Green, Red));
    map.add_edge(r, b, Bicolored(Blue, Red));
    map.add_edge(b, g, Bicolored(Green, Blue));

    map.add_edge(c1, r, Unicolored(Red, Forward));
    map.add_edge(c1, b, Unicolored(Blue, Forward));
    let trg = map.add_edge(c1, c2, Unicolored(Green, Forward));

    map.add_edge(c2, r, Unicolored(Red, Forward));
    map.add_edge(c2, g, Unicolored(Green, Forward));
    let src = map.add_edge(c2, b, Unicolored(Blue, Forward));

    println!("is_simple() = {}", map.is_simple());
    println!("is_connected() = {}", map.is_connected());

    map.set_embedding(vec![
        (vec![r,g,b], 1),
        (vec![r,b,c1], 2),
        (vec![b,g,c2], 3),
        (vec![r,c1,c2], 4),
        (vec![c1,b,c2], 5),
        (vec![r,c2,g], 5)
    ]);



    map.contract_embedded_edge(trg, &(|e1, e2| *e1));

    let (dual, ..) = map.get_dual(true);

    let mut schnyder_map = SchnyderMap::from(map);
    schnyder_map.debug();

    println!("-----");
    println!("{:?}", dual);
    println!("-----");
    println!("{:?}", schnyder_map.calculate_face_counts(r));
    println!("{:?}", schnyder_map.calculate_face_counts(g));
    println!("{:?}", schnyder_map.calculate_face_counts(b));
    //println!("{:?}", schnyder_map.calculate_face_counts(c1));
    println!("{:?}", schnyder_map.calculate_face_counts(c2));



    // println!("ref_integrity = {}, check_wood = {}, edge_count = {}", map.check_referential_integrity(), map.check_wood(), map.edge_count(), );
    // println!("{:?}", map.calculate_face_counts(c1));
    // println!("{:?}", map.calculate_face_counts(c2));
    //
    // map.calculate_face_counts(c2);
    // //map.debug();
    //
    // //map.contract_embedded_edge(trg, &(|e1, e2| *e1));
    //
    // println!("ref_integrity = {}, check_wood = {}, edge_count = {}", map.check_referential_integrity(), map.check_wood(), map.edge_count(), );
    //
    //
    // //map.merge(src, trg);
    // //println!("-------");
    // //map.debug();
    // //println!("{}, {}", map.check_wood(), map.edge_count());
    //
    // //map.split(trg, CCW, b, Black));
    // //println!("-------");
    // //map.debug();
    // //println!("{}, {}", map.check_wood(), map.edge_count());
    //
    // let dual = map.get_dual();
    // println!("ref_integrity = {}, check_wood = {}, edge_count = {}", dual.check_referential_integrity(), false, dual.edge_count(), );
    //
    // let v:Vec<usize> = vec![1,2,3];
    //
    // let v2:Vec<(_,_)> = v.cycle(0, true).tuple_windows().collect_vec();
    //
    // println!("{:?}", v2);



}
