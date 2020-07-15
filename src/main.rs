use std::rc::Rc;
use std::collections::HashSet;
use std::cmp::Ordering;
use itertools::Itertools;

use crate::graph::{PlanarMap, VertexI};
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use crate::graph::schnyder::SchnyderEdgeDirection::{Bicolored, Unicolored, Black};
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::{SchnyderVertexType, SchnyderEdgeDirection};
use crate::graph::Signum::Forward;
use crate::util::iterators::cyclic::CyclicIterable;
use crate::graph::ClockDirection::CCW;

mod graph;
mod util;

struct SchnyderVertex {
    pub kind: SchnyderVertexType
}

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

    let mut map = PlanarMap::<SchnyderVertex, SchnyderEdge, _>::new();

    let r = map.add_vertex(SchnyderVertex::new(Suspension(Red)));
    let g = map.add_vertex(SchnyderVertex::new(Suspension(Green)));
    let b = map.add_vertex(SchnyderVertex::new(Suspension(Blue)));
    let c1 = map.add_vertex(SchnyderVertex::new(Normal(0)));
    let c2 = map.add_vertex(SchnyderVertex::new(Normal(1)));

    map.add_edge(r, g, SchnyderEdge::new(Bicolored(Green, Red))).expect("edge");
    map.add_edge(r, b, SchnyderEdge::new(Bicolored(Blue, Red))).expect("edge");
    map.add_edge(b, g, SchnyderEdge::new(Bicolored(Green, Blue))).expect("edge");

    map.add_edge(c1, r, SchnyderEdge::new(Unicolored(Red, Forward))).expect("edge");
    map.add_edge(c1, b, SchnyderEdge::new(Unicolored(Blue, Forward))).expect("edge");
    let trg = map.add_edge(c1, c2, SchnyderEdge::new(Unicolored(Green, Forward))).expect("edge");

    map.add_edge(c2, r, SchnyderEdge::new(Unicolored(Red, Forward))).expect("edge");
    map.add_edge(c2, g, SchnyderEdge::new(Unicolored(Green, Forward))).expect("edge");
    let src = map.add_edge(c2, b, SchnyderEdge::new(Unicolored(Blue, Forward))).expect("edge");


    println!("is_simple() = {}", map.is_simple());
    println!("is_connected() = {}", map.is_connected());

    map.set_embedding(vec![
        (vec![r,g,b], 1),
        (vec![r,b,c1], 2),
        (vec![b,g,c2], 3),
        (vec![r,c1,c2], 4),
        (vec![c1,b,c2], 5),
        (vec![r,c2,g], 5)
    ]).expect("not");

    //map.set_embedding(vec![(vec![r,g,b], 1), (vec![r,b,g], 2)]).expect("not");
    println!("Embedding set.");

    println!("{}, {}", map.check_wood(), map.edge_count());
    map.debug();


    map.merge(src, trg);
    println!("-------");
    map.debug();
    println!("{}, {}", map.check_wood(), map.edge_count());

    map.split(trg, CCW, b, SchnyderEdge::new(Black));
    println!("-------");
    map.debug();
    println!("{}, {}", map.check_wood(), map.edge_count());



}
