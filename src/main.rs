use std::rc::Rc;
use std::collections::HashSet;
use std::cmp::Ordering;

use crate::graph::{PlanarMap, VertexI};
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use crate::graph::schnyder::SchnyderEdgeDirection::{Bicolored, Unicolored};
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::{SchnyderVertexType, SchnyderEdgeDirection};
use crate::graph::Signum::Forward;

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
    let c = map.add_vertex(SchnyderVertex::new(Normal(0)));

    map.add_edge(r, g, SchnyderEdge::new(Bicolored(Green, Red))).expect("edge");
    map.add_edge(r, b, SchnyderEdge::new(Bicolored(Blue, Red))).expect("edge");
    map.add_edge(b, g, SchnyderEdge::new(Bicolored(Green, Blue))).expect("edge");

    map.add_edge(c, r, SchnyderEdge::new(Unicolored(Red, Forward))).expect("edge");
    map.add_edge(c, g, SchnyderEdge::new(Unicolored(Green, Forward))).expect("edge");
    map.add_edge(c, b, SchnyderEdge::new(Unicolored(Blue, Forward))).expect("edge");

    println!("is_simple() = {}", map.is_simple());
    println!("is_connected() = {}", map.is_connected());

    map.set_embedding(vec![
        (vec![r,g,b], 1),
        (vec![r,b,c], 2),
        (vec![b,g,c], 3),
        (vec![r,c,g], 4)
    ]).expect("not");

    //map.set_embedding(vec![(vec![r,g,b], 1), (vec![r,b,g], 2)]).expect("not");

    println!("Embedding set.");

    println!("{}", map.check_wood());

}
