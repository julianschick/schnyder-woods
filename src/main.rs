mod graph;

use std::rc::Rc;
use crate::graph::SchnyderNode;
use crate::graph::SchnyderEdge;
use crate::graph::SchnyderColor;
use petgraph::dot::Dot;
use crate::graph::{PlanarMap, VertexIndex};
use std::collections::HashSet;
use std::cmp::Ordering;
use crate::graph::SchnyderNode::Suspension;
use crate::graph::SchnyderEdge::{Bicolored, Unicolored};

fn print_plus_two(a: Rc<i32>) {
    println!("{}", *a + 2)
}

fn main() {


    let mut map = PlanarMap::<SchnyderNode, SchnyderEdge>::new();

    let r = map.add_vertex(Suspension(SchnyderColor::Red));
    let g = map.add_vertex(Suspension(SchnyderColor::Green));
    let b = map.add_vertex(Suspension(SchnyderColor::Blue));
    let c = map.add_vertex(SchnyderNode::Normal(0));

    map.add_edge(r, g, Bicolored(SchnyderColor::Green, SchnyderColor::Red));
    map.add_edge(r, b, Bicolored(SchnyderColor::Blue, SchnyderColor::Red));
    map.add_edge(b, g, Bicolored(SchnyderColor::Green, SchnyderColor::Blue));

    map.add_edge(c, r, Unicolored(SchnyderColor::Red));
    map.add_edge(c, g, Unicolored(SchnyderColor::Green));
    map.add_edge(c, b, Unicolored(SchnyderColor::Blue));

    println!("{}", map.is_simple());

    map.set_embedding(vec![
        vec![r,g,b],
        vec![r,b,c],
        vec![b,g,c],
        vec![r,c,g]
    ]).expect("not");

}
