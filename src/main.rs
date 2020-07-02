mod graph;

use std::rc::Rc;
use crate::graph::schnyder::{SchnyderNode, SchnyderColor};
use petgraph::dot::Dot;
use crate::graph::{PlanarMap, VertexIndex};
use std::collections::HashSet;
use std::cmp::Ordering;
use crate::graph::schnyder::SchnyderNode::Suspension;
use crate::graph::schnyder::SchnyderEdgeType::{self, Bicolored, Unicolored};

fn print_plus_two(a: Rc<i32>) {
    println!("{}", *a + 2)
}

fn main() {


    let mut map = PlanarMap::<SchnyderNode, SchnyderEdgeType>::new();

    let r = map.add_vertex(Suspension(SchnyderColor::Red));
    let g = map.add_vertex(Suspension(SchnyderColor::Green));
    //let b = map.add_vertex(Suspension(SchnyderColor::Blue));
    //let c = map.add_vertex(SchnyderNode::Normal(0));

    //map.add_edge(r, g, Bicolored(SchnyderColor::Green, SchnyderColor::Red));
    //map.add_edge(r, b, Bicolored(SchnyderColor::Blue, SchnyderColor::Red));
    //map.add_edge(b, g, Bicolored(SchnyderColor::Green, SchnyderColor::Blue));

    //map.add_edge(c, r, Unicolored(SchnyderColor::Red));
    //map.add_edge(c, g, Unicolored(SchnyderColor::Green));
    //map.add_edge(c, b, Unicolored(SchnyderColor::Blue));}*/

    println!("is_simple() = {}", map.is_simple());
    println!("is_connected() = {}", map.is_connected());

    /*map.set_embedding(vec![
        vec![r,g,b],
        vec![r,b,c],
        vec![b,g,c],
        vec![r,c,g]
    ]).expect("not");*/

    //map.set_embedding(vec![vec![r,g]]).expect("not");

    println!("Embedding set.");

}
