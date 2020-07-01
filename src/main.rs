mod graph;

use std::rc::Rc;
use petgraph::{Undirected, Directed};
use petgraph::Graph;
use crate::SchnyderNode::Suspension;
use crate::SchnyderEdge::{Bicolored, Unicolored};
use petgraph::dot::Dot;
use crate::graph::PlanarMap;

fn print_plus_two(a: Rc<i32>) {
    println!("{}", *a + 2)
}

#[derive(Debug)]
enum SchnyderColor {
    Red, Green, Blue
}

#[derive(Debug)]
enum SchnyderEdge {
    Black,
    Unicolored(SchnyderColor),
    Bicolored(SchnyderColor, SchnyderColor)
}

#[derive(Debug)]
enum SchnyderNode {
    Normal(usize),
    Suspension(SchnyderColor)
}

fn main() {
    println!("Hello, world!");

    let two = Rc::new(2);
    let a = &Rc::new(4);
    let b = &Rc::new(6);
    println!("{}", a < b);
    print_plus_two(two);

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
    map.add_edge(c, c, Unicolored(SchnyderColor::Blue));

    //println!("{:?}", Dot::with_config(&map, &[]));
    //println!("{}", map.is_simple());



}
