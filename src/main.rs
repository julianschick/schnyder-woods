use std::collections::HashSet;
use std::cmp::Ordering;
use itertools::Itertools;

use crate::graph::{PlanarMap, VertexI};
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use crate::graph::schnyder::SchnyderEdgeDirection::{Bicolored, Unicolored, Black};
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::{SchnyderVertexType, SchnyderEdgeDirection, SchnyderMap, SchnyderEdge};
use crate::graph::Signum::Forward;
use crate::util::iterators::cyclic::CyclicIterable;
use crate::util::debug::Debug;
use crate::graph::ClockDirection::{CCW, CW};
use std::fs::File;
use std::sync::RwLock;
use std::io::Write;
use std::io::Read;
use std::process::Command;
use crate::graph::schnyder::SchnyderBuildMode::{LeftMost, RightMost, Random};
use crate::graph::io::{read_plantri_planar_code};
use crate::graph::schnyder::algorithm::make_contractable;
use chrono::Utc;
use petgraph::graph::Edge;


#[macro_use]
extern crate lazy_static;

mod graph;
mod util;

lazy_static! {
    static ref DEBUG: RwLock<Debug> = RwLock::new(Debug::new("/tmp/schnyder", "/tmp/schnyder/output"));
}

fn main() {

    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(1001), |i| i.0, |i| i.0, |i| i.0);

    //let mut i = 0;
    //for m in maps {

        let mut map = &maps[142];

        //println!("{:?}", map);

        let a = VertexI(3);
        let b = VertexI(9);

        let mut wood = SchnyderMap::build_on_triangulation(map, map.get_left_face(VertexI(0), VertexI(1)), LeftMost);
        DEBUG.write().unwrap().output(&wood, Some("The wood"), &wood.calculate_face_counts());

        let edge = wood.map.get_edge(a, b).unwrap();
        let seq = make_contractable(&mut wood, edge);

        println!("pre-contracted refint = {}", wood.map.check_referential_integrity());
        DEBUG.write().unwrap().output(&wood, Some("Uncontracted"), &wood.calculate_face_counts());
        let fc = wood.calculate_face_counts();

        wood.schnyder_contract(wood.map.get_edge(a, b).unwrap());

        println!("contracted refint = {}", wood.map.check_referential_integrity());
        DEBUG.write().unwrap().output(&wood, Some("Contracted"), &fc);
        DEBUG.write().unwrap().output(&wood, Some("Contracted"), &wood.calculate_face_counts());

        println!("{:?}", seq);

        //debug_output(wood, &format!("foo{}", i), Some(&format!("Number {}", i)));

        /*let mut map = PlanarMap::<SchnyderVertexType, SchnyderEdgeDirection, _>::new();

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
        ]);*/
        //map.contract_embedded_edge(trg, &(|e1, e2| *e1));

        /*let (dual, ..) = wood.map.get_dual(true);

        let mut schnyder_map = SchnyderMap::from(map);
        schnyder_map.debug();
        schnyder_map.merge(src, trg);
        //schnyder_map.split(trg, CW, g);

        println!("-----");
        println!("{:?}", dual);*/

      //  i += 1;
    //}


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
