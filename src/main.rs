use std::collections::HashSet;
use std::cmp::Ordering;
use itertools::Itertools;

use crate::graph::{PlanarMap, VertexI, Side};
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
use crate::graph::schnyder::algorithm::{make_contractible, make_inner_edge};
use chrono::Utc;
use petgraph::graph::Edge;
use crate::algorithm::{compute_contraction_candidates, find_sequence};
use crate::graph::EdgeEnd::{Tail, Head};

#[macro_use]
extern crate lazy_static;

mod graph;
mod util;
mod algorithm;

lazy_static! {
    static ref DEBUG: RwLock<Debug> = RwLock::new(Debug::new("/tmp/schnyder", "/tmp/schnyder/output"));
}

fn main() {
    main3();
}

fn main3() {
    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(1001), |i| i.0, |i| i.0, |i| i.0);

    let map = &maps[256];
    let mut wood = SchnyderMap::build_on_triangulation(map, map.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();

    DEBUG.write().unwrap().activate();
    let fc = wood.calculate_face_counts();
    DEBUG.write().unwrap().output("std", &wood, Some("The Wood"), &wood.calculate_face_counts());

    wood.ext_merge(VertexI(1), VertexI(7));
    wood.ext_merge(VertexI(0), VertexI(1));
    wood.ext_merge(VertexI(7), VertexI(0));
    wood.ext_merge(VertexI(2), VertexI(1));
    wood.ext_merge(VertexI(6), VertexI(0)).expect("hä?");

    DEBUG.write().unwrap().output("std", &wood, Some("Merged"), &wood.calculate_face_counts());

    wood.ext_split(VertexI(1), VertexI(7));

    DEBUG.write().unwrap().output("std", &wood, Some("Resplit"), &wood.calculate_face_counts());
}

fn main2() {
    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(1001), |i| i.0, |i| i.0, |i| i.0);

    let map1 = &maps[4];
    let map2 = &maps[8];

    let mut wood1 = SchnyderMap::build_on_triangulation(map1, map1.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();
    let mut wood2 = SchnyderMap::build_on_triangulation(map2, map2.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();

    //DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std",&wood1, Some("Wood1 (-1)"), &wood1.calculate_face_counts());
    DEBUG.write().unwrap().output("std",&wood2, Some("Wood2 (-1)"), &wood2.calculate_face_counts());
    DEBUG.write().unwrap().deactivate();

    let seq = find_sequence(&mut wood1, &mut wood2);

    DEBUG.write().unwrap().output("std",&wood1, Some("Final1 (-1)"), &wood1.calculate_face_counts());
    DEBUG.write().unwrap().output("std",&wood2, Some("Final2 (-1)"), &wood2.calculate_face_counts());

    /*let mut wood1 = SchnyderMap::build_on_triangulation(map1, map1.get_left_face(VertexI(0), VertexI(1)), LeftMost).unwrap();

    DEBUG.write().unwrap().output(&wood1, Some("Wood1 POST"), &wood1.calculate_face_counts());
    for op in seq {
        wood1.do_operation(&op);
        DEBUG.write().unwrap().output(&wood1, Some("Wood1 POST"), &wood1.calculate_face_counts());
    }*/

    let mut i = 0;
    DEBUG.write().unwrap().activate();
    DEBUG.write().unwrap().output("std",&wood1, Some(&format!("Step {}",i)), &wood1.calculate_face_counts());
    println!("{} operations", seq.len());
    for op in &seq {
        i += 1;
        wood1.do_operation(op).expect(&format!("first level operation {:?} execution failed!", op));
        DEBUG.write().unwrap().output("std",&wood1, Some(&format!("Step {}",i)), &wood1.calculate_face_counts());
    }

}

fn main1() {

    let mut file = File::open("/tmp/test.tri").unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data);

    let maps = read_plantri_planar_code(&data, Some(1001), |i| i.0, |i| i.0, |i| i.0);

    //let mut i = 0;
    //for m in maps {

        let mut map = &maps[123];

        //println!("{:?}", map);

        let (nr_a, nr_b) = (8, 9);
        let a = VertexI(nr_a);
        let b = VertexI(nr_b);

        let mut wood = SchnyderMap::build_on_triangulation(map, map.get_face(VertexI(0), VertexI(1), Side::Left), LeftMost).unwrap();
        DEBUG.write().unwrap().output("std",&wood, Some("The Wood"), &wood.calculate_face_counts());

        let edge = wood.map.get_edge(a, b).unwrap();
        let seq = make_contractible(&mut wood, edge);

        println!("pre-contracted refint = {}", wood.map.check_referential_integrity());
        DEBUG.write().unwrap().output("std",&wood, Some("Uncontracted"), &wood.calculate_face_counts());
        let fc = wood.calculate_face_counts();

        let contraction = wood.schnyder_contract(wood.map.get_edge(a, b).unwrap()).unwrap();
        println!("contraction = {:#?}", contraction);

        println!("contracted refint = {}", wood.map.check_referential_integrity());
        DEBUG.write().unwrap().output("std", &wood, Some("Contracted"), &fc);
        //DEBUG.write().unwrap().output(&wood, Some("Contracted w/ Updated Vertex Positions"), &wood.calculate_face_counts());

        //let edge_to_discontract = wood.find_outgoing_edge(contraction.retained_vertex, contraction.color).unwrap(); // should fail sometimes!!!
        wood.revert_schnyder_contraction(&contraction);

        DEBUG.write().unwrap().output("std", &wood, Some("Discontracted"), &fc);
        println!("discontracted refint = {}", wood.map.check_referential_integrity());

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
