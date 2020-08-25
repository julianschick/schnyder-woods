use crate::graph::{PlanarMap, NbVertex, Edge, Signum, VertexI, Vertex, EdgeI, ClockDirection, FaceI};
use crate::graph::schnyder::SchnyderVertexType::{Suspension, Normal};
use itertools::{Itertools, Merge};
use crate::graph::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::graph::schnyder::SchnyderEdgeDirection::{Unicolored, Bicolored, Black};
use crate::graph::EdgeEnd::{Tail, Head};
use crate::graph::Signum::{Forward, Backward};
use std::collections::{HashSet, HashMap};
use array_tool::vec::Intersect;
use crate::graph::ClockDirection::{CCW, CW};
use std::io::{Split, empty};
use std::ops::{Deref, BitAnd};
use std::fmt::{Debug, Formatter};
use std::iter::FromIterator;
use core::fmt;
use crate::util::is_in_cyclic_order;
use std::slice::Iter;
use crate::util::iterators::cyclic::CyclicIterable;
use rand::{thread_rng, Rng};

pub mod algorithm;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SchnyderColor {
    Red, Green, Blue
}

impl SchnyderColor {
    pub fn to_tikz(&self) -> &str {
        match self {
            Red => "red", Green => "green", Blue => "blue"
        }
    }
}

impl IndexedEnum<SchnyderColor> for SchnyderColor {

    fn index(&self) -> usize {
        match self {
            Red => 0, Green => 1, Blue => 2
        }
    }

    fn from_index(index: usize) -> Self {
        match index {
            0 => Red, 1 => Green, 2 => Blue, _ => panic!("invalid index")
        }
    }

    fn number() -> usize {
        3
    }
}

pub trait IndexedEnum<T> {
    fn index(&self) -> usize;
    fn from_index(index: usize) -> T;
    fn number() -> usize;

    fn next(&self) -> T {
        Self::from_index((self.index() + 1) % Self::number())
    }

    fn prev(&self) -> T {
        Self::from_index((self.index() + (Self::number()-1)) % Self::number())
    }

    fn all() -> Vec<T> {
        (0..Self::number()).map(|i| Self::from_index(i)).collect_vec()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SchnyderVertexType {
    Normal(usize),
    Suspension(SchnyderColor)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SchnyderEdgeDirection {
    Black,
    Unicolored(SchnyderColor, Signum),
    Bicolored(SchnyderColor, SchnyderColor)
}

impl SchnyderEdgeDirection {
    pub fn is_unicolored(&self) -> bool {
        match self {
            Unicolored(_, _) => true,
            _ => false
        }
    }

    pub fn is_bicolored(&self) -> bool {
        match self {
            Bicolored(_, _) => true,
            _ => false
        }
    }
}

pub trait SchnyderVertex {
    fn get_type(&self) -> SchnyderVertexType;
    fn set_type(&mut self, t: SchnyderVertexType);
}

pub trait SchnyderEdge {
    fn get_direction(&self) -> SchnyderEdgeDirection;
    fn set_direction(&mut self, d: SchnyderEdgeDirection);
}

impl Vertex<SchnyderVertexType> {
    fn debug(&self) {
        print!("v[{}]: ", self.id.0);
        for nb in self.neighbors.iter() {
            print!("v[{}] . ", nb.other.0);
        }
        println!();
    }
}

impl Edge<SchnyderEdgeDirection> {

    fn debug(&self) {
        let tail_color = match self.weight {
            Black => None,
            Unicolored(c, Forward) => Some(c),
            Bicolored(c, _) => Some(c),
            _ => None
        };
        let head_color = match self.weight {
            Black => None,
            Unicolored(c, Backward) => Some(c),
            Bicolored(_, c) => Some(c),
            _ => None
        };

        println!("e[{}]: v[{}] ={:=<5}======={:=>5}=> v[{}] (L = f[{}], R = f[{}])",
           self.id.0,
           self.tail.0,
           match tail_color {
               Some(c) => format!("{:?}", c),
               _ => String::new()
           },
           match head_color {
               Some(c) => format!("{:?}", c),
               _ => String::new()
           },
           self.head.0,
           self.left_face.unwrap().0,
           self.right_face.unwrap().0
        )
    }
}

struct MergeData<'a> {
    v: &'a Vertex<SchnyderVertexType>,
    source_edge: &'a Edge<SchnyderEdgeDirection>,
    source_nb: &'a NbVertex,
    source_color: SchnyderColor,
    target_edge: &'a Edge<SchnyderEdgeDirection>,
    target_nb: &'a NbVertex,
    target_color: SchnyderColor,
    direction: ClockDirection
}

struct SplitData {
    hinge_vid: VertexI,
    target_vids: Vec<VertexI>,
    split_color: SchnyderColor,
    target_face: FaceI
}

pub struct SchnyderMap<F: Clone> {
    pub map: PlanarMap<SchnyderVertexType, SchnyderEdgeDirection, F>,
    //
    outer_face: FaceI,
    red_vertex: VertexI,
    green_vertex: VertexI,
    blue_vertex: VertexI
}

pub enum SchnyderBuildMode {
    LeftMost, Random, RightMost
}

impl<F: Clone> SchnyderMap<F> {

    pub fn from(map: PlanarMap<SchnyderVertexType, SchnyderEdgeDirection, F>) -> SchnyderMap<F> {

        let mut result = SchnyderMap {
            map,
            outer_face: FaceI(0),
            red_vertex: VertexI(0),
            green_vertex: VertexI(0),
            blue_vertex: VertexI(0)
        };

        if !result.init_wood() {
            panic!("wood initialization failed");
        }
        return result;
    }

    /// the suspension vertex with the lowest index gets red, the next one green, and the vertex
    /// with the hightest index gets blue.
    pub fn build_on_triangulation<E, N>(
        map: &PlanarMap<N, E, F>,
        outer_face: FaceI,
        mode: SchnyderBuildMode
    ) -> SchnyderMap<F> {

        if !map.is_embedded() {
            panic!("map has to have an embedding");
        }

        if !map.is_triangulation() {
            panic!("needs to be a triangulation");
        }

        let mut smap = map.clone_with_maps(
            |vertex_weight| Normal(0),
            |edge_weight| Black,
            Some(|face_weight| face_weight.clone())
        );

        // by convention
        let mut suspension_vertices = smap.face(outer_face).angles.iter()
            .sorted_by_key(|vid| vid.0);
        let r = *suspension_vertices.next().unwrap();
        let g = *suspension_vertices.next().unwrap();
        let b = *suspension_vertices.next().unwrap();

        // Color the suspension vertices
        smap.vertex_mut(r).weight = Suspension(Red);
        smap.vertex_mut(g).weight = Suspension(Green);
        smap.vertex_mut(b).weight = Suspension(Blue);

        // Color the outer face cycle
        for (&a, &b) in vec![r,g,b].cycle(0, true).tuple_windows() {
            let (e, signum) = smap.get_edge_with_signum(a, b);

            if let Suspension(color_a) = smap.vertex(a).weight {
                if let Suspension(color_b) = smap.vertex(b).weight {
                    match signum {
                        Backward => smap.edge_mut(e).weight = Bicolored(color_a, color_b),
                        Forward => smap.edge_mut(e).weight = Bicolored(color_b, color_a),
                    }
                }
            }
        }

        // Color incoming red edges of red suspension vertex
        for (eid, signum) in smap.vertex(r).between(b, g, CCW).iter().map(|nb|
            (nb.edge, match nb.end { Head => Forward, Tail => Backward})
        ).collect_vec() {
            smap.edge_mut(eid).weight = Unicolored(Red, signum);
        }

        let mut frontier = smap.vertex(r).between(b, g, CCW).iter()
            .map(|nb| nb.other)
            .collect_vec();

        for (&a,&b,&c) in  [b].iter().chain(frontier.iter()).chain([g].iter()).tuple_windows() {
            if smap.get_edge(a, b).is_none() {
                println!("{:?}-{:?} ab not existent", a, b);
            }
            if smap.get_edge(b, c).is_none() {
                println!("{:?}-{:?} bc not existent", a, b);
            }
        }

        println!("r = {:?}", r);
        println!("g = {:?}", g);
        println!("b = {:?}", b);
        let b_singleton = [b];
        let g_singleton = [g];

        while frontier.len() > 0 {
            for (&a,&b,&c) in [b].iter().chain(frontier.iter()).chain([g].iter()).tuple_windows() {
                if smap.get_edge(a, b).is_none() {
                    println!("{:?}-{:?} ab in loop not existent", a, b);
                }
                if smap.get_edge(b, c).is_none() {
                    println!("{:?}-{:?} bc in loop  not existent", a, b);
                }
            }

            let (pos, (&left_neighbour, &pivot, &right_neighbour)) = {
                let candidates: Vec<_> = b_singleton.iter().chain(frontier.iter()).chain(g_singleton.iter())
                    .tuple_windows()
                    .enumerate()
                    .filter(|(pos, (&left_neighbour, &pivot, &right_neighbour))| {
                        !smap.vertex(pivot).between(right_neighbour, left_neighbour, CW)
                            .iter().any(|nb| frontier.contains(&nb.other) || nb.other == g || nb.other == b)
                    }
                    ).collect();

                match mode {
                    SchnyderBuildMode::LeftMost => candidates.first().unwrap().clone(),
                    SchnyderBuildMode::RightMost => candidates.last().unwrap().clone(),
                    SchnyderBuildMode::Random => candidates[thread_rng().gen_range(0, candidates.len())].clone()
                }
            };

            let (eid_left, signum_left) = smap.get_edge_with_signum(pivot, left_neighbour);
            let (eid_right, signum_right) = smap.get_edge_with_signum(pivot, right_neighbour);

            smap.edge_mut(eid_left).weight = Unicolored(Blue, signum_left);
            smap.edge_mut(eid_right).weight = Unicolored(Green, signum_right);

            for (eid, signum) in smap.vertex(pivot).between(right_neighbour, left_neighbour, CW).iter().map(|nb|
                (nb.edge, match nb.end { Head => Forward, Tail => Backward})
            ).collect_vec() {
                smap.edge_mut(eid).weight = Unicolored(Red, signum);
            }

            let check_v = smap.vertex(pivot)
                .between(left_neighbour, right_neighbour, CCW)
                .iter().map(|nb| nb.other).collect_vec();

            if check_v.len()>0 {
                if smap.get_edge(left_neighbour, check_v[0]).is_none() {
                    println!("first problem");
                }
                if smap.get_edge(check_v[check_v.len() -1 ], right_neighbour).is_none() {
                    println!("last problem");
                }

                for (&a, &b) in check_v.iter().tuple_windows() {
                    if smap.get_edge(a,b).is_none() {
                        println!("mid problem");
                    }
                }
            }

            frontier.remove(pos);
            frontier.splice(pos..pos, smap.vertex(pivot)
                .between(left_neighbour, right_neighbour, CCW)
                .iter().map(|nb| nb.other).collect_vec());
        }

        println!("OK SO FAR");
        return SchnyderMap::from(smap);
    }

    pub fn debug(&self) {
        for v in self.map.vertices.get_map().values().sorted_by_key(|v| v.id.0) {
            v.debug();
        }

        for e in self.map.edges.get_map().values().sorted_by_key(|e| e.id.0) {
            e.debug();
        }

        /*for f in self.vertices.get_map().values() {
            println!("{:?}", f);
        }*/
    }

    fn color(&self, e: &Edge<SchnyderEdgeDirection>, sig: Signum) -> Option<SchnyderColor> {
        return match e.weight {
            Unicolored(c, s) => if sig == s { Some(c) } else { None }
            Bicolored(fwd_c, bwd_c) => match sig { Forward => Some(fwd_c), Backward => Some(bwd_c)},
            Black => None
        }
    }

    fn replace_color_nb(&self, nb: &NbVertex, color: SchnyderColor, sig: Signum) -> SchnyderEdgeDirection {
        let effective_signum = match nb.end {
            Tail => sig,
            Head => sig.reversed()
        };

        self.replace_color(self.map.edge(nb.edge), color, effective_signum)
    }

    fn replace_color(&self, e: &Edge<SchnyderEdgeDirection>, color: SchnyderColor, sig: Signum) -> SchnyderEdgeDirection {
        match e.weight {
            Black => Unicolored(color, sig),
            Unicolored(c, s) => if s == sig {
                Unicolored(color, sig)
            } else {
                match sig {
                    Forward => Bicolored(color, c),
                    Backward => Bicolored(c, color)
                }
            },
            Bicolored(fwd_c, bwd_c) => {
                match sig {
                    Forward => Bicolored(color, bwd_c),
                    Backward => Bicolored(fwd_c, color)
                }
            }
        }
    }

    fn find_outgoing(&self, v: &Vertex<SchnyderVertexType>, c: SchnyderColor) -> Option<usize> {
        v.neighbors.iter()
            .position(|nb| match self.outgoing_color(nb) { Some(cc) if cc == c => true, _ => false })
    }

    pub fn outgoing_color(&self, nb: &NbVertex) -> Option<SchnyderColor> {
        self.color(self.map.edge(nb.edge), match nb.end { Tail => Forward, Head => Backward})
    }

    pub fn incoming_color(&self, nb: &NbVertex) -> Option<SchnyderColor> {
        self.color(self.map.edge(nb.edge), match nb.end { Tail => Backward, Head => Forward})
    }

    pub fn get_angle_color(&self, fid: FaceI, vid: VertexI) -> SchnyderColor {
        if let Some((v, idx, nb1, nb2)) = self.map.get_knee_by_face(fid, vid) {
            if let Some(out_before) = self.outgoing_color(nb1) {
                return out_before.prev();
            } else if let Some(in_before) = self.incoming_color(nb1) {
                return in_before;
            } else {
                panic!("black edge!");
            }
        } else {
            panic!("no valid knee!");
        }
    }

    fn assemble_merge_data(&self, source: EdgeI, target: EdgeI) -> Option<MergeData> {
        let knee = self.map.get_knee(source, target);

        if let Some((v, nb1, nb2)) = knee {

            let source_edge = self.map.edge(source);
            let target_edge = self.map.edge(target);
            let direction = if source == nb1.edge { CW } else { CCW };
            let (source_nb, target_nb) = match direction {
                CW => (nb1, nb2),
                CCW => (nb2, nb1)
            };

            // both edges involved must be unicolored
            if !source_edge.weight.is_unicolored() || !target_edge.weight.is_unicolored() {
                return None;
            }

            let source_color = match self.outgoing_color(source_nb) {
                None => return None,
                Some(c) => c
            };
            let target_color = match self.incoming_color(target_nb) {
                None => return None,
                Some(c) => c
            };

            return Some(MergeData {
                v, source_edge, source_nb, source_color, target_edge, target_nb, target_color, direction
            });
        } else {
            return None;
        }
    }

    pub fn mergeable(&self, source: EdgeI, target: EdgeI) -> Option<ClockDirection> {
        self.assemble_merge_data(source, target).map(|d| d.direction)
    }

    pub fn merge(&mut self, source: EdgeI, target: EdgeI) -> ClockDirection {
        let s: *mut Self = self;
        let data = match self.assemble_merge_data(source, target) {
            Some(d) => d,
            None => panic!("not mergeable")
        };

        let dir = self.replace_color_nb(data.target_nb, data.source_color, Forward);

        unsafe {
            (*s).map.edge_mut(data.target_edge.id).weight = dir;
            (*s).map.remove_embedded_edge_by_id(data.source_edge.id, &(|a, b| a));
        }
        return data.direction;
    }

    fn assemble_split_data(&mut self, eid: EdgeI, direction: ClockDirection, target_vid: Option<VertexI>) -> Option<SplitData> {
        if let Bicolored(tail_color, head_color) = self.map.edge(eid).weight {
            let tail_split_dir = if tail_color.next() == head_color {
                CW
            } else if tail_color.prev() == head_color {
                CCW
            } else {
                panic!("invalid bicolored edge");
                return None;
            };

            let fid = match tail_split_dir {
                CW => self.map.edge(eid).right_face,
                CCW => self.map.edge(eid).left_face
            };

            let split_side = if tail_split_dir == direction { Tail } else { Head };
            let (split_color, constant_color) = match split_side {
                Tail => (tail_color, head_color),
                Head => (head_color, tail_color)
            };

            self.map.edge_mut(eid).weight = Unicolored(constant_color, match split_side {
                Tail => Backward,
                Head => Forward
            });

            let hinge_vid = self.map.edge(eid).get_vertex(split_side);

            return if let Some(tvid) = target_vid {
                let angle_color = self.get_angle_color(fid.unwrap(), tvid);

                if angle_color == split_color && tvid != hinge_vid && self.map.face(fid.unwrap()).angles.contains(&tvid) {
                    Some(SplitData {
                        hinge_vid,
                        target_vids: vec![tvid],
                        split_color,
                        target_face: fid.unwrap()
                    })
                } else {
                    panic!("vertex is not a good parking lot");
                    None
                }
            } else {
                let target_vids = self.map.face(fid.unwrap()).angles.iter()
                    .filter(|&&vid| self.get_angle_color(fid.unwrap(), vid) == split_color)
                    .filter(|&&vid| vid != hinge_vid)
                    .cloned().collect_vec();

                if target_vids.is_empty() {
                    panic!("no good parking lot!");
                    None
                } else {
                    Some(SplitData {
                        hinge_vid,
                        target_vids,
                        split_color,
                        target_face: fid.unwrap()
                    })
                }
            }

        } else {
            panic!("not bicolored");
        }
        return None;
    }

    pub fn splittable(&mut self, eid: EdgeI, direction: ClockDirection, target_vid: VertexI) -> bool {
        return match self.assemble_split_data(eid, direction, Some(target_vid)) {
            Some(_) => true, None => false
        }
    }

    pub fn splittable_to(&mut self, eid: EdgeI, direction: ClockDirection) -> Vec<VertexI> {
        return match self.assemble_split_data(eid, direction, None) {
            Some(data) => data.target_vids,
            None => Vec::new()
        }
    }

    pub fn split(&mut self, eid: EdgeI, direction: ClockDirection, target_vid: VertexI) {
        if let Some(data) = self.assemble_split_data(eid, direction, Some(target_vid)) {
            self.map.add_embedded_edge(data.hinge_vid, target_vid, Unicolored(data.split_color, Forward), data.target_face);
        } else {
            panic!("not splittable!");
        }
    }

    pub fn split_to_any(&mut self, eid: EdgeI, direction: ClockDirection) -> VertexI {
        if let Some(data) = self.assemble_split_data(eid, direction, None) {
            self.map.add_embedded_edge(data.hinge_vid, data.target_vids[0], Unicolored(data.split_color, Forward), data.target_face);
            return data.target_vids[0];
        } else {
            panic!("not splittable!");
        }
    }

    pub fn generate_tikz(&self, title: Option<&str>, face_labels: bool, face_counts: &HashMap<VertexI, (usize, usize, usize)>) -> String {
        let preamble = "\\documentclass[crop,tikz,border=10pt]{standalone}\\begin{document}\\tikzset{>=latex}\\usetikzlibrary{calc}\\begin{tikzpicture}[x=10mm, y=10mm]";
        let tail = "\\end{tikzpicture}\\end{document}";
        let mut mid = String::new();

        for v in self.map.vertices.get_map().values() {
            let (r, g, b) = face_counts.get(&v.id).unwrap();
            mid.extend(format!("\\coordinate ({}) at ({},{});", v.id.0, g, r).chars());
            mid.extend(format!("\\node at ({}) [above] {{${}$}};", v.id.0, v.id.0).chars());
        }

        for edge in self.map.edges.get_map().values() {
            mid.extend(match edge.weight {
                Black => format!("\\draw ({}) -- ({});", edge.head.0, edge.tail.0),
                Unicolored(color, signum) => match signum {
                    Forward => if face_labels {
                        format!("\\draw[->, {}, shorten >= 2pt, thick] ({}) -- node[auto, inner sep=0pt] {{{}}} node[auto, swap, inner sep=0pt] {{{}}} ({});", color.to_tikz(), edge.tail.0,  edge.left_face.unwrap().0,  edge.right_face.unwrap().0, edge.head.0)
                    } else {
                        format!("\\draw[->, {}, shorten >= 2pt, thick] ({}) -- ({});", color.to_tikz(), edge.tail.0, edge.head.0)
                    },
                    Backward => if face_labels {
                        format!("\\draw[->, {}, shorten >= 2pt, thick] ({}) --  node[auto, inner sep=0pt] {{{}}} node[auto, swap, inner sep=0pt] {{{}}} ({});", color.to_tikz(), edge.head.0, edge.right_face.unwrap().0, edge.left_face.unwrap().0, edge.tail.0)
                    } else {
                        format!("\\draw[->, {}, shorten >= 2pt, thick] ({}) -- ({});", color.to_tikz(), edge.head.0, edge.tail.0)
                    }
                }
                Bicolored(fwd_c, bwd_c) => {
                    let mid_point = format!("{}_{}", edge.tail.0, edge.head.0);

                    if face_labels {
                        format!("\\coordinate ({}) at ($({})!0.5!({})$) {{}};\\draw[->, {}, thick] ({}) -- node[auto, inner sep=0pt] {{{}}} node[auto, swap, inner sep=0pt] {{{}}} ({});\\draw[->, {}, thick] ({}) -- ({});",
                                mid_point, edge.tail.0, edge.head.0, fwd_c.to_tikz(), edge.tail.0, edge.left_face.unwrap().0,  edge.right_face.unwrap().0, mid_point, bwd_c.to_tikz(), edge.head.0, mid_point)
                    } else {
                        format!("\\coordinate ({}) at ($({})!0.5!({})$) {{}};\\draw[->, {}, thick] ({}) -- ({});\\draw[->, {}, thick] ({}) -- ({});",
                            mid_point, edge.tail.0, edge.head.0, fwd_c.to_tikz(), edge.tail.0, mid_point, bwd_c.to_tikz(), edge.head.0, mid_point)
                    }
                }
            }.chars());
        }

        for v in self.map.vertices.get_map().values() {
            let (r, g, b) = face_counts.get(&v.id).unwrap();
            mid.extend(format!("\\fill ({}) circle (2pt);", v.id.0).chars());
        }

        mid.extend(format!("\\draw[{},fill={}] ({}) circle (1pt);", Red.to_tikz(),  Red.to_tikz(), self.red_vertex.0).chars());
        mid.extend(format!("\\draw[{},fill={}] ({}) circle (1pt);", Green.to_tikz(),  Green.to_tikz(), self.green_vertex.0).chars());
        mid.extend(format!("\\draw[{},fill={}] ({}) circle (1pt);", Blue.to_tikz(),  Blue.to_tikz(), self.blue_vertex.0).chars());

        if let Some(title) = title {
            mid.extend(format!("\\node[yshift=2em, align=center, font=\\large\\bfseries] at (current bounding box.north) {{{}}};", title).chars());
        }

        return format!("{}{}{}", preamble, mid, tail);
    }

    fn random_face_counts(&self) -> HashMap<VertexI, (usize, usize, usize)> {
        let mut result = HashMap::new();
        let mut rand = thread_rng();
        for &vid in self.map.vertices.get_map().keys() {
            result.insert(vid, (rand.gen_range(0, 10), rand.gen_range(0, 10), rand.gen_range(0, 10)));
        }
        return result;
    }

    pub fn calculate_face_counts(&self) -> HashMap<VertexI, (usize, usize, usize)> {

        let number_of_faces = self.map.face_count() - 1;
        let (dual, _, edge_to_edge, face_to_vertex) = self.map.get_dual(true);

        let mut result = HashMap::new();

        for &vid in self.map.vertices.get_map().keys() {

            if let Suspension(color) = self.map.vertex(vid).weight {
                result.insert(vid, match color {
                    Red => (number_of_faces, 0, 0),
                    Green => (0, number_of_faces, 0),
                    Blue => (0, 0, number_of_faces)
                });
            } else {
                let mut primal_no_cross_edges = HashSet::new();
                let mut out_edge = [EdgeI(0); 3];
                let mut counts = [0; 3];

                for color in &[Red, Green, Blue] {
                    let path: Vec<_> = self.color_path(vid, *color)
                        .iter().tuple_windows()
                        .map(|(&a, &b)| self.map.get_edge(a, b).unwrap())
                        .collect();

                    out_edge[color.next().index()] = path[0];
                    primal_no_cross_edges.extend(path);
                }


                let mut dual_no_cross_edges: HashSet<_> = primal_no_cross_edges.iter().map(|eid|
                    *edge_to_edge.get(eid).unwrap()
                ).collect();

                for nb in &dual.vertex(*face_to_vertex.get(&self.outer_face).unwrap()).neighbors {
                    dual_no_cross_edges.insert(nb.edge);
                }

                for color in SchnyderColor::all() {
                    let e = self.map.edge(out_edge[color.index()]);
                    let f = match e.get_signum_by_tail(vid) {
                        Forward => e.left_face,
                        Backward => e.right_face
                    }.unwrap();

                    if f != self.outer_face {
                        let start_vertex = face_to_vertex.get(&f).unwrap();
                        counts[color.index()] = dual.connected_component(start_vertex, &dual_no_cross_edges).len();
                    } else {
                        counts[color.index()] = 0;
                    }
                }

                result.insert(vid, (counts[0], counts[1], counts[2]));
            }
        }

        return result;
    }

    fn color_path(&self, vid: VertexI, color: SchnyderColor) -> Vec<VertexI> {
        let mut path = Vec::new();
        let mut current_vertex = vid;

        while let Normal(_) = self.map.vertex(current_vertex).weight {
            if path.contains(&current_vertex) {
                panic!("cycle dected!");
            }

            path.push(current_vertex);

            let out_index = self.find_outgoing(self.map.vertex(current_vertex), color).unwrap();
            current_vertex = self.map.vertex(current_vertex).neighbors[out_index].other;
        }

        if let Suspension(c) = self.map.vertex(current_vertex).weight {
            if c == color {
                path.push(current_vertex);
                return path;
            } else {
                panic!("wrong suspension vertex reached");
            }
        } else {
            panic!("internal disagree")
        }
    }

    fn init_wood(&mut self) -> bool {

        let suspension_vertices = self.map.vertices.get_map().values().filter(|v|
            match v.weight {
                Suspension(c) => true,
                _ => false
            }
        ).collect_vec();

        if suspension_vertices.len() != 3 {
            return false;
        }

        let rgb = (
            suspension_vertices.iter().find(|&v| match v.weight { Suspension(Red) => true, _ => false}),
            suspension_vertices.iter().find(|&v| match v.weight { Suspension(Green) => true, _ => false}),
            suspension_vertices.iter().find(|&v| match v.weight { Suspension(Blue) => true, _ => false})
        );

        let (r,g,b) = match rgb {
            (Some(r_), Some(g_), Some(b_)) => (r_.id, g_.id, b_.id),
            _ => return false
        };

        self.red_vertex = r;
        self.green_vertex = g;
        self.blue_vertex = b;

        if let Some(outer_face) = self.map.find_outer_face(r, g, b) {
            self.outer_face = outer_face;
        } else {
            return false;
        }

        // check if every bicolored edge has two distinct colors
        if self.map.edges.get_map().values()
            .any(|nb| match nb.weight
                { Bicolored(a, b) if a == b => true, _ => false}) {
            return false;
        }

        // check no black edges
        if self.map.edges.get_map().values().any(|nb|match nb.weight { Black => true, _ => false}) {
            return false;
        }

        // check vertex rule
        for v in self.map.vertices.get_map().values() {
            let l = v.neighbors.len();

            let begin_sector = match v.weight {
                Normal(id) => Blue,
                Suspension(c) => c
            };

            match v.weight {
                Normal(id) => if l < 3 { return false },
                Suspension(c) => if l < 2 { return false }
            }

            let begin = self.find_outgoing(v, begin_sector.next());
            if begin.is_none() {
                return false
            }

            let mut i = (begin.unwrap() + 1) % l;
            let mut state = begin_sector; // inbound sector
            let mut last_outgoing = None;

            while i != begin.unwrap() {
                let nb = v.neighbors.get(i).unwrap();
                last_outgoing = self.outgoing_color(nb);

                match last_outgoing {
                    Some(c) => if c != state.prev() {
                        return false
                    } else {
                        state = state.next();
                    },
                    _ => match self.incoming_color(nb) {
                        Some(c) => if c != state {
                            return false
                        },
                        None => panic!("assertion failed")
                    }
                }

                i = (i + 1) % l;
            }

            match v.weight {
                Normal(id) => if state != begin_sector.prev() { return false },
                Suspension(c) => if state != begin_sector.next() || last_outgoing != Some(c.prev()) { return false }
            }
        }

        // check face cycle rule
        for f in self.map.faces.get_map().values() {
            let l = f.angles.len();
            let mut fwd_colors = HashSet::new();
            let mut bwd_colors = HashSet::new();

            for (&v1, &v2) in f.angles.cycle(0, true).tuple_windows() {

                if let None = self.map.get_edge(v1, v2) {
                    panic!("{} - {} invalid in face {}", v1.0, v2.0, f.id.0);
                }

                let e = self.map.edge(self.map.get_edge(v1, v2).unwrap());
                let signum = e.get_signum(v1, v2);

                fwd_colors.insert(self.color(e, signum));
                bwd_colors.insert(self.color(e, signum.reversed()));
            }

            if (fwd_colors.len() == 1 && !fwd_colors.iter().next().unwrap().is_none()) || (bwd_colors.len() == 1 && !bwd_colors.iter().next().unwrap().is_none()) {
                return false;
            }
        }

        return true;
    }
}

impl<F:Clone> PlanarMap<SchnyderVertexType, SchnyderEdgeDirection, F> {
    fn find_outer_face(&self, r: VertexI, g: VertexI, b: VertexI) -> Option<FaceI> {
        let outer_face_candidates = self.faces.get_map().values().filter(|&f|
            f.angles.contains(&r) &&
                f.angles.contains(&g) &&
                f.angles.contains(&b)
        ).collect_vec();

        if outer_face_candidates.len() < 1 || outer_face_candidates.len() > 2 {
            return None;
        }

        let outer_face = outer_face_candidates.iter().filter(|&&f|
            is_in_cyclic_order(&f.angles, &vec![r, g, b])
        ).collect_vec();

        return match outer_face.len() {
            1 => Some(outer_face[0].id),
            _ => None
        }
    }

}