use crate::graph::PlanarMap;
use crate::schnyder::SchnyderVertexType::{Suspension, Normal};
use itertools::{Itertools};
use crate::schnyder::SchnyderColor::{Red, Green, Blue};
use crate::schnyder::SchnyderEdgeDirection::{Unicolored, Bicolored};
use crate::graph::enums::EdgeEnd::{Tail, Head};
use crate::graph::enums::Signum::{Forward, Backward};
use std::collections::{HashSet, HashMap, VecDeque};
use crate::graph::enums::ClockDirection::{CCW, CW};
use std::fmt::{Debug};
use crate::util::is_in_cyclic_order;
use crate::util::iterators::cyclic::CyclicIterable;
use crate::util::iterators::cyclic::CyclicIterableByElement;
use rand::{thread_rng, Rng};
use crate::schnyder::algorithm::{make_contractible, Operation, Contraction, check_triangle};
use crate::DEBUG;
use std::convert::TryFrom;
use take_until::TakeUntilExt;
use bimap::BiMap;
use crate::arraytree::{ArrayTree, WalkAroundDirection};
use crate::graph::indices::{EdgeI, FaceI, VertexI};
use crate::graph::error::{IndexAccessError, GraphErr, GraphResult};
use crate::graph::enums::{Signum, ClockDirection, Side};
use crate::graph::data_holders::{NbVertex, Vertex, Face, Edge};

static INVALID_WOOD: &str = "Assertion failed, invalid Schnyder wood detected.";
static INTERNAL_ASSERTION: &str = "Internal assertion of class SchnyderMap failed.";

#[macro_export]
macro_rules! invalid_wood {
    () => { panic!(INVALID_WOOD) };
}

pub mod algorithm;
pub mod tikz;
pub mod io;
pub mod figures;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SchnyderColor {
    Red, Green, Blue
}

impl IndexedEnum<SchnyderColor> for SchnyderColor {

    fn index(&self) -> usize {
        match self {
            Red => 0, Green => 1, Blue => 2
        }
    }

    fn from_index(index: usize) -> Self {
        match index {
            0 => Red, 1 => Green, 2 => Blue, _ => panic!("Invalid index given for IndexedEnum<SchnyderColor>")
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

    pub fn reversed(&self) -> Self {
        match self {
            Unicolored(c, signum) => Unicolored(*c, signum.reversed()),
            Bicolored(a, b) => Bicolored(*b, *a)
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
    #[allow(dead_code)]
    fn debug(&self) {
        print!("v[{}]: ", self.id.0);
        for nb in self.neighbors.iter() {
            print!("v[{}] . ", nb.other.0);
        }
        println!();
    }
}

impl Edge<SchnyderEdgeDirection> {

    fn color(&self, sig: Signum) -> Option<SchnyderColor> {
        return match self.weight {
            Unicolored(c, s) => if sig == s { Some(c) } else { None }
            Bicolored(fwd_c, bwd_c) => match sig { Forward => Some(fwd_c), Backward => Some(bwd_c)},
        }
    }

    #[allow(dead_code)]
    fn debug(&self) {
        let tail_color = match self.weight {
            Unicolored(c, Forward) => Some(c),
            Bicolored(c, _) => Some(c),
            _ => None
        };
        let head_color = match self.weight {
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

impl Face<()> {
    #[allow(dead_code)]
    fn debug(&self) {
        print!("f[{}]: ", self.id.0);
        for angles in self.angles.iter() {
            print!("v[{}] . ", angles.0);
        }
        println!();
    }
}

struct MergeData<'a> {
    //hinge_vertex: &'a Vertex<SchnyderVertexType>,
    //source_edge: &'a Edge<SchnyderEdgeDirection>,
    //source_nb: &'a NbVertex,
    source_color: SchnyderColor,
    //target_edge: &'a Edge<SchnyderEdgeDirection>,
    target_nb: &'a NbVertex,
    //target_color: SchnyderColor,
}

struct SplitData {
    target_vids: Vec<VertexI>,
    split_color: SchnyderColor,
    remaining_color: SchnyderEdgeDirection,
    target_face: FaceI
}

pub struct SchnyderMap {
    pub map: PlanarMap<SchnyderVertexType, SchnyderEdgeDirection, ()>,
    //
    outer_face: FaceI,
    red_vertex: VertexI,
    green_vertex: VertexI,
    blue_vertex: VertexI
}

pub enum SchnyderBuildMode {
    LeftMost, Random, RightMost
}

impl SchnyderMap {

    pub fn debug(&self) {
        for e in self.map.edges() {
            e.debug();
        }

        for v in self.map.vertices() {
            v.debug();
        }

        for f in self.map.faces() {
            f.debug();
        }
    }

    pub fn build_simple_stack(vertex_count: usize, color: SchnyderColor) -> GraphResult<SchnyderMap> {
        if vertex_count < 3 {
            return GraphErr::new_err("Simple stacks cannot be constructed on less than 3 vertices.");
        }

        let mut map = PlanarMap::new();

        let r = map.add_vertex(Suspension(Red));
        let g = map.add_vertex(Suspension(Green));
        let b = map.add_vertex(Suspension(Blue));

        map.add_edge(r, g, Bicolored(Green, Red));
        map.add_edge(g, b, Bicolored(Blue, Green));
        map.add_edge(b, r, Bicolored(Red, Blue));

        let mut susp = HashMap::new();
        susp.insert(Red, r);
        susp.insert(Green, g);
        susp.insert(Blue, b);

        let mut faces = Vec::with_capacity(2 * vertex_count - 4);
        faces.push((vec![r, g, b], ())); // outer face

        let mut path = Vec::with_capacity(vertex_count - 2);
        path.push(*susp.get(&color).unwrap());
        for _ in 0..vertex_count-3 {
            path.push(map.add_vertex(Normal(0)))
        }

        for (a, b) in path.iter().tuple_windows() {
            map.add_edge(*b, *a, Unicolored(color, Forward));

            // left and right edge
            map.add_edge(*b,  *susp.get(&color.prev()).unwrap(), Unicolored(color.prev(), Forward));
            map.add_edge(*b,  *susp.get(&color.next()).unwrap(), Unicolored(color.next(), Forward));

            faces.push((vec![*b, *a, *susp.get(&color.prev()).unwrap()], ())); // left face
            faces.push((vec![*a, *b, *susp.get(&color.next()).unwrap()], ())); // right face
        }

        // base face
        faces.push((vec![*susp.get(&color.prev()).unwrap(), *susp.get(&color.next()).unwrap(), *path.last().unwrap()], ()));
        map.set_embedding_by_face_cycles(faces)?;

        return SchnyderMap::try_from(map);
    }

    pub fn build_min_edge_wood(vertex_count: usize, color: SchnyderColor) -> GraphResult<SchnyderMap> {
        if vertex_count < 3 {
            return GraphErr::new_err("Minimal edge woods cannot be constructed on less than 3 vertices.");
        }

        // comments are from the perspective of color=Green
        let n = vertex_count;
        let mut map = PlanarMap::new();

        let r = map.add_vertex(Suspension(color.prev()));
        let g = map.add_vertex(Suspension(color));
        let b = map.add_vertex(Suspension(color.next()));

        // 'upper' and 'lower' section of the outer face boundary
        let m = (n - 3) / 2;
        let mut upper = Vec::with_capacity(m + 2); upper.push(r);
        let mut lower = Vec::with_capacity(m + 2); lower.push(b);

        for _ in 0..m {
            upper.push(map.add_vertex(Normal(0)));
            lower.push(map.add_vertex(Normal(0)));
        }
        upper.push(g); lower.push(g);

        // outer edges
        for (v1, v2) in upper.iter().tuple_windows() {
            map.add_edge(*v1, *v2, Bicolored(color, color.prev()));
        }
        for (v1, v2) in lower.iter().tuple_windows() {
            map.add_edge(*v1, *v2, Bicolored(color, color.next()));
        }

        // non-outer edges
        for i in 1..=m {
            map.add_edge(lower[i], upper[i], Bicolored(color.prev(), color.next()));
        }

        // special vertex 'eve' for even case
        let eve = if n % 2 == 0 {
            Some(map.add_vertex(Normal(0)))
        } else {
            None
        };

        // left outer edge (or two edges in the even case)
        if let Some(eve) = eve {
            map.add_edge(b, eve, Bicolored(color.prev(), color.next()));
            map.add_edge(eve, r, Bicolored(color.prev(), color.next()));
            map.add_edge(eve, lower[1], Unicolored(color, Forward));
        } else {
            map.add_edge(b, r, Bicolored(color.prev(), color.next()));
        }

        let mut faces = Vec::new();

        // outer face
        let mut outer_face = upper.iter().cloned().collect_vec();
        outer_face.extend(lower.iter().rev().skip(1).collect_vec());
        if let Some(fill_vertex) = eve {
            outer_face.push(fill_vertex);
        }
        faces.push((outer_face, ()));

        // inner faces
        for i in 0..m {
            let mut face = vec![upper[i], lower[i], lower[i+1], upper[i+1]];
            if let Some(fill_vertex) = eve {
                if i == 0 {
                    face[1] = fill_vertex;
                    faces.push((vec![fill_vertex, b, lower[1]], ()));
                }
            }
            faces.push((face, ()));
        }

        // triangular face in the right bottom corner
        faces.push((vec![upper[m], lower[m], upper[m + 1]], ()));

        map.set_embedding_by_face_cycles(faces)?;
        return SchnyderMap::try_from(map);
    }

    pub fn build_from_3tree_code(code: &Vec<u8>) -> GraphResult<SchnyderMap> {
        if (code.len() % 3) != 0 {
            return GraphErr::new_err("Not a valid 3tree code, length not divisible by three");
        }
        if code.len() > 256*3 {
            return GraphErr::new_err("Not a valid 3tree code, too many bytes");
        }
        let n = code.len()  / 3;

        let trees = vec![
            ArrayTree::from_tree_code(&code[0..n])?,
            ArrayTree::from_tree_code(&code[n..2*n])?,
            ArrayTree::from_tree_code(&code[2*n..3*n])?
        ];

        let mut map: PlanarMap<_,SchnyderEdgeDirection,()> = PlanarMap::new();
        let mut vertex_indices = Vec::with_capacity(n);
        let colors = [Red, Green, Blue];

        // add all vertices
        for _ in 0..n {
            vertex_indices.push(map.add_vertex(Normal(0)));
        }

        // set suspension vertices
        for c in 0..3 {
            map.set_vertex_weight(vertex_indices[trees[c].get_root() as usize], Suspension(colors[c]))?;
        }

        for c in 0..3 {

            for v1 in trees[c].iter_dfs() {

                if let Some(v2) = trees[c].get_parent(v1) {
                    let (v1, v2) = (vertex_indices[v1 as usize], vertex_indices[v2 as usize]);

                    if let Ok((e, signum)) = map.edge_with_signum(v1, v2) {
                        match *map.edge_weight(e)? {
                            Bicolored(_, _) => return GraphErr::new_err("Invalid 3tree code"),
                            Unicolored(opposite_color, sig) => {
                                if sig == signum {
                                    return GraphErr::new_err("Invalid 3tree code");
                                } else {
                                    map.set_edge_weight(e, match signum {
                                        Forward => Bicolored(colors[c], opposite_color),
                                        Backward => Bicolored(opposite_color, colors[c])
                                    })?;
                                }
                            }
                        }
                    } else {
                        map.add_edge(v1, v2, Unicolored(colors[c], Forward));
                    }
                }
            }
        }

        let mut neighbor_orders = (0..n).map(|_| Vec::new()).collect_vec();

        // red outgoing
        for v in 0..n {
           if let Some(p) = trees[0].get_parent(v as u8) {
                neighbor_orders[v].push(p);
           }
        }

        // blue incoming
        for v in 0..n {
            let blue_inbound: HashSet<_> = trees[2].get_children(v as u8).collect();

            for other in trees[0].iter_walkaround(v as u8, WalkAroundDirection::HiToLo) {
                if blue_inbound.contains(&other) &&
                    trees[0].get_parent(v as u8) != Some(other) &&
                    trees[1].get_parent(v as u8) != Some(other) {

                    neighbor_orders[v].push(other);
                }
            }
        }

        // green outgoing
        for v in 0..n {
           if let Some(p) = trees[1].get_parent(v as u8) {
                neighbor_orders[v].push(p);
            }
        }

        // red incoming
        for v in 0..n {
            for &other in trees[0].get_children(v as u8) {
                if trees[1].get_parent(v as u8) != Some(other) &&
                    trees[2].get_parent(v as u8) != Some(other) {

                    neighbor_orders[v].push(other);
                }
            }
        }

        // blue outgoing
        for v in 0..n {
            if let Some(p) = trees[2].get_parent(v as u8) {
                neighbor_orders[v].push(p);
            }
        }

        // green incoming
        for v in 0..n {
            let green_inbound: HashSet<_> = trees[1].get_children(v as u8).collect();
            let mut sub_order = Vec::new();

            for other in trees[0].iter_walkaround(v as u8, WalkAroundDirection::LoToHi) {
                if green_inbound.contains(&other) &&
                    trees[0].get_parent(v as u8) != Some(other) &&
                    trees[2].get_parent(v as u8) != Some(other) {

                    sub_order.push(other);
                }
            }
            neighbor_orders[v].extend(sub_order.iter().rev());
        }

        let converted_nb_orders = neighbor_orders.iter().enumerate().map(|(i, nb)| {
            (
                vertex_indices[i],
                nb.iter().map(|j| vertex_indices[*j as usize]).collect_vec()
            )
        }).collect_vec();

        map.set_embedding_by_vertex_orders(converted_nb_orders, |_|())?;
        SchnyderMap::try_from(map)
    }

    /// the suspension vertex with the lowest index gets red, the next one green, and the vertex
    /// with the hightest index gets blue.
    pub fn build_on_triangulation<N, E>(
        map: &PlanarMap<N, E, ()>,
        outer_face: FaceI,
        mode: SchnyderBuildMode
    ) -> GraphResult<SchnyderMap> {

        if !map.is_embedded() {
            return GraphErr::new_err("Underlying planar map has to be embedded");
        }

        if !map.is_triangulation() {
            return GraphErr::new_err("Underlying planar map has to be a triangulation (i.e. every face including the outer face has to be a triangle)");
        }

        let mut smap = map.clone_with_maps(
            |_| Normal(0),
            |_| Unicolored(Red, Forward),
            Some(|face_weight| face_weight.clone())
        );

        // by convention
        let mut suspension_vertices = smap.try_face(outer_face)?.angles.iter()
            .sorted_by_key(|vid| vid.0);

        let r = *suspension_vertices.next().unwrap();
        let g = *suspension_vertices.next().unwrap();
        let b = *suspension_vertices.next().unwrap();

        // Color the suspension vertices
        smap.set_vertex_weight(r, Suspension(Red))?;
        smap.set_vertex_weight(g, Suspension(Green))?;
        smap.set_vertex_weight(b, Suspension(Blue))?;

        // Color the outer face cycle
        for (&a, &b) in vec![r,g,b].cycle(0, true).tuple_windows() {
            let (e, signum) = smap.edge_with_signum(a, b)?;

            if let Suspension(color_a) = smap.try_vertex(a)?.weight {
                if let Suspension(color_b) = smap.try_vertex(b)?.weight {
                    match signum {
                        Backward => smap.set_edge_weight(e, Bicolored(color_a, color_b))?,
                        Forward => smap.set_edge_weight(e, Bicolored(color_b, color_a))?
                    };
                }
            }
        }

        // Color incoming red edges of red suspension vertex
        for (eid, signum) in smap.try_vertex(r)?.sector_between(b, g, CCW).iter().map(|nb|
            (nb.edge, match nb.end { Head => Forward, Tail => Backward})
        ).collect_vec() {
            smap.set_edge_weight(eid, Unicolored(Red, signum))?;
        }

        //
        // execute the main part of the algorithm which moves the 'frontier' line away from the
        // red suspension vertex until no vertices are left 'beyond' the frontier.
        //
        let mut frontier = smap.try_vertex(r)?.sector_between(b, g, CCW).iter()
            .map(|nb| nb.other)
            .collect_vec();

        let b_singleton = [b];
        let g_singleton = [g];
        let mut rand = thread_rng();

        while frontier.len() > 0 {

            // find a pivot vertex at which the frontier can be pushed further
            let (pos, (&left_neighbour, &pivot, &right_neighbour)) = {
                let candidates: Vec<_> = b_singleton.iter().chain(frontier.iter()).chain(g_singleton.iter())
                    .tuple_windows()
                    .enumerate()
                    .filter(|(_, (&left_neighbour, &pivot, &right_neighbour))| {
                        !smap.try_vertex(pivot).unwrap().sector_between(right_neighbour, left_neighbour, CW)
                            .iter().any(|nb| frontier.contains(&nb.other) || nb.other == g || nb.other == b)
                    }
                    ).collect();

                assert!(!candidates.is_empty());

                match mode {
                    SchnyderBuildMode::LeftMost => candidates.first().unwrap().clone(),
                    SchnyderBuildMode::RightMost => candidates.last().unwrap().clone(),
                    SchnyderBuildMode::Random => candidates[rand.gen_range(0, candidates.len())].clone()
                }
            };

            // color the left and right connecting edge of the pivot vertex on the current frontier
            let (eid_left, signum_left) = smap.edge_with_signum(pivot, left_neighbour)?;
            let (eid_right, signum_right) = smap.edge_with_signum(pivot, right_neighbour)?;

            smap.set_edge_weight(eid_left, Unicolored(Blue, signum_left))?;
            smap.set_edge_weight(eid_right, Unicolored(Green, signum_right))?;

            // color the edges reaching from the pivot vertex to the new part of the frontier
            for (eid, signum) in smap.try_vertex(pivot)?.sector_between(right_neighbour, left_neighbour, CW).iter().map(|nb|
                (nb.edge, match nb.end { Head => Forward, Tail => Backward})
            ).collect_vec() {
                smap.set_edge_weight(eid, Unicolored(Red, signum))?;
            }

            // alter the frontier (remove the pivot vertex and add its neighbors beyond the old frontier)
            frontier.remove(pos);
            frontier.splice(pos..pos, smap.try_vertex(pivot)?
                .sector_between(left_neighbour, right_neighbour, CCW)
                .iter().map(|nb| nb.other).collect_vec());
        }

        return SchnyderMap::try_from(smap);
    }

    pub fn clone(&self) -> SchnyderMap {
        SchnyderMap {
            map: self.map.clone_with_maps(|v| *v, |e| *e, Some(|_|())),
            outer_face: self.outer_face,
            red_vertex: self.red_vertex,
            green_vertex: self.green_vertex,
            blue_vertex: self.blue_vertex
        }
    }

    pub fn get_vertex_map(&self, wood2: &SchnyderMap) -> GraphResult<BiMap<VertexI, VertexI>> {
        let wood1 = self;
        if wood1.map.vertex_count() != wood2.map.vertex_count() {
            return GraphErr::new_err("A vertex map can only be found between Schnyder woods of same vertex count");
        }

        DEBUG.write().unwrap().output("map", &wood1, Some("Wood1"), &wood1.calculate_face_counts());
        DEBUG.write().unwrap().output("map", &wood2, Some("Wood2"), &wood1.calculate_face_counts());

        let vertices1 = wood1.get_vertices_in_bfs_order(Red, CW);
        let vertices2 = wood2.get_vertices_in_bfs_order(Red, CW);
        let mut result = BiMap::new();

        for i in 0..vertices1.len() {
            result.insert(vertices1[i], vertices2[i]);
        }

        eprintln!("result = {:?}", result);

        return Ok(result)
    }

    fn get_vertices_in_bfs_order(&self, color: SchnyderColor, direction: ClockDirection) -> Vec<VertexI> {
        let mut queue = VecDeque::new();
        queue.push_back(self.get_suspension_vertex(color));
        let mut result = Vec::new();

        while let Some(first) = queue.pop_front() {
            result.push(first);
            match direction {
                CW => queue.extend( self.get_incoming_sector(first, color, true).into_iter()),
                CCW => queue.extend( self.get_incoming_sector(first, color, true).into_iter().rev()),
            }
        }

        return result;
    }

    pub fn compute_3tree_code(&self) -> Vec<u8> {
        self.compute_3tree_code_with_rotation(Red, CW)
    }

    pub fn compute_3tree_code_with_rotation(&self, color: SchnyderColor, direction: ClockDirection) -> Vec<u8> {
        if self.map.vertex_count() > 256 {
            panic!("geht nicht");
        }

        let mut result = Vec::with_capacity(self.map.vertex_count() * 3);

        let vertices = self.get_vertices_in_bfs_order(color, direction);
        let index_map: HashMap<_, _> = vertices.iter().enumerate().map(|(index, v)| (v, index)).collect();

        let cw = [color, color.next(), color.prev()];
        let ccw = [color, color.prev(), color.next()];

        let order = match direction {
            CW => &cw,
            CCW => &ccw
        };

        for c in order {
            for v in &vertices {
                let parent = if let Ok(parent_vertex) = self.find_outgoing_endvertex(*v, *c) {
                    *index_map.get(&parent_vertex).unwrap()
                } else {
                    *index_map.get(v).unwrap()
                } as u8;
                result.push(parent);
            }
        }

        return result;
    }

    pub fn is_outer_triangular(&self) -> bool {
        self.map.try_face(self.outer_face).expect("TODO").angles.len() == 3
    }

    fn replace_color_nb(&self, nb: &NbVertex, color: SchnyderColor, sig: Signum) -> GraphResult<SchnyderEdgeDirection> {
        let effective_signum = match nb.end {
            Tail => sig,
            Head => sig.reversed()
        };
        self.replace_color(nb.edge, color, effective_signum)
    }

    fn replace_color(&self, e: EdgeI, color: SchnyderColor, sig: Signum) -> GraphResult<SchnyderEdgeDirection> {
        Ok(match *self.map.edge_weight(e)? {
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
        })
    }

    fn find_outgoing_nb(&self, vid: VertexI, color: SchnyderColor) -> GraphResult<&NbVertex> {
        let result = self.map.try_vertex(vid)?
            .neighbors.iter()
            .find(|nb| match self.outgoing_color(nb) { Some(cc) if cc == color => true, _ => false });

        return if let Some(nb) = result {
            Ok(nb)
        } else {
            match self.map.vertex_weight(vid)? {
                Normal(_) => {

                    self.debug();
                    panic!("In a valid schnyder wood there should be an outgoing edge of each color at non-suspension vertices")
                },
                Suspension(c) if *c == color => return GraphErr::new_err("The suspension vertices have no outgoing edges of their very color"),
                Suspension(_) => panic!("In a valid schnyder wood there should be an outgoing edge of the other two colors at suspension vertices"),
            }
        }
    }

    pub fn find_outgoing_edge(&self, vid: VertexI, color: SchnyderColor) -> GraphResult<EdgeI> {
        self.find_outgoing_nb(vid, color).map(|nb| nb.edge)
    }

    pub fn find_outgoing_endvertex(&self, vid: VertexI, color: SchnyderColor) -> GraphResult<VertexI> {
        self.find_outgoing_nb(vid, color).map(|nb| nb.other)
    }

    // TODO; vertex id --- mitigate
    fn find_outgoing(&self, v: &Vertex<SchnyderVertexType>, c: SchnyderColor) -> Option<usize> {
        v.neighbors.iter()
            .position(|nb| match self.outgoing_color(nb) { Some(cc) if cc == c => true, _ => false })
    }

    fn get_incoming_sector_as_nb(&self, vid: VertexI, color: SchnyderColor, with_bicolored: bool) -> Vec<&NbVertex> {
        match self.map.vertex_weight(vid) {
            Ok(Suspension(c)) if *c != color => return vec![],
            _ => ()
        }

        let v = self.map.try_vertex(vid).unwrap(); //TODO unwrap
        let before_out = v.neighbors.iter().find(|nb| self.outgoing_color(nb) == Some(color.next())).expect("TODO");
        let after_out = v.neighbors.iter().find(|nb| self.outgoing_color(nb) == Some(color.prev())).expect("TODO");

        return if !with_bicolored {
            v.nb_sector_between(before_out, after_out, CW)
        } else {
            let mut tmp = v.nb_sector_including(before_out, after_out, CW);
            assert!(tmp.len() >= 2);

            if self.incoming_color(tmp[0]) != Some(color) {
                tmp.remove(0);
            }

            if self.incoming_color(tmp.last().unwrap()) != Some(color) {
                tmp.pop();
            }

            tmp
        }


    }

    fn get_incoming_sector_as_edges(&self, vid: VertexI, color: SchnyderColor, with_bicolored: bool) -> Vec<EdgeI> {
        self.get_incoming_sector_as_nb(vid, color, with_bicolored).iter().map(|nb| nb.edge).collect()
    }

    pub fn get_incoming_sector(&self, vid: VertexI, color: SchnyderColor, with_bicolored: bool) -> Vec<VertexI> {
        self.get_incoming_sector_as_nb(vid, color, with_bicolored).iter().map(|nb| nb.other).collect()
    }

    pub fn outgoing_color(&self, nb: &NbVertex) -> Option<SchnyderColor> {
        self.map.try_edge(nb.edge).unwrap()
            .color(match nb.end { Tail => Forward, Head => Backward})
    }

    pub fn incoming_color(&self, nb: &NbVertex) -> Option<SchnyderColor> {
        self.map.try_edge(nb.edge).unwrap()
            .color(match nb.end { Tail => Backward, Head => Forward})
    }

    pub fn get_color(&self, v1: VertexI, v2: VertexI) -> GraphResult<SchnyderEdgeDirection> {
        self.map.get_color(v1, v2)
    }

    pub fn get_color_orientation(&self, e: EdgeI) -> GraphResult<Option<(SchnyderColor, VertexI, VertexI)>> {
        Ok(if let Unicolored(color, signum) = *self.map.edge_weight(e)? {
            let (a, b) = self.map.edge_pair(e, signum)?;
            Some((color, a, b))
        } else {
            None
        })
    }

    pub fn get_suspension_vertex(&self, color: SchnyderColor) -> VertexI {
        return match color {
            Red => self.red_vertex,
            Green => self.green_vertex,
            Blue => self.blue_vertex
        }
    }

    pub fn get_inner_vertices(&self) -> Vec<VertexI> {
        self.map.vertices()
            .filter(|v| match v.weight {
                Normal(_) => true,
                _ => false
            })
            .map(|v| v.id)
            .collect()
    }

    pub fn is_inner_edge(&self, e: EdgeI) -> GraphResult<bool> {
        let e = self.map.try_edge(e)?;
        let weights = vec![self.map.vertex_weight(e.tail)?, self.map.vertex_weight(e.head)?];
        return Ok(!weights.iter().any(|w| if let Suspension(_) = w { true } else { false }));
    }

    pub fn get_angle_color(&self, fid: FaceI, vid: VertexI) -> GraphResult<SchnyderColor> {
        let (_, _, nb1, _) = self.map.get_knee_by_face(fid, vid)?;
        if let Some(out_before) = self.outgoing_color(nb1) {
            return Ok(out_before.prev());
        } else if let Some(in_before) = self.incoming_color(nb1) {
            return Ok(in_before);
        }

        invalid_wood!();
    }

    fn assemble_merge_data(&self, source: EdgeI, target: EdgeI) -> GraphResult<MergeData> {
        let knee = self.map.get_knee(source, target);

        if let Ok((_hinge_vertex, nb1, nb2)) = knee {

            let source_edge = self.map.try_edge(source)?;
            let target_edge = self.map.try_edge(target)?;
            let (source_nb, target_nb) = if source == nb1.edge { (nb1, nb2) } else { (nb2, nb1) };

            if !source_edge.weight.is_unicolored() {
                return GraphErr::new_err("Source edge for merge is not unicolored");
            }
            if !target_edge.weight.is_unicolored() {
                return GraphErr::new_err("Target edge for merge is not unicolored");
            }

            let source_color = match self.outgoing_color(source_nb) {
                None => return GraphErr::new_err("Source edge for merge must be directed away from hinge"),
                Some(c) => c
            };
            match self.incoming_color(target_nb) {
                None => return GraphErr::new_err("Target edge for merge must be directed towards hinge"),
                Some(c) => c
            };

            Ok(MergeData {
                /*hinge_vertex, source_edge, source_nb,*/ source_color, /*target_edge,*/ target_nb, /*target_color*/
            })
        } else {
            GraphErr::new_err("The source edge is not mergeable onto the target edge, as they do not share a common angle")
        }
    }

    pub fn mergeable(&self, source: EdgeI, target: EdgeI) -> GraphResult<()> {
        self.assemble_merge_data(source, target).map(|_| ())
    }

    pub fn merge(&mut self, source: EdgeI, target: EdgeI) -> GraphResult<Operation> {

        let dir = {
            let data = self.assemble_merge_data(source, target)?;
            self.replace_color_nb(data.target_nb, data.source_color, Forward)?
        };

        let source_as_pair = self.map.edge_pair(source, Forward)?;
        let target_as_pair = self.map.edge_pair(target, Forward)?;

        self.map.set_edge_weight(target, dir)?;
        self.map.remove_embedded_edge_by_index(source, &(|a, _| a))?;

        Ok(Operation::merge(
            source_as_pair,
            target_as_pair
        ))
    }

    fn assemble_split_data(&self, eid: EdgeI, hinge_vid: VertexI, target_vid: Option<VertexI>) -> GraphResult<SplitData> {
        if !self.map.edge_contains(&eid, &hinge_vid) {
            return GraphErr::new_err("Hinge vertex not part of given edge");
        }

        let e = self.map.try_edge(eid)?;
        if let Bicolored(tail_color, head_color) = e.weight {

            let hinge_end = if e.tail == hinge_vid { Tail } else { Head };
            let target_face = if tail_color.next() == head_color {
                self.map.try_edge(eid)?.right_face
            } else if tail_color.prev() == head_color {
                self.map.try_edge(eid)?.left_face
            } else {
                panic!("TODO");

            }.unwrap();

            if target_face == self.outer_face {
                return GraphErr::new_err("Splits into the outer face are not possible");
            }

            let (split_color, constant_color) = match hinge_end {
                Tail => (tail_color, head_color),
                Head => (head_color, tail_color)
            };

            let target_vids = self.map.try_face(target_face)?.angles.iter()
                .filter(|&&vid| self.get_angle_color(target_face, vid).unwrap() == split_color)
                .filter(|&&vid| vid != hinge_vid)
                .cloned().collect_vec();

            if let Some(tvid) = target_vid {
                if !target_vids.contains(&tvid) {
                    return GraphErr::new_err("Target vertex is not a valid target for this split");
                }
            }

            assert!(!target_vids.is_empty());

            Ok(SplitData {
                target_vids,
                split_color,
                remaining_color: match hinge_end { Tail => Unicolored(constant_color, Backward), Head => Unicolored(constant_color, Forward)},
                target_face
            })

        } else {
            return GraphErr::new_err("Edge is not splittable as it is not bicolored");
        }
    }

    pub fn splittable(&self, eid: EdgeI, hinge_vid: VertexI, target_vid: Option<VertexI>) -> GraphResult<()> {
        self.assemble_split_data(eid, hinge_vid, target_vid)
            .map(|_| ())
    }

    pub fn find_split_target(&self, eid: EdgeI, hinge_vid: VertexI) -> GraphResult<Vec<VertexI>> {
        self.assemble_split_data(eid, hinge_vid, None)
            .map(|data| data.target_vids)
    }

    pub fn split(&mut self, eid: EdgeI, hinge_vid: VertexI, target_vid: Option<VertexI>) -> GraphResult<Operation> {
        let data = self.assemble_split_data(eid, hinge_vid, target_vid)?;
        let effective_target = match target_vid {
            Some(target_vid) => target_vid,
            _ => data.target_vids[0]
        };
        self.map.add_embedded_edge(hinge_vid, effective_target, Unicolored(data.split_color, Forward), data.target_face)?;
        self.map.set_edge_weight(eid, data.remaining_color)?;

        Ok(Operation::split(hinge_vid, self.map.edge_opposite_vertex(eid,hinge_vid)?, effective_target))
    }

    pub fn split_any(&mut self) -> bool {
        let admissible_ops = self.get_admissible_ops().expect("TODO");
        let split = admissible_ops.iter().find(|op| op.is_upwards());

        return if let Some(split) = split {
            if let Err(e) = self.do_operation(split) {
                panic!("Internal assertion failed: {}", e);
            }
            true
        } else {
            false
        }
    }

    pub fn merge_and_resplit(&mut self, source: EdgeI, target: EdgeI, resplit_target: Option<VertexI>) -> GraphResult<Vec<Operation>> {
        let merge_op = self.merge(source, target)?;
        let split_hinge = self.map.edge_opposite_vertex(target,merge_op.hinge_vertex)?;
        let split_op = self.split(target, split_hinge, resplit_target)?;
        Ok(vec![merge_op, split_op])
    }

    fn ext_splittable_(&self, hinge: VertexI, opposite: VertexI) -> GraphResult<(VertexI, VertexI, SchnyderColor, SchnyderColor)> {

        let angles = &self.map.try_face(self.outer_face)?.angles;

        // the two hinges must be part of the outer face cycle
        if !angles.contains(&hinge) || !angles.contains(&opposite) {
            return GraphErr::new_err("The hinges for an external split must lie on the outer face cycle.");
        }

        {
            let between: Vec<_> = angles.cycle_by_element(&hinge, false).skip(1).take_while(|&&v| v != opposite).collect();

            // there must not be a suspension vertex between the two hinges
            if between.iter().any(|v| match self.map.vertex_weight(**v).unwrap() { Suspension(_) => true, _ => false }) {
                return GraphErr::new_err("The hinges for an must not enclose a suspension vertex.");
            }

            if let (Some(&hinge_other), Some(&opposite_other)) = (between.first(), between.last()) {
                let hinge_orientation = self.get_color(hinge, *hinge_other)?;
                let opposite_orientation = self.get_color(opposite, *opposite_other)?;

                return if let (Bicolored(hinge_color, hinge_color_bwd),  Bicolored(opposite_color, opposite_color_bwd)) = (hinge_orientation, opposite_orientation) {
                    if hinge_color != opposite_color || hinge_color_bwd != opposite_color || opposite_color_bwd != hinge_color {
                        Ok((*hinge_other, *opposite_other, hinge_color, opposite_color))
                    } else {
                        panic!("Invalid Schnyder wood detected: External split of strangely colored edges.");
                    }
                } else {
                    GraphErr::new_err("Both split edges for an external split must be bicolored.")
                }
            } else {
                return GraphErr::new_err("The hinges for an external split must enclose more than one edge.");
            }
        }
    }

    pub fn ext_splittable(&self, hinge: VertexI, opposite: VertexI) -> GraphResult<()> {
        self.ext_splittable_(hinge, opposite).map(|_| ())
    }

    pub fn ext_split(&mut self, hinge: VertexI, opposite: VertexI) -> GraphResult<Operation> {
        let (hinge_other, opposite_other, hinge_color, opposite_color) = self.ext_splittable_(hinge, opposite)?;
        let new_edge = self.map.add_embedded_edge(hinge, opposite, Bicolored(hinge_color, opposite_color), self.outer_face)?;

        let (e1, signum1) = self.map.edge_with_signum(hinge_other, hinge)?;
        self.map.set_edge_weight(e1,  Unicolored(opposite_color, signum1))?;
        let (e2, signum2) = self.map.edge_with_signum(opposite_other, opposite)?;
        self.map.set_edge_weight(e2, Unicolored(hinge_color, signum2))?;

        self.outer_face = self.map.try_edge(new_edge)?.left_face.unwrap();

        Ok(Operation::ext_split(hinge, opposite))
    }

    fn ext_mergeable_(&self, hinge: VertexI, opposite: VertexI) -> GraphResult<(SchnyderColor, SchnyderColor, VertexI, VertexI)> {

        if hinge == opposite {
            return GraphErr::new_err("The hinges must be distinct.");
        }

        let angles = &self.map.try_face(self.outer_face)?.angles;

        // the two hinges must be part of the outer face cycle
        if !angles.contains(&hinge) || !angles.contains(&opposite) {
            return GraphErr::new_err("The hinges for an external merge must lie on the outer face cycle.");
        }

        match angles.cycle_by_element(&hinge, false).nth(1) {
            Some(v) if *v == opposite => {
                if let Bicolored(fwd, bwd) = self.get_color(hinge, opposite)? {
                    let hinge_target_nb = self.map.next_nb(hinge, opposite, CW)?;
                    let opposite_target_nb = self.map.next_nb(opposite, hinge, CCW)?;

                    if let (
                        Unicolored(hinge_color, Backward),
                        Unicolored(opposite_color, Backward)
                    ) = (
                        self.get_color(hinge, hinge_target_nb)?,
                        self.get_color(opposite, opposite_target_nb)?
                    ) {
                        return if hinge_color == bwd && opposite_color == fwd {
                            Ok((fwd, bwd, hinge_target_nb, opposite_target_nb))
                        } else {
                            GraphErr::new_err("Target edges do not have the right colors for an external merge.")
                        }
                    } else {
                        GraphErr::new_err("Target edges do not have the right colors for an external merge.")
                    }

                } else {
                    return GraphErr::new_err("Unidirected edges cannot be subject to an external merge.");
                }
            }
            _ => return GraphErr::new_err("The hinges for an external merge must be neighbors.")
        }
    }

    pub fn ext_mergeable(&self, hinge: VertexI, opposite: VertexI) -> GraphResult<()> {
        self.ext_mergeable_(hinge, opposite).map(|_| ())
    }

    pub fn ext_merge(&mut self, hinge: VertexI, opposite: VertexI) -> GraphResult<Operation> {
        let (color1, color2, vertex1, vertex2) = self.ext_mergeable_(hinge, opposite)?;

        let (_, new_face) = self.map.remove_embedded_edge(hinge, opposite, &|a, _| a)?;
        self.outer_face = new_face;

        let (e1, signum1) = self.map.edge_with_signum(hinge, vertex1)?;
        let e1_new_colors = self.replace_color(e1, color1, signum1)?;
        let (e2, signum2) = self.map.edge_with_signum(opposite, vertex2)?;
        let e2_new_colors = self.replace_color(e2, color2, signum2)?;

        self.map.set_edge_weight(e1, e1_new_colors)?;
        self.map.set_edge_weight(e2, e2_new_colors)?;

        Ok(Operation::ext_merge(hinge, opposite))
    }

    // TODO this should not be a result but a plain vector
    pub fn get_admissible_ops(&self) -> GraphResult<Vec<Operation>> {

        let mut result = Vec::new();
        let outer_cycle : Vec<_> = self.map.try_face(self.outer_face)?.angles.iter().cloned().collect();

        for v in self.map.vertices().sorted_by_key(|v| v.id.0) {

            if let Normal(_) = v.weight {

                // collect all splits and merges at v
                for c in &[Red, Green, Blue] {
                    let source = self.find_outgoing_nb(v.id, *c)?;

                    let w = *self.map.edge_weight(source.edge)?;
                    if let Unicolored(_, _) = w {
                        let target1 = self.map.try_vertex(v.id)?.next(source, CW);
                        let target2 = self.map.try_vertex(v.id)?.next(source, CCW);

                        if self.mergeable(source.edge, target1.edge).is_ok() {
                            result.push(Operation::merge_by_vertices(v.id, source.other, target1.other));
                        }
                        if self.mergeable(source.edge, target2.edge).is_ok() {
                            result.push(Operation::merge_by_vertices(v.id, source.other, target2.other));
                        }
                    } else if let Bicolored(_, _) = w {
                        if let Ok(targets) = self.find_split_target(source.edge, v.id) {
                            for target in targets {
                                result.push(Operation::split(v.id, source.other, target));
                            }
                        }
                    }
                }
            }
        }

        for c in &[Red, Green, Blue] {
            let begin = &self.get_suspension_vertex(*c);
            let end = &self.get_suspension_vertex(c.next());
            let sector = outer_cycle.cycle_by_element(begin, false).take_until(|v| *v == end).collect_vec();

            for x in 0..sector.len() {
                for y in (x+1)..sector.len() {
                    if self.ext_mergeable(*sector[x], *sector[y]).is_ok() {
                        result.push(Operation::ext_merge(*sector[x], *sector[y]));
                    }

                    if self.ext_splittable(*sector[x], *sector[y]).is_ok() {
                        result.push(Operation::ext_split(*sector[x], *sector[y]));
                    }
                }
            }
        }


        return Ok(result);
    }


    pub fn is_schnyder_contractible(&self, eid: EdgeI) -> GraphResult<()> {
        if !self.is_inner_edge(eid)? {
            return GraphErr::new_err("Only inner edges can be schnyder contractible.");
        }

        return match self.map.edge_weight(eid)? {
            Unicolored(_, _) => {
                check_triangle(self, eid, Side::Left)?;
                check_triangle(self, eid, Side::Right)?;
                Ok(())
            },
            _ => GraphErr::new_err("Bicolored edges are not schnyder contractible")
        }
    }

    pub fn schnyder_contract(&mut self, eid: EdgeI) -> GraphResult<Contraction> {
        self.is_schnyder_contractible(eid)?;

        if let Unicolored(color, signum) = *self.map.edge_weight(eid)? {
            let (retained_vertex, dropped_vertex, dropped_edge) = self.map.contract_embedded_edge(
                eid,
                match signum {
                    Forward => Tail,
                    Backward => Head
                }
            )?;

            Ok(Contraction {
                retained_vertex,
                color,
                color_orientation: signum,
                dropped_vertex,
                dropped_edge
            })
        } else {
            assert!(false); panic!();
        }
    }

    pub fn revert_schnyder_contraction(&mut self, contraction: &Contraction) -> GraphResult<(VertexI, EdgeI)> {
        // last parameter: a new edge index is requested, as the old one might have been turned unavailable by intermediate operations
        self.schnyder_uncontract(contraction.retained_vertex, contraction.color, Some(contraction.dropped_vertex), None)
    }

    pub fn schnyder_uncontract(&mut self, pivot_vid: VertexI, color: SchnyderColor, new_vertex_index: Option<VertexI>, new_edge_index: Option<EdgeI>) -> GraphResult<(VertexI, EdgeI)> {
        let eid = self.find_outgoing_edge(pivot_vid, color)?;

        let (new_end, old_weight) = {
            let e = self.map.try_edge(eid)?;
            assert!(e.head == pivot_vid || e.tail == pivot_vid);
            (if e.head == pivot_vid { Head } else { Tail }, e.weight)
        };

        // by the vertex rule for schnyder woods
        assert!(self.map.try_vertex(pivot_vid)?.neighbors.len() >= 3);

        //patch borders
        let border_ccw = self.find_outgoing_endvertex(pivot_vid,color.prev()).unwrap();
        let border_cw = self.find_outgoing_endvertex(pivot_vid, color.next()).unwrap();

        let (new_vid, _) = self.map.split_embedded_edge( // TODO maybe move the called function here
            eid, new_end,
            Some((border_ccw, border_cw)),
            new_vertex_index,
            new_edge_index,
            Normal(0),
            old_weight
        ).unwrap();//TODO: unwrap

        // moved sector
        let prev_eid = self.find_outgoing_edge(pivot_vid, color.prev()).unwrap();
        let next_eid = self.find_outgoing_edge(pivot_vid, color.next()).unwrap();
        let prev_vid = self.map.edge_opposite_vertex(prev_eid, pivot_vid)?;
        let next_vid = self.map.edge_opposite_vertex(next_eid,pivot_vid)?;

        //let cw_nb = self.map.vertex(pivot_vid).next_nb(new_vid, CW).other;
        //let ccw_nb = self.map.vertex(pivot_vid).next_nb(new_vid, CCW).other;

        /*println!("cw_face = {}", cw_face.0);
        println!("ccw_face = {}", ccw_face.0);
        println!("pivot = {}", pivot_vid.0);
        eprintln!("cw_nb = {:?}", cw_nb);
        eprintln!("ccw_nb = {:?}", ccw_nb);*/

        let prev_face = self.map.get_face(pivot_vid, prev_vid, Side::Right);
        let next_face = self.map.get_face(pivot_vid, next_vid, Side::Left);

        /*let (prev_face, next_face) = {
            let new_e = self.map.edge(new_eid);
            (
                match new_end { Head => new_e.left_face, Tail => new_e.right_face }.unwrap(),
                match new_end { Head => new_e.right_face, Tail => new_e.left_face }.unwrap(),
            )
        };*/

        //eprintln!("prev_face = {:?}", prev_face);
        //eprintln!("next_face = {:?}", next_face);

        //println!("{:#?}", self.map.face(prev_face).angles);
        //println!("{:#?}", self.map.face(next_face).angles);

        //println!("{} -> {} in {}", new_vid.0, prev_vid.0, prev_face.0);
        //println!("{} -> {} in {}", new_vid.0, next_vid.0, next_face.0);

        if let Unicolored(color, _) = old_weight {
            self.map.add_embedded_edge(new_vid, prev_vid, Unicolored(color.prev(), Forward), prev_face)?;
            self.map.add_embedded_edge(new_vid, next_vid, Unicolored(color.next(), Forward), next_face)?;
        } else {
            panic!("undiscontractible edge");
        }

        return Ok((pivot_vid, eid));
    }

    pub fn swap_locally(&mut self, a: &VertexI, b: &VertexI) -> Result<Vec<Operation>, GraphErr> {
        if !self.map.is_triangulation() {
            return GraphErr::new_err("Swaps can only be done on triangulations");
        }

        let eid = self.map.get_edge(*a, *b)?;
        if !self.is_inner_edge(eid)? {
            return GraphErr::new_err("Edge for swap has to be an inner edge");
        }

        if let Some((color, tail, head)) = self.get_color_orientation(eid)? {

            let leading_seq = make_contractible(self, eid)?;
            let mut mid_seq = Vec::new();
            //DEBUG.write().unwrap().output("std", &self, Some("Made contractible"), &self.calculate_face_counts());

            // following comments:
            // colors referring to the example with the pivot edge being red
            //
            // incoming red edges at tail over to head
            for tail_incoming_red in self.get_incoming_sector_as_edges(tail, color, false) {

                let source = self.find_outgoing_edge(tail, color.next()).unwrap();
                let target = tail_incoming_red;

                mid_seq.extend(self.merge_and_resplit(source, target, Some(head))?);
                //DEBUG.write().unwrap().output("std", &self, Some("Red incoming over to head"), &self.calculate_face_counts());
            }

            // outgoing blue edge at tail merged onto pivot edge
            let out_prev = self.find_outgoing_edge(head, color.prev()).unwrap();
            mid_seq.extend(self.merge_and_resplit(out_prev, eid, None)?);
            //DEBUG.write().unwrap().output("std", &self, Some("Blue down"), &self.calculate_face_counts());

            // incoming green edges at head over to tail
            for head_incoming_green in self.get_incoming_sector_as_edges(head, color.next(), false) {
                let target = self.find_outgoing_edge(tail, color).unwrap();
                let source = head_incoming_green;

                mid_seq.extend(self.merge_and_resplit(source, target, None)?);
                //DEBUG.write().unwrap().output("std",&self, Some("Green incoming over to tail"), &self.calculate_face_counts());
            }

            // split red part away from pivot edge (leaving it green)
            {
                let source = self.find_outgoing_edge(tail, color.next()).unwrap();
                let target = eid;

                mid_seq.extend(self.merge_and_resplit(source, target, None)?);
                //DEBUG.write().unwrap().output("std",&self, Some("Blue -> Green"), &self.calculate_face_counts());
            }

            // incoming blue edges at head over to tail
            for blue_incoming_edge in self.get_incoming_sector_as_edges(head, color.prev(), false) {
                let source = self.find_outgoing_edge(head, color).unwrap();
                let target = blue_incoming_edge;

                mid_seq.extend(self.merge_and_resplit(source, target, None)?);
                //DEBUG.write().unwrap().output("std",&self, Some("Blue incoming over to tail"), &self.calculate_face_counts());
            }

            // pivot edge is green and turns red
            {
                let source = self.find_outgoing_edge(head, color).unwrap();
                let target = eid;

                mid_seq.extend(self.merge_and_resplit(source, target, None)?);
                //DEBUG.write().unwrap().output("std",&self, Some("Green -> Red"), &self.calculate_face_counts());
            }

            // revert the "make contractible" step
            for op in leading_seq.iter().rev() {
                let inv_op = op.swapped_vertices(&a, &b).inverted();
                self.do_operation(&inv_op)?;
                mid_seq.push(inv_op);
            }

            mid_seq.splice(0..0, leading_seq);

            //DEBUG.write().unwrap().output("std",&self, Some("Unmade contractible"), &self.calculate_face_counts());

            Ok(mid_seq)
        } else {
            GraphErr::new_err("Edge for swap has to be unicolored")
        }
    }

    pub fn swap(&mut self, a: &VertexI, b: &VertexI) -> GraphResult<Vec<Operation>> {
        if a == b {
            return Ok(Vec::new());
        }

        let forbidden = vec![self.red_vertex, self.blue_vertex, self.green_vertex];

        let route = self.map.shortest_path(a, b, &forbidden.into_iter().collect());

        /*let (mut g, vmap): (Graph<_,_,_,_>, HashMap<_,_>) = self.map.into_petgraph();

        g.remove_node(*vmap.get(&self.red_vertex).unwrap());
        g.remove_node(*vmap.get(&self.blue_vertex).unwrap());
        g.remove_node(*vmap.get(&self.green_vertex).unwrap());

        let from = vmap.get(&a).unwrap();
        let to = vmap.get(&b).unwrap();

        let rmap : HashMap<_, _> = vmap.iter().map(|(k,v)| (v, k)).collect();

        //println!("connected = {}", has_path_connecting(g, from, to, None));

         let (_, route) = petgraph::algo::astar(
             &g,
             *from,
             |n| n == *to,
             |n| 1,
             |n| 1
         ).expect("no route found! (a*)");

        /*petgraph::algo::dijkstra(
            &g,
            *from,
            Some(to),
            |n| 1,
        ).expect("no route found! (dijkstra)");*/

        let translated_route = route.iter().map(|idx| *rmap.get(idx).unwrap()).collect_vec();*/
        eprintln!("route = {:?}", route);
        let mut result = Vec::new();
        for tmp in route.iter().skip(1) {
            result.extend(self.swap_locally(a, tmp)?);
        }
        for tmp in route.iter().skip(1).rev().skip(1) {
            result.extend(self.swap_locally(b, tmp)?);
        }

        return Ok(result);
    }

    pub fn calculate_face_counts(&self) -> HashMap<VertexI, (usize, usize, usize)> {

        let number_of_faces = self.map.face_count() - 1;
        let (dual, _, edge_to_edge, face_to_vertex) = self.map.get_dual(false);

        let mut result = HashMap::new();

        for &vid in self.map.vertex_indices() {

            if let Suspension(color) = self.map.vertex_weight(vid).unwrap() {
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
                    let path: Vec<_> = self.color_path(vid, *color).expect(INVALID_WOOD)
                        .iter().tuple_windows()
                        .map(|(&a, &b)| self.map.get_edge(a, b).unwrap())
                        .collect();

                    out_edge[color.next().index()] = path[0];
                    primal_no_cross_edges.extend(path);
                }


                let mut dual_no_cross_edges: HashSet<_> = primal_no_cross_edges.iter().map(|eid|
                    *edge_to_edge.get(eid).unwrap()
                ).collect();

                for nb in &dual.try_vertex(*face_to_vertex.get(&self.outer_face).expect(INTERNAL_ASSERTION)).expect(INVALID_WOOD).neighbors {
                    dual_no_cross_edges.insert(nb.edge);
                }

                for color in SchnyderColor::all() {
                    let e = self.map.try_edge(out_edge[color.index()]).expect(INVALID_WOOD);
                    let f = match e.get_signum_by_tail(vid) {
                        Forward => e.left_face,
                        Backward => e.right_face
                    }.unwrap();

                    if f != self.outer_face {
                        let start_vertex = face_to_vertex.get(&f).unwrap();
                        counts[color.index()] = dual.connected_component(*start_vertex, &dual_no_cross_edges).expect(INVALID_WOOD).len();
                    } else {
                        counts[color.index()] = 0;
                    }
                }

                result.insert(vid, (counts[0], counts[1], counts[2]));
            }
        }

        return result;
    }

    fn color_path(&self, vid: VertexI, color: SchnyderColor) -> GraphResult<Vec<VertexI>> {
        let mut path = Vec::new();
        let mut current_vertex = vid;

        while let Normal(_) = self.map.vertex_weight(current_vertex)? {
            assert!(!path.contains(&current_vertex));

            path.push(current_vertex);

            let out_index = self.find_outgoing(self.map.try_vertex(current_vertex)?, color).unwrap();
            current_vertex = self.map.try_vertex(current_vertex)?.neighbors[out_index].other;
        }

        if let Suspension(c) = *self.map.vertex_weight(current_vertex)? {
            assert_eq!(c, color);
            path.push(current_vertex);
            return Ok(path);
        } else {
            invalid_wood!()
        }
    }

    fn with_initialized_wood(mut self) -> GraphResult<Self> {

        let suspension_vertices = self.map.vertices().filter(|v|
            match v.weight {
                Suspension(_) => true,
                _ => false
            }
        ).collect_vec();

        if suspension_vertices.len() != 3 {
            return GraphErr::new_err("There have to be exactly three suspension vertices");
        }

        let rgb = (
            suspension_vertices.iter().find(|&v| match v.weight { Suspension(Red) => true, _ => false}),
            suspension_vertices.iter().find(|&v| match v.weight { Suspension(Green) => true, _ => false}),
            suspension_vertices.iter().find(|&v| match v.weight { Suspension(Blue) => true, _ => false})
        );

        let (r,g,b) = match rgb {
            (Some(r_), Some(g_), Some(b_)) => (r_.id, g_.id, b_.id),
            _ => return GraphErr::new_err("There has to be one suspension vertex for each of the colors red, green, and blue")
        };

        self.red_vertex = r;
        self.green_vertex = g;
        self.blue_vertex = b;

        self.outer_face = self.map.find_outer_face(r, g, b)?;

        // check if every bicolored edge has two distinct colors
        if self.map.edges()
            .any(|nb| match nb.weight
                { Bicolored(a, b) if a == b => true, _ => false}) {
            return GraphErr::new_err("Every bicolored edge has to have two distinct colors");
        }

        // check vertex rule
        for v in self.map.vertices() {
            let l = v.neighbors.len();

            let begin_sector = match v.weight {
                Normal(_) => Blue,
                Suspension(c) => c
            };

            match v.weight {
                Normal(_) => if l < 3 { return GraphErr::new_err("Vertex rule violated"); },
                Suspension(_) => if l < 2 { return GraphErr::new_err("Vertex rule violated"); }
            }

            let begin = self.find_outgoing(v, begin_sector.next());
            if begin.is_none() {
                return GraphErr::new_err("Vertex rule violated");
            }

            let mut i = (begin.unwrap() + 1) % l;
            let mut state = begin_sector; // inbound sector
            let mut last_outgoing = None;

            while i != begin.unwrap() {
                let nb = v.neighbors.get(i).unwrap();
                last_outgoing = self.outgoing_color(nb);

                match last_outgoing {
                    Some(c) => if c != state.prev() {
                        return GraphErr::new_err("Vertex rule violated");
                    } else {
                        state = state.next();
                    },
                    _ => match self.incoming_color(nb) {
                        Some(c) => if c != state {
                            return GraphErr::new_err("Vertex rule violated");
                        },
                        None => assert!(false)
                    }
                }

                i = (i + 1) % l;
            }

            match v.weight {
                Normal(_) => if state != begin_sector.prev() { return GraphErr::new_err("Vertex rule violated") },
                Suspension(c) => if state != begin_sector.next() || last_outgoing != Some(c.prev()) { return GraphErr::new_err("Vertex rule violated") }
            }
        }

        // check face cycle rule
        for f in self.map.faces() {
            //let l = f.angles.len();
            let mut fwd_colors = HashSet::new();
            let mut bwd_colors = HashSet::new();

            for (&v1, &v2) in f.angles.cycle(0, true).tuple_windows() {

                let eid = self.map.get_edge(v1, v2).map_err(|_|
                    GraphErr::new("Face cycle rule cannot be checked as face cycle contains invalid edges")
                )?;

                let e = self.map.try_edge(eid)?;
                let signum = e.get_signum(v1, v2);

                fwd_colors.insert(e.color(signum)); //TODO oldish ---- color mitigate
                bwd_colors.insert(e.color(signum.reversed()));
            }

            if (fwd_colors.len() == 1 && !fwd_colors.iter().next().unwrap().is_none()) || (bwd_colors.len() == 1 && !bwd_colors.iter().next().unwrap().is_none()) {
                return GraphErr::new_err("Face cycle rule violated");
            }
        }

        return Ok(self);
    }
}

impl PlanarMap<SchnyderVertexType, SchnyderEdgeDirection, ()> {
    fn find_outer_face(&self, r: VertexI, g: VertexI, b: VertexI) -> GraphResult<FaceI> {
        let outer_face_candidates = self.faces().filter(|&f|
            f.angles.contains(&r) &&
                f.angles.contains(&g) &&
                f.angles.contains(&b)
        ).collect_vec();

        if outer_face_candidates.len() < 1 || outer_face_candidates.len() > 2 {
            return GraphErr::new_err("There are not exactly two outer face candidates, this means the embedding is invalid");
        }

        let outer_face = outer_face_candidates.iter().filter(|&&f|
            is_in_cyclic_order(&f.angles, &vec![r, g, b])
        ).collect_vec();

        return match outer_face.len() {
            1 => Ok(outer_face[0].id),
            _ => return GraphErr::new_err("There is no outer face in which the suspension vertices appear in the right order, this means the embedding is invalid")
        }
    }

    pub fn get_color(&self, v1: VertexI, v2: VertexI) -> GraphResult<SchnyderEdgeDirection> {
        Ok(match self.edge_with_signum(v1, v2)? {
            (e, Forward) => *self.edge_weight(e)?,
            (e, Backward) => self.edge_weight(e)?.reversed()
        })
    }

    pub fn get_incoming_by_color<'a>(&'a self, v: VertexI, color: SchnyderColor) -> Result<impl Iterator<Item=&'a VertexI> + 'a, IndexAccessError<VertexI>> {
        Ok(self.try_vertex(v)?.neighbors.iter()
            .filter(move |nb| match self.get_color(v, nb.other).unwrap() {
                Unicolored(c, Backward) if c == color => true,
                Bicolored(_, c) if c == color => true,
                _ => false
            })
            .map(|nb| &nb.other))
    }
}

impl TryFrom<PlanarMap<SchnyderVertexType, SchnyderEdgeDirection, ()>> for SchnyderMap {
    type Error = GraphErr;

    fn try_from(map: PlanarMap<SchnyderVertexType, SchnyderEdgeDirection, ()>) -> GraphResult<SchnyderMap> {

        let result = SchnyderMap {
            map,
            outer_face: FaceI(0),
            red_vertex: VertexI(0),
            green_vertex: VertexI(0),
            blue_vertex: VertexI(0)
        };

        return result.with_initialized_wood();
    }
}

impl PartialEq for SchnyderMap {
    fn eq(&self, other: &Self) -> bool {
        self.compute_3tree_code() == other.compute_3tree_code()
    }
}

impl Eq for SchnyderMap {
    // an equivalence relation it is
}