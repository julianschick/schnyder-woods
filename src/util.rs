use itertools::Itertools;
use crate::util::iterators::cyclic::CyclicIterable;

pub mod iterators {

    pub mod cyclic {

        pub trait CyclicIterable<T> {
            fn cycle(&self, offset: usize, wrap: bool) -> CyclicIterator<T>;
        }

        pub struct CyclicIterator<'a, T> {
            inner: &'a Vec<T>,
            left_pos: isize,
            right_pos: isize,
            offset: isize
        }

        impl<'a, T> Iterator for CyclicIterator<'a, T> {
            type Item = &'a T;

            fn next(&mut self) -> Option<Self::Item> {
                return if self.left_pos > self.right_pos || self.inner.is_empty() {
                    None
                } else {
                    let p = self.left_pos;
                    self.left_pos += 1;
                    Some(&self.inner[((p + self.offset) % self.inner.len() as isize) as usize])
                }
            }
        }

        impl<'a, T> DoubleEndedIterator for CyclicIterator<'a, T> {
            fn next_back(&mut self) -> Option<Self::Item> {
                return if self.left_pos > self.right_pos || self.inner.is_empty() {
                    None
                } else {
                    let p = self.right_pos;
                    self.right_pos -= 1;
                    Some(&self.inner[((p + self.offset) % self.inner.len() as isize) as usize])
                }
            }
        }

        impl<T> CyclicIterable<T> for Vec<T> {
            fn cycle(&self, start: usize, wrap: bool) -> CyclicIterator<T> {
                if start < self.len() || start == 0 {
                    CyclicIterator {
                        inner: self,
                        left_pos: 0,
                        right_pos: if !wrap {
                            (self.len() as isize) - 1
                        } else {
                            self.len() as isize
                        },
                        offset: start as isize,
                    }
                } else {
                    panic!("invalid cycle start");
                }
            }
        }

    }

}

pub fn is_in_cyclic_order<T: Eq>(vec: &Vec<T>, order: &Vec<T>)  -> bool {
    if order.len() <= 2 {
        return true;
    }

    let positions: Vec<_> = order.iter()
        .map(|item| vec.iter().position(|set_item| item == set_item))
        .filter_map(|o| o)
        .collect();


    if let Some(index_of_min_pos) = positions.iter().position_min() {
        for (a, b) in positions.cycle(index_of_min_pos, false).tuple_windows() {
            if a > b { return false };
        }
    } else {
        // no element of 'order' is in 'vec' contained
        return true;
    }

    return true;
}

pub mod debug {
    use std::path::Path;
    use std::fs::{create_dir, read_dir, remove_file, File};
    use crate::graph::schnyder::SchnyderMap;
    use std::collections::HashMap;
    use crate::graph::VertexI;
    use std::io::Write;
    use std::process::Command;

    pub struct Debug {
        base_dir: &'static str,
        output_dir: &'static str,
        active: bool,
        counter: usize
    }

    impl Debug {

        fn delete_all_files(dir: &str) {
            for entry in read_dir(dir).unwrap() {
                let p = entry.unwrap().path();
                if p.is_file() {
                    remove_file(p);
                }
            }
        }

        pub fn new(base_dir: &'static str, output_dir: &'static str) -> Debug  {
            let result = Debug {
                base_dir,
                output_dir,
                active: true,
                counter: 0
            };

            if !Path::new(&base_dir).is_dir() {
                create_dir(base_dir).expect("Unable to create temporary output dir");
            }
            if !Path::new(&output_dir).is_dir() {
                create_dir(&output_dir).expect("Unable to create output dir");
            }

            Self::delete_all_files(base_dir);
            Self::delete_all_files(output_dir);

            return result;
        }

        pub fn output<F: Clone>(&mut self, wood: &SchnyderMap<F>, title: Option<&str>, face_counts: &HashMap<VertexI, (usize, usize, usize)>) {
            if !self.active {
                return;
            }

            let tikz_string = wood.generate_tikz(title, false, face_counts);
            let name = format!("{}", self.counter);
            self.counter += 1;

            let basedir = "/tmp/schnyder";
            if !Path::new(&basedir).is_dir() {
                create_dir(basedir).expect("Unable to create temporary output dir");
            }
            let outputdir = format!("{}/output", basedir);
            if !Path::new(&outputdir).is_dir() {
                create_dir(&outputdir).expect("Unable to create output dir");
            }

            let mut f = File::create(format!("{}/{}.tex", basedir, name)).expect("Unable to create file");
            f.write_all(tikz_string.as_bytes()).expect("Unable to write data");

            Command::new("xelatex").current_dir(basedir).arg(format!("{}.tex", name)).output();
            Command::new("pdftoppm").current_dir(basedir)
                .arg(format!("{}.pdf", name))
                .arg(format!("{}/{}", outputdir, name))
                .arg("-png").arg("-singlefile").output();
        }
    }

}

pub mod errors {
    use crate::graph::{VertexI, EdgeI, FaceI};

    type GraphResult<T> = Result<T, GraphErr>;

    pub struct GraphErr {
        problem: String,
        operation: Option<String>,
        vertex: Option<VertexI>,
        edge: Option<EdgeI>,
        face: Option<FaceI>
    }

    impl GraphErr {

        pub fn new(problem: &str) -> Self {
            GraphErr {
                problem: problem.to_string(),
                operation: None,
                vertex: None,
                edge: None,
                face: None
            }
        }

        pub fn new_err<T>(problem: &str) -> Result<T, Self> {
            return Err(GraphErr::new(problem));
        }

        pub fn invalid_edge_index(eid: EdgeI) -> GraphErr {
            GraphErr::new("Invalid edge index")
                .with_edge(eid)
        }

        pub fn with_operation(mut self, operation: &str) -> Self {
            self.operation = Some(operation.to_string());
            return self;
        }

        pub fn with_vertex(mut self, v: VertexI) -> Self {
            self.vertex = Some(v);
            return self;
        }

        pub fn with_edge(mut self, e: EdgeI) -> Self {
            self.edge = Some(e);
            return self;
        }

        pub fn with_face(mut self, f: FaceI) -> Self {
            self.face = Some(f);
            return self;
        }

    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_in_cyclic_order() {
        assert!(is_in_cyclic_order(&vec![1,2,3,4,5,6], &vec![1,3,5]));
        assert!(!is_in_cyclic_order(&vec![1,2,3,4,5,6], &vec![1,5,3]));
        assert!(is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![1,3,5]));
        assert!(!is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![1,5,3]));
        assert!(is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![1,2,7,3]));
        assert!(is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![100,200]));
        assert!(is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![100]));
        assert!(is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![100,200,300]));
        assert!(is_in_cyclic_order(&vec![], &vec![100,200,300]));
        assert!(is_in_cyclic_order(&vec![], &vec![1,2,3]));
        assert!(is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![3,4,1]));
        assert!(!is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![4,3,1]));
        assert!(is_in_cyclic_order(&vec![1,2,3,4,5,6], &vec![1,2,3,4,5,6]));
        assert!(is_in_cyclic_order(&vec![1,2,3,4,5,6], &vec![4,5,6,1,2,3]));
        assert!(is_in_cyclic_order(&vec![4,5,6,1,2,3], &vec![1,2,3,4,5,6]));
    }
}

