use itertools::Itertools;
use crate::util::iterators::cyclic::CyclicIterable;

pub mod macros {
    #[macro_export]
    macro_rules! ge {
        ($e:expr) => { return Err(GraphErr::new($e)) };
    }
}

pub mod iterators {

    pub mod cyclic {

        pub trait CyclicIterable<T> {
            fn cycle(&self, offset: usize, wrap: bool) -> CyclicIterator<T>;
        }

        pub trait CyclicIterableByElement<T: Eq> {
            fn cycle_by_element(&self, start_element: &T, wrap: bool) -> CyclicIterator<T>;
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

        impl<T: Eq> CyclicIterableByElement<T> for Vec<T> {
            fn cycle_by_element(&self, start: &T, wrap: bool) -> CyclicIterator<T> {
                if let Some(start_index) = self.iter().position(|element| element == start) {
                    return self.cycle(start_index, wrap);
                } else {
                    panic!("invalid cycle starting element");
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

pub fn swapped<T: Eq + Copy>(a: &T, b: &T, handled: &T) -> T {
    if handled == a {
        *b
    } else if handled == b {
        *a
    } else {
        *handled
    }
}

pub mod debug {
    use std::path::Path;
    use std::fs::{create_dir, File, remove_dir_all};
    use crate::schnyder::SchnyderMap;
    use std::collections::HashMap;
    use crate::graph::indices::VertexI;
    use std::io::Write;

    pub struct Debug {
        base_dir: &'static str,
        counters: HashMap<String, usize>,
        active: bool,
    }

    impl Debug {

        #[allow(dead_code)]
        pub fn activate(&mut self) {
            self.active = true;
        }

        #[allow(dead_code)]
        pub fn deactivate(&mut self) {
            self.active = false;
        }

        /*fn delete_all_files(dir: &str) {
            for entry in read_dir(dir).unwrap() {
                let p = entry.unwrap().path();
                if p.is_file() {
                    remove_file(p);
                }
            }
        }*/

        pub fn new(base_dir: &'static str) -> Debug  {
            let result = Debug {
                base_dir,
                active: false,
                counters: HashMap::new()
            };

            let rr = remove_dir_all(base_dir);

            if let Err(_) = rr {
                println!("Debug was not present, no need to be wiped.")
            }

            /*if !Path::new(&base_dir).is_dir() {
                create_dir(base_dir).expect("Unable to create temporary output dir");
            }
            if !Path::new(&output_dir).is_dir() {
                create_dir(&output_dir).expect("Unable to create output dir");
            }*/

            //Self::delete_all_files(base_dir);
            //Self::delete_all_files(output_dir);

            return result;
        }

        pub fn output(&mut self, context: &str, wood: &SchnyderMap, title: Option<&str>, _face_counts: &HashMap<VertexI, (usize, usize, usize)>) {
            if !self.active {
                return;
            }

            let tikz_string = wood.generate_tikz(title, true, true, false, None);
            if !self.counters.contains_key(context) {
                self.counters.insert(context.to_string(), 0);
            }
            let name = format!("{}", self.counters.get(context).unwrap());
            self.counters.insert(context.to_string(), self.counters.get(context).unwrap() + 1);

            let basedir = self.base_dir;
            let contextdir = &format!("{}/{}", basedir, context);
            let outputdir = format!("{}/{}/output", basedir, context);

            if !Path::new(&basedir).is_dir() {
                create_dir(basedir).expect("Unable to create temporary output dir");
            }
            if !Path::new(&contextdir).is_dir() {
                create_dir(contextdir).expect("Unable to create temporary output dir");
            }
            if !Path::new(&outputdir).is_dir() {
                create_dir(&outputdir).expect("Unable to create output dir");
            }

            let mut f = File::create(format!("{}/{}/{}.tex", basedir, context, name)).expect("Unable to create file");
            f.write_all(tikz_string.as_bytes()).expect("Unable to write data");

            //Command::new("xelatex").current_dir(format!("{}/{}", basedir, context)).arg(format!("{}.tex", name)).output();
            /*Command::new("pdftoppm").current_dir(format!("{}/{}", basedir, context))
                .arg(format!("{}.pdf", name))
                .arg(format!("{}/{}", outputdir, name))
                .arg("-png").arg("-singlefile").output();*/
            /*let s = String::from_utf8(Command::new("convert")
                .current_dir(format!("{}/{}", basedir, context))
                .arg("-density")
                .arg("300")
                .arg("-background")
                .arg("#FFFFFF")
                .arg("-flatten")
                .arg(format!("{}.pdf", name))
                .arg(format!("{}/{}.png", outputdir, name)).output().unwrap().stderr);*/

            //println!("{}", s.unwrap());
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

