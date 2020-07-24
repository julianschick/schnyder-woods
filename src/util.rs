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

