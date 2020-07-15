
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
                return if self.left_pos > self.right_pos {
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
                return if self.left_pos > self.right_pos {
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
                if start < self.len() {
                    CyclicIterator {
                        inner: self,
                        left_pos: 0,
                        right_pos: if !wrap {
                            (self.len() - 1) as isize
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


