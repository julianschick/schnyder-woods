

pub mod cyclic_iterator {
    pub struct CyclicIterator<'a, T> {
        inner: &'a Vec<T>,
        begin: bool,
        start: usize,
        pos: usize
    }

    impl<'a, T> Iterator for CyclicIterator<'a, T> {
        type Item = &'a T;

        fn next(&mut self) -> Option<Self::Item> {
            return if self.pos == self.start && !self.begin {
                None
            } else {
                let p = self.pos;
                self.pos = (self.pos + 1) % self.inner.len();
                self.begin = false;

                Some(&self.inner[p])
            }
        }
    }

    pub fn cyclic_iterator<T>(vec: &Vec<T>, start: usize) -> Option<CyclicIterator<T>> {
        if start < vec.len() {
            Some(CyclicIterator {
                inner: vec,
                begin: true,
                start,
                pos: start
            })
        } else {
            None
        }
    }
}