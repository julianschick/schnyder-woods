use crate::graph::error::{GraphErr, GraphResult};
use std::fmt::{Debug, Error};
use serde::export::Formatter;

pub struct ArrayTree {
    size: usize,
    root: u8,
    children: Vec<Vec<u8>>,
    parents: Vec<u8>,
}

pub enum WalkAroundDirection {
    HiToLo, LoToHi
}

pub struct WalkAroundIterator<'a> {
    tree: &'a ArrayTree,
    direction: WalkAroundDirection,
    current_node: u8,
    next_dive: Vec<u8>
}

impl<'a> Iterator for WalkAroundIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {

        loop {
            // do next dive
            let num_children = self.tree.number_of_children(self.current_node);
            if self.next_dive[self.current_node as usize] < num_children {
                let child_index = match self.direction {
                    WalkAroundDirection::LoToHi => self.next_dive[self.current_node as usize] as usize,
                    WalkAroundDirection::HiToLo => num_children as usize - 1 - self.next_dive[self.current_node as usize] as usize
                };

                let child = self.tree.children[self.current_node as usize][child_index];
                self.next_dive[self.current_node as usize] += 1;
                self.current_node = child;
                return Some(child);

            // no dive possible, then climb
            } else {
                if self.current_node == self.tree.root { return None; }

                let parent = self.tree.get_parent_(self.current_node);
                let parent_num_children = self.tree.number_of_children(parent);
                if self.next_dive[parent as usize] == 0 {
                    self.next_dive[parent as usize] =  match self.direction {
                        WalkAroundDirection::LoToHi => self.tree.get_child_index(parent, self.current_node) + 1,
                        WalkAroundDirection::HiToLo => parent_num_children - self.tree.get_child_index(parent, self.current_node)
                    };
                }

                self.current_node = parent;
            }
        }
    }
}

pub struct DfsIterator<'a> {
    tree: &'a ArrayTree,
    stack: Vec<u8>
}

impl<'a> Iterator for DfsIterator<'a> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.stack.pop() {
            self.stack.extend(self.tree.children[current as usize].iter().rev());
            Some(current)
        } else {
            None
        }
    }
}

impl ArrayTree {

    pub fn from_tree_code(code: &[u8]) -> GraphResult<Self> {
        if code.len() > 256 {
            return GraphErr::new_err("Invalid tree code: More than 255 vertices are not supported.");
        }
        if code.iter().enumerate().filter(|(index, &k)| (*index) as u8 == k).count() != 1 {
            return GraphErr::new_err("Invalid tree code: Exactly one root expected.");
        }

        let mut tree = ArrayTree {
            size: code.len(),
            root: code.iter().enumerate().position(|(index, &k)| index as u8 == k).unwrap() as u8,
            children: (0..code.len()).map(|_| Vec::new()).collect(),
            parents: Vec::with_capacity(code.len()),
        };

        for (i, &value) in code.iter().enumerate() {
            if value != i as u8 {
                tree.children[value as usize].push(i as u8);
            }
            tree.parents.push(value);
        }

        for c in &mut tree.children {
            c.sort();
        }

        return Ok(tree);
    }

    pub fn get_parent(&self, node: u8) -> Option<u8> {
        match self.parents.get(node as usize) {
            Some(&p) if p != node => Some(p),
            _ => None
        }
    }

    fn get_parent_(&self, node: u8) -> u8 {
        self.parents[node as usize]
    }

    pub fn number_of_children(&self, node: u8) -> u8 {
        self.children[node as usize].len() as u8
    }

    pub fn get_children(&self, node: u8) -> impl Iterator<Item = &u8> {
        self.children[node as usize].iter()
    }

    pub fn get_child_index(&self, node: u8, child: u8) -> u8 {
        self.children[node as usize].iter().position(|&c| c == child).unwrap() as u8
    }

    pub fn get_root(&self) -> u8 {
        self.root
    }

    pub fn iter_dfs(&self) -> DfsIterator {
        DfsIterator {
            tree: self,
            stack: vec![self.root]
        }
    }

    pub fn iter_walkaround(&self, start: u8, dir: WalkAroundDirection) -> WalkAroundIterator {
        let mut result = WalkAroundIterator {
            tree: self,
            direction: dir,
            current_node: start,
            next_dive: Vec::with_capacity(self.parents.len())
        };

        (0..self.size).for_each(|_| result.next_dive.push(0));
        result.next_dive[start as usize] = self.number_of_children(start);

        return result;
    }

    #[allow(dead_code)]
    pub fn print(&self) {
        println!("{:?}", self.parents);
        println!("{:#?}", self.children);
    }

}

impl Debug for ArrayTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.debug_struct("ArrayTree")
            .field("parents", &self.parents)
            .field("children", &self.children)
            .finish()
    }
}