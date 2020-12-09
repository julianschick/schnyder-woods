use crate::graph::data_holders::{Edge, Face, Vertex};
use crate::graph::enums::Signum::{Backward, Forward};
use crate::schnyder::SchnyderEdgeDirection::{Bicolored, Unicolored};
use crate::schnyder::{SchnyderEdgeDirection, SchnyderMap, SchnyderVertexType};
use std::fmt::Write;

type Result = std::fmt::Result;

trait AltDebug {
    fn debug(&self, w: &mut dyn Write) -> Result;
}

impl AltDebug for SchnyderMap {
    fn debug(&self, w: &mut dyn Write) -> Result {
        for e in self.map.edges() {
            e.debug(w)?;
        }
        for v in self.map.vertices() {
            v.debug(w)?;
        }
        for f in self.map.faces() {
            f.debug(w)?;
        }
        Ok(())
    }
}

impl AltDebug for Vertex<SchnyderVertexType> {
    fn debug(&self, f: &mut dyn Write) -> Result {
        write!(f, "{:?}", self) // use standard trait 'Debug'
    }
}

impl AltDebug for Edge<SchnyderEdgeDirection> {
    fn debug(&self, f: &mut dyn Write) -> Result {
        let tail_color = match self.weight {
            Unicolored(c, Forward) => Some(c),
            Bicolored(c, _) => Some(c),
            _ => None,
        };
        let head_color = match self.weight {
            Unicolored(c, Backward) => Some(c),
            Bicolored(_, c) => Some(c),
            _ => None,
        };

        write!(
            f,
            "{:?}: {:?} ={:=<5}======={:=>5}=> {:?} (L = {:?}, R = {:?})",
            self.id,
            self.tail,
            match tail_color {
                Some(c) => format!("{:?}", c),
                _ => "".to_string(),
            },
            match head_color {
                Some(c) => format!("{:?}", c),
                _ => "".to_string(),
            },
            self.head,
            self.left_face.unwrap(),
            self.right_face.unwrap()
        )
    }
}

impl AltDebug for Face<()> {
    fn debug(&self, f: &mut dyn Write) -> Result {
        write!(f, "{:?}", self) // use standard trait 'Debug'
    }
}
