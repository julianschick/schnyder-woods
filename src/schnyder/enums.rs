use crate::graph::enums::{RevertibleEnum, Signum};
use crate::schnyder::enums::SchnyderEdgeDirection::{Bicolored, Unicolored};
use itertools::Itertools;
use std::str::FromStr;
use SchnyderColor::{Blue, Green, Red};

pub trait IndexedEnum<T> {
    fn index(&self) -> usize;
    fn from_index(index: usize) -> T;
    fn number() -> usize;

    fn next(&self) -> T {
        Self::from_index((self.index() + 1) % Self::number())
    }

    fn prev(&self) -> T {
        Self::from_index((self.index() + (Self::number() - 1)) % Self::number())
    }

    fn all() -> Vec<T> {
        (0..Self::number())
            .map(|i| Self::from_index(i))
            .collect_vec()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SchnyderColor {
    Red,
    Green,
    Blue,
}

impl IndexedEnum<SchnyderColor> for SchnyderColor {
    fn index(&self) -> usize {
        match self {
            Red => 0,
            Green => 1,
            Blue => 2,
        }
    }

    fn from_index(index: usize) -> Self {
        match index {
            0 => Red,
            1 => Green,
            2 => Blue,
            _ => panic!("Invalid index given for IndexedEnum<SchnyderColor>"),
        }
    }

    fn number() -> usize {
        3
    }
}

impl FromStr for SchnyderColor {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "r" | "red" => Ok(SchnyderColor::Red),
            "g" | "green" => Ok(SchnyderColor::Green),
            "b" | "blue" => Ok(SchnyderColor::Blue),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SchnyderVertexType {
    Normal(usize),
    Suspension(SchnyderColor),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SchnyderEdgeDirection {
    Unicolored(SchnyderColor, Signum),
    Bicolored(SchnyderColor, SchnyderColor),
}

impl SchnyderEdgeDirection {
    pub fn is_unicolored(&self) -> bool {
        match self {
            Unicolored(_, _) => true,
            _ => false,
        }
    }

    pub fn is_bicolored(&self) -> bool {
        match self {
            Bicolored(_, _) => true,
            _ => false,
        }
    }

    pub fn reversed(&self) -> Self {
        match self {
            Unicolored(c, signum) => Unicolored(*c, signum.reversed()),
            Bicolored(a, b) => Bicolored(*b, *a),
        }
    }
}
