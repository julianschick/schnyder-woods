

#[derive(Debug, Copy, Clone)]
pub enum SchnyderColor {
    Red, Green, Blue
}

impl SchnyderColor {
    pub fn next(&self) -> Self {
        match self {
            Red => SchnyderColor::Green, Green => SchnyderColor::Blue, Blue => SchnyderColor::Red
        }
    }

    pub fn prev(&self) -> Self {
        match self {
            Red => SchnyderColor::Blue, Blue => SchnyderColor::Green, Green => SchnyderColor::Red
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum SchnyderVertexType {
    Normal(usize),
    Suspension(SchnyderColor)
}

pub trait SchnyderVertexTypeHolder {
    fn get_type(&self) -> SchnyderVertexType;
}

pub struct SchnyderVertex {
    vertex_type: SchnyderVertexType
}

impl SchnyderVertexTypeHolder for SchnyderVertex {
    fn get_type(&self) -> SchnyderVertexType {
        self.vertex_type
    }
}

#[derive(Debug, Copy, Clone)]
pub enum SchnyderEdgeType {
    Black,
    Unicolored(SchnyderColor),
    Bicolored(SchnyderColor, SchnyderColor)
}

#[derive(Debug)]
pub enum SchnyderNode {
    Normal(usize),
    Suspension(SchnyderColor)
}