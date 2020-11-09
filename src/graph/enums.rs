
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum EdgeEnd {
    Tail, Head
}

impl EdgeEnd {
    pub fn inverted(&self) -> Self {
        match self {
            EdgeEnd::Tail => EdgeEnd::Head,
            EdgeEnd::Head => EdgeEnd::Tail
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Signum {
    Forward, Backward
}

impl Signum {
    pub fn reversed(&self) -> Self {
        match self {
            Signum::Forward => Signum::Backward,
            Signum::Backward => Signum::Forward
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ClockDirection {
    CW, CCW
}

impl ClockDirection {

    pub fn reversed(&self) -> Self {
        match self {
            ClockDirection::CW => ClockDirection::CCW,
            ClockDirection::CCW => ClockDirection::CW
        }
    }

    pub fn reversed_if(&self, cond: bool) -> Self {
        if cond {
            self.reversed()
        } else {
            *self
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Side {
    Left, Right
}


impl Side {
    pub fn reversed(&self) -> Self {
        match self {
            Side::Left => Side::Right,
            Side::Right => Side::Left
        }
    }
}