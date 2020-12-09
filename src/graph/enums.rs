pub trait RevertibleEnum {
    fn reversed(&self) -> Self;
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum EdgeEnd {
    Tail,
    Head,
}

impl RevertibleEnum for EdgeEnd {
    fn reversed(&self) -> Self {
        match self {
            EdgeEnd::Tail => EdgeEnd::Head,
            EdgeEnd::Head => EdgeEnd::Tail,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Signum {
    Forward,
    Backward,
}

impl RevertibleEnum for Signum {
    fn reversed(&self) -> Self {
        match self {
            Signum::Forward => Signum::Backward,
            Signum::Backward => Signum::Forward,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ClockDirection {
    CW,
    CCW,
}

impl RevertibleEnum for ClockDirection {
    fn reversed(&self) -> Self {
        match self {
            ClockDirection::CW => ClockDirection::CCW,
            ClockDirection::CCW => ClockDirection::CW,
        }
    }
}

impl ClockDirection {
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
    Left,
    Right,
}

impl RevertibleEnum for Side {
    fn reversed(&self) -> Self {
        match self {
            Side::Left => Side::Right,
            Side::Right => Side::Left,
        }
    }
}
