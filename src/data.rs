use lang::Functor;
use std::fmt::{Display, Formatter};
use std::ops;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct HeapPtr(pub usize);

impl ops::Add<usize> for HeapPtr {
    type Output = HeapPtr;

    fn add(self, rhs: usize) -> HeapPtr {
        HeapPtr(self.0 + rhs)
    }
}

impl ops::AddAssign<usize> for HeapPtr {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs
    }
}

impl Display for HeapPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "HEAP[{}]", self.0)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct RegPtr(pub usize);

impl Display for RegPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "X{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Ref(pub HeapPtr);

impl Display for Ref {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "<REF,{}>", self.0 .0)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Str(pub HeapPtr);

impl Display for Str {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "<STR,{}>", self.0 .0)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Data {
    Empty,
    Ref(Ref),
    Str(Str),
    Functor(Functor),
}

impl Display for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Data::Empty => write!(f, "<EMPTY>"),
            Data::Ref(r) => write!(f, "{}", r),
            Data::Str(s) => write!(f, "{}", s),
            Data::Functor(functor) => write!(f, "{}", functor),
        }
    }
}
