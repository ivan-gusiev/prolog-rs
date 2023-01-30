use lang::Functor;
use std::fmt::{Display, Formatter};
use std::ops;

use crate::symbol::SymDisplay;

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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
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
            Data::Ref(r) => write!(f, "{r}"),
            Data::Str(s) => write!(f, "{s}"),
            Data::Functor(functor) => write!(f, "{functor}"),
        }
    }
}

impl SymDisplay for Data {
    fn sym_fmt(
        &self,
        f: &mut Formatter<'_>,
        symbol_table: &crate::symbol::SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Data::Functor(functor) => functor.sym_fmt(f, symbol_table),
            _ => self.fmt(f),
        }
    }
}

impl From<Ref> for Data {
    fn from(value: Ref) -> Self {
        Data::Ref(value)
    }
}

impl From<Str> for Data {
    fn from(value: Str) -> Self {
        Data::Str(value)
    }
}

impl From<Functor> for Data {
    fn from(value: Functor) -> Self {
        Data::Functor(value)
    }
}

impl Data {
    pub fn get_functor(&self) -> Option<Functor> {
        match self {
            &Data::Functor(functor) => Some(functor),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Mode {
    Read,
    Write,
}

impl Display for Mode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Mode::Read => write!(f, "read"),
            Mode::Write => write!(f, "write"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Addr {
    Heap(HeapPtr),
    Reg(RegPtr),
}

impl Display for Addr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Addr::Heap(h) => write!(f, "{h}"),
            Addr::Reg(r) => write!(f, "{r}"),
        }
    }
}

impl From<RegPtr> for Addr {
    fn from(ptr: RegPtr) -> Self {
        Addr::Reg(ptr)
    }
}

impl From<HeapPtr> for Addr {
    fn from(ptr: HeapPtr) -> Self {
        Addr::Heap(ptr)
    }
}
