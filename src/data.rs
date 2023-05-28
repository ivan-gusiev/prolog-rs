use lang::Functor;
use std::fmt::{Display, Formatter};
use std::ops;

use crate::symbol::SymDisplay;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
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

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct StackPtr(pub usize);

impl Display for StackPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Y{}", self.0)
    }
}

impl From<usize> for StackPtr {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct CodePtr(pub usize);

impl Display for CodePtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "CODE[{}]", self.0)
    }
}

impl ops::Add<usize> for CodePtr {
    type Output = CodePtr;

    fn add(self, rhs: usize) -> CodePtr {
        CodePtr(self.0 + rhs)
    }
}

impl From<CodePtr> for usize {
    fn from(value: CodePtr) -> Self {
        value.0
    }
}

/// represents the number of variables in a stack frame
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct StackDepth(pub usize);

impl Display for StackDepth {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.0.fmt(f)
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Addr {
    Heap(HeapPtr),
    Reg(RegPtr),
    Stack(StackPtr),
}

impl Display for Addr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Heap(h) => write!(f, "{h}"),
            Self::Reg(r) => write!(f, "{r}"),
            Self::Stack(s) => write!(f, "{s}"),
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

impl From<StackPtr> for Addr {
    fn from(ptr: StackPtr) -> Self {
        Addr::Stack(ptr)
    }
}
