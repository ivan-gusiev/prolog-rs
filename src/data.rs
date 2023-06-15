use lang::{Functor, VarName};
use std::{
    fmt::{Display, Formatter},
    ops,
};
use symbol::SymDisplay;

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
pub struct FramePtr(pub usize);

impl Display for FramePtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Y{}", self.0)
    }
}

impl From<usize> for FramePtr {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<FramePtr> for usize {
    fn from(FramePtr(index): FramePtr) -> Self {
        index
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct StackPtr(pub usize);

impl Display for StackPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "STACK[{}]", self.0)
    }
}

impl From<usize> for StackPtr {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl ops::Add<usize> for StackPtr {
    type Output = StackPtr;

    fn add(self, rhs: usize) -> StackPtr {
        Self(self.0 + rhs)
    }
}

impl ops::Add<FramePtr> for StackPtr {
    type Output = StackPtr;

    fn add(self, FramePtr(rhs): FramePtr) -> StackPtr {
        Self(self.0 + rhs)
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

impl From<usize> for CodePtr {
    fn from(value: usize) -> Self {
        CodePtr(value)
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

/// A tagged pointer to machine addressable storage
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Addr {
    Heap(HeapPtr),
    Reg(RegPtr),
    Stack(FramePtr),
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

impl From<FramePtr> for Addr {
    fn from(ptr: FramePtr) -> Self {
        Addr::Stack(ptr)
    }
}

impl Addr {
    pub fn get_local(&self) -> Option<Local> {
        match self {
            Addr::Heap(_) => None,
            Addr::Reg(r) => Some(Local::Reg(*r)),
            Addr::Stack(s) => Some(Local::Stack(*s)),
        }
    }
}

/// A reference to local execution environment.
/// Either a register variable or a stack variable
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum Local {
    Reg(RegPtr),
    Stack(FramePtr),
}

impl Local {
    pub fn from_stack(stack: &FramePtr) -> Local {
        Local::Stack(*stack)
    }

    pub fn is_stack(&self) -> bool {
        matches!(self, Local::Stack(_))
    }
}

impl From<Local> for Addr {
    fn from(value: Local) -> Self {
        match value {
            Local::Reg(regptr) => Addr::Reg(regptr),
            Local::Stack(stackptr) => Addr::Stack(stackptr),
        }
    }
}

impl From<RegPtr> for Local {
    fn from(value: RegPtr) -> Self {
        Local::Reg(value)
    }
}

impl From<&RegPtr> for Local {
    fn from(value: &RegPtr) -> Self {
        Local::Reg(*value)
    }
}

impl From<FramePtr> for Local {
    fn from(value: FramePtr) -> Self {
        Local::Stack(value)
    }
}

impl From<&FramePtr> for Local {
    fn from(value: &FramePtr) -> Self {
        Local::Stack(*value)
    }
}

impl Display for Local {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Reg(r) => write!(f, "{r}"),
            Self::Stack(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct VarRecord {
    pub variable: VarName,
    pub mapping: Local,
    pub address: HeapPtr,
}

impl VarRecord {
    pub fn from_mapping(item: (&Local, &VarName)) -> VarRecord {
        Self {
            variable: *item.1,
            mapping: *item.0,
            address: HeapPtr(0),
        }
    }
}
