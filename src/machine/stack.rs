use std::convert::TryFrom;

use crate::{
    data::{CodePtr, Data, FramePtr, HeapPtr, StackDepth, StackPtr},
    lang::Functor,
};

use super::{MResult, Machine, MachineError, MachineResult};

#[derive(Debug, Clone, Copy)]
pub enum StackData {
    Empty,
    Ref(HeapPtr),
    Str(HeapPtr),
    Functor(Functor),
    Stack(StackPtr),
    Code(CodePtr),
    Len(usize),
}

impl StackData {
    pub fn to_len(&self) -> MachineResult<usize> {
        match self {
            Self::Empty => Ok(0),
            Self::Len(l) => Ok(*l),
            _ => Err(MachineError::StackTypeError),
        }
    }

    pub fn to_code(&self) -> MachineResult<CodePtr> {
        match self {
            Self::Code(code) => Ok(*code),
            _ => Err(MachineError::StackTypeError),
        }
    }

    pub fn to_stack(&self) -> MachineResult<StackPtr> {
        match self {
            Self::Stack(stack) => Ok(*stack),
            _ => Err(MachineError::StackTypeError),
        }
    }
}

impl From<StackDepth> for StackData {
    fn from(StackDepth(len): StackDepth) -> Self {
        Self::Len(len)
    }
}

impl From<StackPtr> for StackData {
    fn from(value: StackPtr) -> Self {
        Self::Stack(value)
    }
}

impl From<CodePtr> for StackData {
    fn from(value: CodePtr) -> Self {
        Self::Code(value)
    }
}

impl From<Data> for StackData {
    fn from(value: Data) -> Self {
        match value {
            Data::Empty => Self::Empty,
            Data::Ref(r) => Self::Ref(r),
            Data::Str(s) => Self::Str(s),
            Data::Functor(f) => Self::Functor(f),
        }
    }
}

impl TryFrom<StackData> for Data {
    type Error = MachineError;

    fn try_from(value: StackData) -> Result<Self, Self::Error> {
        match value {
            StackData::Empty => Ok(Self::Empty),
            StackData::Ref(r) => Ok(Self::Ref(r)),
            StackData::Str(s) => Ok(Self::Str(s)),
            StackData::Functor(f) => Ok(Self::Functor(f)),
            _ => Err(MachineError::StackTypeError),
        }
    }
}

// represents a valid stack frame and all its data
// used for external representation independent of the machine
#[derive(Debug)]
pub struct StackFrame {
    pub ce: StackPtr,
    pub cp: CodePtr,
    pub vars: Vec<Data>,
}

impl StackFrame {
    pub fn new(ce: StackPtr, cp: CodePtr, depth: StackDepth) -> Self {
        Self {
            ce,
            cp,
            vars: std::iter::repeat(Data::Empty).take(depth.0).collect(),
        }
    }

    pub fn iter_var(&self) -> impl ExactSizeIterator<Item = (FramePtr, &Data)> + '_ {
        self.vars
            .iter()
            .enumerate()
            .map(|(idx, data)| (FramePtr(idx + 1), data))
    }

    pub fn read_from_slice(machine: &Machine, e: StackPtr) -> MachineResult<StackFrame> {
        let StackPtr(index) = e;
        if index < 3 {
            return Err(MachineError::StackUnderflow);
        }

        let ce = machine.stack_global(e).to_stack()?;
        let cp = machine.stack_global(e + 1).to_code()?;
        let len = machine.stack_global(e + 2).to_len()?;
        let vars = ((e + 3).0..(e + 3 + len).0)
            .map(|y| Data::try_from(machine.stack_global(y.into())))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(StackFrame { ce, cp, vars })
    }
}

// TODO: write a bunch of tests for stack walk
pub struct StackWalk<'a> {
    machine: &'a Machine,
    current: StackPtr,
    is_error: bool,
}

impl<'a> StackWalk<'a> {
    pub fn new(machine: &'a Machine) -> Self {
        Self {
            machine,
            current: machine.e(),
            is_error: false,
        }
    }

    // naive implementation, does not care about looping
    fn next_raw(&mut self) -> Option<MachineResult<StackFrame>> {
        if self.current == 0.into() {
            // special case: empty stack
            return None;
        }

        match StackFrame::read_from_slice(self.machine, self.current) {
            Ok(frame) => {
                if frame.ce >= self.current {
                    return Some(Err(MachineError::StackTypeError));
                }
                self.current = frame.ce;
                Some(Ok(frame))
            }
            Err(e) => Some(Err(e)),
        }
    }
}

impl<'a> std::iter::Iterator for StackWalk<'a> {
    type Item = MachineResult<StackFrame>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_error {
            // returned failure: stop iterating
            return None;
        }

        match self.next_raw() {
            error @ Some(Err(_)) => {
                self.is_error = true;
                error
            }
            other => other,
        }
    }
}

pub struct StackIterator<'a> {
    machine: &'a Machine,
    start: usize,
    current: usize,
    end: usize,
}

impl<'a> StackIterator<'a> {
    pub fn new(machine: &'a Machine, start: usize, end: usize) -> Self {
        Self {
            machine,
            start,
            current: start,
            end,
        }
    }

    fn remaining_count(&self) -> usize {
        self.end - self.current
    }
}

impl<'a> Iterator for StackIterator<'a> {
    type Item = (FramePtr, MachineResult<Data>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.end {
            return None;
        }

        let result = Data::try_from(self.machine.stack_global(self.current.into()));
        self.current += 1;
        Some((FramePtr(self.current - self.start + 1), result))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining_count(), Some(self.remaining_count()))
    }
}

impl<'a> ExactSizeIterator for StackIterator<'a> {}

pub(super) fn stack_smash_check(
    machine: &Machine,
    frame_start: StackPtr,
    FramePtr(index): FramePtr,
) -> MResult {
    let len = machine.stack_global(frame_start + 2).to_len()?;
    if index <= len {
        Ok(())
    } else {
        Err(MachineError::StackSmash)
    }
}
