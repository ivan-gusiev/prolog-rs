use std::{
    convert::TryFrom,
    ops::{Index, Range, RangeFrom},
};

use crate::{
    data::{CodePtr, Data, FramePtr, Ref, StackDepth, StackPtr, Str},
    lang::Functor,
};

use super::{MResult, Machine, MachineError, MachineResult};

#[derive(Debug, Clone, Copy)]
pub enum StackData {
    Empty,
    Ref(Ref),
    Str(Str),
    Functor(Functor),
    Frame(StackPtr),
    Code(CodePtr),
    Len(usize),
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

#[derive(Copy, Clone, Debug)]
pub struct StackSlice<'a>(&'a [StackData]);

impl<'a> From<&'a [StackData]> for StackSlice<'a> {
    fn from(value: &'a [StackData]) -> Self {
        Self(value)
    }
}

impl<'a> Index<RangeFrom<StackPtr>> for StackSlice<'a> {
    type Output = [StackData];

    fn index(&self, index: RangeFrom<StackPtr>) -> &Self::Output {
        &self.0[RangeFrom {
            start: index.start.0,
        }]
    }
}

impl<'a> Index<Range<StackPtr>> for StackSlice<'a> {
    type Output = [StackData];

    fn index(&self, index: Range<StackPtr>) -> &Self::Output {
        &self.0[Range {
            start: index.start.0,
            end: index.end.0,
        }]
    }
}

#[derive(Debug)]
pub struct StackFrame {
    pub ce: StackPtr,
    pub cp: CodePtr,
    pub(super) vars: Vec<Data>,
}

impl StackFrame {
    pub fn new(ce: StackPtr, cp: CodePtr, depth: StackDepth) -> StackFrame {
        StackFrame {
            ce,
            cp,
            vars: std::iter::repeat(Data::Empty).take(depth.0).collect(),
        }
    }

    pub fn size(&self) -> usize {
        3 + self.vars.len()
    }

    pub fn get_var(&self, FramePtr(index): FramePtr) -> MachineResult<Data> {
        self.vars
            .get(index - 1)
            .copied()
            .ok_or(MachineError::StackSmash)
    }

    pub fn set_var(&mut self, FramePtr(index): FramePtr, value: Data) -> MResult {
        match self.vars.get_mut(index - 1) {
            Some(slot) => {
                *slot = value;
                Ok(())
            }
            None => Err(MachineError::StackSmash),
        }
    }

    pub fn iter_var(&self) -> impl ExactSizeIterator<Item = (FramePtr, &Data)> + '_ {
        self.vars
            .iter()
            .enumerate()
            .map(|(idx, data)| (FramePtr(idx + 1), data))
    }

    pub fn write_to_vec(&self) -> Vec<StackData> {
        let mut result = vec![
            StackData::Frame(self.ce),
            StackData::Code(self.cp),
            StackData::Len(self.vars.len()),
        ];

        result.extend(self.vars.iter().copied().map(StackData::from));
        result
    }

    pub fn read_from_slice(data: &[StackData]) -> MachineResult<StackFrame> {
        if data.len() < 3 {
            return Err(MachineError::StackUnderflow);
        }

        let ce = match data[0] {
            StackData::Frame(ce) => ce,
            _ => return Err(MachineError::StackTypeError),
        };
        let cp = match data[1] {
            StackData::Code(cp) => cp,
            _ => return Err(MachineError::StackTypeError),
        };
        let len = match data[2] {
            StackData::Len(len) => len,
            _ => return Err(MachineError::StackTypeError),
        };
        let vars = data[3..]
            .iter()
            .take(len)
            .copied()
            .map(Data::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        if vars.len() < len {
            return Err(MachineError::StackSmash);
        }

        Ok(StackFrame { ce, cp, vars })
    }

    pub fn write_through(&mut self, machine: &mut Machine, ptr: FramePtr, data: Data) -> MResult {
        let var_index = ptr.0 - 1;
        if var_index >= self.vars.len() {
            return Err(MachineError::StackSmash);
        }

        self.vars[var_index] = data;

        let global_index = machine.e() + 3 + var_index;
        let stack_cell = machine
            .stack
            .get_mut(global_index.0)
            .ok_or(MachineError::StackUnderflow)?;
        *stack_cell = data.into();

        Ok(())
    }
}

pub struct StackWalk<'a> {
    machine: &'a Machine,
    current: StackPtr,
    prev: Option<StackPtr>,
    is_error: bool,
}

impl<'a> StackWalk<'a> {
    pub fn new(machine: &'a Machine) -> Self {
        Self {
            machine,
            current: machine.e(),
            prev: None,
            is_error: false,
        }
    }
}

impl<'a> std::iter::Iterator for StackWalk<'a> {
    type Item = MachineResult<StackFrame>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_error {
            // we returned error last time,
            // stop the iteration
            return None;
        }

        if let Some(prev_frame) = self.prev {
            if self.current == prev_frame {
                // when current == prev == 0
                // we have reached the end of the stack
                return None;
            }
        }

        self.prev = Some(self.current);

        let slice = &self.machine.stack_global_deprecated()[self.current..];
        if slice.is_empty() && self.current.0 == 0 {
            // special case: empty stack
            return None;
        }

        match StackFrame::read_from_slice(slice) {
            Ok(frame) => {
                self.current = frame.ce;
                Some(Ok(frame))
            }
            Err(e) => Some(Err(e)),
        }
    }
}

pub struct StackIterator<'a> {
    vars: &'a [StackData],
    current: usize,
}

impl<'a> StackIterator<'a> {
    pub fn new(vars: &'a [StackData]) -> Self {
        Self { vars, current: 0 }
    }

    fn remaining_count(&self) -> usize {
        self.vars.len() - self.current
    }
}

impl<'a> Iterator for StackIterator<'a> {
    type Item = (FramePtr, Data);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.vars.len() {
            return None;
        }

        let result = Data::try_from(self.vars[self.current]).ok();
        self.current += 1;
        result.map(|d| (FramePtr(self.current + 1), d))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining_count(), Some(self.remaining_count()))
    }
}

impl<'a> ExactSizeIterator for StackIterator<'a> {}
