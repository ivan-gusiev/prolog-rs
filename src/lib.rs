use std::ops;
use std::fmt::{Display, Formatter};

type FunctorName = char;
type Arity = u32;

#[derive(Clone, Copy, Debug)]
pub struct HeapPtr(usize);

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

#[derive(Clone, Copy, Debug)]
pub struct RegPtr(pub usize);

impl Display for RegPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> { 
        write!(f, "X{}", self.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Ref(pub HeapPtr);

impl Display for Ref {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> { 
        write!(f, "<REF,{}>", self.0.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Str(pub HeapPtr);

impl Display for Str {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> { 
        write!(f, "<STR,{}>", self.0.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Functor(pub FunctorName, pub Arity);

impl Display for Functor {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> { 
        write!(f, "{}/{}", self.0, self.1)
    }
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    PutStructure(Functor, RegPtr),
    SetVariable(RegPtr),
    SetValue(RegPtr),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> { 
        match self {
            Instruction::PutStructure(functor, reg) => 
                write!(f, "put_structure {}, {}", functor, reg),
            Instruction::SetVariable(reg) => 
                write!(f, "set_variable {}", reg),
            Instruction::SetValue(reg) => 
                write!(f, "set_value {}", reg),
        }
    }
}

#[derive(Debug)]
pub struct Machine {
    heap: Vec<Data>,
    reg: Vec<Data>,
    code: Vec<Instruction>,
    h: HeapPtr,
}

impl Machine {
    pub fn new() -> Machine {
        Machine {
            heap: vec![],
            reg: vec![],
            code: vec![],
            h: HeapPtr(0),
        }
    }

    pub fn get_heap(&self, HeapPtr(index): HeapPtr) -> Data {
        if index < self.heap.len() {
            self.heap[index]
        } else {
            Data::Empty
        }
    }

    pub fn set_heap(&mut self, HeapPtr(index): HeapPtr, value: Data) {
        while index >= self.heap.len() {
            self.heap.push(Data::Empty)
        }
        self.heap[index] = value
    }

    pub fn iter_heap(&self) -> std::slice::Iter<'_, Data> {
        self.heap.iter()
    }

    pub fn get_reg(&self, RegPtr(index): RegPtr) -> Data {
        if index < self.reg.len() {
            self.reg[index]
        } else {
            Data::Empty
        }
    }

    pub fn set_reg(&mut self, RegPtr(index): RegPtr, value: Data) {
        while index >= self.reg.len() {
            self.reg.push(Data::Empty)
        }
        self.reg[index] = value
    }

    pub fn iter_reg(&self) -> std::slice::Iter<'_, Data> {
        self.reg.iter()
    }

    pub fn get_code(&self) -> Vec<Instruction> {
        self.code.to_vec()
    }

    pub fn set_code(&mut self, code: &[Instruction]) {
        self.code = code.to_vec()
    }

    pub fn get_h(&self) -> HeapPtr {
        self.h
    }

    pub fn set_h(&mut self, value: HeapPtr) {
        self.h = value
    }

    pub fn inc_h(&mut self, value: usize) {
        self.h += value
    }
}

fn put_structure(machine: &mut Machine, functor: Functor, register: RegPtr) {
    let h = machine.get_h();
    machine.set_heap(h, Data::Str(Str(h + 1)));
    machine.set_heap(h + 1, Data::Functor(functor));
    machine.set_reg(register, machine.get_heap(h));
    machine.inc_h(2);
}

fn set_variable(machine: &mut Machine, register: RegPtr) {
    let h = machine.get_h();
    machine.set_heap(h, Data::Ref(Ref(h)));
    machine.set_reg(register, machine.get_heap(h));
    machine.inc_h(1);
}

fn set_value(machine: &mut Machine, register: RegPtr) {
    let h = machine.get_h();
    machine.set_heap(h, machine.get_reg(register));
    machine.inc_h(1);
}

fn execute_instruction(machine: &mut Machine, instruction: Instruction) {
    match instruction {
        Instruction::PutStructure(functor, register) => put_structure(machine, functor, register),
        Instruction::SetVariable(register) => set_variable(machine, register),
        Instruction::SetValue(register) => set_value(machine, register),
    }
}

pub fn run_code(machine: &mut Machine) {
    for instruction in machine.get_code() {
        execute_instruction(machine, instruction)
    }
}

pub fn printout<T: Display>(items: &[T]) {
    let mut idx = 0;
    for item in items {
        println!("{:#03}\t{}", idx, item);
        idx += 1;
    }
}