pub mod asm;
pub mod compile;
pub mod data;
pub mod instr;
pub mod lang;
pub mod util;

use data::{Data, HeapPtr, Ref, RegPtr, Str};
use instr::Instruction;
use lang::Functor;

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
        x => panic!("Cannot execute instruction [{}]", x),
    }
}

pub fn run_code(machine: &mut Machine) {
    for instruction in machine.get_code() {
        execute_instruction(machine, instruction)
    }
}
