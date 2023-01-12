pub mod asm;
pub mod compile;
pub mod data;
pub mod instr;
pub mod lang;
pub mod util;

use data::{Addr, Data, HeapPtr, Mode, Ref, RegPtr, Str};
use instr::Instruction;
use lang::Functor;

#[derive(Debug)]
pub struct Machine {
    heap: Vec<Data>,
    reg: Vec<Data>,
    code: Vec<Instruction>,
    h: HeapPtr,
    s: HeapPtr,
    mode: Mode,
    fail: bool,
}

impl Machine {
    pub fn new() -> Machine {
        Machine {
            heap: vec![],
            reg: vec![],
            code: vec![],
            h: HeapPtr(0),
            s: HeapPtr(0),
            mode: Mode::Read,
            fail: false,
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
        self.code = code.to_vec();
        self.reg.clear();
        self.fail = false;
    }

    pub fn get_h(&self) -> HeapPtr {
        self.h
    }

    pub fn set_h(&mut self, value: HeapPtr) {
        self.h = value
    }

    pub fn get_s(&self) -> HeapPtr {
        self.s
    }

    pub fn set_s(&mut self, value: HeapPtr) {
        self.s = value
    }

    pub fn get_mode(&self) -> Mode {
        self.mode
    }

    pub fn set_mode(&mut self, mode: Mode) {
        self.mode = mode
    }

    pub fn get_fail(&self) -> bool {
        self.fail
    }

    pub fn set_fail(&mut self, fail: bool) {
        self.fail = fail
    }

    pub fn get_store(&self, addr: Addr) -> Data {
        match addr {
            Addr::Heap(heap_ptr) => self.get_heap(heap_ptr),
            Addr::Reg(reg_ptr) => self.get_reg(reg_ptr),
        }
    }

    pub fn set_store(&mut self, addr: Addr, value: Data) {
        match addr {
            Addr::Heap(heap_ptr) => self.set_heap(heap_ptr, value),
            Addr::Reg(reg_ptr) => self.set_reg(reg_ptr, value),
        }
    }

}

fn deref(machine: &Machine, mut addr: Addr) -> Addr {
    loop {
        match machine.get_store(addr) {
            Data::Ref(Ref(new_addr)) if addr != new_addr.into() => {
                addr = new_addr.into()
            },
            _ => break,
        }
    }
    addr
}

fn bind(machine: &mut Machine, lhs: Addr, rhs: HeapPtr) {
    machine.set_store(lhs, Ref(rhs).into())
}

fn put_structure(machine: &mut Machine, functor: Functor, register: RegPtr) {
    let h = machine.get_h();
    machine.set_heap(h, Str(h + 1).into());
    machine.set_heap(h + 1, functor.into());
    machine.set_reg(register, machine.get_heap(h));
    machine.set_h(h + 2);
}

fn set_variable(machine: &mut Machine, register: RegPtr) {
    let h = machine.get_h();
    machine.set_heap(h, Data::Ref(Ref(h)));
    machine.set_reg(register, machine.get_heap(h));
    machine.set_h(h + 1);
}

fn set_value(machine: &mut Machine, register: RegPtr) {
    let h = machine.get_h();
    machine.set_heap(h, machine.get_reg(register));
    machine.set_h(h + 1);
}

fn get_structure(machine: &mut Machine, functor: Functor, register: RegPtr) {
    let addr = deref(machine, register.into());
    match machine.get_store(addr) {
        Data::Ref(Ref(_)) => {
            let h = machine.get_h();
            machine.set_heap(h, Str(h + 1).into());
            machine.set_heap(h + 1, functor.into());
            bind(machine, addr, h);
            machine.set_h(h + 2);
            machine.set_mode(Mode::Write);
        },
        Data::Str(Str(a)) => {
            if machine.get_heap(a) == functor.into() {
                machine.set_s(a + 1);
                machine.set_mode(Mode::Read);
            } else {
                machine.set_fail(true);
            }
        },
        _ => machine.set_fail(true),
    }
}

fn unify_variable(machine: &mut Machine, register: RegPtr) {
    let s = machine.get_s();
    let h = machine.get_h();
    match machine.mode {
        Mode::Read => machine.set_reg(register, machine.get_heap(s)),
        Mode::Write => {
            machine.set_heap(h, Ref(h).into());
            machine.set_reg(register, machine.get_heap(h));
            machine.set_h(h + 1);
        }
    }
    machine.set_s(s + 1);
}

fn unify_value(machine: &mut Machine, register: RegPtr) {
    let s = machine.get_s();
    let h = machine.get_h();
    match machine.mode {
        Mode::Read => todo!(),
        Mode::Write => {
            machine.set_heap(h, machine.get_reg(register));
            machine.set_h(h + 1);
        }
    }
    machine.set_s(s + 1);
}

fn execute_instruction(machine: &mut Machine, instruction: Instruction) {
    match instruction {
        Instruction::PutStructure(functor, register) => put_structure(machine, functor, register),
        Instruction::SetVariable(register) => set_variable(machine, register),
        Instruction::SetValue(register) => set_value(machine, register),
        Instruction::GetStructure(functor, register) => get_structure(machine, functor, register),
        Instruction::UnifyVariable(register) => unify_variable(machine, register),
        Instruction::UnifyValue(register) => unify_value(machine, register),
    }
}

pub fn run_code(machine: &mut Machine) {
    for instruction in machine.get_code() {
        execute_instruction(machine, instruction)
    }
}
