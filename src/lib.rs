pub mod asm;
pub mod compile;
pub mod data;
pub mod instr;
pub mod lang;
pub mod symbol;
pub mod util;

use data::{Addr, Data, HeapPtr, Mode, Ref, RegPtr, Str};
use instr::Instruction;
use lang::Functor;
use symbol::SymbolTable;
use std::fmt::{Display, Write};

use util::{writeout, writeout_sym};

#[derive(Debug)]
pub struct Machine {
    heap: Vec<Data>,
    reg: Vec<Data>,
    code: Vec<Instruction>,
    h: HeapPtr,
    s: HeapPtr,
    mode: Mode,
    fail: bool,
    pdl: Vec<Addr>,
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
            pdl: vec![],
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

    pub fn push_pdl(&mut self, addr: Addr) {
        self.pdl.push(addr)
    }

    pub fn pop_pdl(&mut self) -> Option<Addr> {
        self.pdl.pop()
    }

    pub fn empty_pdl(&self) -> bool {
        self.pdl.is_empty()
    }

    pub fn dbg(&self, symbol_table: &SymbolTable) -> String {
        let mut str = String::new();
        writeln!(str, "{}: {}", "h", self.get_h()).unwrap();
        writeln!(str, "{}: {}", "s", self.get_s()).unwrap();
        writeln!(str, "{}: {}", "mode", self.get_mode()).unwrap();
        writeln!(str, "{}: {}", "fail", self.get_fail()).unwrap();
        writeln!(str, "{}:\n{}", "code", writeout_sym(&self.get_code(), symbol_table)).unwrap();
        writeln!(str, "{}:\n{}", "heap", writeout_sym(&self.heap, symbol_table)).unwrap();
        writeln!(str, "{}:\n{}", "regs", writeout_sym(&self.reg, symbol_table)).unwrap();
        writeln!(str, "{}:\n{}", "pdl", writeout(self.pdl.iter())).unwrap();
        str
    }
}

#[derive(Debug)]
pub enum MachineFailure {
    RegBind,
    NoStrFunctor,
}

impl MachineFailure {
    pub fn message(&self) -> &'static str {
        match self {
            Self::RegBind => "Attempted to bind two registers",
            Self::NoStrFunctor => "Struct does not point to a functor",
        }
    }
}

impl Display for MachineFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

type MResult = Result<(), MachineFailure>;

fn deref(machine: &Machine, mut addr: Addr) -> Addr {
    loop {
        match machine.get_store(addr) {
            Data::Ref(Ref(new_addr)) if addr != new_addr.into() => addr = new_addr.into(),
            _ => break,
        }
    }
    addr
}

fn bind(machine: &mut Machine, lhs: Addr, rhs: Addr) -> MResult {
    match (lhs, rhs) {
        (Addr::Reg(_), Addr::Reg(_)) => Err(MachineFailure::RegBind),
        (Addr::Heap(_), Addr::Reg(_)) => bind(machine, rhs, lhs),
        (_, Addr::Heap(rhs_heap)) => Ok(machine.set_store(lhs, Ref(rhs_heap).into())),
    }
}

fn unify(machine: &mut Machine, a1: Addr, a2: Addr) -> MResult {
    machine.push_pdl(a1);
    machine.push_pdl(a2);
    machine.set_fail(false);
    while !(machine.empty_pdl() || machine.get_fail()) {
        let mut d1 = machine.pop_pdl().expect("cannot pop d1");
        d1 = deref(machine, d1);
        let mut d2 = machine.pop_pdl().expect("cannot pop d2");
        d2 = deref(machine, d2);

        if d1 == d2 {
            break;
        }

        match (machine.get_store(d1), machine.get_store(d2)) {
            (Data::Ref(_), _) => bind(machine, d1, d2)?,
            (_, Data::Ref(_)) => bind(machine, d1, d2)?,
            (Data::Str(Str(v1)), Data::Str(Str(v2))) => {
                let f1 = machine
                    .get_heap(v1)
                    .get_functor()
                    .ok_or(MachineFailure::NoStrFunctor)?;
                let f2 = machine
                    .get_heap(v2)
                    .get_functor()
                    .ok_or(MachineFailure::NoStrFunctor)?;
                if f1 == f2 {
                    for i in 1..=f1.arity() {
                        machine.push_pdl((v1 + i as usize).into());
                        machine.push_pdl((v2 + i as usize).into());
                    }
                } else {
                    machine.set_fail(true)
                };
            }
            _ => machine.set_fail(true),
        }
    }
    Ok(())
}

fn put_structure(machine: &mut Machine, functor: Functor, register: RegPtr) -> MResult {
    let h = machine.get_h();
    machine.set_heap(h, Str(h + 1).into());
    machine.set_heap(h + 1, functor.into());
    machine.set_reg(register, machine.get_heap(h));
    machine.set_h(h + 2);
    Ok(())
}

fn set_variable(machine: &mut Machine, register: RegPtr) -> MResult {
    let h = machine.get_h();
    machine.set_heap(h, Data::Ref(Ref(h)));
    machine.set_reg(register, machine.get_heap(h));
    machine.set_h(h + 1);
    Ok(())
}

fn set_value(machine: &mut Machine, register: RegPtr) -> MResult {
    let h = machine.get_h();
    machine.set_heap(h, machine.get_reg(register));
    machine.set_h(h + 1);
    Ok(())
}

fn get_structure(machine: &mut Machine, functor: Functor, register: RegPtr) -> MResult {
    let addr = deref(machine, register.into());
    match machine.get_store(addr) {
        Data::Ref(Ref(_)) => {
            let h = machine.get_h();
            machine.set_heap(h, Str(h + 1).into());
            machine.set_heap(h + 1, functor.into());
            bind(machine, addr, h.into())?;
            machine.set_h(h + 2);
            machine.set_mode(Mode::Write);
        }
        Data::Str(Str(a)) => {
            if machine.get_heap(a) == functor.into() {
                machine.set_s(a + 1);
                machine.set_mode(Mode::Read);
            } else {
                machine.set_fail(true);
            }
        }
        _ => machine.set_fail(true),
    }
    Ok(())
}

fn unify_variable(machine: &mut Machine, register: RegPtr) -> MResult {
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
    Ok(())
}

fn unify_value(machine: &mut Machine, register: RegPtr) -> MResult {
    let s = machine.get_s();
    let h = machine.get_h();
    match machine.mode {
        Mode::Read => unify(machine, register.into(), s.into())?,
        Mode::Write => {
            machine.set_heap(h, machine.get_reg(register));
            machine.set_h(h + 1);
        }
    }
    machine.set_s(s + 1);
    Ok(())
}

fn execute_instruction(machine: &mut Machine, instruction: Instruction) -> MResult {
    match instruction {
        Instruction::PutStructure(functor, register) => put_structure(machine, functor, register),
        Instruction::SetVariable(register) => set_variable(machine, register),
        Instruction::SetValue(register) => set_value(machine, register),
        Instruction::GetStructure(functor, register) => get_structure(machine, functor, register),
        Instruction::UnifyVariable(register) => unify_variable(machine, register),
        Instruction::UnifyValue(register) => unify_value(machine, register),
    }
}

pub fn run_code(machine: &mut Machine) -> MResult {
    for instruction in machine.get_code() {
        execute_instruction(machine, instruction)?;
        if machine.get_fail() {
            break;
        }
    }
    Ok(())
}
