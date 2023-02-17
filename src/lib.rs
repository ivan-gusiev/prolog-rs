pub mod asm;
pub mod compile;
pub mod data;
pub mod instr;
pub mod lang;
pub mod symbol;
pub mod util;
pub mod var;

use data::{Addr, CodePtr, Data, HeapPtr, Mode, Ref, RegPtr, Str};
use instr::Instruction;
use lang::{Functor, Term, VarName};
use std::fmt::{Display, Write};
use symbol::{to_display, SymDisplay, SymbolTable};
use var::{VarBindings, VarMapping, VarValues};

use util::{writeout, writeout_sym};

use crate::lang::Struct;

#[derive(Debug)]
pub struct Machine {
    heap: Vec<Data>,
    reg: Vec<Data>,
    code: Vec<Instruction>,
    h: HeapPtr, // heap top
    s: HeapPtr, // subterm to be matched
    p: CodePtr, // instruction to be executed
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
            p: CodePtr(0),
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

    pub fn get_p(&self) -> CodePtr {
        self.p
    }

    pub fn set_p(&mut self, value: CodePtr) {
        self.p = value
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

    pub fn is_pdl_empty(&self) -> bool {
        self.pdl.is_empty()
    }

    pub fn trace_reg(&self, reg: RegPtr) -> MachineResult<HeapPtr> {
        match self.get_reg(reg) {
            Data::Ref(Ref(ptr)) | Data::Str(Str(ptr)) => Ok(ptr),
            _ => Err(MachineFailure::NonVarBind),
        }
    }

    pub fn bind_variables(&self, var_mapping: &VarMapping) -> MachineResult<VarBindings> {
        var_mapping.traverse(|&reg| self.trace_reg(reg))
    }

    pub fn load_variables(&self, var_bindings: &VarBindings) -> MachineResult<VarValues> {
        var_bindings.traverse(|&ptr| self.decompile_addr(ptr.into(), var_bindings))
    }

    pub fn decompile_addr(&self, addr: Addr, var_bindings: &VarBindings) -> MachineResult<Term> {
        self.decompile_addr_impl(addr, &var_bindings)
    }

    fn decompile_addr_impl(&self, addr: Addr, var_labels: &VarBindings) -> MachineResult<Term> {
        let decompile_functor = |ptr: HeapPtr, f: Functor| {
            let mut subterms = Vec::<Term>::new();
            for i in 1..=f.arity() {
                subterms.push(self.decompile_addr_impl((ptr + i as usize).into(), var_labels)?)
            }
            Struct::new(f, &subterms)
                .map(Term::Struct)
                .map_err(|_| MachineFailure::BadArity)
        };

        let decompile_str = |Str(ptr): Str| match self.get_heap(ptr) {
            Data::Functor(f) => decompile_functor(ptr, f),
            _ => Err(MachineFailure::NoStrFunctor),
        };

        let decompile_ref = |Ref(mut ptr): Ref| {
            let mut last_name: Option<VarName> = None;
            loop {
                if let Some(new_name) = var_labels.get(&ptr) {
                    last_name = Some(new_name)
                }

                match self.get_heap(ptr) {
                    Data::Ref(Ref(next_ptr)) => {
                        if ptr != next_ptr {
                            ptr = next_ptr
                        } else if let Some(name) = last_name {
                            break Ok(Term::Variable(name));
                        } else {
                            break Err(MachineFailure::UnknownVariable);
                        }
                    }
                    Data::Str(str) => break decompile_str(str),
                    Data::Functor(f) => break decompile_functor(ptr, f),
                    Data::Empty => break Err(MachineFailure::EmptyRef),
                }
            }
        };

        let decompile_heap = |ptr: HeapPtr| match self.get_heap(ptr) {
            Data::Ref(r) => decompile_ref(r),
            Data::Str(str) => decompile_str(str),
            Data::Functor(f) => decompile_functor(ptr, f),
            Data::Empty => Err(MachineFailure::EmptyRef),
        };

        let decompile_reg = |ptr: RegPtr| match self.get_reg(ptr) {
            Data::Ref(r) => decompile_ref(r),
            Data::Str(str) => decompile_str(str),
            _ => Err(MachineFailure::InvalidRegData),
        };

        match addr {
            Addr::Reg(reg_ptr) => decompile_reg(reg_ptr),
            Addr::Heap(heap_ptr) => decompile_heap(heap_ptr),
        }
    }

    pub fn decompile(&self, addr: Addr, var_mapping: &VarMapping) -> Option<Term> {
        fn decompile_functor(
            machine: &Machine,
            ptr: HeapPtr,
            f: Functor,
            var_mapping: &VarMapping,
        ) -> Option<Term> {
            let mut subterms = Vec::<Term>::new();
            for i in 1..=f.arity() {
                if let Some(subterm) = machine.decompile((ptr + i as usize).into(), var_mapping) {
                    subterms.push(subterm)
                } else {
                    println!("Could not decompile subterm");
                    return None;
                }
            }
            Struct::new(f, &subterms).ok().map(Term::Struct)
        }

        fn decompile_str(
            machine: &Machine,
            Str(ptr): Str,
            var_mapping: &VarMapping,
        ) -> Option<Term> {
            match machine.get_heap(ptr) {
                Data::Functor(f) => decompile_functor(machine, ptr, f, var_mapping),
                _ => {
                    println!("COULD NOT DECOMPILE STR");
                    None
                }
            }
        }

        fn decompile_ref(
            machine: &Machine,
            Ref(ptr): Ref,
            var_mapping: &VarMapping,
        ) -> Option<Term> {
            match machine.get_heap(ptr) {
                h @ Data::Ref(Ref(r)) if r == ptr => {
                    for (i, val) in machine.iter_reg().enumerate() {
                        if *val == h {
                            if let Some(known_var) = var_mapping.get(&RegPtr(i)) {
                                return Some(Term::Variable(known_var));
                            }
                        }
                    }
                    println!("Could not decompile ref");
                    None
                }
                Data::Ref(next_ref) => decompile_ref(machine, next_ref, var_mapping),
                Data::Str(str) => decompile_str(machine, str, var_mapping),
                Data::Functor(f) => decompile_functor(machine, ptr, f, var_mapping),
                _ => {
                    println!("COULD NOT DECOMPILE REF");
                    None
                }
            }
        }

        fn decompile_heap(
            machine: &Machine,
            ptr: HeapPtr,
            var_mapping: &VarMapping,
        ) -> Option<Term> {
            match machine.get_heap(ptr) {
                Data::Ref(r) => decompile_ref(machine, r, var_mapping),
                Data::Str(str) => decompile_str(machine, str, var_mapping),
                Data::Functor(f) => decompile_functor(machine, ptr, f, var_mapping),
                _ => {
                    println!("COULD NOT DECOMPILE HEAP");
                    None
                }
            }
        }

        fn decompile_reg(machine: &Machine, ptr: RegPtr, var_mapping: &VarMapping) -> Option<Term> {
            match machine.get_reg(ptr) {
                Data::Ref(r) => decompile_ref(machine, r, var_mapping),
                Data::Str(str) => decompile_str(machine, str, var_mapping),
                _ => {
                    println!("COULD NOT DECOMPILE REG");
                    None
                }
            }
        }

        match addr {
            Addr::Reg(reg_ptr) => decompile_reg(self, reg_ptr, var_mapping),
            Addr::Heap(heap_ptr) => decompile_heap(self, heap_ptr, var_mapping),
        }
    }

    pub fn describe_vars(self: &Machine, var_mapping: &VarMapping) -> Vec<VarDescription> {
        let mut mappings = var_mapping
            .info()
            .map(|(&r, &n)| (n, r))
            .collect::<Vec<(VarName, RegPtr)>>();
        mappings.sort_by_key(|(v, _)| *v);

        mappings
            .into_iter()
            .filter_map(|(name, reg)| {
                self.decompile(reg.into(), var_mapping)
                    .map(|term| VarDescription(name, reg, self.get_reg(reg), term))
            })
            .collect()
    }

    pub fn dbg(&self, symbol_table: &SymbolTable) -> String {
        let mut str = String::new();
        writeln!(str, "h: {}", self.get_h()).unwrap();
        writeln!(str, "s: {}", self.get_s()).unwrap();
        writeln!(str, "mode: {}", self.get_mode()).unwrap();
        writeln!(str, "fail: {}", self.get_fail()).unwrap();
        writeln!(
            str,
            "code:\n{}",
            writeout_sym(&self.get_code(), symbol_table)
        )
        .unwrap();
        writeln!(str, "heap:\n{}", writeout_sym(&self.heap, symbol_table)).unwrap();
        writeln!(str, "regs:\n{}", writeout_sym(&self.reg, symbol_table)).unwrap();
        writeln!(str, "pdl:\n{}", writeout(self.pdl.iter())).unwrap();
        str
    }
}

impl Default for Machine {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum MachineFailure {
    RegBind,
    NoStrFunctor,
    BadArity,
    NonVarBind,
    EmptyRef,
    UnknownVariable, // TODO: this should really be a decompilation failure, not machine failure
    InvalidRegData,
}

impl MachineFailure {
    pub fn message(&self) -> &'static str {
        match self {
            Self::RegBind => "Attempted to bind two registers",
            Self::NoStrFunctor => "Struct does not point to a functor",
            Self::BadArity => "Some functor subterms not found on heap",
            Self::NonVarBind => "Attemted to bind variable to a non-var register",
            Self::EmptyRef => "Term points to an empty heap cell",
            Self::UnknownVariable => "Decompilation of a variable with unknown name",
            Self::InvalidRegData => "Register contains something other than reference to heap",
        }
    }
}

impl Display for MachineFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

type IResult = Result<Option<CodePtr>, MachineFailure>;
type MResult = Result<(), MachineFailure>;
type MachineResult<T> = Result<T, MachineFailure>;

#[derive(Debug)]
pub struct VarDescription(VarName, RegPtr, Data, Term);

impl SymDisplay for VarDescription {
    fn sym_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}\t({}) =\t{}\t// {}",
            to_display(&self.0, symbol_table),
            self.1,
            to_display(&self.2, symbol_table),
            to_display(&self.3, symbol_table),
        )
    }
}

impl VarDescription {
    pub fn short(&self, symbol_table: &SymbolTable) -> String {
        format!(
            "{0} = {1}",
            to_display(&self.0, symbol_table),
            to_display(&self.3, symbol_table)
        )
    }

    pub fn to_assignment(&self) -> (VarName, Term) {
        (self.0, self.3.clone())
    }
}

#[derive(Debug, Default)]
pub struct RunningContext {
    pub machine: Machine,
    pub symbol_table: SymbolTable,
    pub query_variables: VarMapping,
    pub program_variables: VarMapping,
}

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
        (_, Addr::Heap(rhs_heap)) => {
            machine.set_store(lhs, Ref(rhs_heap).into());
            Ok(())
        }
    }
}

fn unify(machine: &mut Machine, a1: Addr, a2: Addr) -> MResult {
    machine.push_pdl(a1);
    machine.push_pdl(a2);
    machine.set_fail(false);
    while !(machine.is_pdl_empty() || machine.get_fail()) {
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

fn put_structure(machine: &mut Machine, functor: Functor, register: RegPtr) -> IResult {
    let h = machine.get_h();
    machine.set_heap(h, Str(h + 1).into());
    machine.set_heap(h + 1, functor.into());
    machine.set_reg(register, machine.get_heap(h));
    machine.set_h(h + 2);
    Ok(None)
}

fn set_variable(machine: &mut Machine, register: RegPtr) -> IResult {
    let h = machine.get_h();
    machine.set_heap(h, Data::Ref(Ref(h)));
    machine.set_reg(register, machine.get_heap(h));
    machine.set_h(h + 1);
    Ok(None)
}

fn set_value(machine: &mut Machine, register: RegPtr) -> IResult {
    let h = machine.get_h();
    machine.set_heap(h, machine.get_reg(register));
    machine.set_h(h + 1);
    Ok(None)
}

fn get_structure(machine: &mut Machine, functor: Functor, register: RegPtr) -> IResult {
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
    Ok(None)
}

fn unify_variable(machine: &mut Machine, register: RegPtr) -> IResult {
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
    Ok(None)
}

fn unify_value(machine: &mut Machine, register: RegPtr) -> IResult {
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
    Ok(None)
}

fn call(_machine: &mut Machine, ptr: CodePtr) -> IResult {
    Ok(Some(ptr))
}

fn proceed(_machine: &mut Machine) -> IResult {
    Ok(None)
}

fn put_variable(machine: &mut Machine, xreg: RegPtr, areg: RegPtr) -> IResult {
    let h = machine.get_h();
    let data = Ref(h).into();
    machine.set_heap(h, data);
    machine.set_reg(xreg, data);
    machine.set_reg(areg, data);
    machine.set_h(h + 1);
    Ok(None)
}

fn put_value(machine: &mut Machine, xreg: RegPtr, areg: RegPtr) -> IResult {
    machine.set_reg(areg, machine.get_reg(xreg));
    Ok(None)
}

fn get_variable(machine: &mut Machine, xreg: RegPtr, areg: RegPtr) -> IResult {
    machine.set_reg(xreg, machine.get_reg(areg));
    Ok(None)
}

fn get_value(machine: &mut Machine, xreg: RegPtr, areg: RegPtr) -> IResult {
    unify(machine, xreg.into(), areg.into())?;
    Ok(None)
}

fn execute_instruction(machine: &mut Machine, instruction: Instruction) -> MResult {
    let nextp = match instruction {
        Instruction::PutStructure(functor, register) => put_structure(machine, functor, register),
        Instruction::SetVariable(register) => set_variable(machine, register),
        Instruction::SetValue(register) => set_value(machine, register),
        Instruction::GetStructure(functor, register) => get_structure(machine, functor, register),
        Instruction::UnifyVariable(register) => unify_variable(machine, register),
        Instruction::UnifyValue(register) => unify_value(machine, register),
        Instruction::Call(code) => call(machine, code),
        Instruction::Proceed => proceed(machine),
        Instruction::PutVariable(xreg, areg) => put_variable(machine, xreg, areg),
        Instruction::PutValue(xreg, areg) => put_value(machine, xreg, areg),
        Instruction::GetVariable(xreg, areg) => get_variable(machine, xreg, areg),
        Instruction::GetValue(xreg, areg) => get_value(machine, xreg, areg),
    }?;
    machine.set_p(nextp.unwrap_or(machine.get_p() + 1));
    Ok(())
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
