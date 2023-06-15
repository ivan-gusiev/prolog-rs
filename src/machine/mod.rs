pub mod stack;

use asm::Assembly;
use construct::{ConstructEnvironment, ConstructResult};
use data::{
    Addr, CodePtr, Data, FramePtr, HeapPtr, Mode, Ref, RegPtr, StackDepth, StackPtr, Str, VarRecord,
};
use instr::Instruction;
use lang::{Functor, Term, VarName};
use machine::stack::StackData;
use std::{
    convert::TryInto,
    fmt::{Display, Write},
};
use symbol::SymbolTable;
use var::{VarBindings, VarDescription, VarMapping};

use util::{writeout, writeout_sym};

use self::stack::{stack_smash_check, StackIterator, StackWalk};

#[derive(Debug)]
pub struct Machine {
    heap: Vec<Data>,
    reg: Vec<Data>,
    code: Vec<Instruction>,
    stack: Vec<StackData>,
    h: HeapPtr,  // heap top
    s: HeapPtr,  // subterm to be matched
    p: CodePtr,  // instruction to be executed
    cp: CodePtr, // continuation instruction to proceed to
    e: StackPtr, // current stack environment
    mode: Mode,
    fail: bool,
    halt: bool,
    pdl: Vec<Addr>,       // push-down list for unification
    vars: Vec<VarRecord>, // variable mappings for query result
}

impl Machine {
    pub fn new() -> Machine {
        Machine {
            heap: vec![],
            reg: vec![],
            code: vec![],
            stack: vec![],
            h: HeapPtr(0),
            s: HeapPtr(0),
            p: CodePtr(0),
            cp: CodePtr(0),
            e: StackPtr(0),
            mode: Mode::Read,
            fail: false,
            halt: false,
            pdl: vec![],
            vars: vec![],
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

    pub fn heap_len(&self) -> usize {
        self.heap.len()
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

    pub fn iter_reg(&self) -> impl ExactSizeIterator<Item = (RegPtr, &Data)> {
        self.reg
            .iter()
            .enumerate()
            .map(|(idx, data)| (RegPtr(idx), data))
    }

    pub fn stack_global(&self, StackPtr(index): StackPtr) -> StackData {
        if index < self.stack.len() {
            self.stack[index]
        } else {
            StackData::Empty
        }
    }

    pub fn stack_global_mut(&mut self, StackPtr(index): StackPtr) -> &mut StackData {
        while index >= self.stack.len() {
            self.stack.push(StackData::Empty)
        }
        &mut self.stack[index]
    }

    pub fn get_frame(&self, ptr: FramePtr) -> MachineResult<Data> {
        let e = self.e();
        stack_smash_check(self, e, ptr)?;
        // FramePtr is 1-based, so the first possible variable is located at `e + 3`
        self.stack_global(e + 2 + ptr).try_into()
    }

    pub fn set_frame(&mut self, ptr: FramePtr, value: Data) -> MResult {
        let e = self.e();
        stack_smash_check(self, e, ptr)?;
        *self.stack_global_mut(e + 2 + ptr) = value.into();
        Ok(())
    }

    pub fn iter_stack(
        &self,
    ) -> MachineResult<impl ExactSizeIterator<Item = (FramePtr, MachineResult<Data>)> + '_> {
        let e = self.e();
        let len = self.stack_global(e + 2).to_len()?;
        Ok(StackIterator::new(self, e.0 + 3, e.0 + 3 + len))
    }

    pub fn walk_stack(&self) -> StackWalk {
        StackWalk::new(self)
    }

    pub fn get_code(&self) -> Vec<Instruction> {
        self.code.to_vec()
    }

    pub fn set_code(&mut self, code: &[Instruction]) {
        self.code = code.to_vec();
        self.fail = false;
        self.halt = false;
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    /// Returns the insertion point, so that P can be set to it
    pub fn append_code(&mut self, code: &[Instruction]) -> CodePtr {
        let result = CodePtr(self.code.len());
        self.code.extend_from_slice(code);
        self.fail = false;
        self.halt = false;
        result
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

    pub fn get_cp(&self) -> CodePtr {
        self.cp
    }

    pub fn set_cp(&mut self, value: CodePtr) {
        self.cp = value
    }

    pub fn e(&self) -> StackPtr {
        self.e
    }

    pub fn e_mut(&mut self) -> &mut StackPtr {
        &mut self.e
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

    pub fn get_halt(&self) -> bool {
        self.halt
    }

    pub fn set_halt(&mut self, halt: bool) {
        self.halt = halt
    }

    pub fn get_store(&self, addr: Addr) -> MachineResult<Data> {
        match addr {
            Addr::Heap(heap_ptr) => Ok(self.get_heap(heap_ptr)),
            Addr::Reg(reg_ptr) => Ok(self.get_reg(reg_ptr)),
            Addr::Stack(stack_ptr) => self.get_frame(stack_ptr),
        }
    }

    pub fn set_store(&mut self, addr: Addr, value: Data) -> MResult {
        match addr {
            Addr::Heap(heap_ptr) => {
                self.set_heap(heap_ptr, value);
                Ok(())
            }
            Addr::Reg(reg_ptr) => {
                self.set_reg(reg_ptr, value);
                Ok(())
            }
            Addr::Stack(stack_ptr) => self.set_frame(stack_ptr, value),
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

    pub fn vars(&self) -> &[VarRecord] {
        &self.vars
    }

    pub fn vars_mut(&mut self) -> &mut Vec<VarRecord> {
        &mut self.vars
    }

    pub fn set_var_mappings(&mut self, mapping: &VarMapping) {
        self.vars.clear();
        self.vars
            .extend(mapping.iter().map(VarRecord::from_mapping));
    }

    pub fn get_var_bindings(&self) -> VarBindings {
        self.vars
            .iter()
            .map(|rec| (rec.address, rec.variable))
            .collect()
    }

    pub fn bind_vars(&mut self) -> MResult {
        for i in 0..self.vars.len() {
            self.vars[i].address = self.trace_heap(self.vars[i].mapping.into())?;
        }

        Ok(())
    }

    pub fn current_instruction(&self) -> Option<Instruction> {
        let index: usize = self.get_p().into();
        self.code.get(index).copied()
    }

    pub fn step(&mut self) -> MResult {
        if self.get_fail() {
            return Err(MachineError::FailState);
        }

        execute_instruction(
            self,
            self.current_instruction()
                .ok_or(MachineError::OutOfBoundsP)?,
        )
    }

    pub fn load_assembly(&mut self, assembly: &Assembly) {
        *self = Default::default();
        self.set_code(&assembly.instructions);
        if let Some(entry_point) = &assembly.entry_point {
            self.set_p(entry_point.location);
            self.set_cp(assembly.instructions.len().into());
            self.set_var_mappings(&entry_point.variables);
        }
    }

    pub fn execute(&mut self) -> ExecutionEnvironment {
        ExecutionEnvironment::new(self)
    }

    pub fn trace_heap(&self, reg: Addr) -> MachineResult<HeapPtr> {
        match self.get_store(reg)? {
            Data::Ref(Ref(ptr)) | Data::Str(Str(ptr)) => Ok(ptr),
            _ => Err(MachineError::NonVarBind),
        }
    }

    pub fn deref(&self, addr: Addr) -> MachineResult<Addr> {
        deref(self, addr)
    }

    pub fn bind_variables(&self, var_mapping: &VarMapping) -> MachineResult<VarBindings> {
        var_mapping.traverse(|&reg| self.trace_heap(reg.into()))
    }

    pub fn bind_good_variables(&self, var_mapping: &VarMapping) -> VarBindings {
        var_mapping.traverse_filter(|&reg| self.trace_heap(reg.into()))
    }

    pub fn construct_term(
        &self,
        addr: Addr,
        var_bindings: &VarBindings,
        symbol_table: &mut SymbolTable,
    ) -> ConstructResult<Term> {
        ConstructEnvironment::new(self, var_bindings, symbol_table).run(addr)
    }

    pub fn describe_vars(
        &self,
        var_bindings: &VarBindings,
        symbol_table: &mut SymbolTable,
    ) -> ConstructResult<Vec<VarDescription>> {
        let mut env = ConstructEnvironment::new(self, var_bindings, symbol_table);
        let mut mappings = var_bindings
            .iter()
            .map(|(&r, &n)| (n, r))
            .collect::<Vec<(VarName, HeapPtr)>>();
        mappings.sort_by_key(|(v, _)| *v);
        mappings
            .into_iter()
            .map(|(name, ptr)| {
                env.run(ptr.into())
                    .map(|term| VarDescription::new(name, ptr.into(), self.get_heap(ptr), term))
            })
            .collect()
    }

    pub fn dbg(&self, symbol_table: &SymbolTable) -> String {
        let mut str = String::new();
        writeln!(str, "h: {}", self.get_h()).unwrap();
        writeln!(str, "s: {}", self.get_s()).unwrap();
        writeln!(str, "p: {}", self.get_p()).unwrap();
        writeln!(str, "mode: {}", self.get_mode()).unwrap();
        writeln!(str, "fail: {}", self.get_fail()).unwrap();
        writeln!(str, "halt: {}", self.get_halt()).unwrap();
        writeln!(
            str,
            "code:\n{}",
            writeout_sym(&self.get_code(), symbol_table)
        )
        .unwrap();
        writeln!(str, "heap:\n{}", writeout_sym(&self.heap, symbol_table)).unwrap();
        writeln!(str, "regs:\n{}", writeout_sym(&self.reg, symbol_table)).unwrap();
        writeln!(str, "pdl:\n{}", writeout(self.pdl.iter())).unwrap();
        for (i, frame_result) in self.walk_stack().enumerate() {
            match frame_result {
                Ok(frame) => {
                    writeln!(str, "stack depth {i}, cp={}:", frame.cp).unwrap();
                    writeln!(str, "{}", writeout_sym(&frame.vars, symbol_table)).unwrap();
                }
                Err(e) => {
                    writeln!(str, "error: {e}").unwrap();
                }
            }
        }
        str
    }
}

impl Default for Machine {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum MachineError {
    NoStrFunctor,
    BadArity,
    NonVarBind,
    EmptyRef,
    InvalidRegData,
    FailState,
    OutOfBoundsP,
    StackUnderflow,
    StackSmash,
    StackTypeError,
}

impl MachineError {
    pub fn message(&self) -> &'static str {
        match self {
            Self::NoStrFunctor => "Struct does not point to a functor",
            Self::BadArity => "Some functor subterms not found on heap",
            Self::NonVarBind => "Attemted to bind variable to a non-var register",
            Self::EmptyRef => "Term points to an empty heap cell",
            Self::InvalidRegData => "Register contains something other than reference to heap",
            Self::FailState => "Attempted to execute an instruction in failed state",
            Self::OutOfBoundsP => "Instruction pointer P is out of code bounds",
            Self::StackUnderflow => "Attempt to access a stack frame which was not allocated",
            Self::StackSmash => "Attempt to access stack variable past the frame length",
            Self::StackTypeError => "Stack data invalid for current context",
        }
    }
}

impl Display for MachineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl From<MachineError> for String {
    fn from(value: MachineError) -> Self {
        format!("{value}")
    }
}

type IResult = Result<Option<CodePtr>, MachineError>;
type MResult = Result<(), MachineError>;
type MachineResult<T> = Result<T, MachineError>;

type MachineHook<'a> = dyn FnMut(&Machine) -> MResult + 'a;

pub struct ExecutionEnvironment<'a> {
    machine: &'a mut Machine,
    call_hook: Option<Box<MachineHook<'a>>>,
}

impl<'a> ExecutionEnvironment<'a> {
    pub fn new(machine: &'a mut Machine) -> Self {
        Self {
            machine,
            call_hook: None,
        }
    }

    pub fn run(mut self) -> MResult {
        self.machine.set_fail(false);
        self.machine.set_halt(false);

        while self.machine.current_instruction().is_some() {
            self.run_hook()?;
            self.machine.step()?;

            if self.machine.get_fail() || self.machine.get_halt() {
                break;
            }
        }
        Ok(())
    }

    fn run_hook(&mut self) -> MResult {
        if let Instruction::Call(_) = self
            .machine
            .current_instruction()
            .ok_or(MachineError::OutOfBoundsP)?
        {
            if let Some(hook) = &mut self.call_hook.take() {
                hook(self.machine)
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    }

    pub fn with_call_hook<F>(mut self, call_hook: F) -> Self
    where
        F: FnMut(&Machine) -> MResult + 'a,
    {
        self.call_hook = Some(Box::new(call_hook));
        self
    }

    pub fn without_call_hook(mut self) -> Self {
        self.call_hook = None;
        self
    }
}

fn deref(machine: &Machine, mut addr: Addr) -> MachineResult<Addr> {
    loop {
        match machine.get_store(addr)? {
            Data::Ref(Ref(new_addr)) if addr != new_addr.into() => addr = new_addr.into(),
            _ => break,
        }
    }
    Ok(addr)
}

fn bind(machine: &mut Machine, lhs: Addr, rhs: Addr) -> MResult {
    let l = machine.get_store(lhs)?;
    let r = machine.get_store(rhs)?;
    match (l, r) {
        (Data::Ref(al), Data::Ref(ar)) if al.0 > ar.0 => bind(machine, rhs, lhs),
        (Data::Ref(_), x) => {
            machine.set_store(lhs, x)?;
            Ok(())
        }
        _ => bind(machine, rhs, lhs),
    }
}

fn unify(machine: &mut Machine, a1: Addr, a2: Addr) -> MResult {
    machine.push_pdl(a1);
    machine.push_pdl(a2);
    machine.set_fail(false);
    while !(machine.is_pdl_empty() || machine.get_fail()) {
        let mut d1 = machine.pop_pdl().expect("cannot pop d1");
        d1 = deref(machine, d1)?;
        let mut d2 = machine.pop_pdl().expect("cannot pop d2");
        d2 = deref(machine, d2)?;

        if d1 == d2 {
            break;
        }

        match (machine.get_store(d1)?, machine.get_store(d2)?) {
            (Data::Ref(_), _) => bind(machine, d1, d2)?,
            (_, Data::Ref(_)) => bind(machine, d1, d2)?,
            (Data::Str(Str(v1)), Data::Str(Str(v2))) => {
                let f1 = machine
                    .get_heap(v1)
                    .get_functor()
                    .ok_or(MachineError::NoStrFunctor)?;
                let f2 = machine
                    .get_heap(v2)
                    .get_functor()
                    .ok_or(MachineError::NoStrFunctor)?;
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

fn put_structure(machine: &mut Machine, functor: Functor, target: Addr) -> IResult {
    let h = machine.get_h();
    machine.set_heap(h, Str(h + 1).into());
    machine.set_heap(h + 1, functor.into());
    machine.set_store(target, machine.get_heap(h))?;
    machine.set_h(h + 2);
    Ok(None)
}

fn set_variable(machine: &mut Machine, target: Addr) -> IResult {
    let h = machine.get_h();
    machine.set_heap(h, Data::Ref(Ref(h)));
    machine.set_store(target, machine.get_heap(h))?;
    machine.set_h(h + 1);
    Ok(None)
}

fn set_value(machine: &mut Machine, target: Addr) -> IResult {
    let h = machine.get_h();
    machine.set_heap(h, machine.get_store(target)?);
    machine.set_h(h + 1);
    Ok(None)
}

fn get_structure(machine: &mut Machine, functor: Functor, target: Addr) -> IResult {
    let addr = deref(machine, target)?;
    match machine.get_store(addr)? {
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

fn unify_variable(machine: &mut Machine, target: Addr) -> IResult {
    let s = machine.get_s();
    let h = machine.get_h();
    match machine.mode {
        Mode::Read => machine.set_store(target, machine.get_heap(s))?,
        Mode::Write => {
            machine.set_heap(h, Ref(h).into());
            machine.set_store(target, machine.get_heap(h))?;
            machine.set_h(h + 1);
        }
    }
    machine.set_s(s + 1);
    Ok(None)
}

fn unify_value(machine: &mut Machine, target: Addr) -> IResult {
    let s = machine.get_s();
    let h = machine.get_h();
    match machine.mode {
        Mode::Read => unify(machine, target, s.into())?,
        Mode::Write => {
            machine.set_heap(h, machine.get_store(target)?);
            machine.set_h(h + 1);
        }
    }
    machine.set_s(s + 1);
    Ok(None)
}

fn call(machine: &mut Machine, ptr: CodePtr) -> IResult {
    machine.set_cp(machine.get_p() + Instruction::Call(ptr).size());
    Ok(Some(ptr))
}

fn proceed(machine: &mut Machine) -> IResult {
    Ok(Some(machine.get_cp()))
}

fn put_variable(machine: &mut Machine, source: Addr, argument: Addr) -> IResult {
    let h = machine.get_h();
    let data = Ref(h).into();
    machine.set_heap(h, data);
    machine.set_store(source, data)?;
    machine.set_store(argument, data)?;
    machine.set_h(h + 1);
    Ok(None)
}

fn put_value(machine: &mut Machine, source: Addr, argument: Addr) -> IResult {
    machine.set_store(argument, machine.get_store(source)?)?;
    Ok(None)
}

fn get_variable(machine: &mut Machine, target: Addr, argument: Addr) -> IResult {
    machine.set_store(target, machine.get_store(argument)?)?;
    Ok(None)
}

fn get_value(machine: &mut Machine, target: Addr, argument: Addr) -> IResult {
    unify(machine, target, argument)?;
    Ok(None)
}

fn allocate(machine: &mut Machine, depth: StackDepth) -> IResult {
    let e = machine.e();
    let new_e = e + machine.stack_global(e + 2).to_len()? + 3;

    *machine.stack_global_mut(new_e) = e.into();
    *machine.stack_global_mut(new_e + 1) = machine.get_cp().into();
    *machine.stack_global_mut(new_e + 2) = depth.into();
    *machine.e_mut() = new_e;

    Ok(None)
}

fn deallocate(machine: &mut Machine) -> IResult {
    let new_p = machine.stack_global(machine.e + 1).to_code()?;
    *machine.e_mut() = machine.stack_global(machine.e).to_stack()?;
    Ok(Some(new_p))
}

fn publish(machine: &mut Machine) -> IResult {
    machine.bind_vars()?;
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
        Instruction::Allocate(depth) => allocate(machine, depth),
        Instruction::Deallocate => deallocate(machine),
        Instruction::Publish => publish(machine),
    }?;
    machine.set_p(nextp.unwrap_or(machine.get_p() + instruction.size()));
    Ok(())
}
