use std::{collections::HashSet, fmt::Display};

use crate::{
    data::{Addr, Data, HeapPtr, Ref, Str},
    lang::{Functor, Struct, Term, VarName},
    machine::{Machine, MachineError},
    symbol::SymbolTable,
    var::VarBindings,
};

#[derive(Debug)]
pub enum DecompileError {
    CircularRef,
    Machine(MachineError),
}

impl DecompileError {
    pub fn message(&self) -> &'static str {
        match self {
            Self::CircularRef => "Circular reference detected when decompiling a term",
            Self::Machine(failure) => failure.message(),
        }
    }
}

impl Display for DecompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl From<DecompileError> for String {
    fn from(value: DecompileError) -> Self {
        format!("{value}")
    }
}

impl From<MachineError> for DecompileError {
    fn from(value: MachineError) -> Self {
        Self::Machine(value)
    }
}

pub type DecompileResult<T> = Result<T, DecompileError>;

pub struct DecompileEnvironment<'a> {
    machine: &'a Machine,
    symbol_table: &'a mut SymbolTable,
    var_bindings: VarBindings,
    seen: HashSet<HeapPtr>,
    unnamed_var_counter: usize,
}

impl<'a> DecompileEnvironment<'a> {
    pub fn new(
        machine: &'a Machine,
        query_bindings: &'a VarBindings,
        symbol_table: &'a mut SymbolTable,
    ) -> Self {
        Self {
            machine,
            symbol_table,
            var_bindings: query_bindings.clone(),
            seen: Default::default(),
            unnamed_var_counter: 0,
        }
    }

    pub fn run(&mut self, addr: Addr) -> DecompileResult<Term> {
        let heap_root = match addr {
            Addr::Heap(ptr) => ptr,
            other => self.machine.trace_heap(other)?,
        };
        let result = self.run_impl(heap_root);
        self.seen.clear();
        result
    }

    fn decompile_str(&mut self, Str(ptr): Str) -> DecompileResult<Term> {
        match self.machine.get_heap(ptr) {
            Data::Functor(f) => self.decompile_functor(ptr, f),
            _ => Err(MachineError::NoStrFunctor.into()),
        }
    }

    fn decompile_functor(&mut self, ptr: HeapPtr, f: Functor) -> DecompileResult<Term> {
        let mut subterms = Vec::<Term>::new();
        for i in 1..=f.arity() {
            subterms.push(self.run_impl(ptr + i as usize)?)
        }
        Struct::new(f, &subterms)
            .map(Term::Struct)
            .map_err(|_| DecompileError::from(MachineError::BadArity))
    }

    fn decompile_ref(&mut self, Ref(mut ptr): Ref) -> DecompileResult<Term> {
        let mut last_name: Option<VarName> = None;
        loop {
            if let Some(new_name) = self.var_bindings.get(&ptr) {
                last_name = Some(new_name)
            }

            match self.machine.get_heap(ptr) {
                Data::Ref(Ref(next_ptr)) => {
                    if ptr != next_ptr {
                        ptr = next_ptr
                    } else if let Some(name) = last_name {
                        break Ok(Term::Variable(name));
                    } else {
                        break Ok(self.new_variable(ptr));
                    }
                }
                Data::Str(str) => break self.decompile_str(str),
                Data::Functor(f) => break self.decompile_functor(ptr, f),
                Data::Empty => break Err(DecompileError::from(MachineError::EmptyRef)),
            }
        }
    }

    fn new_variable(&mut self, heap_ptr: HeapPtr) -> Term {
        self.unnamed_var_counter += 1;
        let name = self
            .symbol_table
            .intern(format!("UNK{}", self.unnamed_var_counter));
        self.var_bindings.insert(heap_ptr, name);
        Term::Variable(name)
    }

    fn run_impl(&mut self, addr: HeapPtr) -> DecompileResult<Term> {
        if self.seen.contains(&addr) {
            // circular reference
            return Err(DecompileError::CircularRef);
        } else {
            self.seen.insert(addr);
        }

        match self.machine.get_heap(addr) {
            Data::Ref(r) => self.decompile_ref(r),
            Data::Str(str) => self.decompile_str(str),
            Data::Functor(f) => self.decompile_functor(addr, f),
            Data::Empty => Err(MachineError::EmptyRef.into()),
        }
    }
}
