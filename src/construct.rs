use std::{collections::HashSet, fmt::Display};

use crate::{
    data::{Addr, Data, HeapPtr, Ref, Str},
    lang::{Functor, Struct, Term, VarName},
    machine::{Machine, MachineError},
    symbol::SymbolTable,
    var::VarBindings,
};

#[derive(Debug)]
pub enum ConstructError {
    CircularRef,
    Machine(MachineError),
}

impl ConstructError {
    pub fn message(&self) -> &'static str {
        match self {
            Self::CircularRef => "Circular reference detected when decompiling a term",
            Self::Machine(failure) => failure.message(),
        }
    }
}

impl Display for ConstructError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl From<ConstructError> for String {
    fn from(value: ConstructError) -> Self {
        format!("{value}")
    }
}

impl From<MachineError> for ConstructError {
    fn from(value: MachineError) -> Self {
        Self::Machine(value)
    }
}

pub type ConstructResult<T> = Result<T, ConstructError>;

pub struct ConstructEnvironment<'a> {
    machine: &'a Machine,
    symbol_table: &'a mut SymbolTable,
    var_bindings: VarBindings,
    seen: HashSet<HeapPtr>,
    unnamed_var_counter: usize,
}

impl<'a> ConstructEnvironment<'a> {
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

    pub fn run(&mut self, addr: Addr) -> ConstructResult<Term> {
        let heap_root = match addr {
            Addr::Heap(ptr) => ptr,
            other => self.machine.trace_heap(other)?,
        };
        let result = self.run_impl(heap_root);
        self.seen.clear();
        result
    }

    fn construct_str(&mut self, Str(ptr): Str) -> ConstructResult<Term> {
        match self.machine.get_heap(ptr) {
            Data::Functor(f) => self.construct_functor(ptr, f),
            _ => Err(MachineError::NoStrFunctor.into()),
        }
    }

    fn construct_functor(&mut self, ptr: HeapPtr, f: Functor) -> ConstructResult<Term> {
        let mut subterms = Vec::<Term>::new();
        for i in 1..=f.arity() {
            subterms.push(self.run_impl(ptr + i as usize)?)
        }
        Struct::new(f, &subterms)
            .map(Term::Struct)
            .map_err(|_| ConstructError::from(MachineError::BadArity))
    }

    fn construct_ref(&mut self, Ref(mut ptr): Ref) -> ConstructResult<Term> {
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
                Data::Str(str) => break self.construct_str(str),
                Data::Functor(f) => break self.construct_functor(ptr, f),
                Data::Empty => break Err(ConstructError::from(MachineError::EmptyRef)),
            }
        }
    }

    fn new_variable(&mut self, heap_ptr: HeapPtr) -> Term {
        self.unnamed_var_counter += 1;
        let name = self
            .symbol_table
            .intern(format!("VAR{}", self.unnamed_var_counter));
        self.var_bindings.insert(heap_ptr, name);
        Term::Variable(name)
    }

    fn run_impl(&mut self, addr: HeapPtr) -> ConstructResult<Term> {
        if self.seen.contains(&addr) {
            // circular reference
            return Err(ConstructError::CircularRef);
        } else {
            self.seen.insert(addr);
        }

        match self.machine.get_heap(addr) {
            Data::Ref(r) => self.construct_ref(r),
            Data::Str(str) => self.construct_str(str),
            Data::Functor(f) => self.construct_functor(addr, f),
            Data::Empty => Err(MachineError::EmptyRef.into()),
        }
    }
}

pub fn construct(
    heap: &[Data],
    target: HeapPtr,
    symbol_table: &mut SymbolTable,
    mut var_bindings: VarBindings,
) -> ConstructResult<Term> {
    enum Target {
        Read(HeapPtr),
        Write(Functor),
    }

    impl From<HeapPtr> for Target {
        fn from(value: HeapPtr) -> Self {
            Self::Read(value)
        }
    }

    impl From<Functor> for Target {
        fn from(value: Functor) -> Self {
            Self::Write(value)
        }
    }

    let mut terms = Vec::<Term>::with_capacity(10);
    let mut targets = Vec::<Target>::with_capacity(10);
    let mut last_name: Option<VarName> = None;
    let mut unnamed_var_counter = 0;

    let at = |ptr: HeapPtr| heap.get(ptr.0).copied().unwrap_or(Data::Empty);

    let mut new_variable = |heap_ptr: HeapPtr, var_bindings: &mut VarBindings| -> Term {
        unnamed_var_counter += 1;
        let name = symbol_table.intern(format!("VAR{unnamed_var_counter}"));
        var_bindings.insert(heap_ptr, name);
        Term::Variable(name)
    };

    targets.push(target.into());
    loop {
        match targets.pop() {
            Some(Target::Read(cur_ptr)) => {
                let cur = at(cur_ptr);
                match cur {
                    Data::Empty => return Err(MachineError::EmptyRef.into()),
                    Data::Str(Str(x)) => targets.push(x.into()),
                    Data::Ref(Ref(x)) => {
                        // consider the following reference chain:
                        //  9:REF|7 -> 7:REF|5 -> 5:REF|3 -> 3:REF|1 -> 1:REF|1
                        // with the following bindings: 5 -> X
                        // the whole reference chain refers therefore to the variable X
                        // we track this by checking the bindings at each ref
                        // and selecting the closest binding to the end of the chain
                        if let Some(new_name) = var_bindings.get(&cur_ptr) {
                            last_name = Some(new_name)
                        }

                        if x == cur_ptr {
                            if let Some(name) = last_name {
                                terms.push(Term::Variable(name));
                                last_name = None
                            } else {
                                terms.push(new_variable(cur_ptr, &mut var_bindings));
                            }
                        } else {
                            targets.push(x.into());
                        }
                    }
                    Data::Functor(f) => {
                        targets.push(f.into());
                        for arg in (0..f.arity()).rev() {
                            targets.push((cur_ptr + 1 + (arg as usize)).into())
                        }
                    }
                }
            }
            Some(Target::Write(functor)) => {
                let mut args = Vec::<Term>::with_capacity(functor.arity() as usize);
                for _ in 0..functor.arity() {
                    args.push(
                        terms
                            .pop()
                            .ok_or(ConstructError::from(MachineError::BadArity))?,
                    );
                }
                terms.push(Term::Struct(
                    Struct::new(functor, &args)
                        .map_err(|_| ConstructError::from(MachineError::BadArity))?,
                ))
            }
            None => break,
        }
    }
    Ok(terms.remove(0))
}

#[cfg(test)]
mod unittests {
    use std::iter::FromIterator;

    use crate::{
        data::{Data, HeapPtr, Ref, Str},
        lang::Functor,
        symbol::{to_display, SymbolTable},
        var::VarBindings,
    };

    use super::construct;

    #[test]
    fn simple_construct_test() {
        let mut symbol_table = SymbolTable::new();
        let h = symbol_table.intern("h");
        let f = symbol_table.intern("f");
        let p = symbol_table.intern("p");
        let z = symbol_table.intern("Z");
        let w = symbol_table.intern("W");

        let heap = vec![
            Data::Str(Str(HeapPtr(1))),
            Data::Functor(Functor(h, 2)),
            Data::Ref(Ref(HeapPtr(2))),
            Data::Ref(Ref(HeapPtr(3))),
            Data::Str(Str(HeapPtr(5))),
            Data::Functor(Functor(f, 1)),
            Data::Ref(Ref(HeapPtr(3))),
            Data::Str(Str(HeapPtr(8))),
            Data::Functor(Functor(p, 3)),
            Data::Ref(Ref(HeapPtr(2))),
            Data::Str(Str(HeapPtr(1))),
            Data::Str(Str(HeapPtr(5))),
        ];

        let var_bindings = VarBindings::from_iter([(HeapPtr(2), z), (HeapPtr(3), w)]);

        let result = construct(&heap, HeapPtr(7), &mut symbol_table, var_bindings).unwrap();
        let result_str = format!("{}", to_display(&result, &symbol_table));
        assert_eq!(result_str, "p(f(W), h(W, Z), Z)");
    }
}
