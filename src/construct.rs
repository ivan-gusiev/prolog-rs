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
        Struct::new_from(f, subterms)
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

/// Constructs a term from its heap representation.
///
/// # Arguments
/// * `heap` - A slice of heap cells. Its first element must correspond to HeapPtr(0).
/// * `target` - Pointer to the cell that represents the target term. Usually corresponds to query bindings.
/// * `symbol_table` - A symbol table. Mutable, in case a name is required for an unnamed free variable.
/// * `var_bindings` - List of all variable bindings. Will potentially be modified.
///
/// # Usage
///
/// During the execution of compiled instructions, a term is deconstructed into a range of data cells,
/// located on the machine heap. However, after the machine finds an answer, it needs to present it back
/// as a fully constructed Prolog term. This method accomplishes that, given a valid heap and
/// a target pointer.
///
/// Any Prolog query, once executed, yields two pieces of information: a flag indicating if it
/// can be satisfied, and a set of variables that satisfies it. The flag is the machine's `fail` flag,
/// and a list of variables can be found as the current set of machine's variable bindings.
/// Each binding consists of a variable name and its address on the machine heap. This function
/// takes such an address and constructs a proper Prolog term that represents the value of the variable.
///
/// For debugging purposes, practically any cell on a valid heap will result in some kind of a term
/// that is useful to inspect in order to understand what's going on with the machine.
///
/// # Implementation
///
/// This algorithm is inspired by the [reverse Polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation).
/// It maintains a stack of subterms of a term under construction. Once all the subterms are
/// themselves constructed, they are removed from the stack and collected in a parent term.
/// Another stack contains the outstanding "tasks" to handle the term under construction.
/// Those "tasks" include heap pointers to be parsed and the functors used to build the parent term.
///
/// While the functor names live directly on the heap, naming the variables is a separate problem.
/// This algorithm solves it by checking each REF in the bindings table. If a variable name is found, the whole
/// REF chain is called using that variable. Otherwise, a new name is generated.
pub fn construct(
    heap: &[Data],
    target: HeapPtr,
    symbol_table: &mut SymbolTable,
    mut var_bindings: VarBindings,
) -> ConstructResult<Term> {
    // a stack of partially constructed subterms
    // to be collected in a single final term
    let mut terms = Vec::<Term>::with_capacity(10);

    // Represents a task required to construct the target term.
    // The algorithm goes through the stack of targets.
    // Once all the targets are processed, the target term is
    // on top of the `terms` stack.
    enum Target {
        // Read next: look at the specified heap cell and decide
        // what to do based on its type.
        // Grows either the target stack or the term stack.
        Read(HeapPtr),
        // Write a functor: take the specified functor f/N,
        // pop N subterms from the `terms` stack, construct
        // a term f(...) from them, push the result on `terms`.
        // Shrinks both target and term stacks.
        Write(Functor),
    }

    // a stack of targets remaining to construct the target term
    let mut targets = Vec::<Target>::with_capacity(10);

    // A complex unification can produce variables that are not present in the query,
    // and therefore missing from var_bindings. In this case, we should produce a term
    // with arbitrary name. The simplest algorithm is to name them VAR1, VAR2, ...
    // The numbering of each new variable is unique up to the term being constructed.
    let mut unnamed_var_counter = 0;
    let mut new_variable = |heap_ptr: HeapPtr, var_bindings: &mut VarBindings| -> Term {
        unnamed_var_counter += 1;
        let name = symbol_table.intern(format!("VAR{unnamed_var_counter}"));
        var_bindings.insert(heap_ptr, name);
        Term::Variable(name)
    };

    // A "global" label for the last var binding name that occurred in the ref chain.
    // If present, the variable corresponding to the final ref will have this name.
    // Otherwise, an new variable will be created.
    let mut last_name: Option<VarName> = None;

    // start with reading the target heap cell
    targets.push(Target::Read(target));
    loop {
        match targets.pop() {
            Some(Target::Read(cur_ptr)) => {
                match *heap.get(cur_ptr.0).unwrap_or(&Data::Empty) {
                    Data::Empty => return Err(EMPTY_REF),
                    // a STR cell must always point to a functor, so we just read the pointee;
                    // this invariant is not checked here, making STR identical to a REF cell
                    // for the purposes of term construction
                    Data::Str(Str(x)) => targets.push(Target::Read(x)),
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
                            // a REF that points to itself is an unbound variable
                            if let Some(name) = last_name {
                                terms.push(Term::Variable(name));
                                last_name = None // clean up before the next REF chain starts
                            } else {
                                terms.push(new_variable(cur_ptr, &mut var_bindings));
                            }
                        } else {
                            // follow the REF chain deeper
                            targets.push(Target::Read(x));
                        }
                    }
                    Data::Functor(f) => {
                        // TODO: optimize for f/0, just push on the terms stack
                        targets.push(Target::Write(f));
                        // read subterms in the reverse order, so when popping them from stack,
                        // they are on their correct places; this is important if they themselves
                        // have subterms
                        for arg in (0..f.arity()).rev() {
                            targets.push(Target::Read(cur_ptr + 1 + arg as usize))
                        }
                    }
                }
            }
            Some(Target::Write(functor)) => {
                let mut args = Vec::<Term>::with_capacity(functor.arity() as usize);
                // pop subterms into the args list
                // throw BAD_ARITY if there are not enough subterms
                for _ in 0..functor.arity() {
                    args.push(terms.pop().ok_or(BAD_ARITY)?);
                }
                // push the constructed term onto `terms` stack
                // throw BAD_ARITY if there are too much subterms
                terms.push(Term::Struct(
                    Struct::new_from(functor, args).map_err(|_| BAD_ARITY)?,
                ))
            }
            // done with all the targets: terms[0] contains the target term
            None => break,
        }
    }

    // TODO: decide if we want our code to contain asserts
    assert!(terms.len() == 1);
    Ok(terms.remove(0))
}

const BAD_ARITY: ConstructError = ConstructError::Machine(MachineError::BadArity);
const EMPTY_REF: ConstructError = ConstructError::Machine(MachineError::EmptyRef);

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
