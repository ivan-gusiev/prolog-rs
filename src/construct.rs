use std::{collections::HashSet, fmt::Display};

use crate::{
    data::{Data, HeapPtr},
    lang::{Functor, Struct, Term, VarName},
    machine::MachineError,
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

const BAD_ARITY: ConstructError = ConstructError::Machine(MachineError::BadArity);
const EMPTY_REF: ConstructError = ConstructError::Machine(MachineError::EmptyRef);

pub type ConstructResult<T> = Result<T, ConstructError>;

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
    mut var_bindings: VarBindings, // TODO: optimize so that we can take reference instead of cloning the whole bindings
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

    // In case if a term references itself (either directly or through a subterm),
    // the heap is invalid and we cannot construct that term. This list will track
    // all the subterms, so we don't fall into an infinite loop.
    let mut seen_terms: HashSet<HeapPtr> = HashSet::default();

    // This keeps track of all the refs that occur in the chain. It is local to every
    // REF object, but is maintained as a single object to optimize away extra allocations.
    let mut seen_refs: HashSet<HeapPtr> = HashSet::default();

    let at = |ptr: HeapPtr| heap.get(ptr.0).unwrap_or(&Data::Empty);

    // start with reading the target heap cell
    targets.push(Target::Read(target));
    loop {
        match targets.pop() {
            Some(Target::Read(cur_ptr)) => {
                match *at(cur_ptr) {
                    Data::Empty => return Err(EMPTY_REF),
                    // a STR cell must always point to a functor, so we just read the pointee;
                    // this invariant is not checked here, making STR identical to a REF cell
                    // for the purposes of term construction
                    Data::Str(x) => targets.push(Target::Read(x)),
                    Data::Ref(_) => {
                        // a simple implementation would be just to set the ref as a target,
                        // or if cur_ptr==target, to emit a new variable term;
                        // however, for the proper circular reference check, we need to
                        // consider the whole reference chain as a single whole heap object

                        // a reference chain can be a part of multiple sub-terms, so we clean
                        // it up every time
                        seen_refs.clear();
                        let mut cur_ref = cur_ptr;

                        loop {
                            if !seen_refs.insert(cur_ref) {
                                return Err(ConstructError::CircularRef);
                            }

                            // consider the following reference chain:
                            //  9:REF|7 -> 7:REF|5 -> 5:REF|3 -> 3:REF|1 -> 1:REF|1
                            // with the following bindings: 5 -> X
                            // the whole reference chain refers therefore to the variable X
                            // we track this by checking the bindings at each ref
                            // and selecting the closest binding to the end of the chain
                            if let Some(new_name) = var_bindings.get(&cur_ref) {
                                last_name = Some(new_name)
                            }

                            match *at(cur_ref) {
                                Data::Ref(x) if x == cur_ref => {
                                    // a REF that points to itself is an unbound variable
                                    if let Some(name) = last_name {
                                        terms.push(Term::Variable(name));
                                        last_name = None // clean up before the next REF chain starts
                                    } else {
                                        terms.push(new_variable(cur_ptr, &mut var_bindings));
                                    }
                                    break;
                                }
                                Data::Ref(x) => cur_ref = x, // follow the ref chain
                                _ => {
                                    targets.push(Target::Read(cur_ref));
                                    break;
                                }
                            }
                        }
                    }
                    Data::Functor(f) => {
                        if !seen_terms.insert(cur_ptr) {
                            // self-referencing term check
                            return Err(ConstructError::CircularRef);
                        }

                        // optimization for constants: just write out the term, no need to grow stack
                        if f.arity() == 0 {
                            terms.push(Term::Struct(Struct::constant(f.name())));
                            continue;
                        }

                        targets.push(Target::Write(f));
                        // read subterms in the reverse order, so they are processed in the right order;
                        // remember to reverse them back when collecting the parent term
                        for arg in (0..f.arity()).rev() {
                            targets.push(Target::Read(cur_ptr + 1 + arg as usize))
                        }
                    }
                }
            }
            Some(Target::Write(functor)) => {
                let arg_count = functor.arity() as usize;
                assert!(terms.len() >= arg_count);

                // take last `arg_count` terms from the subterm stack
                let args: Vec<_> = terms.drain((terms.len() - arg_count)..).collect();

                // push the constructed term onto `terms` stack
                // throw BAD_ARITY if there are not enough subterms
                terms.push(Term::Struct(
                    Struct::new(functor, args).map_err(|_| BAD_ARITY)?,
                ))
            }
            // done with all the targets: terms[0] contains the target term
            None => break,
        }
    }

    assert!(terms.len() == 1);
    Ok(terms.remove(0))
}

#[cfg(test)]
mod unittests {
    use std::iter::FromIterator;

    use crate::{
        construct::ConstructError,
        data::{Data, HeapPtr},
        lang::Functor,
        symbol::{to_display, SymbolTable},
        var::VarBindings,
    };

    use super::construct;

    #[test]
    fn construct_simple_test() {
        let mut symbol_table = SymbolTable::new();
        let h = symbol_table.intern("h");
        let f = symbol_table.intern("f");
        let p = symbol_table.intern("p");
        let z = symbol_table.intern("Z");
        let w = symbol_table.intern("W");

        let heap = vec![
            Data::Str(HeapPtr(1)),
            Data::Functor(Functor(h, 2)),
            Data::Ref(HeapPtr(2)),
            Data::Ref(HeapPtr(3)),
            Data::Str(HeapPtr(5)),
            Data::Functor(Functor(f, 1)),
            Data::Ref(HeapPtr(3)),
            Data::Str(HeapPtr(8)),
            Data::Functor(Functor(p, 3)),
            Data::Ref(HeapPtr(2)),
            Data::Str(HeapPtr(1)),
            Data::Str(HeapPtr(5)),
        ];

        let var_bindings = VarBindings::from_iter([(HeapPtr(2), z), (HeapPtr(3), w)]);

        let result = construct(&heap, HeapPtr(7), &mut symbol_table, var_bindings).unwrap();
        let result_str = format!("{}", to_display(&result, &symbol_table));
        assert_eq!(result_str, "p(Z, h(Z, W), f(W))");
    }

    #[test]
    fn construct_circular_reference_test() {
        let mut symbol_table = SymbolTable::new();
        let z = symbol_table.intern("Z");
        let w = symbol_table.intern("W");

        let heap = vec![Data::Ref(HeapPtr(1)), Data::Ref(HeapPtr(0))];

        let var_bindings = VarBindings::from_iter([(HeapPtr(0), z), (HeapPtr(1), w)]);

        let result = construct(&heap, HeapPtr(0), &mut symbol_table, var_bindings);
        assert!(matches!(result, Err(ConstructError::CircularRef)));
    }

    #[test]
    fn construct_circular_struct_test() {
        let mut symbol_table = SymbolTable::new();
        let w = symbol_table.intern("W");
        let f = symbol_table.intern("f");

        let heap = vec![
            // f(f(...), W)
            Data::Functor(Functor(f, 2)),
            Data::Str(HeapPtr(0)),
            Data::Ref(HeapPtr(2)),
        ];

        let var_bindings = VarBindings::from_iter([(HeapPtr(2), w)]);

        let result = construct(&heap, HeapPtr(0), &mut symbol_table, var_bindings);
        assert!(matches!(result, Err(ConstructError::CircularRef)));
    }
}
