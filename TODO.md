### To Do
- [ ] more compiler tests (e.g. from the brown book)
- [ ] static type system?
- [ ] byte-address `call p/n` instead of index-address into array
    * this means variable-length instructions in general
    * the code should be `[u8]`, and the Instruction stream should be an iterator
- [ ] update `[A-Z]+\.md` files
- [ ] write more docs
    * explain horizontal line
    * explain `var.rs` and binding
- [ ] really fix the code organization, extern crate stuff et al
    * move `main` out of `lib`?
- [ ] construct: employ var names from rules and facts when possible
    * one option would be to have a trace mode, and put a code ref on each call
    * we could use these markers to select a "current term" when decompiling
- [ ] convert getters and setters to read-only and mutable references
    * `get_x(&self) -> &X` to `x(&self) -> &X` 
    * `set_x(&mut self, value)` to `x_mut(&mut self) -> &mut X`
- [ ] asm: instead of labels pointing to the actual code, point them to a thunk
    * will help with `assert`/`retract` in the future
    * will enable referring to terms before defining their facts or rules
- [ ] compile: number temporary variables from X1 and up consecutively
    * consider variables A, B, C, D, E where B and D are permanent
    * this will result in `A->X1, B->Y1, C->X3, D->Y2, E->X5`
    * we want `A->X1, B->Y1, C->X2, D->Y2, E->X3`
- [ ] tests: file-based test cases
- [ ] build: ensure that `just` is installed in the makefile
- [ ] build: ensure that all the cargo dependencies are installed (e.g. insta)

### Bugs
- [x] Type `?- X` then `X`. It cannot figure out the unification. 
    * WONTFIX: after L1 you cannot have root variables
- [x] Unify `f(X, g(X,a))` and `f(b, Y)`. The variable mapping does not seem to be right.
    * fix the variable descriptions by dereferencing mappings:
    * before code execution, turn all RegPtr's into HeapPtr's
    * DONE: the root cause was incorrectly implemented `bind` operation
- [x] construct: make sure that for a horizontal line, `horizontal(A)` constructs correctly
    * today it returns `UnknownVariable`
    * needs either autonaming temp variables, or to keep the list of all program mappings
- [x] construct: decompiling a self-referential (buggy) heap causes stack overflow
    * `executetests::test_program_execute::case_1` fails
    * add a cache of already-seen terms
- [x] `CompileResult::append_to_assembly` steals functors for queries
    * see `asmtests::test_horizontal`
- [x] In the `p(X, Y) :- q(X, Z), r(Z, Y)` example:
    * assert `q(a, b)` and `r(b, c)`
    * now query `?- p(a, V)` returns `V = a`, but is expected to return `V = c`
    * ~~possibly the `call_hook` needs fixing?~~ yes it was
- [ ] Consider the following program. `cat :- meow, legs(four). meow. legs(four).`
    * the query `?- cat.` will run forever
    * seems to be related to how call/proceed works with multiple leaf calls

### Done ✓
- [x] WAM assembly parser
- [x] Prolog query parser
- [x] Prolog program parser
- [x] seems that integration tests are not working (fixed)
- [x] justfile instead of makefile
- [x] golden parameterized tests: show input as well as output
- [x] a symbol table that doesn't repeat the same word twice
- [x] name table, so we can support more than 1 character in our names
- [x] parse_* functions should take a mut ref to symboltable, instead of doing the in-out dance
- [x] fix Clippy warnings
- [x] compile: fix FlatRef, see if possible to optimize
- [x] more compiler tests from brown book:
    * [x] horizontal line
- [x] compile: don't forget to add `call` and `proceed` instructions
- [x] compile: term_map seems to be useless (`term_map[Ti] == Xi`)
    * not useless
    * the equation is only correct if the term does not have repeating terms
- [x] construct: move code to a separate module `construct.rs`
- [x] multi-fact tests
- [x] roll my own topological sort 
    * used generations approach instead of that
- [x] machine: move class to a separate module `machine.rs`
- [x] Full sentence Prolog parser.
- [x] asm: move `asm::Assembly::from_asm` to `assembler::compile_asm`
- [x] actually, I don't like that all query variables are in stack
    * keep some of them in regs? e.g. if there is only one `call` instruction
    * WONTFIX: even for simple instructions, the call still destroys the correct registers
- [x] auto-bind variables in the debug interface
    * WONTFIX: is not relevant after implementing the `publish` instruction
- [x] find a better way of binding variable names to the corresponding data
    * today the process is `var_mapping` -> `var_binding` -> `call_hook`
    * call_hook is weird, and the whole thing is not very robust (?)
    * we could protect query variables in a stack frame, and ditch binding
    * how do we handle the extraction? 
        * e.g. keep the stack frame, and clean up on ExecutionEnvironment::drop
- [x] fix the varmapping for rules (must be able to point to stack)
- [x] compile: warn when a variable from the head of a rule is not used in its goals
    * add a test for that
    * thread the warnings through the system to assembly
- [x] transform stack frame into the real thing
    * it should be a linked list of words
- [x] stack: remove StackFrame, have something like StackView
    * maybe solve the `e==0 && stack.len==0` problem somehow
    * half done, keeping StackFrame only for introspection purposes
- [x] construct: is it possible for `ConstructEnvironment::run_impl` to be non-recursive?
    * built a completely new algorithm, stored in one 87-line function
- [x] data: remove Str and Ref types, they are not as useful as once thought