### To Do
- [ ] more compiler tests (e.g. from the brown book)
- [ ] static type system?
- [ ] byte-address `call p/n` instead of index-address into array
- [ ] decompile: is it possible for `DecompileEnvironment::run_impl` to be non-recursive?
- [ ] update `[A-Z]+\.md` files
- [ ] write more docs
    * explain horizontal line
    * explain `var.rs` and binding
- [ ] really fix the code organization, extern crate stuff et al
    * move `main` out of `lib`?
- [ ] decompile: employ var names from queries when possible
    * maybe wait until L2 machine implementation
- [ ] Convert getters and setters to read-only and mutable references
    * `get_x(&self) -> &X` to `x(&self) -> &X` 
    * `set_x(&mut self, value)` to `x_mut(&mut self) -> &mut X`
- [ ] asm: move `asm::Assembly::from_asm` to `assembler::compile_asm`

### Bugs
- [x] Type `?- X` then `X`. It cannot figure out the unification. 
    * WONTFIX: after L1 you cannot have root variables
- [x] Unify `f(X, g(X,a))` and `f(b, Y)`. The variable mapping does not seem to be right.
    * fix the variable descriptions by dereferencing mappings:
    * before code execution, turn all RegPtr's into HeapPtr's
    * DONE: the root cause was incorrectly implemented `bind` operation
- [x] decompile: make sure that for a horizontal line, `horizontal(A)` decompiles correctly
    * today it returns `UnknownVariable`
    * needs either autonaming temp variables, or to keep the list of all program mappings
- [x] decompile: decompiling a self-referential (buggy) heap causes stack overflow
    * `executetests::test_program_execute::case_1` fails
    * add a cache of already-seen terms
- [ ] `CompileResult::append_to_assembly` steals functors for queries
    * see `asmtests::test_horizontal`

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
- [x] decompile: move code to a separate module `decompile.rs`
- [x] multi-fact tests
- [x] roll my own topological sort 
    * used generations approach instead of that
- [x] machine: move class to a separate module `machine.rs`
- [x] Full sentence Prolog parser.
