### To Do
- [ ] more compiler tests (e.g. from the brown book)
- [ ] static type system?
- [ ] roll my own topological sort 
    * not needed? generations approach might work
- [ ] byte-address `call p/n` instead of index-address into array
- [ ] compile: term_map seems to be useless (`term_map[Ti] == Xi`)
- [ ] decompile: move code to a separate module `decompile.rs`
- [ ] decompile: is it possible for `Machine::decompile_variable` to be non-recursive?
- [ ] machine: move class to a separate module `machine.rs`
- [ ] update `[A-Z]+\.md` files
- [ ] write more docs
    * explain horizontal line
    * explain `var.rs` and binding

### Bugs
- [x] Type `?- X` then `X`. It cannot figure out the unification. 
    * WONTFIX: after L1 you cannot have root variables
- [x] Unify `f(X, g(X,a))` and `f(b, Y)`. The variable mapping does not seem to be right.
    * fix the variable descriptions by dereferencing mappings:
    * before code execution, turn all RegPtr's into HeapPtr's
    * DONE: the root cause was incorrectly implemented `bind` operation
- [ ] decompile: make sure that for a horizontal line, `horizontal(A)` decompiles correctly
    * today it returns `UnknownVariable`
    * needs either autonaming temp variables, or to keep the list of all program mappings
- [ ] decompile: decompiling a self-referential (buggy) heap causes stack overflow
    * `executetests::test_program_execute::case_1` fails
    * add a cache of already-seen terms

### Done ✓
- [x] WAM assembly parser
- [x] Prolog query parser 
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