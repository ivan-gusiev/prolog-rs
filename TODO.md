### To Do
- [ ] compile: fix FlatRef, see if possible to optimize
- [ ] more compiler tests
- [ ] static type system?
- [ ] roll my own topological sort

### Bugs
- [ ] Type `?- X` then `X`. It cannot figure out the unification.

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