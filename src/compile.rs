use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::iter::FromIterator;

use data::RegPtr;
use instr::Instruction;
use lang::{Functor, Struct, Term, VarName};
use var::VarMapping;

// TODO: for some reason rustc requires `extern crate` definitions, fix this
extern crate topological_sort;
use self::topological_sort::TopologicalSort;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct TermId(usize);

impl TermId {
    fn inc(&mut self) {
        self.0 += 1;
    }
}

impl Display for TermId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "t{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum FlatRef {
    Term(TermId),
    Register(RegPtr),
}

impl Display for FlatRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FlatRef::Term(id) => id.fmt(f),
            FlatRef::Register(ptr) => ptr.fmt(f),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct FlatStruct<Ptr>(Functor, Vec<Ptr>);

impl<Ptr: Display> Display for FlatStruct<Ptr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.0)?;
        for term in self.1.iter() {
            write!(f, "{term},")?
        }
        write!(f, ")")
    }
}

impl<Ptr: Copy> FlatStruct<Ptr> {
    fn transform<NewPtr, F>(&self, mapper: F) -> FlatStruct<NewPtr>
    where
        F: FnMut(Ptr) -> NewPtr,
    {
        FlatStruct(
            self.0,
            self.1.iter().copied().map(mapper).collect::<Vec<_>>(),
        )
    }
}

impl<Ptr> FlatStruct<Ptr> {
    fn functor(&self) -> Functor {
        self.0
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
enum FlattenedTerm<Ptr> {
    Variable(VarName),
    Struct(FlatStruct<Ptr>),
}

impl<Ptr: Display> Display for FlattenedTerm<Ptr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FlattenedTerm::Variable(n) => n.fmt(f),
            FlattenedTerm::Struct(s) => s.fmt(f),
        }
    }
}

impl<Ptr: Copy> FlattenedTerm<Ptr> {
    fn new_str(f: Functor, refs: Vec<Ptr>) -> FlattenedTerm<Ptr> {
        FlattenedTerm::Struct(FlatStruct(f, refs))
    }

    fn get_str(&self) -> &FlatStruct<Ptr> {
        if let FlattenedTerm::Struct(str) = self {
            str
        } else {
            panic!("not a struct")
        }
    }

    fn transform<NewPtr, F>(&self, mapper: F) -> FlattenedTerm<NewPtr>
    where
        F: FnMut(Ptr) -> NewPtr,
    {
        match self {
            FlattenedTerm::Variable(nm) => FlattenedTerm::Variable(*nm),
            FlattenedTerm::Struct(str) => FlattenedTerm::Struct(str.transform(mapper)),
        }
    }
}

struct FlattenedReg(RegPtr, FlattenedTerm<RegPtr>);

impl FlattenedReg {
    fn to_tuple(&self) -> (RegPtr, &FlattenedTerm<RegPtr>) {
        (self.0, &self.1)
    }
}

fn flatten_struct(root_struct: Struct) -> (Vec<FlattenedReg>, HashMap<VarName, RegPtr>) {
    let root_functor = root_struct.functor();
    let root_term = Term::Struct(root_struct);
    let mut term_map = HashMap::<TermId, RegPtr>::new();
    let mut var_map = HashMap::<VarName, RegPtr>::new();
    let mut queue = VecDeque::from([(&root_term, TermId(0))]);
    let mut flatrefs = Vec::<FlattenedTerm<FlatRef>>::new();
    let mut term_counter = TermId(0);

    let mut next_term_id = || {
        term_counter.inc();
        term_counter
    };

    loop {
        match queue.pop_front() {
            None => break,
            Some((t @ Term::Variable(nm), id)) => {
                if var_map.contains_key(nm) {
                    term_map.insert(id, var_map[nm]);
                } else if id.0 <= root_functor.arity() as usize {
                    //let var_id = next_term_id();
                    flatrefs.push(FlattenedTerm::Variable(*nm));
                    term_map.insert(id, RegPtr(id.0));
                    queue.push_back((t, next_term_id()))
                } else {
                    flatrefs.push(FlattenedTerm::Variable(*nm));
                    var_map.insert(*nm, RegPtr(flatrefs.len() - 1));
                    term_map.insert(id, RegPtr(flatrefs.len() - 1));
                }
            }
            Some((Term::Struct(str), id)) => {
                let mut subterms: Vec<FlatRef> = vec![];

                for subterm in str.terms() {
                    match subterm {
                        v @ Term::Variable(nm) => {
                            if var_map.contains_key(nm) {
                                subterms.push(FlatRef::Register(var_map[nm]))
                            } else {
                                let id = next_term_id();
                                subterms.push(FlatRef::Term(id));
                                queue.push_back((v, id));
                            }
                        }
                        t @ Term::Struct(_) => {
                            let id = next_term_id();
                            subterms.push(FlatRef::Term(id));
                            queue.push_back((t, id));
                        }
                    }
                }
                flatrefs.push(FlattenedTerm::new_str(str.functor(), subterms));
                term_map.insert(id, RegPtr(flatrefs.len() - 1));
            }
        }
    }

    let flat_to_reg = |flat_term| match flat_term {
        FlatRef::Term(id) => match term_map.get(&id) {
            Some(reg) => *reg,
            None => panic!("Could not find a term for {:?}", id),
        },
        FlatRef::Register(reg) => reg,
    };

    let result: Vec<FlattenedReg> = flatrefs
        .iter()
        .enumerate()
        .map(|(i, ft)| FlattenedReg(RegPtr(i), ft.transform(flat_to_reg)))
        .collect();

    (result, var_map)
}

fn extract_structs(terms: &[FlattenedReg]) -> Vec<RegPtr> {
    let mut structs = vec![];
    for term in terms.iter() {
        match term {
            FlattenedReg(reg, FlattenedTerm::Struct(_)) => structs.push(*reg),
            _ => (),
        }
    }
    structs
}

fn order_query_structs(
    terms: &HashMap<RegPtr, &FlattenedTerm<RegPtr>>,
    structs_to_sort: &[RegPtr],
) -> Vec<RegPtr> {
    fn regs(term: &FlattenedTerm<RegPtr>) -> HashSet<RegPtr> {
        if let FlattenedTerm::Struct(FlatStruct(_, refs)) = term {
            HashSet::from_iter(refs.to_owned())
        } else {
            HashSet::default()
        }
    }

    let mut ts = TopologicalSort::<RegPtr>::new();
    for l in 0..structs_to_sort.len() {
        for r in (l + 1)..structs_to_sort.len() {
            let lp = structs_to_sort[l];
            let rp = structs_to_sort[r];
            let regs_lhs = regs(terms[&lp]);
            let regs_rhs = regs(terms[&rp]);

            if regs_lhs.contains(&rp) {
                ts.add_dependency(rp, lp);
            } else if regs_rhs.contains(&lp) {
                ts.add_dependency(lp, rp);
            }
        }
    }

    let mut result = vec![];
    loop {
        let mut batch = ts.pop_all(); // use pop_all to preserve the original ordering
        if batch.is_empty() {
            break;
        }

        batch.sort();
        result.append(&mut batch)
    }

    // special case when there are no dependencies between terms, e.g. only one term to sort
    if result.is_empty() && structs_to_sort.len() == 1 {
        result.push(structs_to_sort[0]);
    }

    result
}

pub fn compile_query(query: Struct) -> CompileResult {
    let (registers_with_root, vars) = flatten_struct(query);
    let registers = &registers_with_root[1..];
    let reg_map = HashMap::from_iter(registers.iter().map(FlattenedReg::to_tuple));
    let structs = order_query_structs(&reg_map, &extract_structs(registers));
    let mut seen = HashSet::<RegPtr>::new();
    let mut result = vec![];

    let FlattenedReg(_, root) = &registers_with_root[0];
    let max_argument = root.get_str().functor().arity() + 1;

    // set up general purpose registers
    for reg @ RegPtr(struct_index) in structs {
        if struct_index < max_argument as usize {
            continue;
        }

        let FlatStruct(f, refs) = reg_map[&reg].get_str();
        result.push(Instruction::PutStructure(*f, reg));
        seen.insert(reg);
        for ref_ptr in refs {
            if seen.contains(ref_ptr) {
                result.push(Instruction::SetValue(*ref_ptr))
            } else {
                seen.insert(*ref_ptr);
                result.push(Instruction::SetVariable(*ref_ptr))
            }
        }
    }
    // set up argument registers
    for i in 1..(max_argument as usize) {
        let areg = RegPtr(i);
        match &reg_map[&areg] {
            FlattenedTerm::Variable(nm) => {
                let xreg = vars[nm];
                if seen.contains(&xreg) {
                    result.push(Instruction::PutValue(xreg, areg));
                } else {
                    seen.insert(xreg);
                    result.push(Instruction::PutVariable(xreg, areg));
                }
            }
            FlattenedTerm::Struct(FlatStruct(f, refs)) => {
                result.push(Instruction::PutStructure(*f, areg));
                for ref_ptr in refs {
                    if seen.contains(ref_ptr) {
                        result.push(Instruction::SetValue(*ref_ptr))
                    } else {
                        seen.insert(*ref_ptr);
                        result.push(Instruction::SetVariable(*ref_ptr))
                    }
                }
            }
        }
    }

    CompileResult {
        instructions: result,
        var_mapping: VarMapping::from_inverse(vars),
    }
}

pub fn compile_program(program: Struct) -> CompileResult {
    let (registers_with_root, vars) = flatten_struct(program);
    let registers = &registers_with_root[1..];
    let reg_map: HashMap<RegPtr, &FlattenedTerm<RegPtr>> =
        HashMap::from_iter(registers.iter().map(FlattenedReg::to_tuple));
    let structs = extract_structs(&registers);
    let mut seen = HashSet::<RegPtr>::new();
    let mut result = vec![];

    let FlattenedReg(_, root) = &registers_with_root[0];
    let max_argument = root.get_str().functor().arity() + 1;

    // set up argument registers
    for i in 1..(max_argument as usize) {
        let areg = RegPtr(i);
        match &reg_map[&areg] {
            FlattenedTerm::Variable(nm) => {
                let xreg = vars[nm];
                if seen.contains(&xreg) {
                    result.push(Instruction::GetValue(xreg, areg));
                } else {
                    seen.insert(xreg);
                    result.push(Instruction::GetVariable(xreg, areg));
                }
            }
            FlattenedTerm::Struct(FlatStruct(f, refs)) => {
                result.push(Instruction::GetStructure(*f, areg));
                for ref_ptr in refs {
                    if seen.contains(ref_ptr) {
                        result.push(Instruction::UnifyValue(*ref_ptr))
                    } else {
                        seen.insert(*ref_ptr);
                        result.push(Instruction::UnifyVariable(*ref_ptr))
                    }
                }
            }
        }
    }

    // set up general purpose registers
    for reg @ RegPtr(struct_index) in structs {
        if struct_index < max_argument as usize {
            continue;
        }

        let FlatStruct(f, refs) = reg_map[&reg].get_str();
        result.push(Instruction::GetStructure(*f, reg));
        seen.insert(reg);
        for ref_ptr in refs {
            if seen.contains(ref_ptr) {
                result.push(Instruction::UnifyValue(*ref_ptr))
            } else {
                seen.insert(*ref_ptr);
                result.push(Instruction::UnifyVariable(*ref_ptr))
            }
        }
    }

    // epilogue
    result.push(Instruction::Proceed);

    CompileResult {
        instructions: result,
        var_mapping: VarMapping::from_inverse(vars),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompileResult {
    pub instructions: Vec<Instruction>,
    pub var_mapping: VarMapping,
}

#[test]
fn test_compile_query() {
    use lang::parse_struct;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();
    let query = parse_struct("p(Z,h(Z,W),f(W))", &mut symbol_table).unwrap();
    let instructions = Instruction::from_assembly(
        r#"
        put_variable X4, A1
        put_structure h/2, A2
        set_value X4
        set_variable X5
        put_structure f/1, A3
        set_value X5
        "#,
        &mut symbol_table,
    )
    .unwrap();
    assert_eq!(
        compile_query(query),
        CompileResult {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("Z")),
                (RegPtr(5), symbol_table.intern("W"))
            ])
        }
    )
}

#[test]
fn test_compile_query2() {
    use lang::parse_struct;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();
    let query = parse_struct("f(b, Y)", &mut symbol_table).unwrap();
    let instructions = Instruction::from_assembly(
        r#"
        put_structure b/0, A1
        put_variable X3, A2
        "#,
        &mut symbol_table,
    )
    .unwrap();
    assert_eq!(
        compile_query(query),
        CompileResult {
            instructions,
            var_mapping: VarMapping::from_iter([(RegPtr(3), symbol_table.intern("Y"))])
        }
    )
}

#[test]
fn test_compile_program() {
    use lang::parse_struct;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();
    let program = parse_struct("p(f(X), h(Y,f(a)), Y)", &mut symbol_table).unwrap();
    let instructions = Instruction::from_assembly(
        r#"
        get_structure f/1, A1
        unify_variable X4
        get_structure h/2, A2
        unify_variable X5
        unify_variable X6
        get_value X5, A3
        get_structure f/1, X6
        unify_variable X7
        get_structure a/0, X7
        proceed
        "#,
        &mut symbol_table,
    )
    .unwrap();
    assert_eq!(
        compile_program(program),
        CompileResult {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("X")),
                (RegPtr(5), symbol_table.intern("Y"))
            ])
        }
    )
}

#[test]
fn test_compile_query_line() {
    use lang::parse_struct;
    let mut symbol_table = SymbolTable::new();
    use symbol::SymbolTable;
    // same query with longer names
    // horizontal(line(point(X1, Y), point(X2, Y)))
    let query = parse_struct("h(l(p(A, Y), p(B, Y)))", &mut symbol_table).unwrap();
    let instructions = Instruction::from_assembly(
        r#"
        put_structure p/2, X2
        set_variable X4
        set_variable X5
        put_structure p/2, X3
        set_variable X6
        set_value X5
        put_structure l/2, A1
        set_value X2
        set_value X3
        "#,
        &mut symbol_table,
    )
    .unwrap();
    assert_eq!(
        compile_query(query),
        CompileResult {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("A")),
                (RegPtr(5), symbol_table.intern("Y")),
                (RegPtr(6), symbol_table.intern("B")),
            ])
        }
    )
}

#[test]
fn test_compile_program_line() {
    use lang::parse_struct;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();
    // same query with longer names
    // horizontal(line(point(X1, Y), point(X2, Y)))
    let query = parse_struct("h(l(p(A, Y), p(B, Y)))", &mut symbol_table).unwrap();
    let instructions = Instruction::from_assembly(
        r#"
        get_structure l/2, A1
        unify_variable X2
        unify_variable X3
        get_structure p/2, X2
        unify_variable X4
        unify_variable X5
        get_structure p/2, X3
        unify_variable X6
        unify_value X5
        proceed
        "#,
        &mut symbol_table,
    )
    .unwrap();
    assert_eq!(
        compile_program(query),
        CompileResult {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("A")),
                (RegPtr(5), symbol_table.intern("Y")),
                (RegPtr(6), symbol_table.intern("B")),
            ])
        }
    )
}
