use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::hash::Hash;

use instr::Instruction;
use lang::{Term, VarName};

// TODO: for some reason rustc requires `extern crate` definitions, fix this
extern crate topological_sort;
use self::topological_sort::TopologicalSort;

use crate::{data::RegPtr, lang::Functor};

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
    Term(TermId), // TODO: this shouldn't exist after flatten_term returns
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
struct FlatStruct(Functor, Vec<FlatRef>);

impl Display for FlatStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.0)?;
        for term in self.1.iter() {
            write!(f, "{term},")?
        }
        write!(f, ")")
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
enum FlattenedTerm {
    Variable(VarName),
    Struct(FlatStruct),
}

impl Display for FlattenedTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FlattenedTerm::Variable(n) => n.fmt(f),
            FlattenedTerm::Struct(s) => s.fmt(f),
        }
    }
}

impl FlattenedTerm {
    fn new_str(f: Functor, refs: Vec<FlatRef>) -> FlattenedTerm {
        FlattenedTerm::Struct(FlatStruct(f, refs))
    }

    fn get_str(&self) -> &FlatStruct {
        if let FlattenedTerm::Struct(str) = self {
            str
        } else {
            panic!("not a struct")
        }
    }
}

fn flatten_query(query: Term) -> (Vec<FlattenedTerm>, HashMap<VarName, RegPtr>) {
    let mut term_map = HashMap::<TermId, RegPtr>::new();
    let mut var_map = HashMap::<VarName, RegPtr>::new();
    let mut queue = VecDeque::from([(&query, TermId(0))]);
    let mut result = Vec::<FlattenedTerm>::new();
    let mut term_counter = TermId(0);

    let mut next_term_id = || {
        term_counter.inc();
        term_counter
    };

    loop {
        match queue.pop_front() {
            None => break,
            Some((Term::Variable(nm), id)) => {
                if var_map.contains_key(nm) {
                    term_map.insert(id, var_map[nm]);
                } else {
                    result.push(FlattenedTerm::Variable(*nm));
                    var_map.insert(*nm, RegPtr(result.len()));
                    term_map.insert(id, RegPtr(result.len()));
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
                result.push(FlattenedTerm::new_str(str.functor(), subterms));
                term_map.insert(id, RegPtr(result.len()));
            }
        }
    }

    for term in result.iter_mut() {
        match term {
            FlattenedTerm::Variable(_) => (),
            FlattenedTerm::Struct(FlatStruct(_, subterms)) => {
                for subterm in subterms {
                    if let FlatRef::Term(id) = subterm {
                        match term_map.get(id) {
                            Some(reg) => *subterm = FlatRef::Register(*reg),
                            None => panic!("Could not find a term for {:?}", id),
                        }
                    }
                }
            }
        }
    }

    (result, var_map)
}

fn ptoi(ptr: RegPtr) -> usize {
    ptr.0 - 1
}

fn itop(idx: usize) -> RegPtr {
    RegPtr(idx + 1)
}

fn extract_structs(terms: &[FlattenedTerm]) -> Vec<RegPtr> {
    let mut structs = vec![];
    for (i, term) in terms.iter().enumerate() {
        match term {
            FlattenedTerm::Struct(_) => structs.push(itop(i)),
            FlattenedTerm::Variable(_) => (),
        }
    }
    structs
}

fn order_query_structs(terms: &[FlattenedTerm], structs_to_sort: &[RegPtr]) -> Vec<RegPtr> {
    fn regs(term: &FlattenedTerm) -> HashSet<RegPtr> {
        let mut result = HashSet::new();

        if let FlattenedTerm::Struct(FlatStruct(_, refs)) = term {
            for r in refs {
                if let FlatRef::Register(ptr) = r {
                    result.insert(*ptr);
                }
            }
        }

        result
    }

    let mut ts = TopologicalSort::<RegPtr>::new();
    for l in 0..structs_to_sort.len() {
        for r in (l + 1)..structs_to_sort.len() {
            let lp = structs_to_sort[l];
            let rp = structs_to_sort[r];
            let lhs = &terms[ptoi(lp)];
            let rhs = &terms[ptoi(rp)];

            let regs_lhs = regs(lhs);
            let regs_rhs = regs(rhs);

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

pub fn compile_query(query: Term) -> CompileResult {
    let (registers, vars) = flatten_query(query);
    let structs = order_query_structs(&registers, &extract_structs(&registers));
    let mut seen = HashSet::<RegPtr>::new();
    let mut result = vec![];

    for struct_ptr in structs {
        let FlatStruct(f, refs) = registers[ptoi(struct_ptr)].get_str();
        result.push(Instruction::PutStructure(*f, struct_ptr));
        seen.insert(struct_ptr);
        for flat_ref in refs {
            if let FlatRef::Register(ref_ptr) = flat_ref {
                if seen.contains(ref_ptr) {
                    result.push(Instruction::SetValue(*ref_ptr))
                } else {
                    seen.insert(*ref_ptr);
                    result.push(Instruction::SetVariable(*ref_ptr))
                }
            }
        }
    }

    CompileResult {
        instructions: result,
        var_mapping: vars.into(),
    }
}

pub fn compile_program(program: Term) -> CompileResult {
    let (registers, vars) = flatten_query(program);
    let structs = extract_structs(&registers);
    let mut seen = HashSet::<RegPtr>::new();
    let mut result = vec![];

    for struct_ptr in structs {
        let FlatStruct(f, refs) = registers[ptoi(struct_ptr)].get_str();
        result.push(Instruction::GetStructure(*f, struct_ptr));
        seen.insert(struct_ptr);
        for flat_ref in refs {
            if let FlatRef::Register(ref_ptr) = flat_ref {
                if seen.contains(ref_ptr) {
                    result.push(Instruction::UnifyValue(*ref_ptr))
                } else {
                    seen.insert(*ref_ptr);
                    result.push(Instruction::UnifyVariable(*ref_ptr))
                }
            }
        }
    }

    CompileResult {
        instructions: result,
        var_mapping: vars.into(),
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct VarMapping(HashMap<RegPtr, VarName>);

impl From<HashMap<VarName, RegPtr>> for VarMapping {
    fn from(value: HashMap<VarName, RegPtr>) -> Self {
        let mut result = HashMap::<RegPtr, VarName>::new();
        for (v, r) in value.into_iter() {
            result.insert(r, v);
        }
        Self(result)
    }
}

impl VarMapping {
    pub fn mappings(&self) -> impl Iterator<Item = (&VarName, &RegPtr)> {
        self.0.iter().map(|(r, v)| (v, r))
    }

    pub fn get(&self, ptr: &RegPtr) -> Option<VarName> {
        self.0.get(ptr).copied()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompileResult {
    pub instructions: Vec<Instruction>,
    pub var_mapping: VarMapping,
}

#[cfg(test)]
use crate::symbol::SymbolTable;

#[test]
fn test_order_structs() {
    use lang::parse_term;
    let mut symbol_table = SymbolTable::new();
    let query = parse_term("p(Z,h(Z,W),f(W))", &mut symbol_table).unwrap();
    let (terms, _) = flatten_query(query);
    let structs = extract_structs(&terms);

    assert_eq!(
        &order_query_structs(&terms, &structs),
        &[RegPtr(3), RegPtr(4), RegPtr(1)]
    );
}

#[test]
fn test_flatten_query() {
    use lang::parse_term;
    let mut symbol_table = SymbolTable::new();
    let query = parse_term("p(Z,h(Z,W),f(W))", &mut symbol_table).unwrap();

    let results = flatten_query(query)
        .0
        .into_iter()
        .map(|x| format!("{x:?}"))
        .collect::<Vec<_>>()
        .join("||\n");

    assert_eq!(
        format!("{results}"),
        r#"Struct(FlatStruct(Functor(:p, 3), [Register(RegPtr(2)), Register(RegPtr(3)), Register(RegPtr(4))]))||
Variable(:Z)||
Struct(FlatStruct(Functor(:h, 2), [Register(RegPtr(2)), Register(RegPtr(5))]))||
Struct(FlatStruct(Functor(:f, 1), [Register(RegPtr(5))]))||
Variable(:W)"#.to_string()
    );
}

#[test]
fn test_compile_query() {
    use lang::parse_term;
    let mut symbol_table = SymbolTable::new();
    let query = parse_term("p(Z,h(Z,W),f(W))", &mut symbol_table).unwrap();
    let instructions = Instruction::from_program(
        r#"
        put_structure h/2, X3
        set_variable X2      
        set_variable X5      
        put_structure f/1, X4
        set_value X5         
        put_structure p/3, X1
        set_value X2         
        set_value X3         
        set_value X4         
        "#,
        &mut symbol_table,
    )
    .unwrap();
    assert_eq!(
        compile_query(query),
        CompileResult {
            instructions,
            var_mapping: HashMap::from([
                (symbol_table.intern("W"), RegPtr(5)),
                (symbol_table.intern("Z"), RegPtr(2))
            ])
            .into()
        }
    )
}

#[test]
fn test_compile_program() {
    use lang::parse_term;
    let mut symbol_table = SymbolTable::new();
    let program = parse_term("p(f(X), h(Y,f(a)), Y)", &mut symbol_table).unwrap();
    let instructions = Instruction::from_program(
        r#"
        get_structure p/3, X1
        unify_variable X2
        unify_variable X3
        unify_variable X4
        get_structure f/1, X2
        unify_variable X5
        get_structure h/2, X3
        unify_value X4
        unify_variable X6
        get_structure f/1, X6
        unify_variable X7
        get_structure a/0, X7
        "#,
        &mut symbol_table,
    )
    .unwrap();
    assert_eq!(
        compile_program(program),
        CompileResult {
            instructions,
            var_mapping: HashMap::from([
                (symbol_table.intern("X"), RegPtr(5)),
                (symbol_table.intern("Y"), RegPtr(4))
            ])
            .into()
        }
    )
}
