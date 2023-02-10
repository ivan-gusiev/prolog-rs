use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::iter::FromIterator;

use instr::Instruction;
use lang::{Term, VarName};

// TODO: for some reason rustc requires `extern crate` definitions, fix this
extern crate topological_sort;
use self::topological_sort::TopologicalSort;

use crate::util::{writeout, writeout_dict, writeout_table2};
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

fn get_root_functor(term: &Term) -> Option<Functor> {
    match term {
        Term::Struct(s) => Some(s.functor()),
        _ => None,
    }
}

fn flatten_query(query: Term) -> (Vec<FlattenedTerm<RegPtr>>, HashMap<VarName, RegPtr>) {
    let mut term_map = HashMap::<TermId, RegPtr>::new();
    let mut var_map = HashMap::<VarName, RegPtr>::new();
    let mut queue = VecDeque::from([(&query, TermId(0))]);
    let mut flatrefs = Vec::<FlattenedTerm<FlatRef>>::new();
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
                    flatrefs.push(FlattenedTerm::Variable(*nm));
                    var_map.insert(*nm, RegPtr(flatrefs.len()));
                    term_map.insert(id, RegPtr(flatrefs.len()));
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
                term_map.insert(id, RegPtr(flatrefs.len()));
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

    let result: Vec<FlattenedTerm<RegPtr>> = flatrefs
        .iter()
        .map(|ft| ft.transform(flat_to_reg))
        .collect();

    (result, var_map)
}

// TODO: unfuck it by mapping everything

fn flatten_query_l1(query: Term) -> (Vec<FlattenedTerm<RegPtr>>, HashMap<VarName, RegPtr>) {
    let root_functor = get_root_functor(&query).expect("query root must be a functor");
    let mut term_map = HashMap::<TermId, RegPtr>::new();
    let mut var_map = HashMap::<VarName, RegPtr>::new();
    let mut queue = VecDeque::from([(&query, TermId(0))]);
    let mut flatrefs = Vec::<FlattenedTerm<FlatRef>>::new();
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
                } else if id.0 <= root_functor.arity() as usize {
                    //let var_id = next_term_id();
                    flatrefs.push(FlattenedTerm::Variable(*nm));
                    term_map.insert(id, RegPtr(id.0));
                    //queue.push_back((t, var_id))
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

    println!("FLATREFS\n{}", writeout(flatrefs.iter()));
    println!("TERMS\n{}", writeout_dict(term_map.iter()));
    println!("VARS\n{}", writeout_dict(var_map.iter()));
    let result: Vec<FlattenedTerm<RegPtr>> = flatrefs
        .iter()
        .map(|ft| ft.transform(flat_to_reg))
        .collect();

    println!("REGREFS\n{}", writeout(result.iter()));

    (result, var_map)
}

fn ptoi(ptr: RegPtr) -> usize {
    ptr.0 - 1
}

fn itop(idx: usize) -> RegPtr {
    RegPtr(idx + 1)
}

fn extract_structs(terms: &[FlattenedTerm<RegPtr>]) -> Vec<RegPtr> {
    let mut structs = vec![];
    for (i, term) in terms.iter().enumerate() {
        match term {
            FlattenedTerm::Struct(_) => structs.push(itop(i)),
            FlattenedTerm::Variable(_) => (),
        }
    }
    structs
}

fn order_query_structs(terms: &[FlattenedTerm<RegPtr>], structs_to_sort: &[RegPtr]) -> Vec<RegPtr> {
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
        for ref_ptr in refs {
            if seen.contains(ref_ptr) {
                result.push(Instruction::SetValue(*ref_ptr))
            } else {
                seen.insert(*ref_ptr);
                result.push(Instruction::SetVariable(*ref_ptr))
            }
        }
    }

    CompileResult {
        instructions: result,
        var_mapping: vars.into(),
    }
}

pub fn compile_query_l1(query: Term) -> CompileResult {
    let (registers, vars) = flatten_query_l1(query);
    {
        let regptrs = (1..registers.len()).map(|i| RegPtr(i)).collect::<Vec<_>>();
        println!(
            "compile_query_l1:registers\n{}",
            writeout_table2(&regptrs, &registers[1..])
        );
        let varnames = vars.keys().copied().collect::<Vec<_>>();
        let varptrs = varnames.iter().map(|nm| vars[nm]).collect::<Vec<_>>();
        println!(
            "compile_query_l1:vars\n{}",
            writeout_table2(&varnames, &varptrs)
        );
    }
    let structs = order_query_structs(&registers[1..], &extract_structs(&registers[1..]));
    {
        println!("compile_query_l1:structures\n{}", writeout(structs.iter()));
    }
    let mut seen = HashSet::<RegPtr>::new();
    let mut result = vec![];

    let root = &registers[0];
    let max_argument = root.get_str().functor().arity() + 1;

    // set up general purpose registers
    for struct_ptr @ RegPtr(struct_index) in structs {
        if struct_index <= max_argument as usize {
            println!("skipping {struct_ptr} as its index is withing argument range {max_argument}");
            continue;
        }

        let FlatStruct(f, refs) = registers[struct_ptr.0].get_str();
        result.push(Instruction::PutStructure(*f, struct_ptr));
        seen.insert(struct_ptr);
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
        println!("Argument register {} = {}", areg, &registers[i]);
        match &registers[i] {
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
        for ref_ptr in refs {
            if seen.contains(ref_ptr) {
                result.push(Instruction::UnifyValue(*ref_ptr))
            } else {
                seen.insert(*ref_ptr);
                result.push(Instruction::UnifyVariable(*ref_ptr))
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
        r#"Struct(FlatStruct(Functor(:p, 3), [RegPtr(2), RegPtr(3), RegPtr(4)]))||
Variable(:Z)||
Struct(FlatStruct(Functor(:h, 2), [RegPtr(2), RegPtr(5)]))||
Struct(FlatStruct(Functor(:f, 1), [RegPtr(5)]))||
Variable(:W)"#
            .to_string()
    );
}

#[test]
fn test_compile_query() {
    use lang::parse_term;
    let mut symbol_table = SymbolTable::new();
    let query = parse_term("p(Z,h(Z,W),f(W))", &mut symbol_table).unwrap();
    let instructions = Instruction::from_assembly(
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
fn test_compile_query_l1() {
    use lang::parse_term;
    let mut symbol_table = SymbolTable::new();
    let query = parse_term("p(Z,h(Z,W),f(W))", &mut symbol_table).unwrap();
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
        compile_query_l1(query),
        CompileResult {
            instructions,
            var_mapping: HashMap::from([
                (symbol_table.intern("W"), RegPtr(5)),
                (symbol_table.intern("Z"), RegPtr(4))
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
    let instructions = Instruction::from_assembly(
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
