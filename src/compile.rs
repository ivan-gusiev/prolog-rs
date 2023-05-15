use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::iter::FromIterator;

use asm::{Assembly, EntryPoint};
use data::{CodePtr, RegPtr};
use instr::Instruction;
use lang::{Functor, Sentence, Struct, Term, VarName};
use symbol::{to_display, SymDisplay};
use var::VarMapping;

use crate::data::StackDepth;

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
    Struct(FlatStruct<Ptr>, usize),
}

impl<Ptr: Display> Display for FlattenedTerm<Ptr> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FlattenedTerm::Variable(n) => n.fmt(f),
            FlattenedTerm::Struct(s, gen) => write!(f, "{s}:{gen}"),
        }
    }
}

impl<Ptr: Copy> FlattenedTerm<Ptr> {
    fn new_str(f: Functor, gen: usize, refs: Vec<Ptr>) -> FlattenedTerm<Ptr> {
        FlattenedTerm::Struct(FlatStruct(f, refs), gen)
    }

    fn get_str(&self) -> &FlatStruct<Ptr> {
        if let FlattenedTerm::Struct(str, _) = self {
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
            FlattenedTerm::Struct(str, gen) => FlattenedTerm::Struct(str.transform(mapper), *gen),
        }
    }
}

#[derive(PartialEq, Debug)]
struct FlattenedReg(RegPtr, FlattenedTerm<RegPtr>);

impl FlattenedReg {
    fn to_tuple(&self) -> (RegPtr, &FlattenedTerm<RegPtr>) {
        (self.0, &self.1)
    }
}

fn flatten_struct(root_struct: Struct) -> (Vec<FlattenedReg>, HashMap<VarName, RegPtr>, Functor) {
    let root_functor = root_struct.functor();
    let root_term = Term::Struct(root_struct);
    let mut term_map = HashMap::<TermId, RegPtr>::new();
    let mut var_map = HashMap::<VarName, RegPtr>::new();
    let mut queue = VecDeque::from([(&root_term, TermId(0), 0)]);
    let mut flatrefs = Vec::<FlattenedTerm<FlatRef>>::new();
    let mut term_counter = TermId(0);

    let mut next_term_id = || {
        term_counter.inc();
        term_counter
    };

    loop {
        match queue.pop_front() {
            None => break,
            Some((t @ Term::Variable(nm), id, gen)) => {
                if var_map.contains_key(nm) {
                    term_map.insert(id, var_map[nm]);
                } else if id.0 <= root_functor.arity() as usize {
                    //let var_id = next_term_id();
                    flatrefs.push(FlattenedTerm::Variable(*nm));
                    term_map.insert(id, RegPtr(id.0));
                    queue.push_back((t, next_term_id(), gen))
                } else {
                    flatrefs.push(FlattenedTerm::Variable(*nm));
                    var_map.insert(*nm, RegPtr(flatrefs.len() - 1));
                    term_map.insert(id, RegPtr(flatrefs.len() - 1));
                }
            }
            Some((Term::Struct(str), id, gen)) => {
                let mut subterms: Vec<FlatRef> = vec![];

                for subterm in str.terms() {
                    match subterm {
                        v @ Term::Variable(nm) => {
                            if var_map.contains_key(nm) {
                                subterms.push(FlatRef::Register(var_map[nm]))
                            } else {
                                let id = next_term_id();
                                subterms.push(FlatRef::Term(id));
                                queue.push_back((v, id, gen + 1));
                            }
                        }
                        t @ Term::Struct(_) => {
                            let id = next_term_id();
                            subterms.push(FlatRef::Term(id));
                            queue.push_back((t, id, gen + 1));
                        }
                    }
                }
                flatrefs.push(FlattenedTerm::new_str(str.functor(), gen, subterms));
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

    (result, var_map, root_functor)
}

fn extract_structs(terms: &[FlattenedReg]) -> Vec<RegPtr> {
    let mut structs = vec![];
    for term in terms.iter() {
        if let FlattenedReg(reg, FlattenedTerm::Struct(_, _)) = term {
            structs.push(*reg)
        }
    }
    structs
}

fn order_query_structs(
    terms: &HashMap<RegPtr, &FlattenedTerm<RegPtr>>,
    structs_to_sort: &[RegPtr],
) -> Vec<RegPtr> {
    let mut result = structs_to_sort.to_vec();
    result.sort_by_key(|r| match terms.get(r) {
        Some(FlattenedTerm::Struct(_, gen)) => Some(usize::MAX - gen),
        _ => None,
    });
    result
}

fn get_permanent_variables(head: &Struct, goals: &[Struct]) -> HashSet<VarName> {
    fn collect_variables(str: &Struct) -> HashSet<VarName> {
        let mut result = HashSet::default();
        let root = Term::Struct(str.clone());
        let mut q = vec![&root];

        loop {
            match q.pop() {
                Some(Term::Struct(s)) => q.extend(s.terms()),
                Some(Term::Variable(v)) => {
                    result.insert(*v);
                }
                None => break,
            }
        }

        result
    }

    if goals.is_empty() {
        return HashSet::default();
    }

    let head_vars = collect_variables(head);
    let mut goal_vars = goals.iter().map(collect_variables).collect::<Vec<_>>();
    goal_vars[0].extend(head_vars);

    let mut seen = HashSet::<VarName>::new();
    let mut result = HashSet::<VarName>::new();

    for goal in goal_vars.iter() {
        let occur_again = goal.intersection(&seen);
        result.extend(occur_again);
        seen.extend(goal);
    }

    result
}

pub fn compile_query(
    query: Struct,
    programs: &HashMap<Functor, CodePtr>,
) -> Result<CompileInfo, CompileError> {
    let (registers_with_root, vars, root_functor) = flatten_struct(query);
    let program_code_ptr = programs
        .get(&root_functor)
        .ok_or(CompileError::UnknownFunctor(root_functor))?;

    let registers = &registers_with_root[1..];
    let reg_map = HashMap::from_iter(registers.iter().map(FlattenedReg::to_tuple));
    let structs = order_query_structs(&reg_map, &extract_structs(registers));
    let mut seen = HashSet::<RegPtr>::new();
    let mut instructions = vec![];

    let FlattenedReg(_, root) = &registers_with_root[0];
    let max_argument = root.get_str().functor().arity() + 1;

    // set up general purpose registers
    for reg @ RegPtr(struct_index) in structs {
        if struct_index < max_argument as usize {
            continue;
        }

        let FlatStruct(f, refs) = reg_map[&reg].get_str();
        instructions.push(Instruction::PutStructure(*f, reg.into()));
        seen.insert(reg);
        for ref_ptr in refs {
            if seen.contains(ref_ptr) {
                instructions.push(Instruction::SetValue((*ref_ptr).into()))
            } else {
                seen.insert(*ref_ptr);
                instructions.push(Instruction::SetVariable((*ref_ptr).into()))
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
                    instructions.push(Instruction::PutValue(xreg.into(), areg.into()));
                } else {
                    seen.insert(xreg);
                    instructions.push(Instruction::PutVariable(xreg.into(), areg.into()));
                }
            }
            FlattenedTerm::Struct(FlatStruct(f, refs), _) => {
                instructions.push(Instruction::PutStructure(*f, areg.into()));
                for ref_ptr in refs {
                    if seen.contains(ref_ptr) {
                        instructions.push(Instruction::SetValue((*ref_ptr).into()))
                    } else {
                        seen.insert(*ref_ptr);
                        instructions.push(Instruction::SetVariable((*ref_ptr).into()))
                    }
                }
            }
        }
    }
    // call the corresponding program
    instructions.push(Instruction::Call(*program_code_ptr));

    Ok(CompileInfo {
        instructions,
        var_mapping: VarMapping::from_inverse(vars),
        label_functor: None,
    })
}

pub fn compile_program(program: Struct) -> CompileInfo {
    let (registers_with_root, vars, root_functor) = flatten_struct(program);
    let registers = &registers_with_root[1..];
    let reg_map: HashMap<RegPtr, &FlattenedTerm<RegPtr>> =
        HashMap::from_iter(registers.iter().map(FlattenedReg::to_tuple));
    let structs = extract_structs(registers);
    let mut seen = HashSet::<RegPtr>::new();
    let mut instructions = vec![];

    let FlattenedReg(_, root) = &registers_with_root[0];
    let max_argument = root.get_str().functor().arity() + 1;

    // set up argument registers
    for i in 1..(max_argument as usize) {
        let areg = RegPtr(i);
        match &reg_map[&areg] {
            FlattenedTerm::Variable(nm) => {
                let xreg = vars[nm];
                if seen.contains(&xreg) {
                    instructions.push(Instruction::GetValue(xreg.into(), areg.into()));
                } else {
                    seen.insert(xreg);
                    instructions.push(Instruction::GetVariable(xreg.into(), areg.into()));
                }
            }
            FlattenedTerm::Struct(FlatStruct(f, refs), _) => {
                instructions.push(Instruction::GetStructure(*f, areg.into()));
                for ref_ptr in refs {
                    if seen.contains(ref_ptr) {
                        instructions.push(Instruction::UnifyValue((*ref_ptr).into()))
                    } else {
                        seen.insert(*ref_ptr);
                        instructions.push(Instruction::UnifyVariable((*ref_ptr).into()))
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
        instructions.push(Instruction::GetStructure(*f, reg.into()));
        seen.insert(reg);
        for ref_ptr in refs {
            if seen.contains(ref_ptr) {
                instructions.push(Instruction::UnifyValue((*ref_ptr).into()))
            } else {
                seen.insert(*ref_ptr);
                instructions.push(Instruction::UnifyVariable((*ref_ptr).into()))
            }
        }
    }

    // epilogue
    instructions.push(Instruction::Proceed);

    CompileInfo {
        instructions,
        var_mapping: VarMapping::from_inverse(vars),
        label_functor: Some(root_functor),
    }
}

pub fn compile_rule(
    head: Struct,
    goals: Vec<Struct>,
    _programs: &HashMap<Functor, CodePtr>,
) -> Result<CompileInfo, CompileError> {
    let permanent_vars = get_permanent_variables(&head, &goals);
    let mut instructions = vec![];

    // prologue
    instructions.push(Instruction::Allocate(StackDepth(permanent_vars.len())));

    // epilogue
    instructions.push(Instruction::Deallocate);

    Ok(CompileInfo {
        instructions,
        var_mapping: VarMapping::default(),
        label_functor: Some(head.functor()),
    })
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompileInfo {
    pub instructions: Vec<Instruction>,
    pub var_mapping: VarMapping,
    pub label_functor: Option<Functor>,
}

impl CompileInfo {
    pub fn append_to_assembly(self, assembly: &mut Assembly) -> EntryPoint {
        let base_address = CodePtr(assembly.instructions.len());
        if let Some(label_functor) = self.label_functor {
            assembly.label_map.insert(label_functor, base_address);
            assembly
                .bindings_map
                .insert(label_functor, self.var_mapping.clone());
        }
        assembly.instructions.extend(self.instructions);
        EntryPoint {
            location: base_address,
            variables: self.var_mapping,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompileError {
    UnknownFunctor(Functor),
    UnsupportedRule(Sentence),
    UnsupportedComplexQuery(Sentence),
}

impl SymDisplay for CompileError {
    fn sym_fmt(
        &self,
        f: &mut Formatter<'_>,
        symbol_table: &crate::symbol::SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Self::UnknownFunctor(func) => write!(
                f,
                "Query refers to an unknown functor {}.",
                to_display(func, symbol_table)
            ),
            Self::UnsupportedRule(rule) => write!(
                f,
                "Cannot compile `{}`. Rule compilation not supported yet.",
                to_display(rule, symbol_table)
            ),
            Self::UnsupportedComplexQuery(query) => write!(
                f,
                "Cannot compile `{}`. Query with multiple (or zero) goals not supported yet.",
                to_display(query, symbol_table)
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompileWarning {
    IgnoredEntryPoint(CodePtr),
}

impl SymDisplay for CompileWarning {
    fn sym_fmt(
        &self,
        f: &mut Formatter<'_>,
        _symbol_table: &crate::symbol::SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Self::IgnoredEntryPoint(ptr) => write!(
                f,
                "Program contains multiple entry points. Ignoring the one at @{ptr}."
            ),
        }
    }
}

pub fn compile_sentences(
    sentences: Vec<Sentence>,
    assembly: &mut Assembly,
) -> Result<Vec<CompileWarning>, CompileError> {
    let mut warnings = Vec::<CompileWarning>::new();
    for mut sentence in sentences {
        if sentence.is_rule() {
            return Err(CompileError::UnsupportedRule(sentence));
        }

        if sentence.is_query() {
            if sentence.goals.len() != 1 {
                return Err(CompileError::UnsupportedComplexQuery(sentence));
            }

            let query_result = compile_query(sentence.goals.remove(0), &assembly.label_map)?;
            let entry_point = query_result.append_to_assembly(assembly);
            if let Some(old_entry_point) = assembly.entry_point.take() {
                warnings.push(CompileWarning::IgnoredEntryPoint(old_entry_point.location))
            }
            assembly.entry_point = Some(entry_point);
        }

        if let Some(head) = sentence.head {
            let program_result = compile_program(head);
            program_result.append_to_assembly(assembly);
        }
    }
    Ok(warnings)
}

#[test]
fn test_flatten_struct() {
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();
    let p2 = Functor(symbol_table.intern("p"), 2);
    let r2 = Functor(symbol_table.intern("r"), 2);
    let x = symbol_table.intern("X");
    let y = symbol_table.intern("Y");
    let z = symbol_table.intern("Z");
    let rzy = Term::Struct(Struct::new(r2, &[Term::Variable(y), Term::Variable(z)]).unwrap());
    let root = Struct::new(p2, &[Term::Variable(x), rzy]).unwrap();
    let (regs, vars, f) = flatten_struct(root);
    let regs_strings = regs.iter().map(|x| format!("{:?}", x)).collect::<Vec<_>>();
    assert_eq!(
        (regs_strings.iter().map(String::as_str).collect::<Vec<_>>(), vars, f),
        (vec![
            "FlattenedReg(RegPtr(0), Struct(FlatStruct(Functor(:p, 2), [RegPtr(1), RegPtr(2)]), 0))", 
            "FlattenedReg(RegPtr(1), Variable(:X))", 
            "FlattenedReg(RegPtr(2), Struct(FlatStruct(Functor(:r, 2), [RegPtr(4), RegPtr(5)]), 1))", 
            "FlattenedReg(RegPtr(3), Variable(:X))", 
            "FlattenedReg(RegPtr(4), Variable(:Y))", 
            "FlattenedReg(RegPtr(5), Variable(:Z))"],
         HashMap::from_iter([(x, RegPtr(3)), (y, RegPtr(4)), (z, RegPtr(5))]),
         p2));
}

#[test]
fn test_compile_query() {
    use assembler::compile_asm;
    use lang::parse_struct;
    use symbol::SymbolTable;
    use util::lbl_for;
    let mut symbol_table = SymbolTable::new();
    let query = parse_struct("p(Z,h(Z,W),f(W))", &mut symbol_table).unwrap();
    let labels = lbl_for(query.functor());
    let instructions = compile_asm(
        r#"
        put_variable X4, A1
        put_structure h/2, A2
        set_value X4
        set_variable X5
        put_structure f/1, A3
        set_value X5
        call @0
        "#,
        &mut symbol_table,
    )
    .unwrap()
    .instructions;
    assert_eq!(
        compile_query(query, &labels).unwrap(),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("Z")),
                (RegPtr(5), symbol_table.intern("W"))
            ]),
            label_functor: None
        }
    )
}

#[test]
fn test_compile_query2() {
    use assembler::compile_asm;
    use lang::parse_struct;
    use symbol::SymbolTable;
    use util::lbl_for;
    let mut symbol_table = SymbolTable::new();
    let query = parse_struct("f(b, Y)", &mut symbol_table).unwrap();
    let labels = lbl_for(query.functor());
    let instructions = compile_asm(
        r#"
        put_structure b/0, A1
        put_variable X3, A2
        call @0
        "#,
        &mut symbol_table,
    )
    .unwrap()
    .instructions;
    assert_eq!(
        compile_query(query, &labels).unwrap(),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([(RegPtr(3), symbol_table.intern("Y"))]),
            label_functor: None,
        }
    )
}

#[test]
fn test_compile_program() {
    use assembler::compile_asm;
    use lang::parse_struct;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();
    let program = parse_struct("p(f(X), h(Y,f(a)), Y)", &mut symbol_table).unwrap();
    let instructions = compile_asm(
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
    .unwrap()
    .instructions;
    assert_eq!(
        compile_program(program),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("X")),
                (RegPtr(5), symbol_table.intern("Y"))
            ]),
            label_functor: Some(Functor(symbol_table.intern("p"), 3)),
        }
    )
}

#[test]
fn test_compile_query_line() {
    use assembler::compile_asm;
    use lang::parse_struct;
    use symbol::SymbolTable;
    use util::lbl_for;
    let mut symbol_table = SymbolTable::new();
    // same query with longer names
    // horizontal(line(point(X1, Y), point(X2, Y)))
    let query = parse_struct("h(l(p(A, Y), p(B, Y)))", &mut symbol_table).unwrap();
    let labels = lbl_for(query.functor());
    let instructions = compile_asm(
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
        call @0
        "#,
        &mut symbol_table,
    )
    .unwrap()
    .instructions;
    assert_eq!(
        compile_query(query, &labels).unwrap(),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("A")),
                (RegPtr(5), symbol_table.intern("Y")),
                (RegPtr(6), symbol_table.intern("B")),
            ]),
            label_functor: None,
        }
    )
}

#[test]
fn test_compile_program_line() {
    use assembler::compile_asm;
    use lang::parse_struct;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();
    // same program with longer names
    // horizontal(line(point(X1, Y), point(X2, Y)))
    let fact = parse_struct("h(l(p(A, Y), p(B, Y)))", &mut symbol_table).unwrap();
    let instructions = compile_asm(
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
    .unwrap()
    .instructions;
    assert_eq!(
        compile_program(fact),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("A")),
                (RegPtr(5), symbol_table.intern("Y")),
                (RegPtr(6), symbol_table.intern("B")),
            ]),
            label_functor: Some(Functor(symbol_table.intern("h"), 1)),
        }
    )
}

#[test]
fn test_get_permanent_variables() {
    use lang::parse_sentence;
    use symbol::SymbolTable;

    let mut symbol_table = SymbolTable::new();
    let sentence = parse_sentence("p(X, Y) :- q(X, Z), r(Z, Y).", &mut symbol_table).unwrap();
    let items = get_permanent_variables(&sentence.head.unwrap(), &sentence.goals);
    assert_eq!(
        items,
        HashSet::from([symbol_table.intern("Y"), symbol_table.intern("Z")])
    );

    let sentence = parse_sentence("a :- b(X), c(X).", &mut symbol_table).unwrap();
    let items = get_permanent_variables(&sentence.head.unwrap(), &sentence.goals);
    assert_eq!(items, HashSet::from([symbol_table.intern("X")]))
}

#[test]
fn test_compile_rule() {
    use assembler::compile_asm;
    use lang::parse_sentence;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();

    let rule = parse_sentence("p(X, Y) :- q(X, Z), r(Z, Y).", &mut symbol_table).unwrap();
    let expected_assembly = compile_asm(
        r#"
        allocate 2
        get_variable X3, A1
        get_variable Y1, A2
        put_value X3, A1
        put_variable Y2, A2
        call @100 ; q/2
        put_value Y2, A1
        put_value Y1, A2
        call @110 ; r/2
        deallocate
        "#,
        &mut symbol_table,
    )
    .unwrap();
    let p = symbol_table.intern("p");
    let q = symbol_table.intern("q");
    let r = symbol_table.intern("r");
    let p2 = Functor(p, 2);
    let q2 = Functor(q, 2);
    let r2 = Functor(r, 2);
    let label_map = HashMap::from_iter([(q2, CodePtr(100)), (r2, CodePtr(110))]);
    assert_eq!(
        compile_rule(rule.head.unwrap(), rule.goals, &label_map),
        Ok(CompileInfo {
            instructions: expected_assembly.instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4), symbol_table.intern("A")),
                (RegPtr(5), symbol_table.intern("Y")),
                (RegPtr(6), symbol_table.intern("B")),
            ]),
            label_functor: Some(p2),
        })
    )
}
