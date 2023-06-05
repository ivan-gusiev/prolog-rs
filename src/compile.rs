use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::iter::FromIterator;

use asm::{Assembly, EntryPoint};
use data::{CodePtr, Local, RegPtr, StackDepth, StackPtr};
use instr::Instruction;
use lang::{var_name_is_not_ignorable, Functor, Sentence, Struct, Term, VarName};
use symbol::{to_display, SymDisplay};
use util::WriteVec;
use var::{VarMapping, VarRegs};

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

// Extracts all the variables from the term and orders them left to right
fn collect_variables<I: From<usize>>(
    str: &Struct,
    var_counter: &mut HashMap<VarName, I>,
) -> HashSet<VarName> {
    let mut result = HashSet::default();
    let root = Term::Struct(str.clone());
    let mut q = vec![&root];

    loop {
        match q.pop() {
            Some(Term::Struct(s)) => q.extend(s.terms().iter().rev()),
            Some(Term::Variable(v)) => {
                let next_index = var_counter.len() + 1;
                var_counter.entry(*v).or_insert(next_index.into());
                result.insert(*v);
            }
            None => break,
        }
    }

    result
}

/// Calculates which variables are permanent in the specified rule.
/// Returns a tuple. First item is the set of singleton variables (for warnings).
/// Second item is map from variable name to its proposed index in the stack.
/// The indices are ordered by earliest appearance, left to right.
fn get_permanent_variables(
    head: Option<&Struct>,
    goals: &[Struct],
) -> (HashSet<VarName>, HashMap<VarName, StackPtr>) {
    // assign a sequential number to each variable
    let mut var_counter = HashMap::<VarName, usize>::new();

    if goals.is_empty() {
        return (HashSet::default(), HashMap::default());
    }

    let mut head_vars = head
        .map(|h| collect_variables(h, &mut var_counter))
        .unwrap_or_default();
    let mut singleton_vars = head_vars.clone();
    let goal_vars = goals
        .iter()
        .map(|g| collect_variables(g, &mut var_counter))
        .collect::<Vec<_>>();

    let mut seen = HashSet::<VarName>::new();
    let mut permanent = HashSet::<VarName>::new();

    for (i, mut goal) in goal_vars.into_iter().enumerate() {
        for var in goal.iter() {
            singleton_vars.remove(var);
        }

        if i == 0 {
            // the first goal and the head share the variable set,
            // because the head can never mutate its variables
            goal.extend(head_vars.drain());
        }

        let occur_again = goal.intersection(&seen);
        permanent.extend(occur_again);
        seen.extend(goal);
    }

    // order permanent vars by incidence, and assign them sequential stack indices
    let mut ordered = permanent
        .into_iter()
        .map(|var| (var, var_counter.remove(&var).unwrap_or_default()))
        .collect::<Vec<_>>();
    ordered.sort_by_key(|(_, index)| *index);
    (
        singleton_vars,
        ordered
            .into_iter()
            .enumerate()
            .map(|(idx, (var, _))| (var, (idx + 1).into()))
            .collect(),
    )
}

// compiles goals for queries or rules
fn compile_goal(
    query: Struct,
    programs: &HashMap<Functor, CodePtr>,
    stack_vars: &HashMap<VarName, StackPtr>,
    seen: &mut HashSet<Local>,
) -> Result<CompileInfo, CompileError> {
    let (registers_with_root, vars, root_functor) = flatten_struct(query);
    let var_regs = VarRegs::from_inverse(&vars);
    let get_local = |regptr: &RegPtr| {
        var_regs
            .get(regptr)
            .and_then(|var| stack_vars.get(&var))
            .map(Local::from)
            .unwrap_or_else(|| Local::from(regptr))
    };
    let program_code_ptr = programs
        .get(&root_functor)
        .ok_or(CompileError::UnknownFunctor(root_functor))?;

    let registers = &registers_with_root[1..];
    let reg_map = HashMap::from_iter(registers.iter().map(FlattenedReg::to_tuple));
    let structs = order_query_structs(&reg_map, &extract_structs(registers));
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
        seen.insert(reg.into());
        for ref_ptr in refs {
            let local_ref = get_local(ref_ptr);
            if seen.contains(&local_ref) {
                instructions.push(Instruction::SetValue(local_ref.into()))
            } else {
                seen.insert(local_ref);
                instructions.push(Instruction::SetVariable(local_ref.into()))
            }
        }
    }
    // set up argument registers
    for i in 1..(max_argument as usize) {
        let areg = RegPtr(i);
        match &reg_map[&areg] {
            FlattenedTerm::Variable(nm) => {
                let xreg = get_local(&vars[nm]);
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
                    let local_ref = get_local(ref_ptr);
                    if seen.contains(&local_ref) {
                        instructions.push(Instruction::SetValue(local_ref.into()))
                    } else {
                        seen.insert(local_ref);
                        instructions.push(Instruction::SetVariable(local_ref.into()))
                    }
                }
            }
        }
    }
    // call the corresponding program
    instructions.push(Instruction::Call(*program_code_ptr));

    let var_mapping = VarMapping::from_iter(var_regs.iter().map(|(r, &name)| (get_local(r), name)));

    Ok(CompileInfo {
        instructions,
        var_mapping,
        label_functor: None,
        warnings: vec![],
    })
}

pub fn compile_query(
    goals: Vec<Struct>,
    programs: &HashMap<Functor, CodePtr>,
) -> Result<CompileInfo, CompileError> {
    let stack_vars = {
        let mut vars = HashMap::default();
        for goal in goals.iter() {
            collect_variables(goal, &mut vars);
        }
        vars
    };
    let mut instructions = vec![];

    // prologue
    if !stack_vars.is_empty() {
        instructions.push(Instruction::Allocate(StackDepth(stack_vars.len())));
    }

    let mut seen = HashSet::<Local>::new();
    let mut var_mapping = VarMapping::default();

    for goal in goals {
        let mut goal_result = compile_goal(goal, programs, &stack_vars, &mut seen)?;
        instructions.append(&mut goal_result.instructions);
        seen.retain(Local::is_stack); // "remember" only the permanent variables
        var_mapping.append(goal_result.var_mapping);
    }

    // epilogue
    instructions.push(Instruction::Publish);
    if !stack_vars.is_empty() {
        instructions.push(Instruction::Deallocate);
        var_mapping.retain_keys(Local::is_stack);
    }

    Ok(CompileInfo {
        instructions,
        var_mapping,
        label_functor: None,
        warnings: vec![],
    })
}

fn compile_head(
    goal: Struct,
    stack_vars: &HashMap<VarName, StackPtr>,
    seen: &mut HashSet<Local>,
) -> CompileInfo {
    let (registers_with_root, vars, root_functor) = flatten_struct(goal);
    let var_regs = VarRegs::from_inverse(&vars);
    let get_local = |regptr: &RegPtr| {
        var_regs
            .get(regptr)
            .and_then(|var| stack_vars.get(&var))
            .map(Local::from)
            .unwrap_or_else(|| Local::from(regptr))
    };
    let registers = &registers_with_root[1..];
    let reg_map: HashMap<RegPtr, &FlattenedTerm<RegPtr>> =
        HashMap::from_iter(registers.iter().map(FlattenedReg::to_tuple));
    let structs = extract_structs(registers);
    let mut instructions = vec![];

    let FlattenedReg(_, root) = &registers_with_root[0];
    let max_argument = root.get_str().functor().arity() + 1;

    // set up argument registers
    for i in 1..(max_argument as usize) {
        let areg = RegPtr(i);
        match &reg_map[&areg] {
            FlattenedTerm::Variable(nm) => {
                let xreg = get_local(&vars[nm]);
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
                    let local_ref = get_local(ref_ptr);
                    if seen.contains(&local_ref) {
                        instructions.push(Instruction::UnifyValue(local_ref.into()))
                    } else {
                        seen.insert(local_ref);
                        instructions.push(Instruction::UnifyVariable(local_ref.into()))
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
        seen.insert(reg.into());
        for ref_ptr in refs {
            let local_ref = get_local(ref_ptr);
            if seen.contains(&local_ref) {
                instructions.push(Instruction::UnifyValue((*ref_ptr).into()))
            } else {
                seen.insert(local_ref);
                instructions.push(Instruction::UnifyVariable((*ref_ptr).into()))
            }
        }
    }

    let var_mapping = VarMapping::from_iter(var_regs.iter().map(|(r, &name)| (get_local(r), name)));
    CompileInfo {
        instructions,
        var_mapping,
        label_functor: Some(root_functor),
        warnings: vec![],
    }
}

pub fn compile_fact(program: Struct) -> CompileInfo {
    let mut result = compile_head(program, &HashMap::default(), &mut HashSet::default());
    result.instructions.push(Instruction::Proceed);
    result
}

pub fn compile_rule(
    head: Struct,
    goals: Vec<Struct>,
    programs: &HashMap<Functor, CodePtr>,
) -> Result<CompileInfo, CompileError> {
    let (mut singleton_vars, permanent_vars) = get_permanent_variables(Option::Some(&head), &goals);
    let mut instructions = vec![];
    let mut warnings = vec![];

    singleton_vars.retain(var_name_is_not_ignorable);
    if !singleton_vars.is_empty() {
        warnings.push(CompileWarning::UnusedVariables(
            singleton_vars.into_iter().collect(),
        ))
    }

    // prologue
    if !permanent_vars.is_empty() {
        instructions.push(Instruction::Allocate(StackDepth(permanent_vars.len())));
    }

    let mut seen = HashSet::<Local>::new();
    let mut head_result = compile_head(head, &permanent_vars, &mut seen);
    let mut var_mapping = head_result.var_mapping;
    instructions.append(&mut head_result.instructions);
    for goal in goals {
        let mut goal_result = compile_goal(goal, programs, &permanent_vars, &mut seen)?;
        instructions.append(&mut goal_result.instructions);
        var_mapping.append(goal_result.var_mapping);
        seen.retain(Local::is_stack); // "remember" only the permanent variables
    }

    // epilogue
    if !permanent_vars.is_empty() {
        instructions.push(Instruction::Deallocate);
        var_mapping.retain_keys(Local::is_stack);
    }

    Ok(CompileInfo {
        instructions,
        var_mapping,
        label_functor: head_result.label_functor,
        warnings,
    })
}

#[derive(Debug, PartialEq, Eq)]
pub struct CompileInfo {
    pub instructions: Vec<Instruction>,
    pub var_mapping: VarMapping,
    pub label_functor: Option<Functor>,
    pub warnings: Vec<CompileWarning>,
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
    UnsupportedSentenceType(Sentence),
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
            Self::UnsupportedSentenceType(query) => write!(
                f,
                "Cannot compile sentence `{}`. Unknown sentence format.",
                to_display(query, symbol_table)
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompileWarning {
    IgnoredEntryPoint(CodePtr),
    UnusedVariables(Vec<VarName>),
}

impl SymDisplay for CompileWarning {
    fn sym_fmt(
        &self,
        f: &mut Formatter<'_>,
        symbol_table: &crate::symbol::SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Self::IgnoredEntryPoint(ptr) => write!(
                f,
                "Program contains multiple entry points. Ignoring the one at @{ptr}."
            ),
            Self::UnusedVariables(vars) => write!(
                f,
                "Rule contains variables [{}] that are not bound to any goals.",
                to_display(&WriteVec::new(vars), symbol_table),
            ),
        }
    }
}

pub fn compile_sentence(
    sentence: Sentence,
    label_map: &HashMap<Functor, CodePtr>,
) -> Result<CompileInfo, CompileError> {
    if sentence.is_fact() {
        let fact = sentence.unwrap_fact();
        Ok(compile_fact(fact))
    } else if sentence.is_rule() {
        let (head, goals) = sentence.unwrap_rule();
        compile_rule(head, goals, label_map)
    } else if sentence.is_query() {
        let goals = sentence.unwrap_query();
        compile_query(goals, label_map)
    } else {
        Err(CompileError::UnsupportedSentenceType(sentence))
    }
}

pub fn compile_sentences(
    sentences: Vec<Sentence>,
    assembly: &mut Assembly,
) -> Result<Vec<CompileWarning>, CompileError> {
    let mut warnings = Vec::<CompileWarning>::new();
    for sentence in sentences {
        if sentence.is_fact() {
            let mut fact_info = compile_fact(sentence.unwrap_fact());
            warnings.extend(fact_info.warnings.drain(..));
            fact_info.append_to_assembly(assembly);
        } else if sentence.is_rule() {
            let (head, goals) = sentence.unwrap_rule();
            let mut rule_info = compile_rule(head, goals, &assembly.label_map)?;
            warnings.extend(rule_info.warnings.drain(..));
            rule_info.append_to_assembly(assembly);
        } else if sentence.is_query() {
            let goals = sentence.unwrap_query();
            let mut query_info = compile_query(goals, &assembly.label_map)?;
            warnings.extend(query_info.warnings.drain(..));
            let entry_point = query_info.append_to_assembly(assembly);
            if let Some(old_entry_point) = assembly.entry_point.take() {
                warnings.push(CompileWarning::IgnoredEntryPoint(old_entry_point.location))
            }
            assembly.entry_point = Some(entry_point);
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
    let rzy = Term::Struct(Struct::new(r2, &[Term::Variable(z), Term::Variable(y)]).unwrap());
    let root = Struct::new(p2, &[Term::Variable(x), rzy]).unwrap();
    let (regs, vars, f) = flatten_struct(root);
    let regs_strings = regs.iter().map(|x| format!("{x:?}")).collect::<Vec<_>>();
    assert_eq!(
        (regs_strings.iter().map(String::as_str).collect::<Vec<_>>(), vars, f),
        (vec![
            "FlattenedReg(RegPtr(0), Struct(FlatStruct(Functor(:p, 2), [RegPtr(1), RegPtr(2)]), 0))", 
            "FlattenedReg(RegPtr(1), Variable(:X))", 
            "FlattenedReg(RegPtr(2), Struct(FlatStruct(Functor(:r, 2), [RegPtr(4), RegPtr(5)]), 1))", 
            "FlattenedReg(RegPtr(3), Variable(:X))", 
            "FlattenedReg(RegPtr(4), Variable(:Z))", 
            "FlattenedReg(RegPtr(5), Variable(:Y))"],
         HashMap::from_iter([(x, RegPtr(3)), (y, RegPtr(5)), (z, RegPtr(4))]),
         p2));
}

#[test]
fn test_flatten_big_struct() {
    use crate::lang::parse_struct;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();
    let root = parse_struct("h(l(p(A, X), p(B, X)))", &mut symbol_table).unwrap();
    let h1 = Functor(symbol_table.intern("h"), 1);
    let x = symbol_table.intern("X");
    let a = symbol_table.intern("A");
    let b = symbol_table.intern("B");
    let (regs, vars, f) = flatten_struct(root);
    let regs_strings = regs.iter().map(|x| format!("{x:?}")).collect::<Vec<_>>();
    assert_eq!(
        (regs_strings.iter().map(String::as_str).collect::<Vec<_>>(), vars, f),
        (vec![
            "FlattenedReg(RegPtr(0), Struct(FlatStruct(Functor(:h, 1), [RegPtr(1)]), 0))", 
            "FlattenedReg(RegPtr(1), Struct(FlatStruct(Functor(:l, 2), [RegPtr(2), RegPtr(3)]), 1))", 
            "FlattenedReg(RegPtr(2), Struct(FlatStruct(Functor(:p, 2), [RegPtr(4), RegPtr(5)]), 2))", 
            "FlattenedReg(RegPtr(3), Struct(FlatStruct(Functor(:p, 2), [RegPtr(6), RegPtr(5)]), 2))", 
            "FlattenedReg(RegPtr(4), Variable(:A))", 
            "FlattenedReg(RegPtr(5), Variable(:X))", 
            "FlattenedReg(RegPtr(6), Variable(:B))"],
         HashMap::from_iter([(a, RegPtr(4)), (x, RegPtr(5)), (b, RegPtr(6))]),
         h1));
}

#[test]
fn test_compile_query() {
    use assembler::compile_asm;
    use lang::parse_sentence;
    use symbol::SymbolTable;
    use util::lbl_for;
    let mut symbol_table = SymbolTable::new();
    let query = parse_sentence("?- p(Z,h(Z,W),f(W)).", &mut symbol_table).unwrap();
    let labels = lbl_for(&query.goals);
    let instructions = compile_asm(
        r#"
        allocate 2
        put_variable Y1, A1
        put_structure h/2, A2
        set_value Y1
        set_variable Y2
        put_structure f/1, A3
        set_value Y2
        call @0
        publish
        deallocate
        "#,
        &mut symbol_table,
    )
    .unwrap()
    .instructions;
    assert_eq!(
        compile_query(query.goals, &labels).unwrap(),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([
                (StackPtr(1).into(), symbol_table.intern("Z")),
                (StackPtr(2).into(), symbol_table.intern("W"))
            ]),
            label_functor: None,
            warnings: vec![],
        }
    )
}

#[test]
fn test_compile_query_no_allocate() {
    use assembler::compile_asm;
    use lang::parse_sentence;
    use symbol::SymbolTable;
    use util::lbl_for;
    let mut symbol_table = SymbolTable::new();
    let query = parse_sentence("?- f(b, a).", &mut symbol_table).unwrap();
    let labels = lbl_for(&query.goals);
    let instructions = compile_asm(
        r#"
        put_structure b/0, A1
        put_structure a/0, A2
        call @0
        publish
        "#,
        &mut symbol_table,
    )
    .unwrap()
    .instructions;
    assert_eq!(
        compile_query(query.goals, &labels).unwrap(),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::default(),
            label_functor: None,
            warnings: vec![],
        }
    )
}

#[test]
fn test_compile_fact() {
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
        compile_fact(program),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4).into(), symbol_table.intern("X")),
                (RegPtr(5).into(), symbol_table.intern("Y")),
            ]),
            label_functor: Some(Functor(symbol_table.intern("p"), 3)),
            warnings: vec![],
        }
    )
}

#[test]
fn test_compile_query_line() {
    use assembler::compile_asm;
    use lang::parse_sentence;
    use symbol::SymbolTable;
    use util::lbl_for;
    let mut symbol_table = SymbolTable::new();
    // same query as this, but with shorter names:
    // horizontal(line(point(X1, Y), point(X2, Y)))
    let query = parse_sentence("?- h(l(p(A, Y), p(B, Y))).", &mut symbol_table).unwrap();
    let labels = lbl_for(&query.goals);
    let instructions = compile_asm(
        r#"
        allocate 3
        put_structure p/2, X2
        set_variable Y1
        set_variable Y2
        put_structure p/2, X3
        set_variable Y3
        set_value Y2
        put_structure l/2, A1
        set_value X2
        set_value X3
        call @0
        publish
        deallocate
        "#,
        &mut symbol_table,
    )
    .unwrap()
    .instructions;
    assert_eq!(
        compile_query(query.goals, &labels).unwrap(),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([
                (StackPtr(1).into(), symbol_table.intern("A")),
                (StackPtr(2).into(), symbol_table.intern("Y")),
                (StackPtr(3).into(), symbol_table.intern("B")),
            ]),
            label_functor: None,
            warnings: vec![],
        }
    )
}
#[test]
fn test_compile_fact_line() {
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
        compile_fact(fact),
        CompileInfo {
            instructions,
            var_mapping: VarMapping::from_iter([
                (RegPtr(4).into(), symbol_table.intern("A")),
                (RegPtr(5).into(), symbol_table.intern("Y")),
                (RegPtr(6).into(), symbol_table.intern("B")),
            ]),
            label_functor: Some(Functor(symbol_table.intern("h"), 1)),
            warnings: vec![],
        }
    )
}

#[test]
fn test_collect_variables() {
    use lang::parse_sentence;
    use symbol::SymbolTable;

    let mut symbol_table = SymbolTable::new();
    let sentence = parse_sentence("?- p(X, Y), q(X, Z), r(Z, Y).", &mut symbol_table).unwrap();
    let mut result = HashMap::<VarName, StackPtr>::default();

    for goal in sentence.goals {
        collect_variables(&goal, &mut result);
    }

    assert_eq!(
        result,
        HashMap::from([
            (symbol_table.intern("X"), StackPtr(1)),
            (symbol_table.intern("Y"), StackPtr(2)),
            (symbol_table.intern("Z"), StackPtr(3))
        ])
    );
}

#[test]
fn test_get_permanent_variables() {
    use lang::parse_sentence;
    use symbol::SymbolTable;

    let mut symbol_table = SymbolTable::new();
    let sentence = parse_sentence("p(X, Y) :- q(X, Z), r(Z, Y).", &mut symbol_table).unwrap();
    let items = get_permanent_variables(sentence.head.as_ref(), &sentence.goals);
    assert_eq!(
        items,
        (
            HashSet::default(),
            HashMap::from([
                (symbol_table.intern("Y"), StackPtr(1)),
                (symbol_table.intern("Z"), StackPtr(2))
            ])
        )
    );

    let sentence = parse_sentence("a :- b(X), c(X).", &mut symbol_table).unwrap();
    let items = get_permanent_variables(sentence.head.as_ref(), &sentence.goals);
    assert_eq!(
        items,
        (
            HashSet::default(),
            HashMap::from([(symbol_table.intern("X"), StackPtr(1))])
        )
    );

    let sentence = parse_sentence("r(A, B) :- s(B), t(B).", &mut symbol_table).unwrap();
    let items = get_permanent_variables(sentence.head.as_ref(), &sentence.goals);
    assert_eq!(
        items,
        (
            HashSet::from([symbol_table.intern("A")]),
            HashMap::from([(symbol_table.intern("B"), StackPtr(1))])
        )
    )
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
    let y = symbol_table.intern("Y");
    let z = symbol_table.intern("Z");
    let label_map = HashMap::from_iter([(q2, CodePtr(100)), (r2, CodePtr(110))]);
    assert_eq!(
        compile_rule(rule.head.unwrap(), rule.goals, &label_map),
        Ok(CompileInfo {
            instructions: expected_assembly.instructions,
            var_mapping: VarMapping::from_iter([(StackPtr(1).into(), y), (StackPtr(2).into(), z),]),
            label_functor: Some(p2),
            warnings: vec![],
        })
    )
}

#[test]
fn test_compile_multigoal_query() {
    use assembler::compile_asm;
    use lang::parse_sentence;
    use symbol::SymbolTable;
    let mut symbol_table = SymbolTable::new();

    let rule = parse_sentence("?- q(X, Z), r(Z, Y).", &mut symbol_table).unwrap();
    let expected_assembly = compile_asm(
        r#"
        allocate 3
        put_variable Y1, A1
        put_variable Y2, A2
        call @100 ; q/2
        put_value Y2, A1
        put_variable Y3, A2
        call @110 ; r/2
        publish
        deallocate
        "#,
        &mut symbol_table,
    )
    .unwrap();
    let q = symbol_table.intern("q");
    let r = symbol_table.intern("r");
    let q2 = Functor(q, 2);
    let r2 = Functor(r, 2);
    let x = symbol_table.intern("X");
    let y = symbol_table.intern("Y");
    let z = symbol_table.intern("Z");
    let label_map = HashMap::from_iter([(q2, CodePtr(100)), (r2, CodePtr(110))]);
    assert_eq!(
        compile_query(rule.goals, &label_map),
        Ok(CompileInfo {
            instructions: expected_assembly.instructions,
            var_mapping: VarMapping::from_iter([
                (StackPtr(1).into(), x),
                (StackPtr(2).into(), z),
                (StackPtr(3).into(), y),
            ]),
            label_functor: None,
            warnings: vec![],
        })
    )
}

#[test]
fn test_compile_empty_query() {
    assert_eq!(
        compile_query(vec![], &HashMap::default()),
        Ok(CompileInfo {
            instructions: vec![Instruction::Publish],
            var_mapping: VarMapping::default(),
            label_functor: None,
            warnings: vec![],
        })
    );
}
