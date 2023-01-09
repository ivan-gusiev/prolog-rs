use std::cmp::Ordering;
use std::collections::{HashMap, HashSet, VecDeque};

use instr::Instruction;
use lang::{Term, VarName};

use crate::{data::RegPtr, lang::Functor};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct TermId(usize);

impl TermId {
    fn inc(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum FlatRef {
    Term(TermId), // TODO: this shouldn't exist after flatten_term returns
    Register(RegPtr),
}

#[derive(Debug)]
struct FlatStruct(Functor, Vec<FlatRef>);

#[derive(Debug)]
enum FlattenedTerm {
    Variable(VarName),
    Struct(FlatStruct),
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

fn flatten_query(query: Term) -> Vec<FlattenedTerm> {
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
                    match subterm {
                        FlatRef::Term(id) => match term_map.get(id) {
                            Some(reg) => *subterm = FlatRef::Register(*reg),
                            None => panic!("Could not find a term for {:?}", id),
                        },
                        _ => (),
                    }
                }
            }
        }
    }

    result
}

fn ptoi(ptr: RegPtr) -> usize {
    ptr.0 - 1
}

fn itop(idx: usize) -> RegPtr {
    RegPtr(idx + 1)
}

fn order_structs(terms: &[FlattenedTerm]) -> Vec<RegPtr> {
    fn regs(term: &FlattenedTerm) -> HashSet<RegPtr> {
        let mut result = HashSet::new();
        match term {
            FlattenedTerm::Struct(FlatStruct(_, refs)) => {
                for r in refs {
                    match r {
                        FlatRef::Register(ptr) => {
                            result.insert(*ptr);
                        }
                        _ => (),
                    }
                }
            }
            _ => (),
        }
        result
    }

    let mut result = vec![];
    for (i, term) in terms.iter().enumerate() {
        let ptr = itop(i);
        match term {
            FlattenedTerm::Struct(_) => result.push(ptr),
            FlattenedTerm::Variable(_) => (),
        }
    }

    result.sort_by(|l, r| {
        let lhs = &terms[ptoi(*l)];
        let rhs = &terms[ptoi(*r)];

        let regs_lhs = regs(lhs);
        let regs_rhs = regs(rhs);

        if regs_lhs.contains(r) {
            Ordering::Greater
        } else if regs_rhs.contains(l) {
            Ordering::Less
        } else {
            Ordering::Equal
        }
    });
    result
}

pub fn compile_query(query: Term) -> Vec<Instruction> {
    let registers = flatten_query(query);
    let structs = order_structs(&registers);
    let mut seen = HashSet::<RegPtr>::new();
    let mut result = vec![];

    for struct_ptr in structs {
        let FlatStruct(f, refs) = registers[ptoi(struct_ptr)].get_str();
        result.push(Instruction::PutStructure(*f, struct_ptr));
        seen.insert(struct_ptr);
        for flat_ref in refs {
            match flat_ref {
                FlatRef::Register(ref_ptr) => {
                    if seen.contains(ref_ptr) {
                        result.push(Instruction::SetValue(*ref_ptr))
                    } else {
                        seen.insert(*ref_ptr);
                        result.push(Instruction::SetVariable(*ref_ptr))
                    }
                }
                _ => (),
            }
        }
    }

    return result;
}

#[test]
fn test_order_structs() {
    use lang::parse_term;
    let query = parse_term("p(Z,h(Z,W),f(W))").unwrap();
    let terms = flatten_query(query);

    assert_eq!(
        order_structs(terms.as_slice()).as_slice(),
        [RegPtr(3), RegPtr(4), RegPtr(1)]
    );
}

#[test]
fn test_flatten_query() {
    use lang::parse_term;
    let query = parse_term("p(Z,h(Z,W),f(W))").unwrap();

    let results = flatten_query(query)
        .into_iter()
        .map(|x| format!("{:?}", x))
        .collect::<Vec<_>>()
        .join("||");

    assert_eq!(format!("{}", results), "Struct(FlatStruct(Functor('p', 3), [Register(RegPtr(2)), Register(RegPtr(3)), Register(RegPtr(4))]))||Variable('Z')||Struct(FlatStruct(Functor('h', 2), [Register(RegPtr(2)), Register(RegPtr(5))]))||Struct(FlatStruct(Functor('f', 1), [Register(RegPtr(5))]))||Variable('W')".to_string());
}

#[test]
fn test_compile_query() {
    use lang::parse_term;
    let query = parse_term("p(Z,h(Z,W),f(W))").unwrap();
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
    )
    .unwrap();
    assert_eq!(compile_query(query), instructions)
}
