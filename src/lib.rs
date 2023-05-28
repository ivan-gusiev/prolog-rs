pub mod asm;
pub mod assembler;
pub mod compile;
pub mod data;
pub mod decompile;
pub mod instr;
pub mod lang;
pub mod machine;
pub mod symbol;
pub mod util;
pub mod var;

use asm::Assembly;
use compile::{compile_sentences, CompileError, CompileInfo};
use lang::{Sentence, Struct};
use machine::{Machine, MachineError};
use symbol::{SymDisplay, SymbolTable};
use var::{VarBindings, VarMapping};

#[derive(Debug, Default)]
pub struct PrologApp {
    pub machine: Machine,
    pub symbol_table: SymbolTable,
    pub assembly: Assembly,
    pub query: Option<CompileInfo>,
    pub program: Option<VarMapping>,
    pub query_variables: VarBindings,
    pub program_variables: VarBindings,
    pub immediate_execution: bool,
}

impl PrologApp {
    pub fn ready_to_run(&self) -> bool {
        self.query.is_some()
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum PrologError {
    Compile(CompileError),
    Machine(MachineError),
}

impl SymDisplay for PrologError {
    fn sym_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Self::Compile(ce) => ce.sym_fmt(f, symbol_table),
            Self::Machine(me) => write!(f, "{}", me.message()),
        }
    }
}

impl From<CompileError> for PrologError {
    fn from(value: CompileError) -> Self {
        PrologError::Compile(value)
    }
}

impl From<MachineError> for PrologError {
    fn from(value: MachineError) -> Self {
        PrologError::Machine(value)
    }
}

#[derive(Debug, Default)]
pub struct Solution {
    pub machine: Machine,
    pub assembly: Assembly,
    pub query_bindings: VarBindings,
    pub program_bindings: VarBindings,
}

pub fn l1_solve(program: Struct, query: Struct) -> Result<Solution, PrologError> {
    let mut solution = Solution::default();

    let sentences = vec![Sentence::fact(program), Sentence::query(vec![query])];
    solution.assembly = {
        let mut a = Assembly::new();
        compile_sentences(sentences, &mut a)?;
        a
    };
    let query_mapping = solution
        .assembly
        .entry_point
        .as_ref()
        .unwrap()
        .variables
        .clone();
    let program_mapping = solution
        .assembly
        .bindings_map
        .iter()
        .next()
        .unwrap()
        .1
        .clone();
    let mut query_bindings = VarBindings::default();
    solution.machine = Machine::new();
    solution.machine.load_assembly(&solution.assembly);
    solution
        .machine
        .execute()
        .with_call_hook(|machine| {
            query_bindings = machine.bind_variables(&query_mapping)?;
            Ok(())
        })
        .run()?;
    solution.query_bindings = query_bindings;
    solution.program_bindings = solution.machine.bind_variables(&program_mapping)?;

    Ok(solution)
}

pub fn l2_solve(mut program: Vec<Sentence>, query: Sentence) -> Result<Solution, PrologError> {
    let mut solution = Solution::default();

    program.push(query);
    solution.assembly = {
        let mut a = Assembly::new();
        compile_sentences(program, &mut a)?;
        a
    };
    let query_mapping = solution
        .assembly
        .entry_point
        .as_ref()
        .unwrap()
        .variables
        .clone();
    let program_mapping = solution
        .assembly
        .bindings_map
        .iter()
        .next()
        .unwrap()
        .1
        .clone();
    solution.machine = Machine::new();
    solution.machine.load_assembly(&solution.assembly);
    solution.machine.execute().run()?;
    solution.query_bindings = solution.machine.bind_variables(&query_mapping)?;
    solution.program_bindings = solution.machine.bind_variables(&program_mapping)?;

    Ok(solution)
}
