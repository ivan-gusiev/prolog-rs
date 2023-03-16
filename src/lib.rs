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
use compile::CompileInfo;
use machine::Machine;
use symbol::SymbolTable;
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
