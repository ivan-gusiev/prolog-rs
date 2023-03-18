use std::collections::HashMap;

use crate::{data::CodePtr, instr::Instruction, lang::Functor, var::VarMapping};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EntryPoint {
    pub location: CodePtr,
    pub variables: VarMapping,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Assembly {
    pub instructions: Vec<Instruction>,
    pub label_map: HashMap<Functor, CodePtr>,
    pub bindings_map: HashMap<Functor, VarMapping>,
    pub entry_point: Option<EntryPoint>,
}

impl Assembly {
    pub fn new() -> Assembly {
        Default::default()
    }
}
