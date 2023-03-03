use asm::{parse_program, Arg, Command};
use data::RegPtr;
use lang::Functor;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use crate::{
    asm::Label,
    data::CodePtr,
    symbol::{to_display, SymDisplay, SymbolTable},
    util::WriteVec,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
    PutStructure(Functor, RegPtr),
    SetVariable(RegPtr),
    SetValue(RegPtr),
    GetStructure(Functor, RegPtr),
    UnifyVariable(RegPtr),
    UnifyValue(RegPtr),
    Call(CodePtr),
    Proceed,
    PutVariable(RegPtr, RegPtr),
    PutValue(RegPtr, RegPtr),
    GetVariable(RegPtr, RegPtr),
    GetValue(RegPtr, RegPtr),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Instruction::PutStructure(functor, reg) => {
                write!(f, "put_structure {functor}, {reg}")
            }
            Instruction::SetVariable(reg) => write!(f, "set_variable {reg}"),
            Instruction::SetValue(reg) => write!(f, "set_value {reg}"),
            Instruction::GetStructure(functor, reg) => {
                write!(f, "get_structure {functor}, {reg}")
            }
            Instruction::UnifyVariable(reg) => write!(f, "unify_variable {reg}"),
            Instruction::UnifyValue(reg) => write!(f, "unify_value {reg}"),
            Instruction::Call(code) => write!(f, "call @{}", usize::from(*code)),
            Instruction::Proceed => write!(f, "proceed"),
            Instruction::PutVariable(x, a) => write!(f, "put_variable {x}, {a}"),
            Instruction::PutValue(x, a) => write!(f, "put_value {x}, {a}"),
            Instruction::GetVariable(x, a) => write!(f, "get_variable {x}, {a}"),
            Instruction::GetValue(x, a) => write!(f, "get_value {x}, {a}"),
        }
    }
}

impl SymDisplay for Instruction {
    fn sym_fmt(
        &self,
        f: &mut Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Instruction::PutStructure(functor, reg) => write!(
                f,
                "put_structure {}, {}",
                to_display(functor, symbol_table),
                reg
            ),
            Instruction::GetStructure(functor, reg) => write!(
                f,
                "get_structure {}, {}",
                to_display(functor, symbol_table),
                reg
            ),
            _ => self.fmt(f),
        }
    }
}

impl Instruction {
    pub fn from_assembly(
        program: &str,
        symbol_table: &mut SymbolTable,
    ) -> Result<Vec<Instruction>, String> {
        let lines = parse_program(program, symbol_table)?;
        let mut instructions: Vec<Instruction> = vec![];
        let mut label_map: HashMap<Label, CodePtr> = HashMap::new();

        // first assign labels to instructions
        for (i, Command(labels, _, _)) in lines.iter().enumerate() {
            for label in labels {
                label_map.insert(*label, CodePtr(i));
            }
        }

        for line in lines {
            instructions.push(command_to_instr(line, &label_map, symbol_table)?)
        }

        Ok(instructions)
    }
}

fn arg_to_functor(arg: Arg) -> Result<Functor, String> {
    match arg {
        Arg::Func(f, a) => Ok(Functor(f, a)),
        x => Err(format!("Argument {x:?} is not a register reference")),
    }
}

fn arg_to_reg(arg: Arg) -> Result<RegPtr, String> {
    match arg {
        Arg::Reg(i) => Ok(RegPtr(i)),
        x => Err(format!("Argument {x:?} is not a register reference")),
    }
}

fn arg_to_code(
    arg: Arg,
    label_map: &HashMap<Label, CodePtr>,
    symbol_table: &SymbolTable,
) -> Result<CodePtr, String> {
    match arg {
        Arg::Code(i) => Ok(CodePtr(i)),
        Arg::Func(s, a) => label_map.get(&Label(s, a)).copied().ok_or(format!(
            "Unbound label {}/{a}",
            to_display(&s, symbol_table)
        )),
        x => Err(format!(
            "Argument {} is not a label or instruction reference",
            to_display(&x, symbol_table)
        )),
    }
}

fn command_to_instr(
    line: Command,
    label_map: &HashMap<Label, CodePtr>,
    symbol_table: &SymbolTable,
) -> Result<Instruction, String> {
    let bad_args = |nm: &str, args: &[Arg]| {
        Err(format!(
            "Incorrect arguments for {nm}: {}",
            to_display(&WriteVec::new(args), symbol_table)
        ))
    };

    let Command(_, cmd, args) = line;

    match (cmd.as_str(), &args[..]) {
        ("put_structure", [f, r]) => Ok(Instruction::PutStructure(
            arg_to_functor(*f)?,
            arg_to_reg(*r)?,
        )),
        (nm @ "put_structure", args) => bad_args(nm, args),
        ("set_variable", [r]) => Ok(Instruction::SetVariable(arg_to_reg(*r)?)),
        (nm @ "set_variable", args) => bad_args(nm, args),
        ("set_value", [r]) => Ok(Instruction::SetValue(arg_to_reg(*r)?)),
        (nm @ "set_value", args) => bad_args(nm, args),
        ("get_structure", [f, r]) => Ok(Instruction::GetStructure(
            arg_to_functor(*f)?,
            arg_to_reg(*r)?,
        )),
        (nm @ "get_structure", args) => bad_args(nm, args),
        ("unify_variable", [r]) => Ok(Instruction::UnifyVariable(arg_to_reg(*r)?)),
        (nm @ "unify_variable", args) => bad_args(nm, args),
        ("unify_value", [r]) => Ok(Instruction::UnifyValue(arg_to_reg(*r)?)),
        (nm @ "unify_value", args) => bad_args(nm, args),
        ("put_variable", [x, a]) => Ok(Instruction::PutVariable(arg_to_reg(*x)?, arg_to_reg(*a)?)),
        (nm @ "put_variable", args) => bad_args(nm, args),
        ("put_value", [x, a]) => Ok(Instruction::PutValue(arg_to_reg(*x)?, arg_to_reg(*a)?)),
        (nm @ "put_value", args) => bad_args(nm, args),
        ("get_variable", [x, a]) => Ok(Instruction::GetVariable(arg_to_reg(*x)?, arg_to_reg(*a)?)),
        (nm @ "get_variable", args) => bad_args(nm, args),
        ("get_value", [x, a]) => Ok(Instruction::GetValue(arg_to_reg(*x)?, arg_to_reg(*a)?)),
        (nm @ "get_value", args) => bad_args(nm, args),
        ("call", [c]) => Ok(Instruction::Call(arg_to_code(*c, label_map, symbol_table)?)),
        (nm @ "call", args) => bad_args(nm, args),
        ("proceed", []) => Ok(Instruction::Proceed),
        (nm @ "proceed", args) => bad_args(nm, args),
        (x, _) => Err(format!("Unknown command {x}")),
    }
}
