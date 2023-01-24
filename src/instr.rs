use asm::{parse_program, Arg, Command};
use data::RegPtr;
use lang::Functor;
use std::fmt::{Display, Formatter};

use crate::symbol::{SymbolTable, SymDisplay, to_display};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
    PutStructure(Functor, RegPtr),
    SetVariable(RegPtr),
    SetValue(RegPtr),
    GetStructure(Functor, RegPtr),
    UnifyVariable(RegPtr),
    UnifyValue(RegPtr),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Instruction::PutStructure(functor, reg) => {
                write!(f, "put_structure {}, {}", functor, reg)
            }
            Instruction::SetVariable(reg) => write!(f, "set_variable {}", reg),
            Instruction::SetValue(reg) => write!(f, "set_value {}", reg),
            Instruction::GetStructure(functor, reg) => {
                write!(f, "get_structure {}, {}", functor, reg)
            }
            Instruction::UnifyVariable(reg) => write!(f, "unify_variable {}", reg),
            Instruction::UnifyValue(reg) => write!(f, "unify_value {}", reg),
        }
    }
}

impl SymDisplay for Instruction {
    fn sym_fmt(&self, f: &mut Formatter<'_>, symbol_table: &SymbolTable) -> Result<(), std::fmt::Error> {
        match self {
            Instruction::PutStructure(functor, reg) => write!(f, "put_structure {}, {}", to_display(functor, symbol_table), reg),
            Instruction::GetStructure(functor, reg) => write!(f, "get_structure {}, {}", to_display(functor, symbol_table), reg),
            _ => self.fmt(f)
        }
    }
}

impl Instruction {
    pub fn from_program(
        program: &str,
        symbol_table: &mut SymbolTable,
    ) -> Result<Vec<Instruction>, String> {
        let lines = parse_program(program, symbol_table)?;
        let mut instructions: Vec<Instruction> = vec![];

        for line in lines {
            instructions.push(command_to_instr(line)?)
        }

        Ok(instructions)
    }
}

fn arg_to_functor(arg: Arg) -> Result<Functor, String> {
    match arg {
        Arg::Func(f, a) => Ok(Functor(f, a)),
        x => Err(format!("Argument {:?} is not a register reference", x)),
    }
}

fn arg_to_reg(arg: Arg) -> Result<RegPtr, String> {
    match arg {
        Arg::Reg(i) => Ok(RegPtr(i)),
        x => Err(format!("Argument {:?} is not a register reference", x)),
    }
}

fn command_to_instr(line: Command) -> Result<Instruction, String> {
    fn bad_args(nm: &str, args: &[Arg]) -> Result<Instruction, String> {
        Err(format!("Incorrect arguments for {}: {:?}", nm, args))
    }

    let Command(cmd, args) = line;

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
        (x, _) => Err(format!("Unknown command {}", x)),
    }
}
