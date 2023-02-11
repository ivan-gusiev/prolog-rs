use asm::{parse_program, Arg, Command};
use data::RegPtr;
use lang::Functor;
use std::fmt::{Display, Formatter};

use crate::{
    data::CodePtr,
    symbol::{to_display, SymDisplay, SymbolTable},
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

        for line in lines {
            instructions.push(command_to_instr(line)?)
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

fn arg_to_code(arg: Arg) -> Result<CodePtr, String> {
    match arg {
        Arg::Code(i) => Ok(CodePtr(i)),
        x => Err(format!("Argument {x:?} is not a register reference")),
    }
}

fn command_to_instr(line: Command) -> Result<Instruction, String> {
    fn bad_args(nm: &str, args: &[Arg]) -> Result<Instruction, String> {
        Err(format!("Incorrect arguments for {nm}: {args:?}"))
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
        ("put_variable", [x, a]) => Ok(Instruction::PutVariable(arg_to_reg(*x)?, arg_to_reg(*a)?)),
        ("put_value", [x, a]) => Ok(Instruction::PutValue(arg_to_reg(*x)?, arg_to_reg(*a)?)),
        ("get_variable", [x, a]) => Ok(Instruction::GetVariable(arg_to_reg(*x)?, arg_to_reg(*a)?)),
        ("get_value", [x, a]) => Ok(Instruction::GetValue(arg_to_reg(*x)?, arg_to_reg(*a)?)),
        ("call", [c]) => Ok(Instruction::Call(arg_to_code(*c)?)),
        ("proceed", []) => Ok(Instruction::Proceed),
        (x, _) => Err(format!("Unknown command {x}")),
    }
}
