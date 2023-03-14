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
    pub fn size(&self) -> usize {
        1
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Assembly {
    pub instructions: Vec<Instruction>,
    pub label_map: HashMap<Functor, CodePtr>,
}

impl Assembly {
    pub fn from_asm(program: &str, symbol_table: &mut SymbolTable) -> Result<Assembly, String> {
        let lines = parse_program(program, symbol_table)?;
        let mut instructions: Vec<Instruction> = vec![];
        let mut label_map: HashMap<Functor, CodePtr> = HashMap::new();

        // first assign labels to instructions
        for (i, Command(labels, _, _)) in lines.iter().enumerate() {
            for label in labels {
                label_map.insert(label_to_functor(label), CodePtr(i));
            }
        }

        for line in lines {
            instructions.push(command_to_instr(line, &label_map, symbol_table)?)
        }

        Ok(Assembly {
            instructions,
            label_map,
        })
    }
}

fn label_to_functor(lbl: &Label) -> Functor {
    Functor(lbl.0, lbl.1)
}

fn command_to_instr(
    line: Command,
    label_map: &HashMap<Functor, CodePtr>,
    symbol_table: &SymbolTable,
) -> Result<Instruction, String> {
    let arg_to_functor = |arg| match arg {
        Arg::Func(f, a) => Ok(Functor(f, a)),
        x => Err(format!(
            "Argument {} is not a register reference",
            to_display(&x, symbol_table)
        )),
    };

    let arg_to_reg = |arg| match arg {
        Arg::Reg(i) => Ok(RegPtr(i)),
        x => Err(format!(
            "Argument {} is not a register reference",
            to_display(&x, symbol_table)
        )),
    };

    let arg_to_code = |arg| match arg {
        Arg::Code(i) => Ok(CodePtr(i)),
        Arg::Func(s, a) => label_map.get(&Functor(s, a)).copied().ok_or(format!(
            "Unbound label {}/{a}",
            to_display(&s, symbol_table)
        )),
        x => Err(format!(
            "Argument {} is not a label or instruction reference",
            to_display(&x, symbol_table)
        )),
    };

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
        ("call", [c]) => Ok(Instruction::Call(arg_to_code(*c)?)),
        (nm @ "call", args) => bad_args(nm, args),
        ("proceed", []) => Ok(Instruction::Proceed),
        (nm @ "proceed", args) => bad_args(nm, args),
        (x, _) => Err(format!("Unknown command {x}")),
    }
}
