use data::{Addr, CodePtr};
use lang::Functor;
use std::fmt::{Display, Formatter};
use symbol::{to_display, SymDisplay, SymbolTable};

use crate::data::StackDepth;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
    PutStructure(Functor, Addr),
    SetVariable(Addr),
    SetValue(Addr),
    GetStructure(Functor, Addr),
    UnifyVariable(Addr),
    UnifyValue(Addr),
    Call(CodePtr),
    Proceed,
    PutVariable(Addr, Addr),
    PutValue(Addr, Addr),
    GetVariable(Addr, Addr),
    GetValue(Addr, Addr),
    Allocate(StackDepth),
    Deallocate,
    Publish,
    Fail,
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
            Instruction::Allocate(d) => write!(f, "allocate {d}"),
            Instruction::Deallocate => write!(f, "deallocate"),
            Instruction::Publish => write!(f, "publish"),
            Instruction::Fail => write!(f, "fail"),
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
