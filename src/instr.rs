use asm::{parse_program, Arg, Line};
use data::{Functor, RegPtr};
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
    PutStructure(Functor, RegPtr),
    SetVariable(RegPtr),
    SetValue(RegPtr),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Instruction::PutStructure(functor, reg) => {
                write!(f, "put_structure {}, {}", functor, reg)
            }
            Instruction::SetVariable(reg) => write!(f, "set_variable {}", reg),
            Instruction::SetValue(reg) => write!(f, "set_value {}", reg),
        }
    }
}

impl Instruction {
    pub fn from_program(program: &str) -> Result<Vec<Instruction>, String> {
        let lines = parse_program(program)?;
        let mut instructions: Vec<Instruction> = vec![];

        for line in lines {
            instructions.push(line_to_instr(line)?)
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

fn line_to_instr(line: Line) -> Result<Instruction, String> {
    fn bad_args(nm: &str, args: &[Arg]) -> Result<Instruction, String> {
        Err(format!("Incorrect arguments for {}: {:?}", nm, args))
    }

    match line {
        Line::Cmd(cmd, args) => match (cmd.as_str(), &args[..]) {
            ("put_structure", [f, r]) => Ok(Instruction::PutStructure(
                arg_to_functor(*f)?,
                arg_to_reg(*r)?,
            )),
            (nm @ "put_structure", args) => bad_args(nm, args),
            ("set_variable", [r]) => Ok(Instruction::SetVariable(arg_to_reg(*r)?)),
            (nm @ "set_variable", args) => bad_args(nm, args),
            ("set_value", [r]) => Ok(Instruction::SetValue(arg_to_reg(*r)?)),
            (nm @ "set_value", args) => bad_args(nm, args),
            (x, _) => Err(format!("Unknown command {}", x)),
        },
        Line::Empty => Err("Cannot make an instruction from empty line".to_string()),
    }
}
