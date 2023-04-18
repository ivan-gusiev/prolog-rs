use std::{collections::HashMap, iter::once};

use asm::Assembly;
use data::{Addr, CodePtr, RegPtr, StackDepth, StackPtr};
use instr::Instruction;
use lang::Functor;
use symbol::{to_display, SymDisplay, Symbol, SymbolTable};
use util::WriteVec;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Arg {
    Reg(usize),
    Func(Symbol, u32),
    Code(usize),
    Stack(usize),
    Num(usize),
}

impl SymDisplay for Arg {
    fn sym_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Arg::Reg(i) => write!(f, "X{i}"),
            Arg::Func(s, a) => write!(f, "{}/{a}", to_display(s, symbol_table)),
            Arg::Code(i) => write!(f, "@{i}"),
            Arg::Stack(i) => write!(f, "Y{i}"),
            Arg::Num(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Label(pub Symbol, pub u32);

type Labels = Vec<Label>;

impl SymDisplay for Label {
    fn sym_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "{}/{}", to_display(&self.0, symbol_table), self.1)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Command(pub Vec<Label>, pub String, pub Vec<Arg>);

impl SymDisplay for Command {
    fn sym_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        for label in self.0.iter() {
            write!(f, "{}: ", to_display(label, symbol_table))?;
        }
        write!(f, "{}", self.1)?;
        if !self.2.is_empty() {
            write!(f, " {}", to_display(&self.2[0], symbol_table))?;
        }
        for arg in &self.2[1..] {
            write!(f, ", {}", to_display(arg, symbol_table))?;
        }
        Ok(())
    }
}

type Line = Option<Command>;

peg::parser!(
    grammar assembly_parser() for str {
        rule number() -> u32
            = n:$(['0'..='9']+) {? n.parse().or(Err("u32")) }

        rule whitespace()
            = [' ' | '\t' | '\r']

        rule comment()
            = ("#" / ";" / "%") [^'\n']*

        rule _()
            = whitespace()* comment()?

        rule rest() -> Vec<char>
            = ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*

        rule identifier() -> String
            = first:(['a'..='z' | 'A'..='Z' | '_']) rest:rest() { once(first).chain(rest).collect() }

        rule label(symbols: &mut SymbolTable) -> Label
            = nm:identifier() "/" ar:number() { Label(symbols.intern(nm), ar) }

        rule xlabels(symbols: &mut SymbolTable) -> Labels
            = (l:label(symbols) _ ":" _ { l })*

        rule consume_labels(labels: &mut Labels, symbols : &mut SymbolTable) -> ()
            = l:xlabels(symbols) { labels.extend_from_slice(&l[..]) }

        rule arg_reg() -> Arg
            = ['X' | 'A'] n:number() { Arg::Reg(n as usize) }

        rule arg_func(symbols : &mut SymbolTable) -> Arg
            = nm:identifier() "/" ar:number() { Arg::Func(symbols.intern(nm), ar) }

        rule arg_coderef() -> Arg
            = "@" n:number() { Arg::Code(n as usize) }

        rule arg_stack() -> Arg
            = "Y" n:number() { Arg::Stack(n as usize) }

        rule arg_num() -> Arg
            = n:number() { Arg::Num(n as usize) }

        rule arg(symbols : &mut SymbolTable) -> Arg
            = arg_reg() / arg_func(symbols) / arg_coderef() / arg_stack() / arg_num()

        rule cmd(labels: &mut Labels, symbols : &mut SymbolTable) -> Command
            = id:identifier() _ args:(arg(symbols) ** ("," _)) {
                let items = labels.to_vec();
                labels.clear();
                Command(items, id, args)
            }

        rule line(labels: &mut Labels, symbols : &mut SymbolTable) -> Line
            = _ consume_labels(labels, symbols) _ l:cmd(labels, symbols)? _ { l }

        pub rule lines(labels: &mut Labels, symbols : &mut SymbolTable) -> Vec<Line>
            = line(labels, symbols) ** "\n"
    }
);

pub fn parse_asm(program: &str, symbol_table: &mut SymbolTable) -> Result<Vec<Command>, String> {
    let mut labels = Labels::with_capacity(2);
    match assembly_parser::lines(program, &mut labels, symbol_table) {
        Ok(lines) => {
            let good_lines = lines.into_iter().flatten().collect();
            Ok(good_lines)
        }
        Err(e) => Err(format!("{e}")),
    }
}

pub fn compile_asm(program: &str, symbol_table: &mut SymbolTable) -> Result<Assembly, String> {
    let lines = parse_asm(program, symbol_table)?;
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
        bindings_map: Default::default(),
        entry_point: None,
    })
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
            "Argument {} is not a functor",
            to_display(&x, symbol_table)
        )),
    };

    let arg_to_reg = |arg| match arg {
        Arg::Reg(i) => Ok(Addr::Reg(RegPtr(i))),
        Arg::Stack(i) => Ok(Addr::Stack(StackPtr(i))),
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

    let arg_to_stack_depth = |arg| match arg {
        Arg::Num(i) => Ok(StackDepth(i)),
        x => Err(format!(
            "Argument {} is not a number for stack depth",
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
        ("allocate", [d]) => Ok(Instruction::Allocate(arg_to_stack_depth(*d)?)),
        (nm @ "allocate", args) => bad_args(nm, args),
        ("proceed", []) => Ok(Instruction::Proceed),
        (nm @ "proceed", args) => bad_args(nm, args),
        ("deallocate", []) => Ok(Instruction::Deallocate),
        (nm @ "deallocate", args) => bad_args(nm, args),
        (x, _) => Err(format!("Unknown command {x}")),
    }
}

#[test]
fn test_program_does_parse() {
    use std::convert::TryFrom;

    const PROGRAM: &str = r#"
   p/3:
   x/0: put_structure h/2, X3
        call @3
        "#;

    let h = Symbol::try_from("h").unwrap();
    let p = Symbol::try_from("p").unwrap();
    let x = Symbol::try_from("x").unwrap();

    assert_eq!(
        parse_asm(PROGRAM, &mut (SymbolTable::new())),
        Ok(vec![
            Command(
                vec![Label(p, 3), Label(x, 0)],
                "put_structure".to_string(),
                vec![Arg::Func(h, 2), Arg::Reg(3)]
            ),
            Command(vec![], "call".to_string(), vec![Arg::Code(3)]),
        ])
    )
}

#[test]
fn test_incorrect_program_does_not_parse() {
    assert!(matches!(parse_asm("42", &mut (SymbolTable::new())), Err(_)))
}

#[test]
fn test_command_display() {
    let text = "test/0: test/1: put_structure f/1, X1";
    let mut symbol_table = SymbolTable::new();
    let first_command = parse_asm(text, &mut symbol_table).unwrap().remove(0);
    assert_eq!(first_command.sym_to_str(&symbol_table), text.to_string())
}
