use std::iter::once;

use symbol::{to_display, SymDisplay, Symbol, SymbolTable};

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Arg {
    Reg(usize),
    Func(Symbol, u32),
    Code(usize),
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

        rule arg(symbols : &mut SymbolTable) -> Arg
            = arg_reg() / arg_func(symbols) / arg_coderef()

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

pub fn parse_asm(
    program: &str,
    symbol_table: &mut SymbolTable,
) -> Result<Vec<Command>, String> {
    let mut labels = Labels::with_capacity(2);
    match assembly_parser::lines(program, &mut labels, symbol_table) {
        Ok(lines) => {
            let good_lines = lines.into_iter().flatten().collect();
            Ok(good_lines)
        }
        Err(e) => Err(format!("{e}")),
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
    assert!(matches!(
        parse_asm("42", &mut (SymbolTable::new())),
        Err(_)
    ))
}

#[test]
fn test_command_display() {
    let text = "test/0: test/1: put_structure f/1, X1";
    let mut symbol_table = SymbolTable::new();
    let first_command = parse_asm(text, &mut symbol_table).unwrap().remove(0);
    assert_eq!(first_command.sym_to_str(&symbol_table), text.to_string())
}
