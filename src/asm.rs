use symbol::{Symbol, SymbolTable};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Arg {
    Reg(usize),
    Func(Symbol, u32),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Command(pub String, pub Vec<Arg>);

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

        rule identifier() -> String
            = s:$(['a'..='z' | 'A'..='Z' | '_']+) { s.to_string() }

        rule arg_reg() -> Arg
            = "X" n:number() { Arg::Reg(n as usize) }

        rule arg_func(symbols : &mut SymbolTable) -> Arg
            = nm:['a'..='z']+ "/" ar:number() { Arg::Func(symbols.intern_chars(nm), ar) }

        rule arg(symbols : &mut SymbolTable) -> Arg
            = arg_reg() / arg_func(symbols)

        rule cmd(symbols : &mut SymbolTable) -> Command
            = id:identifier() _ args:(arg(symbols) ** ("," _)) { Command(id, args) }

        rule line(symbols : &mut SymbolTable) -> Line
            = _ l:cmd(symbols)? _ { l }

        pub rule lines(symbols : &mut SymbolTable) -> Vec<Line>
            = line(symbols) ** "\n"
    }
);

pub fn parse_program(
    program: &str,
    symbol_table: &mut SymbolTable,
) -> Result<Vec<Command>, String> {
    match assembly_parser::lines(program, symbol_table) {
        Ok(lines) => {
            let good_lines = lines
                .into_iter()
                .filter_map(std::convert::identity)
                .collect();
            Ok(good_lines)
        }
        Err(e) => Err(format!("{}", e)),
    }
}
