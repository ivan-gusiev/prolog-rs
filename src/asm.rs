#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Arg {
    Reg(usize),
    Func(char, u32),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Line {
    Empty,
    Cmd(String, Vec<Arg>),
}

impl Line {
    fn flatten(input: Option<Line>) -> Line {
        match input {
            Some(x) => x,
            None => Line::Empty,
        }
    }
}

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

        rule arg_func() -> Arg
            = nm:['a'..='z'] "/" ar:number() { Arg::Func(nm, ar) }

        rule arg() -> Arg
            = arg_reg() / arg_func()

        rule cmd() -> Line
            = id:identifier() _ args:(arg() ** ("," _)) { Line::Cmd(id, args) }

        pub rule line() -> Line
            = _ l:cmd()? _ { Line::flatten(l) }

        pub rule lines() -> Vec<Line>
            = line() ** "\n"
    }
);

pub fn parse_program(program: &str) -> Result<Vec<Line>, String> {
    match assembly_parser::lines(program) {
        Ok(lines) => {
            let good_lines = lines
                .into_iter()
                .filter(|l| !matches!(*l, Line::Empty))
                .collect();

            Ok(good_lines)
        }
        Err(e) => Err(format!("{}", e)),
    }
}
