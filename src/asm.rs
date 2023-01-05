#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Line {
    Empty,
    List(Vec<u32>),
}

impl Line {
    fn from_option(input: Option<Vec<u32>>) -> Line {
        match input {
            Some(vec) => Line::List(vec),
            None => Line::Empty,
        }
    }
}

peg::parser!(
    grammar assembly_parser() for str {
        rule number() -> u32
            = n:$(['0'..='9']+) {? n.parse().or(Err("u32")) }

        rule _() -> Line
            = [' ' | '\t' | '\r']* { Line::Empty }

        rule list() -> Vec<u32>
            = "[" l:(number() ** (_ "," _)) "]" { l }

        pub rule line() -> Line
            = _ l:list()? _ { Line::from_option(l) }

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

#[test]
fn cool() {
    let program = r#"
    [1,2,3]
    [4,5, 6]
    "#;
    assert_eq!(
        parse_program(program),
        Ok(vec![Line::List(vec![1, 2, 3]), Line::List(vec![4, 5, 6]),])
    );
}
