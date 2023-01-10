use std::fmt::{Display, Formatter};

pub type VarName = char;
pub type FunctorName = char;
pub type Arity = u32;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Functor(pub FunctorName, pub Arity);

impl Functor {
    pub fn name(&self) -> VarName {
        self.0
    }

    pub fn arity(&self) -> Arity {
        self.1
    }
}

impl Display for Functor {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}/{}", self.0, self.1)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Struct(Functor, Vec<Term>);

impl Struct {
    pub fn new(functor: Functor, terms: &[Term]) -> Result<Struct, String> {
        if terms.len() != functor.arity() as usize {
            return Err(format!(
                "functor {} arity does not correspond to the terms count",
                functor
            ));
        }

        return Ok(Struct(functor, terms.to_vec()));
    }

    pub fn from_name(name: FunctorName, terms: &[Term]) -> Struct {
        // unwrap is fine because we 100% know that the number of terms and arity matches
        Self::new(Functor(name, terms.len() as u32), terms).unwrap()
    }

    pub fn constant(name: FunctorName) -> Struct {
        Self::new(Functor(name, 0), &[]).unwrap()
    }

    pub fn functor(&self) -> Functor {
        self.0
    }

    pub fn terms(&self) -> &[Term] {
        self.1.as_slice()
    }

    pub fn is_const(&self) -> bool {
        self.functor().arity() == 0
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.functor().name())?;

        if !self.is_const() {
            write!(f, "({}", self.terms()[0])?;
            for term in self.terms().iter().skip(1) {
                write!(f, ", {}", term)?
            }
            write!(f, ")")?
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Variable(VarName),
    Struct(Struct),
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Variable(nm) => write!(f, "{}", nm),
            Self::Struct(s) => s.fmt(f),
        }
    }
}

peg::parser!(
    grammar prolog_parser() for str {
        rule whitespace()
            = [' ' | '\t' | '\r' | '\n']

        rule line_comment()
            = ("#" / "%") [^'\n']*

        rule block_comment()
            = "/*" (!"*/" [_])* "*/"

        rule comment()
            = line_comment() / block_comment()

        rule _()
            = whitespace()* comment()?

        rule variable() -> Term
            = n:['A'..='Z'] { Term::Variable(n) }

        rule structure() -> Term
            = n:['a'..='z'] "(" ts:(term() ** ",") ")" { Term::Struct(Struct::from_name(n, ts.as_slice())) }

        rule constant() -> Term
            = n:['a'..='z'] { Term::Struct(Struct::constant(n)) }

        pub rule term() -> Term
            = _ t:(variable() / structure() / constant()) _ { t }
    }
);

pub fn parse_term(term: &str) -> Result<Term, String> {
    prolog_parser::term(term).map_err(|e| format!("{}", e))
}

#[test]
fn struct_new_success() {
    assert!(matches!(
        Struct::new(Functor('f', 1), &[Term::Struct(Struct::constant('h'))]),
        Ok(_)
    ))
}

#[test]
fn struct_new_fail() {
    assert!(matches!(
        Struct::new(Functor('f', 2), &[Term::Struct(Struct::constant('h'))]),
        Err(_)
    ))
}
