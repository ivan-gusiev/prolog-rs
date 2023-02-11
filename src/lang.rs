use std::fmt::{Display, Formatter};
use std::iter::once;

use symbol::{SymDisplay, Symbol};

use crate::symbol::SymbolTable;

pub type VarName = Symbol;
pub type FunctorName = Symbol;
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

impl SymDisplay for Functor {
    fn sym_fmt(
        &self,
        f: &mut Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        self.0.sym_fmt(f, symbol_table)?;
        write!(f, "/{}", self.1)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Struct(Functor, Vec<Term>);

impl Struct {
    pub fn new(functor: Functor, terms: &[Term]) -> Result<Struct, String> {
        if terms.len() != functor.arity() as usize {
            return Err(format!(
                "functor {functor} arity does not correspond to the terms count"
            ));
        }

        Ok(Struct(functor, terms.to_vec()))
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
                write!(f, ", {term}")?
            }
            write!(f, ")")?
        }

        Ok(())
    }
}

impl SymDisplay for Struct {
    fn sym_fmt(
        &self,
        f: &mut Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        self.functor().name().sym_fmt(f, symbol_table)?;

        if !self.is_const() {
            write!(f, "(")?;
            self.terms()[0].sym_fmt(f, symbol_table)?;
            for term in self.terms().iter().skip(1) {
                write!(f, ", ")?;
                term.sym_fmt(f, symbol_table)?;
            }
            write!(f, ")")?
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Term {
    Variable(VarName),
    Struct(Struct),
}

impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Variable(nm) => write!(f, "{nm}"),
            Self::Struct(s) => s.fmt(f),
        }
    }
}

impl SymDisplay for Term {
    fn sym_fmt(
        &self,
        f: &mut Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Self::Variable(nm) => nm.sym_fmt(f, symbol_table),
            Self::Struct(s) => s.sym_fmt(f, symbol_table),
        }
    }
}

impl Term {
    pub fn into_struct(self) -> Option<Struct> {
        match self {
            Self::Struct(s) => Some(s),
            _ => None,
        }
    }
}

peg::parser!(
    grammar prolog_parser() for str {
        rule whitespace()
            = quiet!{[' ' | '\t' | '\r' | '\n']}

        rule line_comment()
            = quiet!{("#" / "%") [^'\n']*} / expected!("line comment")

        rule block_comment()
            = quiet!{"/*" (!"*/" [_])* "*/"} / expected!("block comment")

        rule comment()
            = line_comment() / block_comment()

        rule rest() -> Vec<char>
            = ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*

        rule varname() -> std::iter::Chain<std::iter::Once<char>, std::vec::IntoIter<char>>
            = quiet!{ initial:['A'..='Z'] rest:rest() {once(initial).chain(rest.into_iter())}}
            / expected!("variable name")

        rule structname() -> std::iter::Chain<std::iter::Once<char>, std::vec::IntoIter<char>>
            = quiet!{ initial:['a'..='z'] rest:rest() {once(initial).chain(rest.into_iter())}}
            / expected!("structure name")

        rule _()
            = whitespace()* comment()?

        rule variable(symbols: &mut SymbolTable) -> Term
            = n:varname() { Term::Variable(symbols.intern_chars( n )) }

        rule structure(symbols: &mut SymbolTable) -> Term
            = n:structname() "(" ts:(term(symbols) ** ",") ")" { Term::Struct(Struct::from_name(symbols.intern_chars(n), ts.as_slice())) }

        rule constant(symbols: &mut SymbolTable) -> Term
            = n:structname() { Term::Struct(Struct::constant(symbols.intern_chars(n))) }

        pub rule term(symbols: &mut SymbolTable) -> Term
            = _ t:(variable(symbols) / structure(symbols) / constant(symbols)) _ { t }
    }
);

pub fn parse_term(term: &str, symbol_table: &mut SymbolTable) -> Result<Term, String> {
    prolog_parser::term(term, symbol_table).map_err(|e| format!("{e}"))
}

pub fn parse_struct(term: &str, symbol_table: &mut SymbolTable) -> Result<Struct, String> {
    parse_term(term, symbol_table)?
        .into_struct()
        .ok_or("term must be a struct.".to_string())
}

#[cfg(test)]
mod unittests {
    use std::convert::TryFrom;

    use crate::symbol::Symbol;

    use super::{Functor, Struct, Term};

    fn dirty(x: char) -> Symbol {
        Symbol::try_from(x.to_string()).unwrap()
    }

    #[test]
    fn struct_new_success() {
        assert!(matches!(
            Struct::new(
                Functor(dirty('f'), 1),
                &[Term::Struct(Struct::constant(dirty('h')))]
            ),
            Ok(_)
        ))
    }

    #[test]
    fn struct_new_fail() {
        assert!(matches!(
            Struct::new(
                Functor(dirty('f'), 2),
                &[Term::Struct(Struct::constant(dirty('h')))]
            ),
            Err(_)
        ))
    }
}
