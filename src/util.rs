use std::fmt::{Debug, Display, Error, Write};

use crate::symbol::{to_display, SymDisplay, SymbolTable};

pub fn writeout<T: Display, I: Iterator<Item = T>>(items: I) -> String {
    fn writeout_impl<T: Display, I: Iterator<Item = T>>(items: I) -> Result<String, Error> {
        let mut out = String::new();
        for (idx, item) in items.enumerate() {
            writeln!(out, "{idx:#03}\t{item}")?;
        }
        Ok(out)
    }

    writeout_impl(items).unwrap_or_else(|e| format!("{e}"))
}

pub fn writeout_sym<T: SymDisplay>(items: &[T], symbol_table: &SymbolTable) -> String {
    writeout(items.iter().map(|item| to_display(item, symbol_table)))
}

pub fn case<T: Display, U: Display>(input: T, output: U) -> String {
    format!("{input}\n-----\n{output}")
}

pub fn case_dbg<T: Debug, U: Debug>(input: T, output: U) -> String {
    format!("{input:#?}\n-----\n{output:#?}")
}
