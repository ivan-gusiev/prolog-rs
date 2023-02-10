use std::fmt::{Debug, Display, Error, Write};

use crate::{
    compile::VarMapping,
    symbol::{to_display, SymDisplay, SymbolTable},
    Machine,
};

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

pub fn writeout_table2<T: Display, U: Display>(ts: &[T], us: &[U]) -> String {
    let items = ts
        .iter()
        .zip(us.iter())
        .map(|(t, u)| format!("{}\t{}", t, u));
    writeout(items)
}

pub fn writeout_dict<T: Display, U: Display, D>(dict: D) -> String
where
    D: IntoIterator<Item = (T, U)>,
{
    let items = dict.into_iter().map(|(t, u)| format!("{}\t{}", t, u));
    writeout(items)
}

pub fn writeout_sym<T: SymDisplay>(items: &[T], symbol_table: &SymbolTable) -> String {
    writeout(items.iter().map(|item| to_display(item, symbol_table)))
}

pub fn writeout_table2_sym<T: SymDisplay, U: SymDisplay>(
    ts: &[T],
    us: &[U],
    symbol_table: &SymbolTable,
) -> String {
    let items = ts.iter().zip(us.iter()).map(|(t, u)| {
        format!(
            "{}\t{}",
            to_display(t, symbol_table),
            to_display(u, symbol_table)
        )
    });
    writeout(items)
}

pub fn case<T: Display, U: Display>(input: T, output: U) -> String {
    format!("{input}\n-----\n{output}")
}

pub fn case_dbg<T: Debug, U: Debug>(input: T, output: U) -> String {
    format!("{input:#?}\n-----\n{output:#?}")
}

pub fn write_program_result(
    machine: &Machine,
    symbol_table: &SymbolTable,
    query_mapping: &VarMapping,
    program_mapping: &VarMapping,
) -> String {
    fn write_program_result_impl(
        machine: &Machine,
        symbol_table: &SymbolTable,
        query_mapping: &VarMapping,
        program_mapping: &VarMapping,
    ) -> Result<String, Error> {
        let mut out = String::new();
        writeln!(out, "QUERY\n----")?;
        writeln!(
            out,
            "{}",
            writeout_sym(&machine.describe_vars(query_mapping), symbol_table)
        )?;
        writeln!(out, "PROGRAM\n____")?;
        writeln!(
            out,
            "{}",
            writeout_sym(&machine.describe_vars(program_mapping), symbol_table)
        )?;
        Ok(out)
    }

    write_program_result_impl(machine, symbol_table, query_mapping, program_mapping)
        .unwrap_or_else(|e| format!("{e}"))
}
