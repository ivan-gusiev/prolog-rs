use std::{
    collections::HashMap,
    fmt::{Debug, Display, Error, Write},
};

use crate::{
    compile::CompileResult,
    data::RegPtr,
    instr::{Assembly, Instruction},
    lang::Functor,
    symbol::{to_display, SymDisplay, SymbolTable},
    var::VarBindings,
    Machine,
};

pub fn collapse<T>(result: Result<T, T>) -> T {
    match result {
        Ok(x) => x,
        Err(x) => x,
    }
}

pub fn writeout<T: Display, I: IntoIterator<Item = T>>(items: I) -> String {
    fn writeout_impl<T: Display, I: IntoIterator<Item = T>>(items: I) -> Result<String, Error> {
        let mut out = String::new();
        for (idx, item) in items.into_iter().enumerate() {
            writeln!(out, "{idx:#03}\t{item}")?;
        }
        Ok(out)
    }

    writeout_impl(items).unwrap_or_else(|e| format!("{e}"))
}

pub fn writeout_table2<T: Display, U: Display>(ts: &[T], us: &[U]) -> String {
    let items = ts.iter().zip(us.iter()).map(|(t, u)| format!("{t}\t{u}"));
    writeout(items)
}

pub fn writeout_dict<T: Display + Ord, U: Display, D>(dict: D) -> String
where
    D: IntoIterator<Item = (T, U)>,
{
    let mut items_vec = dict.into_iter().collect::<Vec<_>>();
    items_vec.sort_by(|(a, _), (b, _)| a.cmp(b));
    let items = items_vec.into_iter().map(|(t, u)| format!("{t}\t{u}"));
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
    query_bindings: &VarBindings,
    program_bindings: &VarBindings,
) -> String {
    fn write_program_result_impl(
        machine: &Machine,
        symbol_table: &SymbolTable,
        query_bindings: &VarBindings,
        program_bindings: &VarBindings,
    ) -> Result<String, Error> {
        let mut out = String::new();
        writeln!(out, "QUERY\n----")?;
        writeln!(
            out,
            "{}",
            writeout_sym(
                &machine.describe_vars(query_bindings).map_err(|_| Error)?,
                symbol_table
            )
        )?;
        writeln!(out, "PROGRAM\n____")?;
        writeln!(
            out,
            "{}",
            writeout_sym(
                &machine.describe_vars(program_bindings).map_err(|_| Error)?,
                symbol_table
            )
        )?;
        Ok(out)
    }

    write_program_result_impl(machine, symbol_table, query_bindings, program_bindings)
        .unwrap_or_else(|e| format!("{e}"))
}

pub fn writeout_annotated_mappings(
    machine: &Machine,
    query_bindings: &VarBindings,
    program_bindings: &VarBindings,
    symbol_table: &SymbolTable,
) -> String {
    fn writeout_annotated_mappings_impl(
        machine: &Machine,
        query_bindings: &VarBindings,
        program_bindings: &VarBindings,
        symbol_table: &SymbolTable,
    ) -> Result<String, Error> {
        let mut out = String::new();
        let mut vars = Vec::<String>::with_capacity(2);
        for reg_id in 1..machine.iter_reg().len() {
            let reg = RegPtr(reg_id);
            vars.clear();
            if let Some(var) = query_bindings.get(&machine.trace_reg(reg).unwrap()) {
                vars.push(format!("query.{}", var.sym_to_str(symbol_table)))
            }
            if let Some(var) = program_bindings.get(&machine.trace_reg(reg).unwrap()) {
                vars.push(format!("program.{}", var.sym_to_str(symbol_table)))
            }
            let annotations = if vars.is_empty() {
                String::new()
            } else {
                format!("// {}", vars.join(", "))
            };
            writeln!(out, "{reg}\t{annotations}")?
        }
        Ok(out)
    }

    writeout_annotated_mappings_impl(machine, query_bindings, program_bindings, symbol_table)
        .unwrap_or_else(|e| format!("{e}"))
}

pub fn writeout_compile_result(
    compile_result: &CompileResult,
    symbol_table: &SymbolTable,
) -> String {
    fn writeout_compile_result_impl(
        compile_result: &CompileResult,
        symbol_table: &SymbolTable,
    ) -> Vec<String> {
        let mut out = Vec::<String>::new();
        let mut annotations = Vec::<String>::with_capacity(2);
        let process_reg = |reg: &RegPtr, ann: &mut Vec<String>| {
            if let Some(var) = compile_result.var_mapping.get(reg) {
                ann.push(format!("{}={}", reg, to_display(&var, symbol_table)))
            }
        };
        let process_regs = |rs: &[&RegPtr], ann: &mut Vec<String>| {
            for r in rs {
                process_reg(r, ann)
            }
        };

        for instr in compile_result.instructions.iter() {
            annotations.clear();
            match instr {
                Instruction::GetStructure(_, reg)
                | Instruction::PutStructure(_, reg)
                | Instruction::UnifyValue(reg)
                | Instruction::UnifyVariable(reg)
                | Instruction::SetValue(reg)
                | Instruction::SetVariable(reg) => process_reg(reg, &mut annotations),
                Instruction::GetValue(r1, r2)
                | Instruction::GetVariable(r1, r2)
                | Instruction::PutValue(r1, r2)
                | Instruction::PutVariable(r1, r2) => process_regs(&[r1, r2], &mut annotations),
                _ => (),
            }
            let comment = if annotations.is_empty() {
                String::new()
            } else {
                format!("// {}", annotations.join(", "))
            };
            out.push(format!("{}\t{}", to_display(instr, symbol_table), comment))
        }
        out
    }

    writeout(
        writeout_compile_result_impl(compile_result, symbol_table)
            .iter()
            .map(String::as_str)
            .map(str::trim),
    )
}

pub fn writeout_assembly(assembly: &Assembly, symbol_table: &SymbolTable) -> String {
    let mut ptr_to_label = HashMap::<usize, Vec<Functor>>::with_capacity(assembly.label_map.len());
    for (lbl, ptr) in assembly.label_map.iter() {
        ptr_to_label
            .entry((*ptr).0)
            .or_insert_with(|| vec![])
            .push(*lbl);
    }

    let lines = assembly.instructions.iter().enumerate().map(|(i, instr)| {
        let labels = ptr_to_label.get(&i).map(Vec::as_slice).unwrap_or(&[]);
        let write_labels = WriteVec::new(labels)
            .with_separator(": ")
            .with_trailing_separator(true);
        format!(
            "{}{}",
            to_display(&write_labels, symbol_table),
            to_display(instr, symbol_table)
        )
    });

    writeout(lines)
}

pub struct WriteVec<'a, T> {
    data: &'a [T],
    separator: &'a str,
    opener: Option<&'a str>,
    closer: Option<&'a str>,
    trailing_separator: bool,
}

impl<'a, T> WriteVec<'a, T> {
    pub fn new(data: &'a [T]) -> Self {
        Self {
            data,
            separator: ", ",
            opener: None,
            closer: None,
            trailing_separator: false,
        }
    }

    pub fn with_separator(self, separator: &'a str) -> Self {
        Self { separator, ..self }
    }

    pub fn with_brackets(self, opener: Option<&'a str>, closer: Option<&'a str>) -> Self {
        Self {
            opener,
            closer,
            ..self
        }
    }

    pub fn with_trailing_separator(self, trailing_separator: bool) -> Self {
        Self {
            trailing_separator,
            ..self
        }
    }

    pub fn with_square_brackets(self) -> Self {
        self.with_brackets(Some("["), Some("]"))
    }
}

impl<'a, T: SymDisplay> SymDisplay for WriteVec<'a, T> {
    fn sym_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), Error> {
        if let Some(opener) = self.opener {
            write!(f, "{opener}")?;
        }
        if !self.data.is_empty() {
            write!(f, "{}", to_display(&self.data[0], symbol_table))?;
            for item in &self.data[1..] {
                write!(f, "{}{}", self.separator, to_display(item, symbol_table))?;
            }
            if self.trailing_separator {
                write!(f, "{}", self.separator)?;
            }
        }
        if let Some(closer) = self.closer {
            write!(f, "{closer}")?;
        }
        Ok(())
    }
}
