use std::{
    collections::HashMap,
    fmt::{Debug, Display, Error, Write},
};

use crate::{
    asm::Assembly,
    compile::CompileInfo,
    data::{Addr, CodePtr, RegPtr},
    instr::Instruction,
    lang::{Functor, Struct},
    machine::{Machine, MachineError},
    symbol::{to_display, SymDisplay, SymbolTable},
    var::VarBindings,
};

pub fn collapse<T>(result: Result<T, T>) -> T {
    match result {
        Ok(x) => x,
        Err(x) => x,
    }
}

#[must_use]
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
    symbol_table: &mut SymbolTable,
    query_bindings: &VarBindings,
    maybe_program_bindings: Option<&VarBindings>,
) -> String {
    fn write_program_result_impl(
        machine: &Machine,
        symbol_table: &mut SymbolTable,
        query_bindings: &VarBindings,
        maybe_program_bindings: Option<&VarBindings>,
    ) -> Result<String, Error> {
        let mut out = String::new();
        writeln!(out, "HEAP ({})\n___", machine.heap_len())?;
        writeln!(out, "{}", writeout(machine.iter_heap().collect::<Vec<_>>()))?;
        writeln!(out, "QUERY\n----")?;
        writeln!(
            out,
            "{}",
            writeout_sym(
                &machine.describe_vars(query_bindings, symbol_table).unwrap(),
                symbol_table
            )
        )?;
        writeln!(out, "PROGRAM\n____")?;
        if let Some(program_bindings) = maybe_program_bindings {
            writeln!(
                out,
                "{}",
                writeout_sym(
                    &machine
                        .describe_vars(program_bindings, symbol_table)
                        .unwrap(),
                    symbol_table
                )
            )?;
        }
        Ok(out)
    }

    write_program_result_impl(
        machine,
        symbol_table,
        query_bindings,
        maybe_program_bindings,
    )
    .unwrap()
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
            if let Some(var) = query_bindings.get(&machine.trace_heap(reg.into()).unwrap()) {
                vars.push(format!("query.{}", var.sym_to_str(symbol_table)))
            }
            if let Some(var) = program_bindings.get(&machine.trace_heap(reg.into()).unwrap()) {
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

pub fn writeout_compile_result(compile_result: &CompileInfo, symbol_table: &SymbolTable) -> String {
    fn writeout_compile_result_impl(
        compile_result: &CompileInfo,
        symbol_table: &SymbolTable,
    ) -> Vec<String> {
        let mut out = Vec::<String>::new();
        let mut annotations = Vec::<String>::with_capacity(2);
        let process_local = |addr: &Addr, ann: &mut Vec<String>| {
            if let Some(local) = addr.get_local() {
                if let Some(var) = compile_result.var_mapping.get(&local) {
                    ann.push(format!("{}={}", local, to_display(&var, symbol_table)))
                }
            }
        };
        let process_locals = |rs: &[&Addr], ann: &mut Vec<String>| {
            for r in rs {
                process_local(r, ann)
            }
        };

        for warning in compile_result.warnings.iter() {
            out.push(format!("; WARN: {}", to_display(warning, symbol_table)));
        }

        for instr in compile_result.instructions.iter() {
            annotations.clear();
            match instr {
                Instruction::GetStructure(_, l)
                | Instruction::PutStructure(_, l)
                | Instruction::UnifyValue(l)
                | Instruction::UnifyVariable(l)
                | Instruction::SetValue(l)
                | Instruction::SetVariable(l) => process_local(l, &mut annotations),
                Instruction::GetValue(l1, l2)
                | Instruction::GetVariable(l1, l2)
                | Instruction::PutValue(l1, l2)
                | Instruction::PutVariable(l1, l2) => process_locals(&[l1, l2], &mut annotations),
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
        let vec = ptr_to_label.entry(ptr.0).or_insert_with(Vec::new);
        match vec.binary_search(lbl) {
            Ok(_) => {} // element already in vector @ `pos`
            Err(pos) => vec.insert(pos, *lbl),
        }
    }

    let entrypoint_ptr = assembly
        .entry_point
        .as_ref()
        .map(|e| e.location.0)
        .unwrap_or(usize::MAX);
    let lines = assembly.instructions.iter().enumerate().map(|(i, instr)| {
        let labels = ptr_to_label.get(&i).map(Vec::as_slice).unwrap_or(&[]);

        let write_labels = WriteVec::new(labels)
            .with_separator(": ")
            .with_trailing_separator(true);
        format!(
            "{}{}{}",
            if entrypoint_ptr == i {
                "ENTRYPOINT: "
            } else {
                ""
            },
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

pub fn run_just_query(
    machine: &mut Machine,
    instructions: &[Instruction],
) -> Result<(), MachineError> {
    machine.set_code(&[Instruction::Proceed]);
    let p = machine.append_code(instructions);
    machine.set_p(p);
    machine.set_cp(CodePtr(instructions.len()) + 1);
    machine.execute().run()
}

pub fn lbl_for(goals: &[Struct]) -> HashMap<Functor, CodePtr> {
    goals
        .iter()
        .map(|goal| (goal.functor(), CodePtr(0)))
        .collect()
}
