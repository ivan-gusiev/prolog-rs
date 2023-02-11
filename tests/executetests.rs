extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod executetests {
    use std::collections::HashMap;
    use std::fmt::Write;

    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        compile::{compile_program_l1, compile_query_l1, VarMapping},
        data::{CodePtr, RegPtr},
        instr::Instruction,
        lang::{parse_struct, Functor, VarName},
        run_code,
        symbol::{to_display, SymbolTable},
        util::{case, writeout_sym},
        Machine,
    };

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        "f(X, g(X,a))",
        "f(b, Y)",
        "a"
    })]
    fn test_query_execute(input: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_struct(input, &mut symbol_table).unwrap();
        let result = compile_query_l1(query);
        let mut machine = Machine::new();
        machine.set_code(&result.instructions);
        run_code(&mut machine).expect("machine failure");

        let output = writeout_sym(
            &machine.iter_heap().copied().collect::<Vec<_>>(),
            &symbol_table,
        );
        assert_display_snapshot!(case(input, output));
    }

    #[parameterized(input = {
        ("p(Z, h(Z,W), f(W))", "p(f(X), h(Y, f(a)), Y)"),
        ("f(b, Y)", "f(X, g(X,a))"),
        ("f(X, g(X,a))", "p(f(X), h(Y, f(a)), Y)"),
    })]
    fn test_program_execute(input: (&str, &str)) {
        let (query_text, program_text) = input;
        let mut symbol_table = SymbolTable::new();
        let query = parse_struct(query_text, &mut symbol_table).unwrap();
        let program = parse_struct(program_text, &mut symbol_table).unwrap();
        let mut machine = Machine::new();

        let query_result = compile_query_l1(query);
        machine.set_code(&query_result.instructions);
        run_code(&mut machine).expect("machine failure");

        let program_result = compile_program_l1(program);
        machine.set_code(&program_result.instructions);
        run_code(&mut machine).expect("machine failure");

        let input = format!("({query_text}, {program_text})");
        let output = if machine.get_fail() {
            machine.dbg(&symbol_table)
        } else {
            format!(
                "{}\n{}\n{}",
                machine.dbg(&symbol_table),
                writeout_sym(
                    &machine.describe_vars(&query_result.var_mapping),
                    &symbol_table
                ),
                writeout_sym(
                    &machine.describe_vars(&program_result.var_mapping),
                    &symbol_table
                )
            )
        };
        assert_display_snapshot!(case(input, output));
    }

    #[test]
    fn test_l1_query() {
        let mut symbol_table = SymbolTable::new();
        let h2 = Functor(symbol_table.intern("h"), 2);
        let f1 = Functor(symbol_table.intern("f"), 1);

        let a1 = RegPtr(1);
        let a2 = RegPtr(2);
        let a3 = RegPtr(3);
        let x4 = RegPtr(4);
        let x5 = RegPtr(5);

        let code = vec![
            Instruction::PutVariable(x4, a1),
            Instruction::PutStructure(h2, a2),
            Instruction::SetValue(x4),
            Instruction::SetVariable(x5),
            Instruction::PutStructure(f1, a3),
            Instruction::SetValue(x5),
            Instruction::Call(CodePtr(0)),
        ];

        let mut machine = Machine::new();
        machine.set_code(&code);
        run_code(&mut machine).expect("machine failure");

        let heap = machine.iter_heap().copied().collect::<Vec<_>>();

        let heap_writeout = writeout_sym(&heap, &symbol_table);
        assert_display_snapshot!(heap_writeout);
    }

    #[test]
    fn test_l1_program_query() {
        let mut symbol_table = SymbolTable::new();
        let h2 = Functor(symbol_table.intern("h"), 2);
        let f1 = Functor(symbol_table.intern("f"), 1);
        let a0 = Functor(symbol_table.intern("a"), 0);

        let a1 = RegPtr(1);
        let a2 = RegPtr(2);
        let a3 = RegPtr(3);
        let x4 = RegPtr(4);
        let x5 = RegPtr(5);
        let x6 = RegPtr(6);
        let x7 = RegPtr(7);

        let query_code = vec![
            Instruction::PutVariable(x4, a1),
            Instruction::PutStructure(h2, a2),
            Instruction::SetValue(x4),
            Instruction::SetVariable(x5),
            Instruction::PutStructure(f1, a3),
            Instruction::SetValue(x5),
            Instruction::Call(CodePtr(0)),
        ];

        let program_code = vec![
            Instruction::GetStructure(f1, a1),
            Instruction::UnifyVariable(x4),
            Instruction::GetStructure(h2, a2),
            Instruction::UnifyVariable(x5),
            Instruction::UnifyVariable(x6),
            Instruction::GetValue(x5, a3),
            Instruction::GetStructure(f1, x6),
            Instruction::UnifyVariable(x7),
            Instruction::GetStructure(a0, x7),
        ];

        let mut machine = Machine::new();

        machine.set_code(&query_code);
        run_code(&mut machine).expect("machine failure");

        machine.set_code(&program_code);
        run_code(&mut machine).expect("machine failure");

        let mut mk_sym = |s: &str| symbol_table.intern(s);

        let vars = HashMap::<VarName, RegPtr>::from([
            (mk_sym("a1"), a1),
            (mk_sym("a2"), a2),
            (mk_sym("a3"), a3),
            (mk_sym("x4"), x4),
            (mk_sym("x5"), x5),
            (mk_sym("x6"), x6),
            (mk_sym("x7"), x7),
        ]);
        let var_mapping = VarMapping::from(vars);
        let mut output = String::new();
        for i in 1..7 {
            if let Some(term) = machine.decompile(RegPtr(i).into(), &var_mapping) {
                writeln!(output, "reg({}) = {}", i, to_display(&term, &symbol_table)).unwrap();
            }
        }

        assert_display_snapshot!(output);
    }
}
