extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod executetests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        compile::{compile_program, compile_query},
        data::CodePtr,
        lang::parse_struct,
        symbol::SymbolTable,
        util::{case, lbl_for, run_just_query, writeout_sym},
        Machine, var::{VarInfo, VarBindings},
    };

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))",
        "f(b, Y)",
        "a"
    })]
    fn test_query_execute(input: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_struct(input, &mut symbol_table).unwrap();

        let labels = lbl_for(query.functor());
        let query_result = compile_query(query, &labels).unwrap();
        let mut machine = Machine::new();
        run_just_query(&mut machine, &query_result.instructions).expect("machine failure");

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
        ("h(l(p(A, Y), p(B, Y)))", "h(l(p(u, v), p(w, H)))"),
    })]
    fn test_program_execute(input: (&str, &str)) {
        let (query_text, program_text) = input;
        let mut symbol_table = SymbolTable::new();
        let query = parse_struct(query_text, &mut symbol_table).unwrap();
        let program = parse_struct(program_text, &mut symbol_table).unwrap();
        let mut machine = Machine::new();

        let labels = lbl_for(query.functor());
        let query_result = compile_query(query, &labels).unwrap();
        let mut query_bindings = VarBindings::default();

        let program_result = compile_program(program);
        machine.set_code(&program_result.instructions);
        let p = machine.append_code(&query_result.instructions);
        machine.set_p(p);
        machine.execute().with_call_hook(|m| {
            m.bind_variables(&query_result.var_mapping).map(|vars| query_bindings = vars)
        }) .run().expect("machine failure");

        let input = format!("({query_text}, {program_text})");
        let output = if machine.get_fail() {
            machine.dbg(&symbol_table)
        } else {
            let program_bindings = machine
                .bind_variables(&program_result.var_mapping)
                .expect("decompile failure");

            format!(
                "{}\n{}\n{}",
                machine.dbg(&symbol_table),
                writeout_sym(
                    &machine.describe_vars(&query_bindings).unwrap(),
                    &symbol_table
                ),
                writeout_sym(
                    &machine.describe_vars(&program_bindings).unwrap(),
                    &symbol_table
                )
            )
        };
        assert_display_snapshot!(case(input, output));
    }
}
