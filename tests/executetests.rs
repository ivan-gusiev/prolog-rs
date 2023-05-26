extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod executetests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        compile::compile_query,
        l1_solve, l2_solve,
        lang::{parse_program, parse_sentence, parse_struct},
        machine::Machine,
        symbol::SymbolTable,
        util::{case, lbl_for, run_just_query, writeout_sym},
    };

    #[parameterized(input = {
        "?- p(Z, h(Z,W), f(W)).",
        "?- f(X, g(X,a)).",
        "?- f(b, Y).",
        "?- a.",
    })]
    fn test_query_execute(input: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_sentence(input, &mut symbol_table).unwrap();

        let labels = lbl_for(&query.goals);
        let query_result = compile_query(query.goals, &labels).unwrap();
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
        ("h(l(p(A, Y), p(B, Y)))", "h(l(p(u, v), p(w, H)))"),
    })]
    fn test_program_execute_l1(input: (&str, &str)) {
        let (query_text, program_text) = input;
        let mut symbol_table = SymbolTable::new();
        let query = parse_struct(query_text, &mut symbol_table).unwrap();
        let program = parse_struct(program_text, &mut symbol_table).unwrap();
        let solution = l1_solve(program, query).unwrap();
        let query_bindings = solution.query_bindings;
        let program_bindings = solution.program_bindings;

        let input = format!("({query_text}, {program_text})");
        let output = if solution.machine.get_fail() {
            solution.machine.dbg(&symbol_table)
        } else {
            format!(
                "{}\n{}\n{}",
                solution.machine.dbg(&symbol_table),
                writeout_sym(
                    &solution
                        .machine
                        .describe_vars(&query_bindings, &mut symbol_table)
                        .unwrap(),
                    &symbol_table
                ),
                writeout_sym(
                    &solution
                        .machine
                        .describe_vars(&program_bindings, &mut symbol_table)
                        .unwrap(),
                    &symbol_table
                )
            )
        };
        assert_display_snapshot!(case(input, output));
    }

    #[parameterized(input = {
        ("?- f(b, Y).", "f(X, g(X,a))."),
        ("?- p(X, Y), q(X, Z).", "p(a, b). q(a, d)."),
    })]
    fn test_program_execute_l2(input: (&str, &str)) {
        let (query_text, program_text) = input;
        let mut symbol_table = SymbolTable::new();
        let query = parse_sentence(query_text, &mut symbol_table).unwrap();
        let program = parse_program(program_text, &mut symbol_table).unwrap();
        let solution = l2_solve(program, query).unwrap();
        let query_bindings = solution.query_bindings;
        let program_bindings = solution.program_bindings;

        let input = format!("({query_text}, {program_text})");
        let output = if solution.machine.get_fail() {
            solution.machine.dbg(&symbol_table)
        } else {
            format!(
                "{}\n{}\n{}",
                solution.machine.dbg(&symbol_table),
                writeout_sym(
                    &solution
                        .machine
                        .describe_vars(&query_bindings, &mut symbol_table)
                        .unwrap(),
                    &symbol_table
                ),
                writeout_sym(
                    &solution
                        .machine
                        .describe_vars(&program_bindings, &mut symbol_table)
                        .unwrap(),
                    &symbol_table
                )
            )
        };
        assert_display_snapshot!(case(input, output));
    }
}
