extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod executetests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        compile::{compile_program, compile_query},
        lang::parse_term,
        run_code,
        symbol::SymbolTable,
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
        let query = parse_term(input, &mut symbol_table).unwrap();
        let result = compile_query(query);
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
        ("a", "D"),
        ("D", "D"),
        ("f(X, g(X,a))", "f(b, Y)"),
        ("f(X, g(X,a))", "p(f(X), h(Y, f(a)), Y)"),
    })]
    fn test_program_execute(input: (&str, &str)) {
        let (query_text, program_text) = input;
        let mut symbol_table = SymbolTable::new();
        let query = parse_term(query_text, &mut symbol_table).unwrap();
        let program = parse_term(program_text, &mut symbol_table).unwrap();
        let mut machine = Machine::new();

        let query_result = compile_query(query);
        machine.set_code(&query_result.instructions);
        run_code(&mut machine).expect("machine failure");

        let program_result = compile_program(program);
        machine.set_code(&program_result.instructions);
        run_code(&mut machine).expect("machine failure");

        let input = format!("({query_text}, {program_text})");
        let output = machine.dbg(&symbol_table);
        assert_display_snapshot!(case(input, output));
    }
}
