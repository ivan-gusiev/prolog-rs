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
        util::{case, writeout},
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
        let query = parse_term(input).unwrap();
        let instructions = compile_query(query);
        let mut machine = Machine::new();
        machine.set_code(&instructions);
        run_code(&mut machine).expect("machine failure");

        let output = writeout(&machine.iter_heap().collect::<Vec<_>>());
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
        let query = parse_term(query_text).unwrap();
        let program = parse_term(program_text).unwrap();
        let mut machine = Machine::new();

        let query_instructions = compile_query(query);
        machine.set_code(&query_instructions);
        run_code(&mut machine).expect("machine failure");

        let program_instructions = compile_program(program);
        machine.set_code(&program_instructions);
        run_code(&mut machine).expect("machine failure");

        let input = format!("({}, {})", query_text, program_text);
        let output = machine.dbg();
        assert_display_snapshot!(case(input, output));
    }
}
