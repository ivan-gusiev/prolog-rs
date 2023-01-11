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
        run_code(&mut machine);

        let output = writeout(&machine.iter_heap().collect::<Vec<_>>());
        assert_display_snapshot!(case(
            input,
            output
        ));
    }

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        "f(X, g(X,a))",
        "f(b, Y)",
        "p(f(X), h(Y, f(a)), Y)",
        "a"
    })]
    fn test_program_execute(input: &str) {
        let program = parse_term(input).unwrap();
        let instructions = compile_program(program);
        let mut machine = Machine::new();
        machine.set_code(&instructions);
        run_code(&mut machine);

        let output = writeout(&machine.iter_heap().collect::<Vec<_>>());
        assert_display_snapshot!(case(
            input,
            output
        ));
    }
}
