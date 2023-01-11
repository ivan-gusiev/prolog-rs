extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod compiletests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::compile::{compile_program, compile_query};
    use prolog_rs::lang::parse_term;

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        "f(X, g(X,a))",
        "f(b, Y)",
        "a"
    })]
    fn test_query_compile(input: &str) {
        let query = parse_term(input).unwrap();
        let instructions = compile_query(query);

        let result = instructions
            .into_iter()
            .map(|i| format!("{}", i))
            .collect::<Vec<_>>()
            .join("\n");
        assert_display_snapshot!(format!("{}\n-----\n{}", input, result));
    }

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        "f(X, g(X,a))",
        "f(b, Y)",
        "p(f(X), h(Y, f(a)), Y)",
        "a"
    })]
    fn test_program_compile(input: &str) {
        let query = parse_term(input).unwrap();
        let instructions = compile_program(query);

        let result = instructions
            .into_iter()
            .map(|i| format!("{}", i))
            .collect::<Vec<_>>()
            .join("\n");
        assert_display_snapshot!(format!("{}\n-----\n{}", input, result));
    }
}
