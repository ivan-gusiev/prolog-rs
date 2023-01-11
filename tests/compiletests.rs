extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod compiletests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::compile::{compile_program, compile_query};
    use prolog_rs::lang::parse_term;
    use prolog_rs::util::{case, writeout};

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

        assert_display_snapshot!(case(input, writeout(&instructions)));
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

        assert_display_snapshot!(case(input, writeout(&instructions)));
    }
}
