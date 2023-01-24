extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod compiletests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        compile::{compile_program, compile_query},
        lang::parse_term,
        symbol::SymbolTable,
        util::{case, writeout_sym},
    };

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        "f(X, g(X,a))",
        "f(b, Y)",
        "a"
    })]
    fn test_query_compile(input: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_term(input, &mut symbol_table).unwrap();
        let instructions = compile_query(query);

        assert_display_snapshot!(case(input, writeout_sym(&instructions, &symbol_table)));
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
        let mut symbol_table = SymbolTable::new();
        let query = parse_term(input, &mut symbol_table).unwrap();
        let instructions = compile_program(query);

        assert_display_snapshot!(case(input, writeout_sym(&instructions, &symbol_table)));
    }
}
