extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod langtests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        lang::{parse_program, parse_term},
        symbol::{to_display, SymDisplay, SymbolTable},
        util::{case, case_dbg, collapse, WriteVec},
    };

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        "",
        "size(Width, Height)",
    })]
    fn test_term_parse(input: &str) {
        assert_display_snapshot!(case_dbg(
            input,
            parse_term(input, &mut (SymbolTable::new()))
        ));
    }

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        "",
        "size(Width, Height)",
        "superConstant",
        "super_constant",
        "superTerm(SuperVariable)"
    })]
    fn test_term_display(input: &str) {
        let mut symbol_table = SymbolTable::new();
        match parse_term(input, &mut symbol_table) {
            Ok(term) => assert_display_snapshot!(case(input, to_display(&term, &symbol_table))),
            Err(err) => assert_display_snapshot!(case(input, err)),
        }
    }

    const PROGRAM_L2: &str = r#"
        q(a, b).
        r(b, c).
        p(X, Y) :- q(X, Z), r(Z, Y).
        ?- p(U, V), p(V, U).
    "#;

    #[test]
    fn test_program_parse() {
        let mut symbol_table = SymbolTable::new();
        let result = parse_program(PROGRAM_L2, &mut symbol_table).map(|program| {
            WriteVec::new(&program)
                .with_separator("\n")
                .sym_to_str(&symbol_table)
        });
        assert_display_snapshot!(case(PROGRAM_L2, collapse(result)))
    }
}
