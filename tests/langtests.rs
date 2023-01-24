extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod langtests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        lang::parse_term,
        symbol::{SymbolTable, to_display},
        util::{case, case_dbg},
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
    })]
    fn test_term_display(input: &str) {
        let mut symbol_table = SymbolTable::new();
        match parse_term(input, &mut symbol_table) {
            Ok(term) => assert_display_snapshot!(case(input, to_display(&term, &symbol_table))),
            Err(err) => assert_display_snapshot!(case(input, err)),
        }
    }
}
