extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod langtests {
    use insta::{assert_debug_snapshot, assert_display_snapshot};
    use parameterized::parameterized;
    use prolog_rs::lang::parse_term;

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        ""
    })]
    fn test_term_parse(input: &str) {
        assert_debug_snapshot!(parse_term(input));
    }

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "D",
        ""
    })]
    fn test_term_display(input: &str) {
        match parse_term(input) {
            Ok(term) => assert_display_snapshot!(term),
            Err(err) => assert_display_snapshot!(err),
        }
    }
}
