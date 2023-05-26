extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod compiletests {
    use std::collections::HashMap;
    use std::iter::FromIterator;

    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        asm::Assembly,
        compile::{compile_fact, compile_query, compile_rule},
        data::CodePtr,
        lang::{parse_sentence, parse_struct, Functor},
        symbol::{to_display, SymbolTable},
        util::{case, collapse, lbl_for, writeout_compile_result},
    };

    #[parameterized(input = {
        "?- p(Z, h(Z,W), f(W)).",
        "?- f(X, g(X,a)).",
        "?- f(b, Y).",
        "?- p(f(X), h(Y, f(a)), Y).",
        "?- a.",
        "?- horizontal(line(pt(three, Y), pt(four, five))).",
        "?- q(X, Z).",
        "?- q(X, Z), f(b, Y).",
        "?- f(X, g(Y, a)), g(X, b).",
    })]
    fn test_query_compile(input: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_sentence(input, &mut symbol_table).unwrap();
        let labels = lbl_for(&query.goals);
        let result = compile_query(query.goals, &labels).unwrap();

        assert_display_snapshot!(case(input, writeout_compile_result(&result, &symbol_table)));
    }

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))",
        "f(b, Y)",
        "p(f(X), h(Y, f(a)), Y)",
        "a",
        "horizontal(line(pt(X1, Y), pt(X2, Y)))",
        "p(X, Y)"
    })]
    fn test_fact_compile(input: &str) {
        let mut symbol_table = SymbolTable::new();
        let program = parse_struct(input, &mut symbol_table).unwrap();
        let result = compile_fact(program);

        assert_display_snapshot!(case(input, writeout_compile_result(&result, &symbol_table)));
    }

    #[parameterized(input = {
        "?- p(Z, h(Z,W), f(W)).",
        "?- f(X, g(X,a)).",
        "?- horizontal(line(pt(X1, Y), pt(X2, Y))).",
        "?- p(a, X, Y), f(b, f(X)).",
    })]
    fn test_compile_multifact_query(input: &str) {
        let mut assembly = Assembly::default();
        let mut symbol_table = SymbolTable::new();
        compile_fact(parse_struct("p(f(X), h(Y, f(a)), Y)", &mut symbol_table).unwrap())
            .append_to_assembly(&mut assembly);
        compile_fact(parse_struct("f(b, Y)", &mut symbol_table).unwrap())
            .append_to_assembly(&mut assembly);

        let result = compile_query(
            parse_sentence(input, &mut symbol_table).unwrap().goals,
            &assembly.label_map,
        )
        .map(|compile_result| writeout_compile_result(&compile_result, &symbol_table))
        .map_err(|err| format!("{}", to_display(&err, &symbol_table)));
        assert_display_snapshot!(case(input, collapse(result)));
    }

    #[parameterized(input = {
        "p(X, Y) :- q(X, Z), r(Z, Y).",
        "f(X) :- p(X, r(Z, Y)), h(l(p(A, X), p(B, X))).",
        "a(A) :- h(A)."
    })]
    fn test_compile_rule(input: &str) {
        let mut symbol_table = SymbolTable::new();

        let rule = parse_sentence(input, &mut symbol_table).unwrap();
        let mut mklabel = |s, i, c| (Functor(symbol_table.intern(s), i), CodePtr(c));
        let label_map = HashMap::from_iter([
            mklabel("q", 2, 100),
            mklabel("r", 2, 110),
            mklabel("p", 2, 120),
            mklabel("h", 1, 130),
        ]);

        let result = compile_rule(rule.head.unwrap(), rule.goals, &label_map)
            .map(|compile_result| writeout_compile_result(&compile_result, &symbol_table))
            .map_err(|err| format!("{}", to_display(&err, &symbol_table)));
        assert_display_snapshot!(case(input, collapse(result)));
    }
}
