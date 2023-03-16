extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod compiletests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        asm::Assembly,
        compile::{compile_program, compile_query},
        lang::parse_struct,
        symbol::{to_display, SymbolTable},
        util::{case, collapse, lbl_for, writeout_compile_result},
    };

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))",
        "f(b, Y)",
        "p(f(X), h(Y, f(a)), Y)",
        "a",
        "horizontal(line(pt(three, Y), pt(four, five)))",
    })]
    fn test_query_compile(input: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_struct(input, &mut symbol_table).unwrap();
        let labels = lbl_for(query.functor());
        let result = compile_query(query, &labels).unwrap();

        assert_display_snapshot!(case(input, writeout_compile_result(&result, &symbol_table)));
    }

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))",
        "f(b, Y)",
        "p(f(X), h(Y, f(a)), Y)",
        "a",
        "horizontal(line(pt(X1, Y), pt(X2, Y)))",
    })]
    fn test_program_compile(input: &str) {
        let mut symbol_table = SymbolTable::new();
        let program = parse_struct(input, &mut symbol_table).unwrap();
        let result = compile_program(program);

        assert_display_snapshot!(case(input, writeout_compile_result(&result, &symbol_table)));
    }

    #[parameterized(input = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))",
        "horizontal(line(pt(X1, Y), pt(X2, Y)))",
    })]
    fn test_compile_multifact_query(input: &str) {
        let mut assembly = Assembly::default();
        let mut symbol_table = SymbolTable::new();
        compile_program(parse_struct("p(f(X), h(Y, f(a)), Y)", &mut symbol_table).unwrap())
            .append_to_assembly(&mut assembly);
        compile_program(parse_struct("f(b, Y)", &mut symbol_table).unwrap())
            .append_to_assembly(&mut assembly);

        let result = compile_query(
            parse_struct(input, &mut symbol_table).unwrap(),
            &assembly.label_map,
        )
        .map(|compile_result| writeout_compile_result(&compile_result, &symbol_table))
        .map_err(|err| format!("{}", to_display(&err, &symbol_table)));
        assert_display_snapshot!(case(input, collapse(result)));
    }
}
