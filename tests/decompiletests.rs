extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod decompiletests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        compile::{compile_program, compile_query},
        lang::parse_term,
        symbol::SymbolTable,
        util::{case, write_program_result},
        Machine, run_code
    };

    #[parameterized(program_str = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))", 
        "a"
    }, query_str = {
        "p(f(X), h(Y, f(a)), Y)", 
        "f(b, Y)", 
        "D"
    })]
    fn test_run_and_decompile(program_str: &str, query_str: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_term(query_str, &mut symbol_table).unwrap();
        let program = parse_term(program_str, &mut symbol_table).unwrap();
        let mut machine = Machine::new();

        let query_result = compile_query(query);
        machine.set_code(&query_result.instructions);
        run_code(&mut machine).expect("machine failure");

        let program_result = compile_program(program);
        machine.set_code(&program_result.instructions);
        run_code(&mut machine).expect("machine failure");

        let program_result = write_program_result(&machine, &symbol_table, &query_result.var_mapping, &program_result.var_mapping);
        assert_display_snapshot!(case(program_str.to_owned() + " | ?- " + query_str, program_result));
    }
}
