extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod decompiletests {
    use std::collections::HashSet;

    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        compile::{compile_program, compile_query},
        lang::{parse_term, Term, VarName},
        run_code,
        symbol::SymbolTable,
        util::{case, write_program_result},
        Machine,
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

        let program_result = write_program_result(
            &machine,
            &symbol_table,
            &query_result.var_mapping,
            &program_result.var_mapping,
        );
        assert_display_snapshot!(case(
            program_str.to_owned() + " | ?- " + query_str,
            program_result
        ));
    }

    #[parameterized(lhs = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))"
    }, rhs = {
        "p(f(X), h(Y, f(a)), Y)", 
        "f(b, Y)"
    })]
    fn test_symmetric_unification(lhs: &str, rhs: &str) {
        fn get_unification_set(query_str: &str, program_str: &str) -> HashSet<(VarName, Term)> {
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

            if machine.get_fail() {
                HashSet::new()
            } else {
                let mut result = HashSet::new();
                let qdesc = machine.describe_vars(&query_result.var_mapping);
                for vd in qdesc {
                    result.insert(vd.to_assignment());
                }
                let pdesc = machine.describe_vars(&program_result.var_mapping);
                for vd in pdesc {
                    result.insert(vd.to_assignment());
                }
                result
            }
        }

        let lr = get_unification_set(lhs, rhs);
        let rl = get_unification_set(rhs, lhs);
        assert_eq!(lr, rl)
    }
}
