extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod decompiletests {
    use std::collections::{HashMap, HashSet};

    use insta::{assert_debug_snapshot, assert_display_snapshot};
    use parameterized::parameterized;
    use prolog_rs::{
        compile::{compile_program, compile_program_l1, compile_query, compile_query_l1},
        data::{Data, HeapPtr},
        lang::{parse_struct, parse_term, Functor, Term, VarName},
        run_code,
        symbol::{SymDisplay, SymbolTable},
        util::{case, collapse, write_program_result, writeout_annotated_mappings},
        var::VarBindings,
        Machine,
    };

    #[parameterized(program_str = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))",
        "f(b, Y)",
        "a"
    }, query_str = {
        "p(f(X), h(Y, f(a)), Y)", 
        "f(b, Y)",
        "f(X, g(X,a))",
        "b"
    })]
    fn test_run_and_decompile(program_str: &str, query_str: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_struct(query_str, &mut symbol_table).unwrap();
        let program = parse_struct(program_str, &mut symbol_table).unwrap();
        let mut machine = Machine::new();

        let query_result = compile_query_l1(query);
        machine.set_code(&query_result.instructions);
        run_code(&mut machine).expect("machine failure");

        let program_result = compile_program_l1(program);
        machine.set_code(&program_result.instructions);
        run_code(&mut machine).expect("machine failure");

        let result = write_program_result(
            &machine,
            &symbol_table,
            &query_result.var_mapping,
            &program_result.var_mapping,
        ) + writeout_annotated_mappings(
            &machine,
            &query_result.var_mapping,
            &program_result.var_mapping,
            &symbol_table,
        )
        .as_str();
        assert_display_snapshot!(case(program_str.to_owned() + " | ?- " + query_str, result));
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

    #[test]
    fn test_bad_addr() {
        let mut machine = Machine::new();
        let mut symbol_table = SymbolTable::new();
        let ptr = HeapPtr(0);
        let var_bindings = VarBindings::from_hash(HashMap::new());
        machine.set_heap(ptr, Data::Functor(Functor(symbol_table.intern("f"), 2)));

        let result = machine
            .decompile_addr(ptr.into(), &var_bindings)
            .map(|term| term.sym_to_str(&symbol_table))
            .map_err(|e| format!("{}", e));
        assert_debug_snapshot!(collapse(result));
    }
}
