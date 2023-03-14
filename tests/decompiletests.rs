extern crate insta;
extern crate parameterized;
extern crate prolog_rs;

#[cfg(test)]
mod decompiletests {
    use std::collections::{HashMap, HashSet};

    use insta::{assert_debug_snapshot, assert_display_snapshot};
    use parameterized::parameterized;
    use prolog_rs::{
        compile::{compile_program, compile_query},
        data::{CodePtr, Data, HeapPtr},
        lang::{parse_struct, Functor, Term, VarName},
        machine::Machine,
        symbol::{to_display, SymDisplay, SymbolTable},
        util::{
            case, collapse, lbl_for, run_just_query, write_program_result, writeout,
            writeout_annotated_mappings,
        },
        var::VarBindings,
    };

    #[parameterized(program_str = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))",
        "f(b, Y)",
        "a",
        "horizontal(line(pt(X1, Y), pt(X2, Y)))"
    }, query_str = {
        "p(f(X), h(Y, f(a)), Y)", 
        "f(b, Y)",
        "f(X, g(X,a))",
        "b",
        "horizontal(A)"
    })]
    fn test_run_and_decompile(program_str: &str, query_str: &str) {
        let mut symbol_table = SymbolTable::new();
        let query = parse_struct(query_str, &mut symbol_table).unwrap();
        let program = parse_struct(program_str, &mut symbol_table).unwrap();
        let mut machine = Machine::new();

        let labels = lbl_for(query.functor());
        let query_result = compile_query(query, &labels).unwrap();
        run_just_query(&mut machine, &query_result.instructions).expect("machine failure");
        let query_bindings = machine
            .bind_variables(&query_result.var_mapping)
            .expect("decompile failure");

        let program_result = compile_program(program);
        machine.set_p(CodePtr(0));
        machine.set_cp(CodePtr(program_result.instructions.len()));
        machine.set_code(&program_result.instructions);
        machine.execute().run().expect("machine failure");
        let program_bindings = machine
            .bind_variables(&program_result.var_mapping)
            .expect("decompile failure");

        let result = write_program_result(
            &machine,
            &mut symbol_table,
            &query_bindings,
            &program_bindings,
        ) + writeout_annotated_mappings(
            &machine,
            &query_bindings,
            &program_bindings,
            &symbol_table,
        )
        .as_str();
        assert_display_snapshot!(case(program_str.to_owned() + " | ?- " + query_str, result));
    }

    #[parameterized(lhs = {
        "p(Z, h(Z,W), f(W))",
        "f(X, g(X,a))",
        "h(l(p(A, Y), p(B, Y)))", 
    }, rhs = {
        "p(f(X), h(Y, f(a)), Y)", 
        "f(b, Y)",
        "h(l(p(u, v), p(w, H)))",
    })]
    fn test_symmetric_unification(lhs: &str, rhs: &str) {
        fn get_unification_set(
            query_str: &str,
            program_str: &str,
        ) -> (SymbolTable, HashSet<(VarName, Term)>) {
            let mut symbol_table = SymbolTable::new();
            let query = parse_struct(query_str, &mut symbol_table).unwrap();
            let program = parse_struct(program_str, &mut symbol_table).unwrap();
            let mut machine = Machine::new();

            let labels = lbl_for(query.functor());
            let query_result = compile_query(query, &labels).unwrap();
            run_just_query(&mut machine, &query_result.instructions).expect("machine failure");
            let query_bindings = machine
                .bind_variables(&query_result.var_mapping)
                .expect("decompile failure");

            let program_result = compile_program(program);
            machine.set_p(CodePtr(0));
            machine.set_cp(CodePtr(program_result.instructions.len()));
            machine.set_code(&program_result.instructions);
            machine.execute().run().expect("machine failure");
            let program_bindings = machine
                .bind_variables(&program_result.var_mapping)
                .expect("decompile failure");

            if machine.get_fail() {
                (symbol_table, HashSet::new())
            } else {
                let mut result = HashSet::new();
                let qdesc = machine
                    .describe_vars(&query_bindings, &mut symbol_table)
                    .unwrap();
                for vd in qdesc {
                    result.insert(vd.to_assignment());
                }
                let pdesc = machine
                    .describe_vars(&program_bindings, &mut symbol_table)
                    .unwrap();
                for vd in pdesc {
                    result.insert(vd.to_assignment());
                }
                (symbol_table, result)
            }
        }

        fn output((symbol_table, set): (SymbolTable, HashSet<(VarName, Term)>)) -> String {
            let mut items = set
                .into_iter()
                .map(|(nm, t)| {
                    format!(
                        "{}={}",
                        to_display(&nm, &symbol_table),
                        to_display(&t, &symbol_table)
                    )
                })
                .collect::<Vec<_>>();
            items.sort();
            writeout(items)
        }

        let lr = output(get_unification_set(lhs, rhs));
        let rl = output(get_unification_set(rhs, lhs));
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
            .decompile(ptr.into(), &var_bindings, &mut symbol_table)
            .map(|term| term.sym_to_str(&symbol_table))
            .map_err(|e| format!("{e}"));
        assert_debug_snapshot!(collapse(result));
    }
}
