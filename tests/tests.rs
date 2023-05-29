extern crate insta;
extern crate prolog_rs;

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, iter::FromIterator};

    use insta::assert_display_snapshot;
    use prolog_rs::{
        asm::Assembly,
        assembler::{compile_asm, compile_asm_to_assembly},
        compile::{compile_query, compile_rule, compile_sentences, CompileInfo},
        data::{CodePtr, Data, HeapPtr, Ref, RegPtr, Str},
        instr::Instruction,
        lang::{parse_program, parse_sentence, Functor, Struct, Term},
        machine::Machine,
        symbol::SymbolTable,
        util::lbl_for,
        var::VarBindings,
    };

    const PROGRAM: &str = r#"
        put_structure h/2, X3 % ?- X3=h
        set_variable X2       %        (Z,
        set_variable X5       %           W),
        put_structure f/1, X4 %    X4=f
        set_value X5          %        (W),
        put_structure p/3, X1 %    X1=p
        set_value X2          %        (Z,
        set_value X3          %           X3,
        set_value X4          %              X4)
        "#;

    const QUERY_ASM_L2: &str = r#"
        allocate 2
        put_variable Y1, A1   % ?- p(Z,
        put_structure h/2, A2 %        h
        set_value Y1          %         (Z,
        set_variable Y2       %            W),
        put_structure f/1, A3 %               f
        set_value Y2          %                (W))
        call @0               % by convention, p/3 points to 0
    "#;

    const QUERY: &str = "?- p(Z,h(Z,W), f(W)).";

    const ALLOCATE_PROGRAM: &str = r#"
        allocate 2
        put_variable Y1, A1
        put_variable Y2, A1
        allocate 1
        put_structure a/1, Y1
    "#;

    #[test]
    fn program_compiles_to_bytecode() {
        let mut symbol_table = SymbolTable::new();
        let query = parse_sentence(QUERY, &mut symbol_table).unwrap();
        let labels = lbl_for(&query.goals);
        let CompileInfo {
            instructions,
            var_mapping: _,
            label_functor: _,
        } = compile_query(query.goals, &labels).unwrap();
        let expected = compile_asm(QUERY_ASM_L2, &mut symbol_table)
            .unwrap()
            .instructions;
        assert_eq!(expected.as_slice(), instructions.as_slice());
    }

    #[test]
    fn instructions_result_in_heap() {
        let mut symbol_table = SymbolTable::new();
        let h2 = Functor(symbol_table.intern("h"), 2);
        let f1 = Functor(symbol_table.intern("f"), 1);
        let p3 = Functor(symbol_table.intern("p"), 3);

        let x1 = RegPtr(1).into();
        let x2 = RegPtr(2).into();
        let x3 = RegPtr(3).into();
        let x4 = RegPtr(4).into();
        let x5 = RegPtr(5).into();

        let code = vec![
            Instruction::PutStructure(h2, x3),
            Instruction::SetVariable(x2),
            Instruction::SetVariable(x5),
            Instruction::PutStructure(f1, x4),
            Instruction::SetValue(x5),
            Instruction::PutStructure(p3, x1),
            Instruction::SetValue(x2),
            Instruction::SetValue(x3),
            Instruction::SetValue(x4),
        ];

        let mut machine = Machine::new();
        machine.set_code(&code);
        machine.execute().run().expect("machine failure");

        fn str(heap: usize) -> Data {
            Data::Str(Str(HeapPtr(heap)))
        }

        fn func(f: Functor) -> Data {
            Data::Functor(f)
        }

        fn refr(heap: usize) -> Data {
            Data::Ref(Ref(HeapPtr(heap)))
        }

        let expected_heap = vec![
            str(1),
            func(h2),
            refr(2),
            refr(3),
            str(5),
            func(f1),
            refr(3),
            str(8),
            func(p3),
            refr(2),
            str(1),
            str(5),
        ];

        let actual_heap = machine.iter_heap().copied().collect::<Vec<_>>();

        assert_eq!(expected_heap.as_slice(), actual_heap.as_slice())
    }

    #[test]
    fn parsing_program_produces_instructions() {
        let mut symbol_table = SymbolTable::new();
        let h2 = Functor(symbol_table.intern("h"), 2);
        let f1 = Functor(symbol_table.intern("f"), 1);
        let p3 = Functor(symbol_table.intern("p"), 3);

        let x1 = RegPtr(1).into();
        let x2 = RegPtr(2).into();
        let x3 = RegPtr(3).into();
        let x4 = RegPtr(4).into();
        let x5 = RegPtr(5).into();

        let code = vec![
            Instruction::PutStructure(h2, x3),
            Instruction::SetVariable(x2),
            Instruction::SetVariable(x5),
            Instruction::PutStructure(f1, x4),
            Instruction::SetValue(x5),
            Instruction::PutStructure(p3, x1),
            Instruction::SetValue(x2),
            Instruction::SetValue(x3),
            Instruction::SetValue(x4),
        ];

        let parse_result = compile_asm(PROGRAM, &mut symbol_table);

        assert_eq!(
            parse_result.unwrap().instructions.as_slice(),
            code.as_slice()
        )
    }

    #[test]
    fn parsing_incorrect_program_produces_error() {
        assert_eq!(
            compile_asm("put_structure X1", &mut (SymbolTable::new())),
            Err("Incorrect arguments for put_structure: X1".to_string())
        )
    }

    #[test]
    fn allocate_debug() {
        let mut symbol_table = prolog_rs::symbol::SymbolTable::new();
        let code = compile_asm(ALLOCATE_PROGRAM, &mut symbol_table).unwrap();
        let mut machine = Machine::new();
        machine.load_assembly(&code);
        machine.set_p(CodePtr(0));
        machine.execute().run().unwrap();
        assert_display_snapshot!(machine.dbg(&symbol_table))
    }

    #[test]
    fn allocate_test() {
        const L2_RULE: &str = r#"
        p/2: allocate 2
             get_variable X3, A1
             get_variable Y1, A2
             put_value X3, A1
             put_variable Y2, A2
             call q/2
             put_value Y2, A1
             put_value Y1, A2
             call r/2
             deallocate
            "#;

        const L2_FACTS: &str = r#"
            q(a, b).
            r(b, c).
        "#;

        const L2_QUERY: &str = "?- p(U, V).";

        let mut symbol_table = prolog_rs::symbol::SymbolTable::new();
        let mut assembly = Assembly::new();
        let sentences = parse_program(L2_FACTS, &mut symbol_table).unwrap();
        let warnings = compile_sentences(sentences, &mut assembly).unwrap();
        assert!(warnings.is_empty());
        compile_asm_to_assembly(L2_RULE, &mut assembly, &mut symbol_table).unwrap();
        let sentences = parse_program(L2_QUERY, &mut symbol_table).unwrap();
        let warnings = compile_sentences(sentences, &mut assembly).unwrap();
        assert!(warnings.is_empty());

        let mut machine = Machine::new();
        let query_mapping = &assembly.entry_point.as_ref().unwrap().variables;
        let mut query_binding = VarBindings::default();
        let mut bound = false;
        machine.load_assembly(&assembly);
        machine
            .execute()
            .with_call_hook(|machine| {
                if bound {
                    return Ok(());
                }

                query_binding = machine.bind_variables(query_mapping)?;
                bound = true;
                Ok(())
            })
            .run()
            .unwrap();

        let u = symbol_table.intern("U");
        let v = symbol_table.intern("V");
        let a = symbol_table.intern("a");
        let c = symbol_table.intern("c");
        for desc in machine
            .describe_vars(&query_binding, &mut symbol_table)
            .unwrap()
        {
            let (name, value) = desc.to_assignment();
            if name == u {
                assert_eq!(value, Term::Struct(Struct::constant(a)))
            }
            if name == v {
                assert_eq!(value, Term::Struct(Struct::constant(c)))
            }
        }
    }

    #[test]
    fn test_no_allocate_0() {
        let mut symbol_table = SymbolTable::new();

        let rule = parse_sentence("a(X) :- b(X).", &mut symbol_table).unwrap();
        let label_map = HashMap::from_iter([(Functor(symbol_table.intern("b"), 1), CodePtr(100))]);

        let result = compile_rule(rule.head.unwrap(), rule.goals, &label_map).unwrap();
        let allocates = result
            .instructions
            .iter()
            .filter(|x| matches!(x, Instruction::Allocate(_)))
            .collect::<Vec<_>>();
        let deallocates = result
            .instructions
            .iter()
            .filter(|x| matches!(x, Instruction::Deallocate))
            .collect::<Vec<_>>();
        assert_eq!(allocates.len(), 0);
        assert_eq!(deallocates.len(), 0);
    }
}
