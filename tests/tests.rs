extern crate prolog_rs;

#[cfg(test)]
mod tests {
    use prolog_rs::{
        asm::{parse_program, Arg, Command},
        compile::{compile_query, CompileResult},
        data::{Data, HeapPtr, Ref, RegPtr, Str},
        instr::Instruction,
        lang::{parse_term, Functor},
        run_code,
        symbol::SymbolTable,
        Machine,
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

    // const PROGRAM1: &str = r#"
    //     put_variable X4, A1   % ?- p(Z,
    //     put_structure h/2, A2 %        h
    //     set_value X4          %         (Z,
    //     set_variable X5       %            W),
    //     put_structure f/1, A3 %               f
    //     set_value X5          %                (W))
    //     call @0               % who knows where this points
    // "#;

    const QUERY: &str = "p(Z,h(Z,W), f(W))";

    #[test]
    fn program_compiles_to_bytecode() {
        let mut symbol_table = SymbolTable::new();
        let query = parse_term(QUERY, &mut symbol_table).unwrap();
        let CompileResult {
            instructions,
            var_mapping: _,
        } = compile_query(query);
        let expected = Instruction::from_assembly(PROGRAM, &mut symbol_table).unwrap();
        assert_eq!(expected.as_slice(), instructions.as_slice());
    }

    #[test]
    fn instructions_result_in_heap() {
        let mut symbol_table = SymbolTable::new();
        let h2 = Functor(symbol_table.intern("h"), 2);
        let f1 = Functor(symbol_table.intern("f"), 1);
        let p3 = Functor(symbol_table.intern("p"), 3);

        let x1 = RegPtr(1);
        let x2 = RegPtr(2);
        let x3 = RegPtr(3);
        let x4 = RegPtr(4);
        let x5 = RegPtr(5);

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
        run_code(&mut machine).expect("machine failure");

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
    fn parsing_produces_program() {
        let mut symbol_table = SymbolTable::new();
        let h = symbol_table.intern("h");
        let f = symbol_table.intern("f");
        let p = symbol_table.intern("p");
        assert_eq!(
            parse_program(PROGRAM, &mut symbol_table),
            Ok(vec![
                Command(
                    "put_structure".to_string(),
                    vec![Arg::Func(h, 2), Arg::Reg(3)]
                ),
                Command("set_variable".to_string(), vec![Arg::Reg(2)]),
                Command("set_variable".to_string(), vec![Arg::Reg(5)]),
                Command(
                    "put_structure".to_string(),
                    vec![Arg::Func(f, 1), Arg::Reg(4)]
                ),
                Command("set_value".to_string(), vec![Arg::Reg(5)]),
                Command(
                    "put_structure".to_string(),
                    vec![Arg::Func(p, 3), Arg::Reg(1)]
                ),
                Command("set_value".to_string(), vec![Arg::Reg(2)]),
                Command("set_value".to_string(), vec![Arg::Reg(3)]),
                Command("set_value".to_string(), vec![Arg::Reg(4)]),
            ])
        )
    }

    #[test]
    fn incorrect_program_does_not_parse() {
        assert!(matches!(
            parse_program("42", &mut (SymbolTable::new())),
            Err(_)
        ))
    }

    #[test]
    fn parsing_program_produces_instructions() {
        let mut symbol_table = SymbolTable::new();
        let h2 = Functor(symbol_table.intern("h"), 2);
        let f1 = Functor(symbol_table.intern("f"), 1);
        let p3 = Functor(symbol_table.intern("p"), 3);

        let x1 = RegPtr(1);
        let x2 = RegPtr(2);
        let x3 = RegPtr(3);
        let x4 = RegPtr(4);
        let x5 = RegPtr(5);

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

        let parse_result = Instruction::from_assembly(PROGRAM, &mut symbol_table);

        assert_eq!(parse_result.unwrap().as_slice(), code.as_slice())
    }

    #[test]
    fn parsing_incorrect_program_produces_error() {
        assert_eq!(
            Instruction::from_assembly("put_structure X1", &mut (SymbolTable::new())),
            Err("Incorrect arguments for put_structure: [Reg(1)]".to_string())
        )
    }
}
