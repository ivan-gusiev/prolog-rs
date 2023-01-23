extern crate prolog_rs;

#[cfg(test)]
mod tests {
    use prolog_rs::{
        asm::{parse_program, Arg, Command},
        compile::compile_query,
        data::{Data, HeapPtr, Ref, RegPtr, Str},
        instr::Instruction,
        lang::{parse_term, Functor},
        run_code, Machine,
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

    const QUERY: &str = "p(Z,h(Z,W), f(W))";

    #[test]
    fn program_compiles_to_bytecode() {
        let query = parse_term(QUERY).unwrap();
        let instructions = compile_query(query);
        let expected = Instruction::from_program(PROGRAM).unwrap();
        assert_eq!(expected.as_slice(), instructions.as_slice());
    }

    #[test]
    fn query_compiles_to_bytecode() {
        let h2 = Functor('h', 2);
        let f1 = Functor('f', 1);
        let p3 = Functor('p', 3);

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

        let actual_heap = machine.iter_heap().map(|x| *x).collect::<Vec<_>>();

        assert_eq!(expected_heap.as_slice(), actual_heap.as_slice())
    }

    #[test]
    fn parsing_produces_program() {
        assert_eq!(
            parse_program(PROGRAM),
            Ok(vec![
                Line::Cmd(
                    "put_structure".to_string(),
                    vec![Arg::Func('h', 2), Arg::Reg(3)]
                ),
                Line::Cmd("set_variable".to_string(), vec![Arg::Reg(2)]),
                Line::Cmd("set_variable".to_string(), vec![Arg::Reg(5)]),
                Line::Cmd(
                    "put_structure".to_string(),
                    vec![Arg::Func('f', 1), Arg::Reg(4)]
                ),
                Line::Cmd("set_value".to_string(), vec![Arg::Reg(5)]),
                Line::Cmd(
                    "put_structure".to_string(),
                    vec![Arg::Func('p', 3), Arg::Reg(1)]
                ),
                Line::Cmd("set_value".to_string(), vec![Arg::Reg(2)]),
                Line::Cmd("set_value".to_string(), vec![Arg::Reg(3)]),
                Line::Cmd("set_value".to_string(), vec![Arg::Reg(4)]),
            ])
        )
    }

    #[test]
    fn incorrect_program_does_not_parse() {
        assert!(matches!(parse_program("42"), Err(_)))
    }

    #[test]
    fn parsing_program_produces_instructions() {
        let h2 = Functor('h', 2);
        let f1 = Functor('f', 1);
        let p3 = Functor('p', 3);

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

        let parse_result = Instruction::from_program(PROGRAM);

        assert_eq!(parse_result.unwrap().as_slice(), code.as_slice())
    }

    #[test]
    fn parsing_incorrect_program_produces_error() {
        assert_eq!(
            Instruction::from_program("put_structure X1"),
            Err("Incorrect arguments for put_structure: [Reg(1)]".to_string())
        )
    }
}
