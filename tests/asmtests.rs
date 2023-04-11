extern crate insta;
extern crate parameterized;
extern crate prolog_rs;
mod testutil;

#[cfg(test)]
mod asmtests {
    use insta::assert_display_snapshot;
    use parameterized::parameterized;
    use prolog_rs::{
        asm::Assembly,
        assembler::compile_asm,
        compile::compile_sentences,
        lang::parse_program,
        symbol::SymbolTable,
        symbol::{to_display, SymDisplay},
        util::{case, writeout_assembly},
    };
    use std::fmt::Write;
    use testutil::load_sample;

    const QUERY_L0: &str = r#"
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

    const QUERY_L1: &str = r#"
    put_variable X4, A1   % ?- p(Z,
    put_structure h/2, A2 %        h
    set_value X4          %         (Z,
    set_variable X5       %            W),
    put_structure f/1, A3 %               f
    set_value X5          %                (W))
    call @0               % who knows where this points
    "#;

    const PROGRAM_L1: &str = r#"
p/3:get_structure f/1, A1
    unify_variable X4
    get_structure h/2, A2
    unify_variable X5
    unify_variable X6
    get_value X5, A3
    get_structure f/1, X6
    unify_variable X7
    get_structure a/0, X7
    proceed
    "#;

    const GOTO_START: &str = r#"
x/0:proceed
    call x/0
    "#;

    const FORWARD_REF: &str = r#"
    call x/0
    put_structure f/1, A1
    put_variable X3, A2
x/0:proceed
    "#;

    const UNBOUND_LABEL: &str = r#"
    call catch/22
    "#;

    const SUPER_BROKEN: &str = r#"
labelone/0:
labeltwo/0:put_structure @3, X2, func/0
    "#;

    const DIGITS_IN_NAMES: &str = r#"
    label1/2: put_structure struct1/0, X2
        "#;

    const MULTI_LABEL: &str = r#"
x/0:
y/1: proceed"#;

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

    #[parameterized(input = {
        QUERY_L0,
        QUERY_L1,
        PROGRAM_L1,
        GOTO_START,
        FORWARD_REF,
        UNBOUND_LABEL,
        SUPER_BROKEN,
        DIGITS_IN_NAMES,
        MULTI_LABEL,
        L2_RULE,
    })]
    fn test_assembler(input: &str) {
        let mut symbol_table = prolog_rs::symbol::SymbolTable::new();
        let result = compile_asm(input, &mut symbol_table)
            .map(|assembly| writeout_assembly(&assembly, &symbol_table));

        let output = match result {
            Ok(s) => s,
            Err(s) => s,
        };

        assert_display_snapshot!(case(input, output));
    }

    #[test]
    fn test_horizontal() -> Result<(), String> {
        let mut symbol_table = SymbolTable::new();
        let mut assembly = Assembly::new();

        let listing = load_sample("horizontal.pro");
        let sentences = parse_program(&listing, &mut symbol_table)?;
        let warnings =
            compile_sentences(sentences, &mut assembly).map_err(|e| e.sym_to_str(&symbol_table))?;

        let mut result = String::new();
        writeln!(result, "{}", writeout_assembly(&assembly, &symbol_table)).unwrap();
        for warning in warnings {
            writeln!(result, "{}", to_display(&warning, &symbol_table)).unwrap();
        }

        Ok(assert_display_snapshot!(case(listing, result)))
    }
}
