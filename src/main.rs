use prolog_rs::{Machine, RunningContext};

extern crate prolog_rs;

fn main() {
    use std::io::{stdin, stdout, Write};

    fn input(prompt: &str) -> String {
        print!("{prompt}");
        let _ = stdout().flush();
        let mut s = String::new();
        stdin().read_line(&mut s).expect("Failed to read!");
        s
    }

    let mut context = RunningContext::default();
    let mut query_ready = false;
    loop {
        match input("> ").trim() {
            "exit" => break,
            "" => (),
            query if query.starts_with("?-") => match parse_and_run_query(query, &mut context) {
                Ok(()) => query_ready = true,
                Err(err) => println!("{err}"),
            },
            program if query_ready => match parse_and_run_program(program, &mut context) {
                Ok(()) => (),
                Err(err) => println!("{err}"),
            },
            _ => println!("Please enter the query first!"),
        }
    }
}

fn parse_and_run_query(input: &str, context: &mut RunningContext) -> Result<(), String> {
    use prolog_rs::{compile::compile_query, lang::parse_term, run_code};

    let query = parse_term(input.trim_start_matches("?-"), &mut context.symbol_table)?;

    let code = compile_query(query);

    context.machine = Machine::new();
    context.machine.set_code(&code.instructions);
    context.query_variables = code.var_mapping;
    run_code(&mut context.machine).map_err(|e| e.message())?;

    //println!("{}", context.machine.dbg(&context.symbol_table));

    Ok(())
}

fn parse_and_run_program(input: &str, context: &mut RunningContext) -> Result<(), String> {
    use prolog_rs::{compile::compile_program, lang::parse_term, run_code};

    let query = parse_term(input, &mut context.symbol_table)?;

    let code = compile_program(query);

    context.machine.set_code(&code.instructions);
    run_code(&mut context.machine).map_err(|e| e.message())?;

    if context.machine.get_fail() {
        println!("no")
    } else {
        for desc in context.machine.describe_vars(&context.query_variables) {
            println!("{}", desc.short(&context.symbol_table))
        }
    }
    //println!("{}", context.machine.dbg(&context.symbol_table));

    //println!("{}", write_program_result(&context.machine, &context.symbol_table, &context.query_variables, &code.var_mapping));

    Ok(())
}
