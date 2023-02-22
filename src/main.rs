use crate::rustyline::{error::ReadlineError, Editor, Result as RustyResult};
use app::debugmode::start_debugmode;
use prolog_rs::{run_code, Machine, RunningContext};

extern crate prolog_rs;
extern crate rustyline;

mod app;

fn main() -> RustyResult<()> {
    let mut rl = Editor::<()>::new()?;
    let mut context = RunningContext::default();
    let mut query_ready = false;
    loop {
        match rl.readline("> ") {
            Ok(str) => {
                match str.trim() {
                    "exit" => break,
                    "" => (),
                    "debug" => start_debugmode(&mut context.machine)
                        .map_err(|_| ReadlineError::Interrupted)?,
                    "dbg" => {
                        println!("{}", context.machine.dbg(&context.symbol_table));
                    }
                    "vars" => {
                        println!(
                            "{}",
                            prolog_rs::util::write_program_result(
                                &context.machine,
                                &context.symbol_table,
                                &context.query_variables,
                                &context.program_variables,
                            )
                        );
                    }
                    "run" => match run_code(&mut context.machine) {
                        Ok(()) => {}
                        Err(err) => println!("{err}"),
                    },
                    query if query.starts_with("?-") => {
                        match parse_and_run_query(query, &mut context) {
                            Ok(()) => query_ready = true,
                            Err(err) => println!("{err}"),
                        }
                    }
                    program if query_ready => match parse_and_run_program(program, &mut context) {
                        Ok(()) => (),
                        Err(err) => println!("{err}"),
                    },
                    _ => println!("Please enter the query first!"),
                }
                rl.add_history_entry(str);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }
    Ok(())
}

fn parse_and_run_query(input: &str, context: &mut RunningContext) -> Result<(), String> {
    use prolog_rs::{compile::compile_query_l1, lang::parse_term};

    let query = parse_term(input.trim_start_matches("?-"), &mut context.symbol_table)?;

    let code = compile_query_l1(query.into_struct().unwrap());

    context.machine = Machine::new();
    context.machine.set_code(&code.instructions);
    //run_code(&mut context.machine)?;
    context.query_variables = context.machine.bind_variables(&code.var_mapping)?;
    Ok(())
}

fn parse_and_run_program(input: &str, context: &mut RunningContext) -> Result<(), String> {
    use prolog_rs::{compile::compile_program_l1, lang::parse_term};

    let program = parse_term(input, &mut context.symbol_table)?;

    let code = compile_program_l1(program.into_struct().unwrap());

    context.machine.set_code(&code.instructions);
    run_code(&mut context.machine)?;
    context.program_variables = context.machine.bind_variables(&code.var_mapping)?;

    output_result(context)?;
    Ok(())
}

fn output_result(context: &RunningContext) -> Result<(), String> {
    if context.machine.get_fail() {
        Ok(println!("no"))
    } else if context.query_variables.is_empty() {
        Ok(println!("yes"))
    } else {
        for desc in context.machine.describe_vars(&context.query_variables)? {
            println!("{}", desc.short(&context.symbol_table))
        }
        Ok(())
    }
}
