use crate::rustyline::{error::ReadlineError, Editor, Result as RustyResult};
use app::debugmode::start_debugmode;
use prolog_rs::{
    compile::{compile_program, compile_query, CompileResult},
    instr::Instruction,
    lang::parse_struct,
    run_code,
    util::write_program_result,
    PrologApp,
};

extern crate prolog_rs;
extern crate rustyline;

mod app;

fn main() -> RustyResult<()> {
    let mut rl = Editor::<()>::new()?;
    let mut prolog = PrologApp::default();
    loop {
        match rl.readline("> ") {
            Ok(str) => {
                match str.trim() {
                    "exit" => break,
                    "" => (),
                    "+x" => {
                        prolog.immediate_execution = true;
                        println!("Immediate execution: enabled");
                    }
                    "-x" => {
                        prolog.immediate_execution = false;
                        println!("Immediate execution: disabled");
                    }
                    "debug" => {
                        start_debugmode(&mut prolog).map_err(|_| ReadlineError::Interrupted)?
                    }
                    "dbg" => {
                        println!("{}", prolog.machine.dbg(&prolog.symbol_table));
                    }
                    "load" => {
                        let mut code = Vec::<Instruction>::new();
                        if let Some(query) = prolog.query.as_ref() {
                            code.extend(query.instructions.iter())
                        }
                        if let Some(program) = prolog.program.as_ref() {
                            code.extend(program.instructions.iter())
                        }
                        prolog.machine.set_code(code.as_slice())
                    }
                    "vars" => succeed(print_vars(&prolog)),
                    "run" => succeed(run_and_output(&mut prolog)),
                    query if query.starts_with("?-") => {
                        match parse_and_compile_query(query, &mut prolog) {
                            Ok(term) => {
                                prolog.query = Some(term);
                                if prolog.ready_to_run() && prolog.immediate_execution {
                                    succeed(run_and_output(&mut prolog));
                                }
                            }
                            Err(err) => println!("{err}"),
                        }
                    }
                    program => match parse_and_compile_program(program, &mut prolog) {
                        Ok(term) => {
                            prolog.program = Some(term);
                            if prolog.ready_to_run() && prolog.immediate_execution {
                                succeed(run_and_output(&mut prolog));
                            }
                        }
                        Err(err) => println!("{err}"),
                    },
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

fn parse_and_compile_query(input: &str, context: &mut PrologApp) -> Result<CompileResult, String> {
    let query = parse_struct(input.trim_start_matches("?-"), &mut context.symbol_table)?;
    Ok(compile_query(query))
}

fn parse_and_compile_program(
    input: &str,
    context: &mut PrologApp,
) -> Result<CompileResult, String> {
    let program = parse_struct(input, &mut context.symbol_table)?;
    Ok(compile_program(program))
}

fn run_and_output(context: &mut PrologApp) -> Result<(), String> {
    let query_result = context.query.as_ref().unwrap();
    let program_result = context.program.as_ref().unwrap();

    context.machine.set_code(&query_result.instructions);
    run_code(&mut context.machine)?;
    context.query_variables = context.machine.bind_variables(&query_result.var_mapping)?;

    context.machine.set_code(&program_result.instructions);
    run_code(&mut context.machine)?;
    context.program_variables = context
        .machine
        .bind_variables(&program_result.var_mapping)?;

    output_result(context)?;
    Ok(())
}

fn output_result(context: &PrologApp) -> Result<(), String> {
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

fn print_vars(app: &PrologApp) -> Result<(), String> {
    println!(
        "{}",
        write_program_result(
            &app.machine,
            &app.symbol_table,
            &app.query_variables,
            &app.program_variables
        )
    );
    Ok(())
}

fn succeed<T: ToString>(result: Result<(), T>) -> () {
    match result {
        Ok(()) => {}
        Err(e) => println!("{}", e.to_string()),
    }
}
