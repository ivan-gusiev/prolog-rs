use crate::rustyline::{error::ReadlineError, Editor, Result as RustyResult};
use app::debugmode::start_debugmode;
use prolog_rs::{
    compile::{compile_program, compile_query, CompileInfo},
    lang::parse_struct,
    symbol::SymDisplay,
    util::write_program_result,
    var::VarBindings,
    PrologApp,
};

extern crate prolog_rs;
extern crate rustyline;

mod app;

fn main() -> RustyResult<()> {
    let mut rl = Editor::<()>::new()?;
    let mut prolog = PrologApp {
        immediate_execution: true,
        ..PrologApp::default()
    };
    prolog.immediate_execution = true;
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
                        prolog.machine.set_code(&prolog.assembly.instructions[..]);
                        if let Some(query) = prolog.query.as_ref() {
                            let query_p = prolog.machine.append_code(&query.instructions[..]);
                            prolog.machine.set_p(query_p);
                        }
                    }
                    "vars" => succeed(print_vars(&mut prolog)),
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
                        Ok(compile_result) => {
                            prolog.program =
                                Some(compile_result.append_to_assembly(&mut prolog.assembly).variables);
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

fn parse_and_compile_query(input: &str, context: &mut PrologApp) -> Result<CompileInfo, String> {
    let query = parse_struct(input.trim_start_matches("?-"), &mut context.symbol_table)?;
    compile_query(query, &context.assembly.label_map)
        .map_err(|err| err.sym_to_str(&context.symbol_table))
}

fn parse_and_compile_program(input: &str, context: &mut PrologApp) -> Result<CompileInfo, String> {
    let program = parse_struct(input, &mut context.symbol_table)?;
    Ok(compile_program(program))
}

fn run_and_output(context: &mut PrologApp) -> Result<(), String> {
    let query_result = context.query.as_ref().ok_or("No query to run")?;
    let program_mapping = context.program.as_ref().ok_or("No program to run")?;
    let assembly = &context.assembly;

    context.machine.set_code(&assembly.instructions);
    let query_p = context.machine.append_code(&query_result.instructions);
    context.machine.set_p(query_p);

    let mut query_vars: VarBindings = VarBindings::default();

    context
        .machine
        .execute()
        .with_call_hook(|machine| {
            machine
                .bind_variables(&query_result.var_mapping)
                .map(|vars| {
                    query_vars = vars;
                })
        })
        .run()?;

    context.query_variables = query_vars;
    context.program_variables = context.machine.bind_good_variables(program_mapping);

    output_result(context)?;
    Ok(())
}

fn output_result(context: &mut PrologApp) -> Result<(), String> {
    if context.machine.get_fail() {
        Ok(println!("no"))
    } else if context.query_variables.is_empty() {
        Ok(println!("yes"))
    } else {
        for desc in context
            .machine
            .describe_vars(&context.query_variables, &mut context.symbol_table)?
        {
            println!("{}", desc.short(&context.symbol_table))
        }
        Ok(())
    }
}

fn print_vars(app: &mut PrologApp) -> Result<(), String> {
    println!(
        "{}",
        write_program_result(
            &app.machine,
            &mut app.symbol_table,
            &app.query_variables,
            &app.program_variables
        )
    );
    Ok(())
}

fn succeed<T: ToString>(result: Result<(), T>) {
    match result {
        Ok(()) => {}
        Err(e) => println!("{}", e.to_string()),
    }
}
