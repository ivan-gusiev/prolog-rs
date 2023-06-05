use crate::rustyline::{error::ReadlineError, Editor, Result as RustyResult};
use app::debugmode::start_debugmode;
use prolog_rs::{
    asm::Assembly,
    assembler::compile_asm,
    compile::{compile_sentence, compile_sentences, CompileInfo, CompileWarning},
    lang::{parse_program, parse_sentence},
    symbol::{to_display, SymDisplay},
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
                            let query_p = prolog.machine.append_code(&query.instructions);
                            prolog.machine.set_p(query_p);
                            prolog.machine.set_cp(prolog.machine.code_len().into());
                            prolog.machine.set_var_mappings(&query.var_mapping);
                        }
                    }
                    load_cmd if load_cmd.starts_with("load") => {
                        let rest = &load_cmd[4..].trim();
                        if rest.ends_with("asm") {
                            succeed(load_asm(rest, &mut prolog))
                        } else {
                            succeed(load_pro(rest, &mut prolog))
                        }
                    }
                    "vars" => succeed(print_vars(&mut prolog)),
                    "run" => succeed(run_and_output(&mut prolog)),
                    query if query.starts_with("?-") => {
                        match parse_and_compile_sentence(query, &mut prolog) {
                            Ok(term) => {
                                prolog.query = Some(term);
                                if prolog.ready_to_run() && prolog.immediate_execution {
                                    succeed(run_and_output(&mut prolog));
                                }
                            }
                            Err(err) => println!("{err}"),
                        }
                    }
                    program => match parse_and_compile_sentence(program, &mut prolog) {
                        Ok(compile_result) => {
                            prolog.program = Some(
                                compile_result
                                    .append_to_assembly(&mut prolog.assembly)
                                    .variables,
                            );
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

fn load_asm(path: &str, prolog: &mut PrologApp) -> Result<(), String> {
    let listing = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
    let assembly = compile_asm(&listing, &mut prolog.symbol_table)?;
    prolog.machine.load_assembly(&assembly);
    Ok(())
}

fn load_pro(path: &str, prolog: &mut PrologApp) -> Result<(), String> {
    let listing = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
    let mut assembly = Assembly::default();
    let sentences = parse_program(&listing, &mut prolog.symbol_table)?;
    let warnings = compile_sentences(sentences, &mut assembly)
        .map_err(|e| e.sym_to_str(&prolog.symbol_table))?;

    print_warnings(&warnings, prolog);

    prolog.machine.load_assembly(&assembly);
    Ok(())
}

fn parse_and_compile_sentence(input: &str, context: &mut PrologApp) -> Result<CompileInfo, String> {
    let program = parse_sentence(input, &mut context.symbol_table)?;
    let result = compile_sentence(program, &context.assembly.label_map)
        .map_err(|err| err.sym_to_str(&context.symbol_table))?;

    print_warnings(&result.warnings, context);

    Ok(result)
}

fn run_and_output(context: &mut PrologApp) -> Result<(), String> {
    let query_result = context.query.as_ref().ok_or("No query to run")?;
    context.machine.set_code(&context.assembly.instructions);

    let query_p = context.machine.append_code(&query_result.instructions);
    context.machine.set_p(query_p);
    context.machine.set_cp(context.machine.code_len().into());
    context.machine.set_var_mappings(&query_result.var_mapping);

    context.machine.execute().run()?;

    context.query_variables = context.machine.get_var_bindings();
    context.program_variables = VarBindings::default();

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
            Option::Some(&app.program_variables),
        )
    );
    Ok(())
}

fn print_warnings(warns: &[CompileWarning], app: &PrologApp) {
    for warn in warns {
        println!("WARNING: {}", to_display(warn, &app.symbol_table))
    }
}

fn succeed<T: ToString>(result: Result<(), T>) {
    match result {
        Ok(()) => {}
        Err(e) => println!("{}", e.to_string()),
    }
}
