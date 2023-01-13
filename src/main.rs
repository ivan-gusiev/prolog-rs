use prolog_rs::Machine;

extern crate prolog_rs;

fn main() {
    use std::io::{stdin, stdout, Write};

    fn input(prompt: &str) -> String {
        print!("{}", prompt);
        let _ = stdout().flush();
        let mut s = String::new();
        stdin().read_line(&mut s).expect("Failed to read!");
        s
    }

    let mut machine = Option::<Machine>::None;
    loop {
        match input("> ").trim() {
            "exit" => break,
            "" => (),
            query if query.starts_with("?-") => match parse_and_run_query(query) {
                Ok(new_machine) => machine = Some(new_machine),
                Err(err) => println!("{}", err),
            },
            program if machine.is_some() => {
                match parse_and_run_program(program, &mut machine.as_mut().unwrap()) {
                    Ok(()) => (),
                    Err(err) => println!("{}", err),
                }
            }
            _ => println!("Please enter the query first!"),
        }
    }
}

fn parse_and_run_query(input: &str) -> Result<Machine, String> {
    use prolog_rs::{compile::compile_query, lang::parse_term, run_code};

    let query = parse_term(input.trim_start_matches("?-"))?;

    let code = compile_query(query);

    let mut machine = Machine::new();
    machine.set_code(&code);
    run_code(&mut machine).map_err(|e| e.message())?;

    println!("{}", machine.dbg());

    Ok(machine)
}

fn parse_and_run_program(input: &str, machine: &mut Machine) -> Result<(), String> {
    use prolog_rs::{compile::compile_program, lang::parse_term, run_code};

    let query = parse_term(input)?;

    let code = compile_program(query);

    machine.set_code(&code);
    run_code(machine).map_err(|e| e.message())?;

    println!("{}", machine.dbg());

    Ok(())
}
