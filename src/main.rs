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

    loop {
        match input("?- ").trim() {
            "exit" => break,
            "" => (),
            query => match parse_and_run_query(query) {
                Ok(()) => (),
                Err(err) => println!("{}", err),
            },
        }
    }
}

fn parse_and_run_query(input: &str) -> Result<(), String> {
    use prolog_rs::{compile::compile_query, lang::parse_term, run_code, util::printout, Machine};

    let query = parse_term(input)?;

    let code = compile_query(query);
    printout("CODE:", &code);

    let mut machine = Machine::new();
    machine.set_code(&code);
    run_code(&mut machine);

    printout("REGISTERS:", &machine.iter_reg().collect::<Vec<_>>());
    printout("DATA:", &machine.iter_heap().collect::<Vec<_>>());

    Ok(())
}
