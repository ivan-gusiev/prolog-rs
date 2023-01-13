use std::fmt::{Debug, Display, Error, Write};

pub fn printout<T: Display>(prompt: &str, items: &[T]) {
    println!("{}", prompt);
    println!("--------");
    for (idx, item) in items.iter().enumerate() {
        println!("{:#03}\t{}", idx, item);
    }
}

pub fn writeout<T: Display>(items: &[T]) -> String {
    fn writeout_impl<T: Display>(items: &[T]) -> Result<String, Error> {
        let mut out = String::new();
        for (idx, item) in items.iter().enumerate() {
            writeln!(out, "{:#03}\t{}", idx, item)?;
        }
        Ok(out)
    }

    writeout_impl(items).unwrap_or_else(|e| format!("{}", e))
}

pub fn case<T: Display, U: Display>(input: T, output: U) -> String {
    format!("{}\n-----\n{}", input, output)
}

pub fn case_dbg<T: Debug, U: Debug>(input: T, output: U) -> String {
    format!("{:#?}\n-----\n{:#?}", input, output)
}
