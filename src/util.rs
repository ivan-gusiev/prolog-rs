use std::fmt::Display;

pub fn printout<T: Display>(prompt: &str, items: &[T]) {
    println!("{}", prompt);
    println!("--------");
    for (idx, item) in items.iter().enumerate() {
        println!("{:#03}\t{}", idx, item);
    }
}
