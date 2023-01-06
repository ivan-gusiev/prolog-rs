extern crate prolog_rs;

use prolog_rs::{
    data::{Functor, RegPtr},
    instr::Instruction,
    printout, run_code, Machine,
};

fn main() {
    let h2 = Functor('h', 2);
    let f1 = Functor('f', 1);
    let p3 = Functor('p', 3);

    let x1 = RegPtr(1);
    let x2 = RegPtr(2);
    let x3 = RegPtr(3);
    let x4 = RegPtr(4);
    let x5 = RegPtr(5);

    let code = vec![
        Instruction::PutStructure(h2, x3),
        Instruction::SetVariable(x2),
        Instruction::SetVariable(x5),
        Instruction::PutStructure(f1, x4),
        Instruction::SetValue(x5),
        Instruction::PutStructure(p3, x1),
        Instruction::SetValue(x2),
        Instruction::SetValue(x3),
        Instruction::SetValue(x4),
    ];
    let mut machine = Machine::new();
    machine.set_code(&code);
    println!("CODE:");
    printout(&code);
    run_code(&mut machine);
    println!("\n REGISTERS:");
    printout(machine.iter_reg().as_slice());
    println!("\n HEAP:");
    printout(machine.iter_heap().as_slice());
}
