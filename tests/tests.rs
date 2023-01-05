extern crate prolog_rs;

#[cfg(test)]
mod tests {
    use prolog_rs::{run_code, Data, Functor, HeapPtr, Instruction, Machine, Ref, RegPtr, Str};

    fn str(heap: usize) -> Data {
        Data::Str(Str(HeapPtr(heap)))
    }

    fn func(f: Functor) -> Data {
        Data::Functor(f)
    }

    fn refr(heap: usize) -> Data {
        Data::Ref(Ref(HeapPtr(heap)))
    }

    #[test]
    fn query_compiles_to_bytecode() {
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
        run_code(&mut machine);

        let expected_heap = vec![
            str(1),
            func(h2),
            refr(2),
            refr(3),
            str(5),
            func(f1),
            refr(3),
            str(8),
            func(p3),
            refr(2),
            str(1),
            str(5),
        ];

        let actual_heap = machine.iter_heap().map(|x| *x).collect::<Vec<_>>();

        assert_eq!(expected_heap.as_slice(), actual_heap.as_slice())
    }
}
