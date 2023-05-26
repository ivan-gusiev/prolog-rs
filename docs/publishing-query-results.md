# Publishing query results

The WAM book does not contain information about how to return query results, 
at least up to chapter 4. Because of this, reading the results back to customer
is one of the code areas that endured the most changes, and was quite buggy
for a long time.

This document shows the approaches taken for different versions of the machine.

## History for the language up to L1

The approach taken until and up to L1 was to track program registers
as well as machine heap during execution.
1. During compilation, establish the mapping `RegPtr -> VarName`
2. During execution, transform it to the `HeapPtr -> VarName` mapping
3. After the machine stops successfully, decompile the corresponding heap pointers, 
and assign them to the correct variable names.

To enable step 2, we used to attach a runtime hook to the machine, 
which binds the query variables to the heap just after the 
`call` instruction is executed. This way the register mapping survives 
the overwrites that happen during program execution.

Of course, with the introduction of rules this approach stops working,
because multiple `call` instructions may be executed during the query execution.
The interim solution would be to only call the hook on the first `call` instruction.

Unfortunately, this approach broke down completely for the multi-goal queries.
Because at this point we would overwrite registers on sucessive goals, and the first `call`
would most likely transition to a fact or a rule when all the registers are still
not initialized.

## Proposed solution for L2

We can protect the query variable registers by putting them all on stack.
This way no overwrites will occur, and reading the results will be much simpler.
Instead of a hook we will introduce a new variant of `deallocate`, 
called `publish`, which will perform exactly the same actions on the machine,
but will also bind the query variables to the corresponding heap locations.

1. Add a new storage area in the machine, `stack_mapping`, which will store the mappings
in the format of `(VarName, StackPtr, HeapPtr)`.
2. Add a new instruction `publish`, that:
    * performs exactly the same actions as `deallocate`
    * after that, mutate the `stack_mapping` to set the correct heap pointers.
3. Query compiler will use the `publish` instruction instead of `deallocate`.
4. All the variables in a query will be considered permanent (and live on stack).