use ascent::ascent;

use crate::util::Either;
use crate::ast::{Signature, Typ};
use crate::x86::reg::Mreg;
use crate::ast::{Ident, MemoryChunk, ExternalFunction, BuiltinArg, BuiltinRes};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition};


pub type Label = usize;

ascent! {
    pub struct MachO;

    relation mach_instruction(usize);

    relation mach_function_next_instruction(usize, usize);
    // id + macho instruction
    relation get_stack(usize, Ptrofs, Typ, Mreg);
    relation set_stack(usize, Mreg, Ptrofs, Typ);
    relation get_param(usize, Ptrofs, Typ, Mreg);
    relation op(usize, Operation, Vec<Mreg>, Mreg); 
    relation load(usize, MemoryChunk, Addressing, Vec<Mreg>, Mreg);
    relation store(usize, MemoryChunk, Addressing, Vec<Mreg>, Mreg);
    relation call(usize, Signature, Either<Mreg, Ident>);
    relation tail_call(usize, Signature, Either<Mreg, Ident>);
    relation builtin(usize, ExternalFunction, Vec<BuiltinArg<Mreg>>, BuiltinRes<Mreg>);
    relation label(usize, Label);
    relation goto(usize, Label);
    relation cond(usize, Condition, Vec<Mreg>, Label);
    relation jump_table(usize, Mreg, Vec<Label>);
    relation ret(usize);

    relation mach_func(usize, Signature, i32, Ptrofs, Ptrofs);
    relation code_in_func(usize, usize);
    relation internal_func(usize);
    relation external_func(usize, ExternalFunction);

    // relation gloabl_def(usize, &'static str); // static variable/fun

    // seems in MachO each program is a function
    relation program_entry(usize);
    relation func_in_program(usize, usize);
}
