use ascent::ascent;

use crate::util::Either;
use crate::ast::{Signature, Typ};
use crate::x86::reg::Mreg;
use crate::ast::{Ident, MemoryChunk, ExternalFunction, BuiltinArg, BuiltinRes};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition};

use super::asm::{Ireg, Preg, Freg};


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
    relation ireg_of(Mreg);
}


ascent! {
    pub struct Asm;

    extern database MachO mach_o;

    relation ireg_of(Mreg, Ireg);
    relation preg_of(Mreg, Preg);
    relation freg_of(Mreg, Freg);

    preg_of(Mreg::AX, Preg::Ir(Ireg::RAX));
    preg_of(Mreg::BX, Preg::Ir(Ireg::RBX));
    preg_of(Mreg::CX, Preg::Ir(Ireg::RCX));
    preg_of(Mreg::DX, Preg::Ir(Ireg::RDX));
    preg_of(Mreg::SI, Preg::Ir(Ireg::RSI));
    preg_of(Mreg::DI, Preg::Ir(Ireg::RDI));
    preg_of(Mreg::BP, Preg::Ir(Ireg::RBP));
    preg_of(Mreg::R8, Preg::Ir(Ireg::R8));
    preg_of(Mreg::R9, Preg::Ir(Ireg::R9));
    preg_of(Mreg::R10, Preg::Ir(Ireg::R10));
    preg_of(Mreg::R11, Preg::Ir(Ireg::R11));
    preg_of(Mreg::R12, Preg::Ir(Ireg::R12));
    preg_of(Mreg::R13, Preg::Ir(Ireg::R13));
    preg_of(Mreg::R14, Preg::Ir(Ireg::R14));
    preg_of(Mreg::R15, Preg::Ir(Ireg::R15));
    preg_of(Mreg::X0, Preg::Fr(Freg::XMM0));
    preg_of(Mreg::X1, Preg::Fr(Freg::XMM1));
    preg_of(Mreg::X2, Preg::Fr(Freg::XMM2));
    preg_of(Mreg::X3, Preg::Fr(Freg::XMM3));
    preg_of(Mreg::X4, Preg::Fr(Freg::XMM4));
    preg_of(Mreg::X5, Preg::Fr(Freg::XMM5));
    preg_of(Mreg::X6, Preg::Fr(Freg::XMM6));
    preg_of(Mreg::X7, Preg::Fr(Freg::XMM7));
    preg_of(Mreg::X8, Preg::Fr(Freg::XMM8));
    preg_of(Mreg::X9, Preg::Fr(Freg::XMM9));
    preg_of(Mreg::X10, Preg::Fr(Freg::XMM10));
    preg_of(Mreg::X11, Preg::Fr(Freg::XMM11));
    preg_of(Mreg::X12, Preg::Fr(Freg::XMM12));
    preg_of(Mreg::X13, Preg::Fr(Freg::XMM13));
    preg_of(Mreg::X14, Preg::Fr(Freg::XMM14));
    preg_of(Mreg::X15, Preg::Fr(Freg::XMM15));
    preg_of(Mreg::FP0, Preg::ST0);




    ireg_of(r, mr) <-- preg_of(r, ?Preg::Ir(mr));
    freg_of(r, fr) <-- preg_of(r, ?Preg::Fr(fr));

    relation mk_mov(Preg, Preg, usize);

    relation tranl_op(Operation, Vec<Mreg>, Mreg, usize);


    // relation 
}
