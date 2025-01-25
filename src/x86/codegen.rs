use ascent::ascent;

use crate::util::Either;
use crate::ast::{Signature, Typ};
use crate::x86::reg::Mreg;
use crate::ast::{Ident, MemoryChunk, ExternalFunction, BuiltinArg, BuiltinRes};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition};
use crate::x86::mach::Function as MachFunction;
use crate::x86::mach::Instruction as MachInstruction;

pub type Label = usize;

ascent! {
    pub struct MachO;


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

    relation transl_instr(MachFunction, MachInstruction, bool, Vec<MachInstruction>);

    // Definition transl_instr (f: Mach.function) (i: Mach.instruction)
    // (ax_is_parent: bool) (k: code) :=
    //     match i with
    //     | Mgetstack ofs ty dst =>
    //     loadind RSP ofs ty dst k
    //     | Msetstack src ofs ty =>
    //     storeind src RSP ofs ty k
    //     | Mgetparam ofs ty dst =>
    //     if ax_is_parent then
    //     loadind RAX ofs ty dst k
    //     else
    //     (do k1 <- loadind RAX ofs ty dst k;
    //     loadind RSP f.(fn_link_ofs) Tptr AX k1)
    //     | Mop op args res =>
    //     transl_op op args res k
    //     | Mload chunk addr args dst =>
    //     transl_load chunk addr args dst k
    //     | Mstore chunk addr args src =>
    //     transl_store chunk addr args src k
    //     | Mcall sig (inl reg) =>
    //     do r <- ireg_of reg; OK (Pcall_r r sig :: k)
    //     | Mcall sig (inr symb) =>
    //     OK (Pcall_s symb sig :: k)
    //     | Mtailcall sig (inl reg) =>
    //     do r <- ireg_of reg;
    //     OK (Pfreeframe f.(fn_stacksize) f.(fn_retaddr_ofs) f.(fn_link_ofs) ::
    //     Pjmp_r r sig :: k)
    //     | Mtailcall sig (inr symb) =>
    //     OK (Pfreeframe f.(fn_stacksize) f.(fn_retaddr_ofs) f.(fn_link_ofs) ::
    //     Pjmp_s symb sig :: k)
    //     | Mlabel lbl =>
    //     OK(Plabel lbl :: k)
    //     | Mgoto lbl =>
    //     OK(Pjmp_l lbl :: k)
    //     | Mcond cond args lbl =>
    //     transl_cond cond args (mk_jcc (testcond_for_condition cond) lbl k)
    //     | Mjumptable arg tbl =>
    //     do r <- ireg_of arg; OK (Pjmptbl r tbl :: k)
    //     | Mreturn =>
    //     OK (Pfreeframe f.(fn_stacksize) f.(fn_retaddr_ofs) f.(fn_link_ofs) ::
    //     Pret :: k)
    //     | Mbuiltin ef args res =>
    //     OK (Pbuiltin ef (List.map (map_builtin_arg preg_of) args) (map_builtin_res preg_of res) :: k)
    //     end.

}
