use ascent::ascent;

use crate::util::Either;
use crate::ast;
use crate::x86::reg;
use crate::x86::op;
use crate::x86::mach;
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
    
    // ax_is_parent: bool, id of mach_instruction
    relation ax_is_parent(usize);

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

    extern database MachO mach_o();

    relation ir(Mreg, Ireg);
    relation preg_of(Mreg, Preg);
    relation fr(Mreg, Freg);

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




    ir(r, mr) <-- preg_of(r, ?Preg::Ir(mr));
    fr(r, fr) <-- preg_of(r, ?Preg::Fr(fr));

    relation mk_mov(Preg, Preg, usize);

    relation tranl_op(Operation, Vec<Mreg>, Mreg, usize);

    // Definition transl_instr (f: Mach.function) (i: Mach.instruction)
    //                     (ax_is_parent: bool) (k: code) :=
    //     match i with
    //     | Mgetstack ofs ty dst =>
    //         loadind RSP ofs ty dst k




    // Definition transl_instr (f: Mach.function) (i: Mach.instruction)
    // (ax_is_parent: bool) (k: code) :=
    //     match i with

    //     | Mgetstack ofs ty dst =>
    //     loadind RSP ofs ty dst k


    // Definition loadind (base: ireg) (ofs: ptrofs) (ty: typ) (dst: mreg) (k: code) :=
    // let a := Addrmode (Some base) None (inl _ (Ptrofs.unsigned ofs)) in
    // match ty, preg_of dst with
    // | Tint, IR r => OK (Pmovl_rm r a :: k)
    // | Tlong, IR r => OK (Pmovq_rm r a :: k)
    // | Tsingle, FR r => OK (Pmovss_fm r a :: k)
    // | Tsingle, ST0  => OK (Pflds_m a :: k)
    // | Tfloat, FR r => OK (Pmovsd_fm r a :: k)
    // | Tfloat, ST0  => OK (Pfldl_m a :: k)
    // | Tany32, IR r => if Archi.ptr64 then Error (msg "Asmgen.loadind1") else OK (Pmov_rm_a r a :: k)
    // | Tany64, IR r => if Archi.ptr64 then OK (Pmov_rm_a r a :: k) else Error (msg "Asmgen.loadind2")
    // | Tany64, FR r => OK (Pmovsd_fm_a r a :: k)
    // | _, _ => Error (msg "Asmgen.loadind")
    // end.
    relation pmovl_rm(Ireg, Addrmode);
    pmovl_rm(r, addrmode) <-- 
        mach_o.get_stack(id, ptrofs, Tint, mreg),
        preg_of(mreg, r),
        ir(r, mreg),
        let addrmode= Addrmode{
            base: Some(Ireg::RSP),
            ofs: None,
            constant: Either(0, ptrofs),
        };
    relation pmovl_rm(Ireg, Addrmode);
    pmovq_rm(r, addrmode) <-- 
        mach_o.get_stack(id, ptrofs, Tlong, mreg),
        preg_of(mreg, r),
        ir(r, mreg),
        let addrmode= Addrmode{
            base: Some(Ireg::RSP),
            ofs: None,
            constant: Either(0, ptrofs),
        };
    // Archi.ptr64?


    // | Msetstack src ofs ty =>
    // storeind src RSP ofs ty k

    // Definition storeind (src: mreg) (base: ireg) (ofs: ptrofs) (ty: typ) (k: code) :=
    // let a := Addrmode (Some base) None (inl _ (Ptrofs.unsigned ofs)) in
    // match ty, preg_of src with
    // | Tint, IR r => OK (Pmovl_mr a r :: k)
    // | Tlong, IR r => OK (Pmovq_mr a r :: k)
    // | Tsingle, FR r => OK (Pmovss_mf a r :: k)
    // | Tsingle, ST0 => OK (Pfstps_m a :: k)
    // | Tfloat, FR r => OK (Pmovsd_mf a r :: k)
    // | Tfloat, ST0 => OK (Pfstpl_m a :: k)
    // | Tany32, IR r => if Archi.ptr64 then Error (msg "Asmgen.storeind1") else OK (Pmov_mr_a a r :: k)
    // | Tany64, IR r => if Archi.ptr64 then OK (Pmov_mr_a a r :: k) else Error (msg "Asmgen.storeind2")
    // | Tany64, FR r => OK (Pmovsd_mf_a a r :: k)
    // | _, _ => Error (msg "Asmgen.storeind")
    // end.
    relation pmovl_mr(Addrmode, Ireg);
    pmovl_mr(a, r) <--
        mach_o.set_stack(id, mreg, ptrofs, Tint),
        preg_of(mreg, r),
        ir(r, mreg),
        let addrmode= Addrmode{
            base: Some(Ireg::RSP),
            ofs: None,
            constant: Either(0, ptrofs),
        };

    relation pmovq_mr(Addrmode, Ireg);
    pmovq_mr(a, r) <--
        mach_o.set_stack(id, mreg, ptrofs, Tlong),
        preg_of(mreg, r),
        ir(r, mreg),
        let addrmode= Addrmode{
            base: Some(Ireg::RSP),
            ofs: None,
            constant: Either(0, ptrofs),
        };
    // Archi.ptr64?


}



// #[test]
// fn translate_mach_asm() {
//     util::test_command("sh", &["test_scripts/test_printMach.sh"]);
//     let data = util::read_file("sample.mach");
//     // test_command("rm", &["-rf", "sample.mach", "test_scripts/a.out"]);   
//     let mut program = ast::Program::from(data);
//     let mut functions = program.prog_defs;
//     let mut instr_vec = Vec::new();
//     for (ident, globedef) in &mut functions {
//         // Identifier, GlobDef<F, V>
//         let function = match globedef {
//             ast::GlobDef::GFun(function) => function,
//             // Also need to fix in printmach.ml
//             _ => continue,
//         };
//         let instrs = &function.fn_code;
//         for instr in instrs {
//             match instr {
//                 ast::Instruction::IReg(_, _, _) => {
//                     instr_vec.push(instr.clone());
//                 }
//                 _ => continue,
//             }
//         }
//     }
//     let mut prog = Mach2Asm::default();

//     prog.instr = instr_vec;
//     prog.run();

// }