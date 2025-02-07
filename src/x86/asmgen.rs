use ascent::ascent;

use crate::ast;
use crate::x86::reg;
use crate::x86::op;
use crate::x86::mach;
use crate::ast::{Signature, Typ};
use either::Either;
use crate::x86::reg::Mreg;
use crate::ast::{Ident, MemoryChunk, ExternalFunction, BuiltinArg, BuiltinRes};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition};

use super::asm::{Ireg, Preg, Freg, Addrmode};


pub type Label = usize;

ascent! {
    pub struct MachO;

    relation mach_instruction(usize);

    relation mach_function_next_instruction(usize, usize);
    
    // ax_is_parent: bool, id of mach_instruction
    relation ax_is_parent(usize);
    index ax_is_parent ();

    // id + macho instruction
    relation get_stack(usize, Ptrofs, Typ, Mreg);
    index get_stack (3,);
    index get_stack (2,);

    relation set_stack(usize, Mreg, Ptrofs, Typ);
    index set_stack (1,);
    index set_stack (3,);

    relation get_param(usize, Ptrofs, Typ, Mreg);
    index get_param (0,);
    index get_param (1,);
    index get_param (0,2,);
    index get_param (2,);
    

    relation op(usize, Operation, Vec<Mreg>, Mreg);
    index op (0,);
    index op (1,);

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
    

    relation get_stack(usize, Ptrofs, Typ, Mreg) in mach_o;
    relation set_stack(usize, Mreg, Ptrofs, Typ) in mach_o;
    relation get_param(usize, Ptrofs, Typ, Mreg) in mach_o;
    relation op(usize, Operation, Vec<Mreg>, Mreg) in mach_o;
    relation ax_is_parent(usize) in mach_o;

    relation ir(Preg, Ireg);
    relation preg_of(Mreg, Preg);
    relation fr(Preg, Freg);

    relation archi_ptr64(usize);

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


    ir(Preg::Ir(*mr), mr) <-- preg_of(_, ?Preg::Ir(mr));
    fr(Preg::Fr(*mr), mr) <-- preg_of(_, ?Preg::Fr(mr));
    relation isst0(Preg, Preg);
    isst0(preg, preg) <-- preg_of(Mreg::FP0, preg);

    relation mk_mov(Preg, Preg);

    relation tranl_op(Operation, Vec<Mreg>, Mreg, usize);


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
  
    macro load_store_ind($mreg: ident, $preg: ident, $r: ident, $base: expr, $a:ident, $ptrofs: ident, $pred: ident) {
        preg_of($mreg, $preg),
        $pred($preg, $r),
        let $a =Addrmode{
            base: Some($base),
            ofs: None,
            constant: Either::Left(*$ptrofs as i64)
        }
    }

    // | Mgetstack ofs ty dst =>
    // loadind RSP ofs ty dst k
    relation pmovl_rm(Ireg, Addrmode);
    pmovl_rm(r, a) <-- 
        mach_o.get_stack(id, ptrofs, Typ::Tint, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RSP, a, ptrofs, ir);

        
    relation pmovq_rm(Ireg, Addrmode);
    pmovq_rm(r, addr) <-- 
        mach_o.get_stack(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ir);


    relation pmov_rm_a(Ireg, Addrmode);
    pmov_rm_a(r, addr) <--
        archi_ptr64(_),
        mach_o.get_stack(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ir);


    pmov_rm_a(r, addr) <--
        !archi_ptr64(_),
        mach_o.get_stack(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ir);


    // | Msetstack src ofs ty =>
    // storeind src RSP ofs ty k
    relation pmovl_mr(Addrmode, Ireg);
    pmovl_mr(addr, r) <--
        mach_o.set_stack(id, src, ptrofs, Typ::Tint),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ir);

    
    relation pmovq_mr(Addrmode, Ireg);
    pmovq_mr(addr, r) <--
        mach_o.set_stack(id, mreg, ptrofs, Typ::Tlong),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ir);

    relation pmov_mr_a(Addrmode, Ireg);
    pmov_mr_a(addr, r) <--
        archi_ptr64(_),
        mach_o.set_stack(id, mreg, ptrofs, Typ::Tany64),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ir);


    pmov_mr_a(addr, r) <--
        !archi_ptr64(_),
        mach_o.set_stack(id, mreg, ptrofs, Typ::Tany32),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ir);

    // | Mgetparam ofs ty dst =>
    // if ax_is_parent then
    //   loadind RAX ofs ty dst k

    // Definition loadind (base: ireg) (ofs: ptrofs) (ty: typ) (dst: mreg) (k: code) :=
    // let a := Addrmode (Some base) None (inl _ (Ptrofs.unsigned ofs)) in
    // match ty, preg_of dst with
    // | Tint, IR r => OK (Pmovl_rm r a :: k)
    pmovl_rm(r, a) <-- 
        mach_o.ax_is_parent(id),
        mach_o.get_param(id, ptrofs, Typ::Tint, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, ir);
    // | Tlong, IR r => OK (Pmovq_rm r a :: k)
    pmovq_rm(r, a) <--
    mach_o.ax_is_parent(id),
        mach_o.get_param(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, ir);
    // | Tsingle, FR r => OK (Pmovss_fm r a :: k)
    relation pmovss_fm(Freg, Addrmode);
    pmovss_fm(r, a) <--
        mach_o.ax_is_parent(id),
        mach_o.get_param(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, fr);
    // | Tsingle, ST0  => OK (Pflds_m a :: k)
    relation pflds_m(Addrmode);
    pflds_m(a) <--
        mach_o.ax_is_parent(id),
        mach_o.get_param(id, ptrofs, Typ::Tsingle, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, isst0);
    // | Tfloat, FR r => OK (Pmovsd_fm r a :: k)
    relation pmovsd_fm(Freg, Addrmode);
    pmovsd_fm(r, a) <--
        mach_o.ax_is_parent(id),
        mach_o.get_param(id, ptrofs, Typ::Tfloat, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, fr);
    // | Tfloat, ST0  => OK (Pfldl_m a :: k)
    relation pfldl_m(Addrmode);
    pfldl_m(a) <--
        mach_o.ax_is_parent(id),
        mach_o.get_param(id, ptrofs, Typ::Tfloat, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, isst0);
    // | Tany32, IR r => if Archi.ptr64 then Error (msg "Asmgen.loadind1") else OK (Pmov_rm_a r a :: k)
    relation pmov_rm_a(Ireg, Addrmode);
    pmov_rm_a(r, a) <--
        archi_ptr64(_),
        mach_o.get_param(id, ptrofs, Typ::Tany64, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, ir);
    // | Tany64, IR r => if Archi.ptr64 then OK (Pmov_rm_a r a :: k) else Error (msg "Asmgen.loadind2")
    pmov_rm_a(r, a) <--
        !archi_ptr64(_),
        mach_o.get_param(id, ptrofs, Typ::Tany32, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, ir);
    // | Tany64, FR r => OK (Pmovsd_fm_a r a :: k)
    relation pmovsd_fm_a(Freg, Addrmode);
    pmovsd_fm_a(r, a) <--
        mach_o.get_param(id, ptrofs, Typ::Tany64, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, fr);
    // | _, _ => Error (msg "Asmgen.loadind")
    // end.

        // else
    //   (do k1 <- loadind RAX ofs ty dst k;
    //    loadind RSP f.(fn_link_ofs) Tptr AX k1)
    // Todo

    // | Mop op args res =>
    // transl_op op args res k

    // | Omove, a1 :: nil =>
    // mk_mov (preg_of res) (preg_of a1) k
    mk_mov(preg_of_res, preg_of_a1) <--
        // relation op(usize, Operation, Vec<Mreg>, Mreg);
        mach_o.op(id, Operation::Omove, args, res),
        if (args.len() == 1),
        preg_of(res, preg_of_res),
        preg_of(args[0], preg_of_a1);
    
    // | Ointconst n, nil =>
    // do r <- ireg_of res;
    // OK ((if Int.eq_dec n Int.zero then Pxorl_r r else Pmovl_ri r n) :: k)
    relation pxorl_r(Ireg);

    pxorl_r(r) <--
        mach_o.op(id, Operation::Ointconst(0), args, res),
        ir(preg_od_res, r),
        preg_of(res,preg_od_res);

    relation pmovl_ri(Ireg, i64);
    pmovl_ri(r, n) <--
        mach_o.op(id, ?Operation::Ointconst(n), args, res),
        if n != 0,
        ir(preg_od_res, r),
        preg_of(res,preg_od_res);

    relation pxorq_r(Ireg);
    pxorq_r(r) <--
        mach_o.op(id, ?Operation::Olongconst(n), args, res),
        if n == 0,
        ir(preg_od_res, r),
        preg_of(res,preg_od_res);
    
    relation pmovq_ri(Ireg, i64);
    pmovq_ri(r, n) <--
        mach_o.op(id, ?Operation::Olongconst(n), args, res),
        if n != 0,
        ir(preg_od_res, r),
        preg_of(res,preg_od_res);

    

}

#[test]
fn translate_mach_asm_get_stack() {
    let mut macho = MachO::default();
    let mut prog = Asm::default();
    // get_param(usize, Ptrofs, Typ, Mreg);
    macho.get_stack = vec![(0, 0, Typ::Tint, Mreg::AX),
                            (1, 1, Typ::Tlong, Mreg::CX)].into_iter().collect();
    macho.ax_is_parent = vec![(0,)].into_iter().collect();
    macho.run();
    prog.run(&macho);
    println!("{:?}", prog.pflds_m);
}

#[test]
fn translate_mach_asm_set_stack() {
    let mut macho = MachO::default();
    let mut prog = Asm::default();
    // get_param(usize, Ptrofs, Typ, Mreg);
    macho.set_stack = vec![(0, Mreg::AX, 0, Typ::Tint),
                            (1, Mreg::CX, 1, Typ::Tlong)].into_iter().collect();
    macho.ax_is_parent = vec![(0,)].into_iter().collect();
    macho.run();
    prog.run(&macho);
    println!("{:?}", prog.pflds_m);
}