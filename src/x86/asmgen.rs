use ascent::ascent;

use crate::ast;
use crate::x86::reg;
use crate::x86::op;
use crate::x86::mach;
use crate::ast::{Signature, Typ};
use either::Either;
use crate::x86::reg::Mreg;
use crate::ast::{Ident, MemoryChunk, ExternalFunction, BuiltinArg, BuiltinRes};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition, F64, F32};
use super::asm;
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
    index op ();

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

    relation preg_of(Mreg, Preg);
    relation ireg_of(Preg, Ireg);
    relation freg_of(Preg, Freg);

    relation archi_ptr64(usize);

    preg_of(Mreg::AX, Preg::ireg_of(Ireg::RAX));
    preg_of(Mreg::BX, Preg::ireg_of(Ireg::RBX));
    preg_of(Mreg::CX, Preg::ireg_of(Ireg::RCX));
    preg_of(Mreg::DX, Preg::ireg_of(Ireg::RDX));
    preg_of(Mreg::SI, Preg::ireg_of(Ireg::RSI));
    preg_of(Mreg::DI, Preg::ireg_of(Ireg::RDI));
    preg_of(Mreg::BP, Preg::ireg_of(Ireg::RBP));
    preg_of(Mreg::R8, Preg::ireg_of(Ireg::R8));
    preg_of(Mreg::R9, Preg::ireg_of(Ireg::R9));
    preg_of(Mreg::R10, Preg::ireg_of(Ireg::R10));
    preg_of(Mreg::R11, Preg::ireg_of(Ireg::R11));
    preg_of(Mreg::R12, Preg::ireg_of(Ireg::R12));
    preg_of(Mreg::R13, Preg::ireg_of(Ireg::R13));
    preg_of(Mreg::R14, Preg::ireg_of(Ireg::R14));
    preg_of(Mreg::R15, Preg::ireg_of(Ireg::R15));

    preg_of(Mreg::X0, Preg::freg_of(Freg::XMM0));
    preg_of(Mreg::X1, Preg::freg_of(Freg::XMM1));
    preg_of(Mreg::X2, Preg::freg_of(Freg::XMM2));
    preg_of(Mreg::X3, Preg::freg_of(Freg::XMM3));
    preg_of(Mreg::X4, Preg::freg_of(Freg::XMM4));
    preg_of(Mreg::X5, Preg::freg_of(Freg::XMM5));
    preg_of(Mreg::X6, Preg::freg_of(Freg::XMM6));
    preg_of(Mreg::X7, Preg::freg_of(Freg::XMM7));
    preg_of(Mreg::X8, Preg::freg_of(Freg::XMM8));
    preg_of(Mreg::X9, Preg::freg_of(Freg::XMM9));
    preg_of(Mreg::X10, Preg::freg_of(Freg::XMM10));
    preg_of(Mreg::X11, Preg::freg_of(Freg::XMM11));
    preg_of(Mreg::X12, Preg::freg_of(Freg::XMM12));
    preg_of(Mreg::X13, Preg::freg_of(Freg::XMM13));
    preg_of(Mreg::X14, Preg::freg_of(Freg::XMM14));
    preg_of(Mreg::X15, Preg::freg_of(Freg::XMM15));

    preg_of(Mreg::FP0, Preg::ST0);


    ireg_of(Preg::ireg_of(*mr), mr) <-- preg_of(_, ?Preg::ireg_of(mr));
    freg_of(Preg::freg_of(*mr), mr) <-- preg_of(_, ?Preg::freg_of(mr));
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
        load_store_ind!(mreg, preg, r, Ireg::RSP, a, ptrofs, ireg_of);

        
    relation pmovq_rm(Ireg, Addrmode);
    pmovq_rm(r, addr) <-- 
        mach_o.get_stack(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ireg_of);


    relation pmov_rm_a(Ireg, Addrmode);
    pmov_rm_a(r, addr) <--
        archi_ptr64(_),
        mach_o.get_stack(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ireg_of);


    pmov_rm_a(r, addr) <--
        !archi_ptr64(_),
        mach_o.get_stack(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ireg_of);


    // | Msetstack src ofs ty =>
    // storeind src RSP ofs ty k
    relation pmovl_mr(Addrmode, Ireg);
    pmovl_mr(addr, r) <--
        mach_o.set_stack(id, src, ptrofs, Typ::Tint),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ireg_of);

    
    relation pmovq_mr(Addrmode, Ireg);
    pmovq_mr(addr, r) <--
        mach_o.set_stack(id, mreg, ptrofs, Typ::Tlong),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ireg_of);

    relation pmov_mr_a(Addrmode, Ireg);
    pmov_mr_a(addr, r) <--
        archi_ptr64(_),
        mach_o.set_stack(id, mreg, ptrofs, Typ::Tany64),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ireg_of);


    pmov_mr_a(addr, r) <--
        !archi_ptr64(_),
        mach_o.set_stack(id, mreg, ptrofs, Typ::Tany32),
        load_store_ind!(mreg, preg, r, Ireg::RSP, addr, ptrofs, ireg_of);

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
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, ireg_of);
    // | Tlong, IR r => OK (Pmovq_rm r a :: k)
    pmovq_rm(r, a) <--
    mach_o.ax_is_parent(id),
        mach_o.get_param(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, ireg_of);
    // | Tsingle, FR r => OK (Pmovss_fm r a :: k)
    relation pmovss_fm(Freg, Addrmode);
    pmovss_fm(r, a) <--
        mach_o.ax_is_parent(id),
        mach_o.get_param(id, ptrofs, Typ::Tlong, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, freg_of);
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
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, freg_of);
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
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, ireg_of);
    // | Tany64, IR r => if Archi.ptr64 then OK (Pmov_rm_a r a :: k) else Error (msg "Asmgen.loadind2")
    pmov_rm_a(r, a) <--
        !archi_ptr64(_),
        mach_o.get_param(id, ptrofs, Typ::Tany32, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, ireg_of);
    // | Tany64, FR r => OK (Pmovsd_fm_a r a :: k)
    relation pmovsd_fm_a(Freg, Addrmode);
    pmovsd_fm_a(r, a) <--
        mach_o.get_param(id, ptrofs, Typ::Tany64, mreg),
        load_store_ind!(mreg, preg, r, Ireg::RAX, a, ptrofs, freg_of);
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
        ireg_of(preg_od_res, r),
        preg_of(res,preg_od_res);

    relation pmovl_ri(Ireg, i64);
    pmovl_ri(r, n) <--
        mach_o.op(id, ?Operation::Ointconst(n), args, res),
        if *n != 0,
        ireg_of(preg_od_res, r),
        preg_of(res,preg_od_res);

    relation pxorq_r(Ireg);
    pxorq_r(r) <--
        mach_o.op(id, ?Operation::Olongconst(n), args, res),
        if *n == 0,
        ireg_of(preg_od_res, r),
        preg_of(res,preg_od_res);
    
    relation pmovq_ri(Ireg, i64);
    pmovq_ri(r, n) <--
        mach_o.op(id, ?Operation::Olongconst(n), args, res),
        if *n != 0,
        ireg_of(preg_od_res, r),
        preg_of(res,preg_od_res);

    // | Ofloatconst f, nil =>
    // do r <- freg_of res;
    // OK ((if Float.eq_dec f Float.zero then Pxorpd_f r else Pmovsd_fi r f) :: k)
    relation pxorpd_f(Freg);
    pxorpd_f(r) <--
        mach_o.op(id, ?Operation::Ofloatconst(f), args, res),
        if *f != F64::from(0.0),
        preg_of(res, preg_od_res).,
        freg_of(preg_od_res, r);

    relation pmovsd_fi(Freg, F64);
    pmovsd_fi(r, f) <--
        mach_o.op(id, ?Operation::Ofloatconst(f), args, res),
        if *f != F64::from(0.0),
        preg_of(res, preg_od_res).,
        freg_of(preg_od_res, r);

    relation pxorps_f(Freg);
    pxorps_f(r) <--
        mach_o.op(id, ?Operation::Osingleconst(f), args, res),
        if *f == F32::from(0.0),
        preg_of(res, preg_od_res).,
        freg_of(preg_od_res, r);
    
    relation pmovss_fi(Freg, F32);
    pmovss_fi(r, f) <--
        mach_o.op(id, ?Operation::Osingleconst(f), args, res),
        if *f != F32::from(0.0),
        preg_of(res, preg_od_res).,
        freg_of(preg_od_res, r);

    // | Oindirectsymbol id, nil =>
    // do r <- ireg_of res;
    // OK (Pmov_rs r id :: k)
    
    relation pmov_rs(Ireg, Ident);
    pmov_rs(r, id) <--
        mach_o.op(opid, ?Operation::Oindirectsymbol(id), args, res),
        ireg_of(preg_od_res, r),
        preg_of(res,preg_od_res);

//     Definition low_ireg (r: ireg) : bool :=
//   match r with RAX | RBX | RCX | RDX => true | _ => false end.
    relation low_ireg(Ireg);
    low_ireg(Ireg::RAX);
    low_ireg(Ireg::RBX);
    low_ireg(Ireg::RCX);
    low_ireg(Ireg::RDX);

    // Definition mk_intconv (mk: ireg -> ireg -> instruction) (rd rs: ireg) (k: code) :=
    // if Archi.ptr64 || low_ireg rs then
    //   OK (mk rd rs :: k)
    // else
    //   OK (Pmov_rr RAX rs :: mk rd RAX :: k).
    // Todo: need to add sequence to make sure this happen after $mk 

    relation pmov_rr(Ireg, Ireg);


    macro mk_intconv($mk: expr, $rd: ident, $rs: ident, $k: ident) {
        $mk($rd, $preg_of_res) <--
            archi_ptr64(_),
            !low_ireg($rs);
            
        $mk($rd, $rs) <--
            !archi_ptr64(_),
            low_ireg($rs);

        $mk($rd, Ireg::RAX) <--
            !low_ireg($rs),
            !archi_ptr64(_);
        
        pmov_rr(Ireg::RAX, $rs);

    }
    

    // | Ocast8signed, a1 :: nil =>
    //     do r1 <- ireg_of a1; do r <- ireg_of res; mk_intconv Pmovsb_rr r r1 k
    relation pmovsb_rr(Ireg, Ireg);
    pmovsb_rr(r, r1) <--
        (archi_ptr64(_) || low_ireg(r)),
        mach_o.op(id, ?Operation::Ocast8signed, args, res),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(res, preg_of_res),
        ireg_of(preg_of_res, r),
        preg_of(a1, preg_of_a1),
        ireg_of(preg_of_a1, r1);

    
    //   OK (Pmov_rr RAX rs :: mk rd RAX :: k).
    pmov_rr(Ireg::RAX, r1) <--
        !archi_ptr64(_),
        mach_o.op(id, ?Operation::Ocast8signed, args, res),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(res, preg_of_res),
        ireg_of(preg_of_res, r),
        preg_of(a1, preg_of_a1),
        ireg_of(preg_of_a1, r1),
        !low_ireg(r);
    

    pmovsb_rr(r, Ireg::RAX) <--
        !archi_ptr64(_),
        mach_o.op(id, ?Operation::Ocast8signed, args, res),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(res, preg_of_res),
        ireg_of(preg_of_res, r),
        preg_of(a1, preg_of_a1),
        ireg_of(preg_of_a1, r1),
        !low_ireg(r);

    // | Ocast8unsigned, a1 :: nil =>
    //     do r1 <- ireg_of a1; do r <- ireg_of res; mk_intconv Pmovzb_rr r r1 k
    // | Ocast16signed, a1 :: nil =>
    //     do r1 <- ireg_of a1; do r <- ireg_of res; OK (Pmovsw_rr r r1 :: k)

    relation pmovzb_rr(Ireg, Ireg);

    pmovzb_rr(r, r1) <--
        mach_o.op(id, ?Operation::Ocast8unsigned, args, res),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(res, preg_of_res),
        ireg_of(preg_of_res, r),
        preg_of(a1, preg_of_a1),
        ireg_of(preg_of_a1, r1);

    relation pmovsw_rr(Ireg, Ireg);
    pmovsw_rr(r, r1) <--
        mach_o.op(id, ?Operation::Ocast16signed, args, res),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(res, preg_of_res),
        ireg_of(preg_of_res, r),
        preg_of(a1, preg_of_a1),
        ireg_of(preg_of_a1, r1);
    

    // Definition transl_addressing (a: addressing) (args: list mreg): res addrmode :=
    // match a, args with
    // | Aindexed n, a1 :: nil =>
    //     do r1 <- ireg_of a1; OK(Addrmode (Some r1) None (inl _ n))
    // | Aindexed2 n, a1 :: a2 :: nil =>
    //     do r1 <- ireg_of a1; do r2 <- ireg_of a2;
    //     OK(Addrmode (Some r1) (Some(r2, 1)) (inl _ n))
    // | Ascaled sc n, a1 :: nil =>
    //     do r1 <- ireg_of a1; OK(Addrmode None (Some(r1, sc)) (inl _ n))
    // | Aindexed2scaled sc n, a1 :: a2 :: nil =>
    //     do r1 <- ireg_of a1; do r2 <- ireg_of a2;
    //     OK(Addrmode (Some r1) (Some(r2, sc)) (inl _ n))
    // | Aglobal id ofs, nil =>
    //     OK(Addrmode None None (inr _ (id, ofs)))
    // | Abased id ofs, a1 :: nil =>
    //     do r1 <- ireg_of a1; OK(Addrmode (Some r1) None (inr _ (id, ofs)))
    // | Abasedscaled sc id ofs, a1 :: nil =>
    //     do r1 <- ireg_of a1; OK(Addrmode None (Some(r1, sc)) (inr _ (id, ofs)))
    // | Ainstack n, nil =>
    //     OK(Addrmode (Some RSP) None (inl _ (Ptrofs.signed n)))
    // | _, _ =>
    //     Error(msg "Asmgen.transl_addressing")
    // end.

    relation transl_addressing(bool, Addressing, Vec<Mreg>, Addrmode);
    // transl_addressing(end?, Addressing, Vec<Mreg>, Addrmode);
    transl_addressing(true, Addressing::Aindexed(*n), args, res) <--
        transl_addressing(false, ?Addressing::Aindexed(n), args, _),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(a, preg_of_a),
        ireg_of(preg_of_a, r1),
        let res = Addrmode{
            base: Some(*r1),
            ofs: None,
            constant: Either::Left(*n as i64)
        };
    
    // | Aindexed2 n, a1 :: a2 :: nil =>
    //     do r1 <- ireg_of a1; do r2 <- ireg_of a2;
    //     OK(Addrmode (Some r1) (Some(r2, 1)) (inl _ n))
    transl_addressing(true, Addressing::Aindexed2(*n), args, res) <--
        transl_addressing(false, ?Addressing::Aindexed2(n), args, _),
        if (args.len() == 2),
        let a1 = args[0],
        let a2 = args[1],
        preg_of(a, preg_of_a),
        ireg_of(preg_of_a, r1),
        ireg_of(preg_of_a, r2),
        let res = Addrmode{
            base: Some(*r1),
            ofs: Some((*r2, 1)),
            constant: Either::Left(*n as i64)
        };


    // | Ascaled sc n, a1 :: nil =>
    //     do r1 <- ireg_of a1; OK(Addrmode None (Some(r1, sc)) (inl _ n))
    transl_addressing(true, Addressing::Ascaled(*sc, *n), args, res) <--
        transl_addressing(false, ?Addressing::Ascaled(sc, n), args, _),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(a, preg_of_a),
        ireg_of(preg_of_a, r1),
        let res = Addrmode{
            base: None,
            ofs: Some((*r1, *sc)),
            constant: Either::Left(*n as i64)
        };
    

    // | Aindexed2scaled sc n, a1 :: a2 :: nil =>
    //     do r1 <- ireg_of a1; do r2 <- ireg_of a2;
    //     OK(Addrmode (Some r1) (Some(r2, sc)) (inl _ n))
    transl_addressing(true, Addressing::Aindexed2scaled(*sc, *n), args, res) <--
        transl_addressing(false, ?Addressing::Aindexed2scaled(sc, n), args, _),
        if (args.len() == 2),
        let a1 = args[0],
        let a2 = args[1],
        preg_of(a, preg_of_a),
        ireg_of(preg_of_a, r1),
        ireg_of(preg_of_a, r2),
        let res = Addrmode{
            base: Some(*r1),
            ofs: Some((*r2, *sc)),
            constant: Either::Left(*n as i64)
        };

    
    // | Aglobal id ofs, nil =>
    //     OK(Addrmode None None (inr _ (id, ofs)))
    transl_addressing(true, Addressing::Aglobal(*id, *ofs), args, res) <--
        transl_addressing(false, ?Addressing::Aglobal(id, ofs), args, _),
        if (args.len() == 0),
        let res = Addrmode{
            base: None,
            ofs: None,
            constant: Either::Right((*id, *ofs))
        };

    // | Abased id ofs, a1 :: nil =>
    //     do r1 <- ireg_of a1; OK(Addrmode (Some r1) None (inr _ (id, ofs)))
    transl_addressing(true, Addressing::Abased(*id, *ofs), args, res) <--
        transl_addressing(false, ?Addressing::Abased(id, ofs), args, _),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(a, preg_of_a),
        ireg_of(preg_of_a, r1),
        let res = Addrmode{
            base: Some(*r1),
            ofs: None,
            constant: Either::Right((*id, *ofs))
        };

    
    // | Abasedscaled sc id ofs, a1 :: nil =>
    //     do r1 <- ireg_of a1; OK(Addrmode None (Some(r1, sc)) (inr _ (id, ofs)))
    transl_addressing(true, Addressing::Abasedscaled(*sc, *id, *ofs), args, res) <--
        transl_addressing(false, ?Addressing::Abasedscaled(sc, id, ofs), args, _),
        if (args.len() == 1),
        let a1 = args[0],
        preg_of(a, preg_of_a),
        ireg_of(preg_of_a, r1),
        let res = Addrmode{
            base: None,
            ofs: Some((*r1, *sc)),
            constant: Either::Right((*id, *ofs))
        };


    // | Ainstack n, nil =>
    //     OK(Addrmode (Some RSP) None (inl _ (Ptrofs.signed n)))
    transl_addressing(true, Addressing::Ainstack(*n), args, res) <--
        transl_addressing(false, ?Addressing::Ainstack(n), args, _),
        if (args.len() == 0),
        let res = Addrmode{
            base: Some(Ireg::RSP),
            ofs: None,
            constant: Either::Left(*n as i64)
        };

    //   | Oleal addr, _ =>
    //       do am <- transl_addressing addr args; do r <- ireg_of res;
    //       OK (match normalize_addrmode_64 am with
    //           | (am', None)       => Pleaq r am' :: k
    //           | (am', Some delta) => Pleaq r am' :: Paddq_ri r delta :: k
    //           end)
    
    // Definition offset_in_range (n: Z) : bool := 
    // zle Int.min_signed n && zle n Int.max_signed.

    // Definition normalize_addrmode_64 (a: addrmode) :=
    // match a with
    // | Addrmode base ofs (inl n) =>
    //     if Op.offset_in_range n
    //     then (a, None)
    //     else (Addrmode base ofs (inl _ 0), Some (Int64.repr n))
    // | Addrmode base ofs (inr (id, delta)) =>
    //     if Op.ptroffset_in_range delta || negb Archi.ptr64
    //     then (a, None)
    //     else (Addrmode base ofs (inr _ (id, Ptrofs.zero)), Some (Ptrofs.to_int64 delta))
    // end.

    relation normalize_addrmode_64(bool, Addrmode, (Addrmode, Option<i64>));
    normalize_addrmode_64(true, Addrmode { base:*base, ofs:*ofs, constant: Either::Left(*n) },
                                (Addrmode { base:*base, ofs:*ofs, constant: Either::Left(*n) }, None)) <--
            normalize_addrmode_64(false, ?Addrmode { base, ofs, constant: Either::Left(n) }, _),
            if (*n >= i64::MIN && *n <= i64::MAX);
    
    normalize_addrmode_64(true, Addrmode { base:*base, ofs:*ofs, constant: Either::Left(*n) },
                                (Addrmode { base:*base, ofs:*ofs, constant: Either::Left(0) }, Some(*n as i64))) <--
            normalize_addrmode_64(false, ?Addrmode { base, ofs, constant: Either::Left(n) }, _),
            if (*n <= i64::MIN && *n >= i64::MAX);
    
    normalize_addrmode_64(true, Addrmode { base:*base, ofs:*ofs, constant: Either::Right(*n) },
        (Addrmode { base:*base, ofs:*ofs, constant: Either::Left(0) }, Some(*n as i64))) <--
        normalize_addrmode_64(false, ?Addrmode { base, ofs, constant: Either::Left(n) }, _),
        if (*n <= i64::MIN && *n >= i64::MAX);

    // Definition ptroffset_in_range (n: ptrofs) : bool := 
    // let n := Ptrofs.signed n in zle ptroffset_min n && zle n ptroffset_max.

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