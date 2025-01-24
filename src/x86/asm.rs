

// Inductive ireg: Type :=
//   | RAX | RBX | RCX | RDX | RSI | RDI | RBP | RSP
//   | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15.

use ascent::rayon::iter::Either;

use crate::ast::{BuiltinArg, BuiltinRes, Ident, Signature};

use super::op::Ptrofs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Ireg {
    RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP,
    R8, R9, R10, R11, R12, R13, R14, R15,
}

// (** Floating-point registers, i.e. SSE2 registers *)

// Inductive freg: Type :=
//   | XMM0  | XMM1  | XMM2  | XMM3  | XMM4  | XMM5  | XMM6  | XMM7
//   | XMM8  | XMM9  | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Freg {
    XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Crbit {
    Ceq, Cne, Clt, Cle, Cgt, Cge,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Preg {
    PC, Ir(Ireg), Fr(Freg), ST0, Cr(Crbit), RA
}


// Coercion IR: ireg >-> preg.
// Coercion FR: freg >-> preg.
// Coercion CR: crbit >-> preg.

impl From<Ireg> for Preg {
    fn from(ireg: Ireg) -> Preg {
        Preg::Ir(ireg)
    }
}

impl From<Freg> for Preg {
    fn from(freg: Freg) -> Preg {
        Preg::Fr(freg)
    }
}

impl From<Crbit> for Preg {
    fn from(crbit: Crbit) -> Preg {
        Preg::Cr(crbit)
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Addrmode {
    base: Option<Ireg>,
    ofs: Option<(Ireg, i64)>,
    constant: Either<i64, (Ident, Ptrofs)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Testcond {
    CondE, CondNe,
    CondB, CondBe, CondAe, CondA,
    CondL, CondLe, CondGe, CondG,
    CondP, CondNp,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
    Pmov_rr(Ireg, Ireg),
    Pmovl_ri(Ireg, i64),
    Pmovq_ri(Ireg, i64),
    Pmov_rs(Ireg, Ident),
    Pmovl_rm(Ireg, Addrmode),
    Pmovq_rm(Ireg, Addrmode),
    Pmovl_mr(Addrmode, Ireg),
    Pmovq_mr(Addrmode, Ireg),
    Pmovsd_ff(Freg, Freg),
    // Pmovsd_fi(Freg, f64),
    Pmovsd_fm(Freg, Addrmode),
    Pmovsd_mf(Addrmode, Freg),
    // Pmovss_fi(Freg, f32),
    Pmovss_fm(Freg, Addrmode),
    Pmovss_mf(Addrmode, Freg),
    Pfldl_m(Addrmode),
    Pfstpl_m(Addrmode),
    Pflds_m(Addrmode),
    Pfstps_m(Addrmode),
    Pmovb_mr(Addrmode, Ireg),
    Pmovw_mr(Addrmode, Ireg),
    Pmovzb_rr(Ireg, Ireg),
    Pmovzb_rm(Ireg, Addrmode),
    Pmovsb_rr(Ireg, Ireg),
    Pmovsb_rm(Ireg, Addrmode),
    Pmovzw_rr(Ireg, Ireg),
    Pmovzw_rm(Ireg, Addrmode),
    Pmovsw_rr(Ireg, Ireg),
    Pmovsw_rm(Ireg, Addrmode),
    Pmovzl_rr(Ireg, Ireg),
    Pmovsl_rr(Ireg, Ireg),
    Pmovls_rr(Ireg),
    Pcvtsd2ss_ff(Freg, Freg),
    Pcvtss2sd_ff(Freg, Freg),
    Pcvttsd2si_rf(Ireg, Freg),
    Pcvtsi2sd_fr(Freg, Ireg),
    Pcvttss2si_rf(Ireg, Freg),
    Pcvtsi2ss_fr(Freg, Ireg),
    Pcvttsd2sl_rf(Ireg, Freg),
    Pcvtsl2sd_fr(Freg, Ireg),
    Pcvttss2sl_rf(Ireg, Freg),
    Pcvtsl2ss_fr(Freg, Ireg),
    Pleal(Ireg, Addrmode),
    Pleaq(Ireg, Addrmode),
    Pnegl(Ireg),
    Pnegq(Ireg),
    Paddl_ri(Ireg, i64),
    Paddq_ri(Ireg, i64),
    Psubl_rr(Ireg, Ireg),
    Psubq_rr(Ireg, Ireg),
    Pimull_rr(Ireg, Ireg),
    Pimulq_rr(Ireg, Ireg),
    Pimull_ri(Ireg, i64),
    Pimulq_ri(Ireg, i64),
    Pimull_r(Ireg),
    Pimulq_r(Ireg),
    Pmull_r(Ireg),
    Pmulq_r(Ireg),
    Pcltd,
    Pcqto,
    Pdivl(Ireg),
    Pdivq(Ireg),
    Pidivl(Ireg),
    Pidivq(Ireg),
    Pandl_rr(Ireg, Ireg),
    Pandq_rr(Ireg, Ireg), 
    Pandl_ri(Ireg, i64),
    Pandq_ri(Ireg, i64),
    Porl_rr(Ireg, Ireg),
    Porq_rr(Ireg, Ireg),
    Porl_ri(Ireg, i64),
    Porq_ri(Ireg, i64),
    Pxorl_r(Ireg),
    Pxorq_r(Ireg),
    Pxorl_rr(Ireg, Ireg),
    Pxorq_rr(Ireg, Ireg),
    Pxorl_ri(Ireg, i64),
    Pxorq_ri(Ireg, i64),
    Pnotl(Ireg),
    Pnotq(Ireg),
    Psall_rcl(Ireg),
    Psalq_rcl(Ireg),
    Psall_ri(Ireg, i64),
    Psalq_ri(Ireg, i64),
    Pshrl_rcl(Ireg),
    Pshrq_rcl(Ireg),
    Pshrl_ri(Ireg, i64),
    Pshrq_ri(Ireg, i64),
    Psarl_rcl(Ireg),
    Psarq_rcl(Ireg),
    Psarl_ri(Ireg, i64),
    Psarq_ri(Ireg, i64),
    Pshld_ri(Ireg, Ireg, i64),
    Prorl_ri(Ireg, i64),
    Prorq_ri(Ireg, i64),
    Pcmpl_rr(Ireg, Ireg),
    Pcmpq_rr(Ireg, Ireg),
    Pcmpl_ri(Ireg, i64),
    Pcmpq_ri(Ireg, i64),
    Ptestl_rr(Ireg, Ireg),
    Ptestq_rr(Ireg, Ireg),
    Ptestl_ri(Ireg, i64),
    Ptestq_ri(Ireg, i64),
    Pcmov(Testcond, Ireg, Ireg),
    Psetcc(Testcond, Ireg),
    Paddd_ff(Freg, Freg),
    Psubd_ff(Freg, Freg),
    Pmuld_ff(Freg, Freg),
    Pdivd_ff(Freg, Freg),
    Pnegd(Freg),
    Pabsd(Freg),
    Pcomisd_ff(Freg, Freg),
    Pxorpd_f(Freg),
    Padds_ff(Freg, Freg),
    Psubs_ff(Freg, Freg),
    Pmuls_ff(Freg, Freg),
    Pdivs_ff(Freg, Freg),
    Pnegs(Freg),
    Pabss(Freg),
    Pcomiss_ff(Freg, Freg),
    Pxorps_f(Freg),
    Pjmp_l(Ident),
    Pjmp_s(Ident, Signature),
    Pjmp_r(Ireg, Signature),
    Pjcc(Testcond, Ident),
    Pjcc2(Testcond, Testcond, Ident),
    Pjmptbl(Ireg, Vec<Ident>),
    Pcall_s(Ident, Signature),
    Pcall_r(Ireg, Signature),
    Pret,
    Pmov_rm_a(Ireg, Addrmode),
    Pmov_mr_a(Addrmode, Ireg),
    Pmovsd_fm_a(Freg, Addrmode),
    Pmovsd_mf_a(Addrmode, Freg),
    Plabel(Ident),
    Pallocframe(i64, Ptrofs, Ptrofs),
    Pfreeframe(i64, Ptrofs, Ptrofs),
    Pbuiltin(Ident, Vec<BuiltinArg<Preg>>, BuiltinRes<Preg>),
    Padcl_ri(Ireg, i64),
    Padcl_rr(Ireg, Ireg),
    Paddl_mi(Addrmode, i64),
    Paddl_rr(Ireg, Ireg),
    Pbsfl(Ireg, Ireg),
    Pbsfq(Ireg, Ireg),
    Pbsrl(Ireg, Ireg),
    Pbsrq(Ireg, Ireg),
    Pbswap64(Ireg),
    Pbswap32(Ireg),
    Pbswap16(Ireg),
    Pcfi_adjust(i64),
    Pfmadd132(Freg, Freg, Freg),
    Pfmadd213(Freg, Freg, Freg),
    Pfmadd231(Freg, Freg, Freg),
    Pfmsub132(Freg, Freg, Freg),
    Pfmsub213(Freg, Freg, Freg),
    Pfmsub231(Freg, Freg, Freg),
    Pfnmadd132(Freg, Freg, Freg),
    Pfnmadd213(Freg, Freg, Freg),
    Pfnmadd231(Freg, Freg, Freg),
    Pfnmsub132(Freg, Freg, Freg),
    Pfnmsub213(Freg, Freg, Freg),
    Pfnmsub231(Freg, Freg, Freg),
    Pmaxsd(Freg, Freg),
    Pminsd(Freg, Freg),
    Pmovb_rm(Ireg, Addrmode),
    Pmovq_rf(Ireg, Freg),
    Pmovsq_mr(Addrmode, Freg),
    Pmovsq_rm(Freg, Addrmode),
    Pmovsb,
    Pmovsw,
    Pmovw_rm(Ireg, Addrmode),
    Pnop,
    Prep_movsl,
    Psbbl_rr(Ireg, Ireg),
    Psqrtsd(Freg, Freg),
    Psubl_ri(Ireg, i64),
    Psubq_ri(Ireg, i64),
}
