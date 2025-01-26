use crate::ast::{Ident, Typ, Z};

use super::reg::Mreg;

pub type Ptrofs = u64;


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Condition {
    Ceq,
    Cne,
    Clt,
    Cle,
    Cgt,
    Cge,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Addressing {
    Aindexed(Z),
    Aindexed2(Z),
    Ascaled(Z, Z),
    Aindexed2scaled(Z, Z),
    Aglobal(Ident, Ptrofs),
    Abased(Ident, Ptrofs),
    Abasedscaled(Z, Ident, Ptrofs),
    Ainstack(Ptrofs)
}

// impl hash for f64 and f32
#[derive(Debug, Clone, Copy)]
pub struct F64(f64);

impl std::hash::Hash for F64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.0.to_bits());
    }
}

// impl eq if < 1e-6
impl PartialEq for F64 {
    fn eq(&self, other: &Self) -> bool {
        (self.0 - other.0).abs() < 1e-6
    }
}
impl Eq for F64 {}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct F32(f32);

impl Eq for F32 {}

impl std::hash::Hash for F32 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u32(self.0.to_bits());
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Operation {
    Omove(Mreg),
    Ointconst(i64),
    Olongconst(i64),
    Ofloatconst(F64),
    Osingleconst(F32),
    Oindirectsymbol(Ident),
    Ocast8signed,
    Ocast8unsigned,
    Ocast16signed,
    Ocast16unsigned,
    Oneg,
    Osub,
    Omul,
    Omulimm(i64),
    Omulhs,
    Omulhu,
    Odiv,
    Odivu,
    Omod,
    Omodu,
    Oand,
    Oandimm(i64),
    Oor,
    Oorimm(i64),
    Oxor,
    Oxorimm(i64),
    Onot,
    Oshl,
    Oshlimm(i64),
    Oshr,
    Oshrimm(i64),
    Oshrximm(i64),
    Oshru,
    Oshruimm(i64),
    Ororimm(i64),
    Oshldimm(i64),
    Olea(Addressing),
    Omakelong,
    Olowlong,
    Ohighlong,
    Ocast32signed,
    Ocast32unsigned,
    Onegl,
    Oaddlimm(i64),
    Osubl,
    Omull,
    Omullimm(i64),
    Omullhs,
    Omullhu,
    Odivl,
    Odivlu,
    Omodl,
    Omodlu,
    Oandl,
    Oandlimm(i64),
    Oorl,
    Oorlimm(i64),
    Oxorl,
    Oxorlimm(i64),
    Onotl,
    Oshll,
    Oshllimm(i64),
    Oshrl,
    Oshrlimm(i64),
    Oshrxlimm(i64),
    Oshrlu,
    Oshrluimm(i64),
    Ororlimm(i64),
    Oleal(Addressing),
    Onegf,
    Oabsf,
    Oaddf,
    Osubf,
    Omulf,
    Odivf,
    Omaxf,
    Ominf,
    Onegfs,
    Oabsfs,
    Oaddfs,
    Osubfs,
    Omulfs,
    Odivfs,
    Osingleoffloat,
    Ofloatofsingle,
    Ointoffloat,
    Ofloatofint,
    Ointofsingle,
    Osingleofint,
    Olongoffloat,
    Ofloatoflong,
    Olongofsingle,
    Osingleoflong,
    Ocmp(Condition),
    Osel(Condition, Typ),
}
