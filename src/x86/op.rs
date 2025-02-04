use crate::ast::{Ident, Typ, Z};
use super::reg::Mreg;
use lexpr::{Value, parse::Error, cons};

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

// let string_of_addressing = function
// | Op.Aindexed ofs -> Printf.sprintf "(Op.addressing.Aindexed %s)" (string_of_ptrofs ofs)
// | Op.Aindexed2 ofs -> Printf.sprintf "(Op.addressing.Aindexed2 %s)" (string_of_ptrofs ofs)
// | Op.Ascaled (scale, ofs) -> Printf.sprintf "(Op.addressing.Ascaled %d %s)" (Z.to_int scale) (string_of_ptrofs ofs)
// | Op.Aindexed2scaled (scale, ofs) -> Printf.sprintf "(Op.addressing.Aindexed2scaled %d %s)" (Z.to_int scale) (string_of_ptrofs ofs)
// | Op.Aglobal (id, ofs) -> Printf.sprintf "(Op.addressing.Aglobal %s %s)" (string_of_positive id) (string_of_ptrofs ofs)
// | Op.Abased (id, ofs) -> Printf.sprintf "(Op.addressing.Abased %s %s)" (string_of_positive id) (string_of_ptrofs ofs)
// | Op.Abasedscaled (scale, id, ofs) -> Printf.sprintf "(Op.addressing.Abasedscaled %d %s %s)" (Z.to_int scale) (Obj.magic id) (string_of_ptrofs ofs)
// | Op.Ainstack ofs -> Printf.sprintf "(Op.addressing.Ainstack %s)" (string_of_ptrofs ofs)

impl From<String> for Addressing {
    fn from(s: String) -> Self {
        let parsed = lexpr::from_str(&s).expect("Failed to parse Addressing");
        match parsed {
            Value::Cons(cons) => {
                let op_type = cons.car();
                match op_type {
                    Value::Symbol(sym) => {
                        match sym.as_ref() {
                            "Op.addressing.Aindexed" => {
                                let args = cons.cdr();
                                match args {
                                    Value::Cons(args) => {
                                        let ofs = args.car().as_i64().unwrap();
                                        Addressing::Aindexed(ofs)
                                    }
                                    _ => panic!("Expected a cons cell"),
                                }
                            }
                            "Op.addressing.Aindexed2" => {
                                let args = cons.cdr();
                                match args {
                                    Value::Cons(args) => {
                                        let ofs = args.car().as_i64().unwrap();
                                        Addressing::Aindexed2(ofs)
                                    }
                                    _ => panic!("Expected a cons cell"),
                                }
                            }
                            "Op.addressing.Ascaled" => {
                                let args = cons.cdr();
                                match args {
                                    Value::Cons(args) => {
                                        let scale = args.car().as_i64().unwrap();
                                        let ofs = args.cdr().to_string().parse::<i64>().unwrap();
                                        Addressing::Ascaled(scale, ofs)
                                    }
                                    _ => panic!("Expected a cons cell"),
                                }
                            }
                            "Op.addressing.Aindexed2scaled" => {
                                let args = cons.cdr();
                                match args {
                                    Value::Cons(args) => {
                                        let scale = args.car().as_i64().unwrap();
                                        let ofs = args.cdr().to_string().parse::<i64>().unwrap();
                                        Addressing::Aindexed2scaled(scale, ofs)
                                    }
                                    _ => panic!("Expected a cons cell"),
                                }
                            }
                            "Op.addressing.Aglobal" => {
                                let args = cons.cdr();
                                match args {
                                    Value::Cons(args) => {
                                        let id = args.car().to_string().parse::<usize>().unwrap();
                                        let ofs = args.cdr().to_string().parse::<u64>().unwrap();
                                        Addressing::Aglobal(id, ofs)
                                    }
                                    _ => panic!("Expected a cons cell"),
                                }
                            }
                            "Op.addressing.Abased" => {
                                let args = cons.cdr();
                                match args {
                                    Value::Cons(args) => {
                                        let id = args.car().to_string().parse::<usize>().unwrap();
                                        let ofs = args.cdr().to_string().parse::<u64>().unwrap();
                                        Addressing::Abased(id, ofs)
                                    }
                                    _ => panic!("Expected a cons cell"),
                                }
                            }
                            "Op.addressing.Abasedscaled" => {
                                let args = cons.cdr();
                                match args {
                                    Value::Cons(args) => {
                                        let scale = args.car().as_i64().unwrap();
                                        let id = args.cdr().to_string().parse::<usize>().unwrap();
                                        let ofs = args.cdr().to_string().parse::<u64>().unwrap();
                                        Addressing::Abasedscaled(scale, id, ofs)
                                    }
                                    _ => panic!("Expected a cons cell"),
                                }
                            }
                            "Op.addressing.Ainstack" => {
                                let args = cons.cdr();
                                match args {
                                    Value::Cons(args) => {
                                        let ofs = args.car().as_u64().unwrap();
                                        Addressing::Ainstack(ofs)
                                    }
                                    _ => panic!("Expected a cons cell"),
                                }
                            }

                            
                            _ => panic!("Unknown addressing type {}", sym.as_ref()),
                        }
                    }
                    _ => panic!("Unknown address {}", s),
                }
            }
            _ => panic!("Expected a cons cell"),
        }
        
    }
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
    Todo
}
impl From<String> for Operation {
    fn from(s: String) -> Self {
        match s.as_str() {
            "Ocast8signed" => Operation::Ocast8signed,
            "Ocast8unsigned" => Operation::Ocast8unsigned,
            "Ocast16signed" => Operation::Ocast16signed,
            "Ocast16unsigned" => Operation::Ocast16unsigned,
            "Oneg" => Operation::Oneg,
            "Osub" => Operation::Osub,
            "Omul" => Operation::Omul,
            "Omulimm" => Operation::Omulimm(0),
            "Omulhs" => Operation::Omulhs,
            "Omulhu" => Operation::Omulhu,
            "Odiv" => Operation::Odiv,
            "Odivu" => Operation::Odivu,
            "Omod" => Operation::Omod,
            "Omodu" => Operation::Omodu,
            _ => Operation::Todo
        }  
    }
}
