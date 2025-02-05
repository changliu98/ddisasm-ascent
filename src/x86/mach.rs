use std::default;
use lexpr::{Value, parse::Error, cons};
use csv;
use crate::ast::{Signature, Typ, Ident, Z, BuiltinArg, BuiltinRes, Positive, ExternalFunction};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition};


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Plus {
    pub x: Mreg,
    pub y: Ident,
}

// (** Allocatable integer regs *)
// | AX | BX | CX | DX | SI | DI | BP
// | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15  (**r only in 64-bit mode *)
// (** Allocatable float regs *)
// | X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
// | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15  (**r only in 64-bit mode *)
// (** Special float reg *)
// | FP0.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mreg {
    // (** Allocatable integer regs *)
    AX, BX, CX, DX, SI, DI, BP,
    R8, R9, R10, R11, R12, R13, R14, R15,
    // (** Allocatable float regs *)
    X0, X1, X2, X3, X4, X5, X6, X7,
    X8, X9, X10, X11, X12, X13, X14, X15,
    // (** Special float reg *)
    FP0
}
impl From<String> for Mreg {
    fn from(s: String) -> Self {
        match s.as_str() {
            "Machregs.AX" => Mreg::AX, "Machregs.BX" => Mreg::BX, "Machregs.CX" => Mreg::CX, 
            "Machregs.DX" => Mreg::DX, "Machregs.SI" => Mreg::SI, "Machregs.DI" => Mreg::DI, 
            "Machregs.BP" => Mreg::BP, "Machregs.R8" => Mreg::R8, "Machregs.R9" => Mreg::R9, 
            "Machregs.R10" => Mreg::R10, "Machregs.R11" => Mreg::R11, "Machregs.R12" => Mreg::R12, 
            "Machregs.R13" => Mreg::R13, "Machregs.R14" => Mreg::R14, "Machregs.R15" => Mreg::R15, 
            "Machregs.X0" => Mreg::X0, "Machregs.X1" => Mreg::X1, "Machregs.X2" => Mreg::X2, 
            "Machregs.X3" => Mreg::X3, "Machregs.X4" => Mreg::X4,
            "Machregs.X5" => Mreg::X5, "Machregs.X6" => Mreg::X6, "Machregs.X7" => Mreg::X7,
            "Machregs.X8" => Mreg::X8, "Machregs.X9" => Mreg::X9, "Machregs.X10" => Mreg::X10,
            "Machregs.X11" => Mreg::X11, "Machregs.X12" => Mreg::X12, "Machregs.X13" => Mreg::X13,
            "Machregs.X14" => Mreg::X14, "Machregs.X15" => Mreg::X15, "Machregs.FP0" => Mreg::FP0,
            _ => panic!("Unknown Mreg: {}", s),
        }
    }
}

pub type Label = Positive;

// backend/Mach.v
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction{
    Mgetstack(Ptrofs, Typ, Mreg),
    Msetstack(Mreg, Ptrofs, Typ),
    Mgetparam(Ptrofs, Typ, Mreg),
    Mop(Operation, Vec<Mreg>, Mreg),
    Mload(MemoryChunk, Addressing, Vec<Mreg>, Mreg),
    Mstore(MemoryChunk, Addressing, Vec<Mreg>, Mreg),
    Mcall(Signature, Plus),
    Mtailcall(Signature, Plus),
    Mbuiltin(ExternalFunction, Vec<BuiltinArg<Mreg>>, BuiltinRes<Mreg>),
    Mlabel(Label),
    Mgoto(Label),
    Mcond(Condition, Vec<Mreg>, Label),
    Mjumptable(Mreg, Vec<Label>),
    Mreturn
}
// let print_instruction oc = function
//   | Mgetstack (ofs, typ, reg) -> fprintf oc "(Mach.instruction.Mgetstack %s %s %s) " (string_of_ptrofs ofs) (string_of_typ typ) (string_of_mreg reg)
//   | Msetstack (arg, ofs, typ) -> fprintf oc "(Mach.instruction.Msetstack %s %s %s) "  (string_of_mreg arg) (string_of_ptrofs ofs) (string_of_typ typ)
//   | Mgetparam (ofs, typ, res) -> 
//     fprintf oc "(Mach.instruction.Mgetparam %s %s %s) " 
//         (string_of_ptrofs ofs) 
//         (string_of_typ typ) 
//         (string_of_mreg res)
//   | Mop (op, args, res) ->
//     fprintf oc "(Mach.instruction.Mop %s (%s) %s) "
//       (string_of_operation op)
//       (String.concat ", " (List.map string_of_mreg args))
//       (string_of_mreg res)
//   | Mload (chunk, addr, args, res) -> fprintf oc "(Mach.instruction.Mload %s %s (%s) %s) " (string_of_chunk chunk) (string_of_addressing addr) (String.concat ", " (List.map string_of_mreg args)) (string_of_mreg res)
//   | Mstore (chunk, addr, args, src) -> fprintf oc "(Mach.instruction.Mstore %s %s (%s) %s) " (string_of_chunk chunk) (string_of_addressing addr) (String.concat ", " (List.map string_of_mreg args)) (string_of_mreg src)
//   | Mcall (sig_, tgt) -> fprintf oc "(Mach.instruction.Mcall %s %s) " (string_of_signature sig_) (string_fn_mcall tgt)
//   | Mtailcall (sig_, tgt) -> fprintf oc "(Mach.instruction.Mtailcall %s %s) " (string_of_signature sig_) (string_fn_mcall tgt)
//   (* | Mbuiltin (ef, args, res) -> fprintf oc "Mbuiltin(%s (%s) %s)\n" (string_of_external_function ef) (String.concat "" (string_of_builtin_args args)) (string_of_builtin_res res) *)
//   (* | Mlabel lbl -> fprintf oc "(Mach.instruction.Mlabel %s) " (string_of_positive lbl) *)
//   | Mgoto lbl -> fprintf oc "(Mach.instruction.Mgoto %s) " (string_of_positive lbl)
//   | Mcond (cond, args, lbl) -> fprintf oc "(Mach.instruction.Mcond %s (%s) %s) " (string_of_condition cond) (String.concat " " (List.map string_of_mreg args)) (string_of_positive lbl)
//   | Mjumptable (reg, lbls) -> fprintf oc "(Mach.instruction.Mjumptable %s (%s)) " (string_of_mreg reg) (String.concat " " (List.map string_of_positive lbls))
//   | Mreturn -> fprintf oc "(Mach.instruction.Mreturn) "
//   | _ -> fprintf oc "(Mach.instruction.NotImplemented) "

impl From<String> for Instruction {
    fn from(s: String) -> Self {
        let parsed = lexpr::from_str(&s).expect("Failed to parse");
        match parsed {
            Value::Cons(cons) => {
                let instr_type = cons.car();
                match instr_type {
                    Value::Symbol(sym) => {
                        match sym.as_ref() {
                            "Mach.instruction.Mgetstack" => {
                                let args = cons.cdr();
                                let mut args = args.list_iter().unwrap();
                                let ofs = args.next().unwrap().as_u64().unwrap();
                                let typ = Typ::from(args.next().unwrap().to_string());
                                let reg = Mreg::from(args.next().unwrap().to_string());
                                Instruction::Mgetstack(ofs, typ, reg)
                            }
                            "Mach.instruction.Msetstack" => {
                                let args = cons.cdr();
                                let mut args = args.list_iter().unwrap();
                                let reg = Mreg::from(args.next().unwrap().to_string());
                                let ofs = args.next().unwrap().as_u64().unwrap();
                                let typ = Typ::from(args.next().unwrap().to_string());
                                Instruction::Msetstack(reg, ofs, typ)
                            }
                            "Mach.instruction.Mgetparam" => {
                                let args = cons.cdr();
                                let mut args = args.list_iter().unwrap();
                                let ofs = args.next().unwrap().as_u64().unwrap();
                                let typ = Typ::from(args.next().unwrap().to_string());
                                let reg = Mreg::from(args.next().unwrap().to_string());
                                Instruction::Mgetparam(ofs, typ, reg)
                            }
                            "Mach.instruction.Mop" => {
                                let args = cons.cdr();
                                let mut args = args.list_iter().unwrap();
                                let op = Operation::from(args.next().unwrap().to_string());
                                let mut mregs = Vec::new();
                                for arg in args.next().unwrap().list_iter().unwrap(){
                                    mregs.push(Mreg::from(arg.to_string()));
                                }
                                let res = Mreg::from(args.next().unwrap().to_string());
                                Instruction::Mop(op, mregs, res)
                            }
                            "Mach.instruction.Mload" => {
                                let args = cons.cdr();
                                let mut args = args.list_iter().unwrap();
                                let chunk = MemoryChunk::from(args.next().unwrap().to_string());
                                let addr = Addressing::from(args.next().unwrap().to_string());
                                let mut mregs = Vec::new();
                                for arg in args.next().unwrap().list_iter().unwrap(){
                                    mregs.push(Mreg::from(arg.to_string()));
                                }
                                let res = Mreg::from(args.next().unwrap().to_string());
                                Instruction::Mload(chunk, addr, mregs, res)
                            }
                            

                            _ => Instruction::Mreturn
                            // _ => panic!("Failed to parse {}", sym.as_ref())
                        }
                    }
                    _ => panic!("Failed to parse {}", instr_type)
                }
            }
            _ => panic!("Failed to parse {}", s)
        }
    }
}

#[test]
fn load_machinstruction(){
    let mut sexprs = Vec::new(); 
    sexprs.push("(Mach.instruction.Mgetstack 8 AST.typ.Tany64 Machregs.BX)");
    sexprs.push("(Mach.instruction.Mgetstack 56 AST.typ.Tlong Machregs.AX)");
    for s in sexprs{
        let mut instr = Instruction::from(s.to_string());
    }
}

pub enum OPCode{
    Mgetstack,
    Msetstack,
    Mgetparam,
    Mop,
    Mload,
    Mstore,
    Mcall,
    Mtailcall,
    Mbuiltin,
    Mlabel,
    Mgoto,
    Mcond,
    Mjumptable,
    Mreturn
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MemoryChunk{
    Mbool,
    Mint8signed,
    Mint8unsigned,
    Mint16signed,
    Mint16unsigned,
    Mint32,
    Mint64,
    Mfloat32,
    Mfloat64,
    Many32,
    Many64
}
impl From<String> for MemoryChunk {
    fn from(s: String) -> Self {
        match s.as_str() {
            "AST.memory_chunk.Mbool" => MemoryChunk::Mbool,
            "AST.memory_chunk.Mint8signed" => MemoryChunk::Mint8signed,
            "AST.memory_chunk.Mint8unsigned" => MemoryChunk::Mint8unsigned,
            "AST.memory_chunk.Mint16signed" => MemoryChunk::Mint16signed,
            "AST.memory_chunk.Mint16unsigned" => MemoryChunk::Mint16unsigned,
            "AST.memory_chunk.Mint32" => MemoryChunk::Mint32,
            "AST.memory_chunk.Mint64" => MemoryChunk::Mint64,
            "AST.memory_chunk.Mfloat32" => MemoryChunk::Mfloat32,
            "AST.memory_chunk.Mfloat64" => MemoryChunk::Mfloat64,
            "AST.memory_chunk.Many32" => MemoryChunk::Many32,
            "AST.memory_chunk.Many64" => MemoryChunk::Many64,
            _ => panic!("AST.memory_chunk unknown memory chunk: {}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function{
    pub fn_sig: Signature,
    pub fn_code: Vec<Instruction>,
    pub fn_stacksize: u64,
    pub fn_link_ofs: Ptrofs,
    pub fn_retaddr_ofs: Ptrofs
}
impl From<String> for Function {
    fn from(s: String) -> Self {
        let parsed = lexpr::from_str(&s).expect("Failed to parse");
        match parsed {
            Value::Cons(cons) => {
                let head = cons.car();
                if *head != Value::Symbol("mkfunction".into()) {
                    panic!("Failed to parse: {}, expected mkfunction", head)
                }
                let args = cons.cdr();
                let mut args_iter = args.list_iter().unwrap();
                let sig = Signature::from(args_iter.next().unwrap().to_string());
                let code = args_iter.next().unwrap().to_string();
                let stacksize = args_iter.next().unwrap().as_u64().unwrap();
                let link_ofs = args_iter.next().unwrap().as_u64().unwrap();
                let retaddr_ofs = args_iter.next().unwrap().as_u64().unwrap();
                let instr_parsed = lexpr::from_str(&code).unwrap();
                let mut instr_vec = Vec::new();
                for instr in instr_parsed.list_iter().unwrap(){
                    let mut instr = Instruction::from(instr.to_string());
                    instr_vec.push(instr);
                }

                Function{fn_sig: sig, fn_code: instr_vec, fn_stacksize: stacksize, fn_link_ofs: link_ofs, fn_retaddr_ofs: retaddr_ofs}
            }
            _ => panic!("Failed to parse Function: {}", s)
        }
    }
}



fn name_of_chunk(chunk: MemoryChunk) -> &'static str {
    match chunk {
        MemoryChunk::Mbool => "bool",
        MemoryChunk::Mint8signed => "int8s",
        MemoryChunk::Mint8unsigned => "int8u",
        MemoryChunk::Mint16signed => "int16s",
        MemoryChunk::Mint16unsigned => "int16u",
        MemoryChunk::Mint32 => "int32",
        MemoryChunk::Mint64 => "int64",
        MemoryChunk::Mfloat32 => "float32",
        MemoryChunk::Mfloat64 => "float64",
        MemoryChunk::Many32 => "any32",
        MemoryChunk::Many64 => "any64"
    }
}

