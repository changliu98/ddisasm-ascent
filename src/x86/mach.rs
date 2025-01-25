use std::string;
use crate::ast::{Signature, Typ};
use crate::x86::op::Ptrofs;

// Inductive instruction: Type :=
//   | Mgetstack: ptrofs -> typ -> mreg -> instruction
//   | Msetstack: mreg -> ptrofs -> typ -> instruction
//   | Mgetparam: ptrofs -> typ -> mreg -> instruction
//   | Mop: operation -> list mreg -> mreg -> instruction
//   | Mload: memory_chunk -> addressing -> list mreg -> mreg -> instruction
//   | Mstore: memory_chunk -> addressing -> list mreg -> mreg -> instruction
//   | Mcall: signature -> mreg + ident -> instruction
//   | Mtailcall: signature -> mreg + ident -> instruction
//   | Mbuiltin: external_function -> list (builtin_arg mreg) -> builtin_res mreg -> instruction
//   | Mlabel: label -> instruction
//   | Mgoto: label -> instruction
//   | Mcond: condition -> list mreg -> label -> instruction
//   | Mjumptable: mreg -> list label -> instruction
//   | Mreturn: instruction.

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction{
    Mgetstack(Ptrofs, Typ, Mreg),
    Msetstack(Mreg, Ptrofs, Typ),
    Mgetparam(Ptrofs, Typ, Mreg),
    Mop(Operation, Vec<Mreg>, Mreg),
    Mload(MemoryChunk, Addressing, Vec<Mreg>, Mreg),
    Mstore(MemoryChunk, Addressing, Vec<Mreg>, Mreg),
    Mcall(Signature, Either<Mreg, Ident>),
    Mtailcall(Signature, Either<Mreg, Ident>),
    Mbuiltin(ExternalFunction, Vec<BuiltinArg<Mreg>>, BuiltinRes<Mreg>),
    Mlabel(Label),
    Mgoto(Label),
    Mcond(Condition, Vec<Mreg>, Label),
    Mjumptable(Mreg, Vec<Label>),
    Mreturn
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

// Record function: Type := mkfunction
//   { fn_sig: signature;
//     fn_code: code;
//     fn_stacksize: Z;
//     fn_link_ofs: ptrofs;
//     fn_retaddr_ofs: ptrofs }.

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function{
    fn_sig: Signature,
    fn_code: Vec<Instruction>,
    fn_stacksize: u64,
    fn_link_ofs: Ptrofs,
    fn_retaddr_ofs: Ptrofs
}

fn load_mach_from_csv(file: &str) -> Vec<Function> {
    let mut rdr = csv::Reader::from_path(file).unwrap();
    let mut mach = Vec::new();
    // CSV format is as follows:
    // fn_sig, instruction opcaode, arg0, arg1, arg2
    for result in rdr.records() {
        let record = result.unwrap();
        let fn_sig = record[0].parse::<Signature>().unwrap();
        let opcode = record[1].parse::<OPCode>().unwrap();
        let arg0 = record[2].parse::<String>().unwrap();
        let arg1 = record[3].parse::<String>().unwrap();
        let arg2 = record[4].parse::<String>().unwrap();
        let instr = match opcode {
            OPCode::Mgetstack => Instruction::Mgetstack(arg0, arg1, arg2),
            OPCode::Msetstack => Instruction::Msetstack(arg0, arg1, arg2),
            OPCode::Mgetparam => Instruction::Mgetparam(arg0, arg1, arg2),
            OPCode::Mop => Instruction::Mop(arg0, arg1, arg2),
            OPCode::Mload => Instruction::Mload(arg0, arg1, arg2),
            OPCode::Mstore => Instruction::Mstore(arg0, arg1, arg2),
            OPCode::Mcall => Instruction::Mcall(arg0, arg1),
            OPCode::Mtailcall => Instruction::Mtailcall(arg0, arg1),
            OPCode::Mbuiltin => Instruction::Mbuiltin(arg0, arg1, arg2),
            OPCode::Mlabel => Instruction::Mlabel(arg0),
            OPCode::Mgoto => Instruction::Mgoto(arg0),
            OPCode::Mcond => Instruction::Mcond(arg0, arg1, arg2),
            OPCode::Mjumptable => Instruction::Mjumptable(arg0, arg1),
            OPCode::Mreturn => Instruction::Mreturn
        };
        mach.push(Function{fn_sig, fn_code: vec![instr], fn_stacksize: 0, fn_link_ofs: 0, fn_retaddr_ofs: 0});
    }
    mach
}