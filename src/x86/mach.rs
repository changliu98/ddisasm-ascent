use std::{any, string};
use csv;
use crate::ast::{Signature, Typ};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition, Ident, Z};
use std::any::Any;

pub type Positive = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Plus {
    pub x: Mreg,
    pub y: Ident,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mreg {
    // (** Allocatable integer regs *)
    R0, R1, R2, R3, R4, R5, R6, R7,
    R8, R9, R10, R11, R12, R13, R14, R15,
    R17, R19, R20, R21, R22, R23,
    R24, R25, R26, R27, R28, R29,
    //   (** Allocatable floating-point regs *)
    F0, F1, F2, F3, F4, F5, F6, F7,
    F8, F9, F10, F11, F12, F13, F14, F15,
    F16, F17, F18, F19, F20, F21, F22, F23,
    F24, F25, F26, F27, F28, F29, F30, F31
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExternalFunction{
    EF_external(String, Signature),
    EF_builtin(String, Signature),
    EF_runtime(String, Signature),
    EF_vload(MemoryChunk),
    EF_vstore(MemoryChunk),
    EF_malloc,
    EF_free,
    EF_memcpy(Z, Z),
    EF_annot(Positive, String, Vec<Typ>),
    EF_annot_val(Positive, String, Typ),
    EF_inline_asm(String, Signature, Vec<String>),
    EF_debug(Positive, Ident, Vec<Typ>)
}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function{
    fn_sig: Signature,
    fn_code: Vec<Instruction>,
    fn_stacksize: u64,
    fn_link_ofs: Ptrofs,
    fn_retaddr_ofs: Ptrofs
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

// Inductive addressing: Type :=
//   | Aindexed: Z -> addressing       (**r Address is [r1 + offset] *)
//   | Aindexed2: Z -> addressing      (**r Address is [r1 + r2 + offset] *)
//   | Ascaled: Z -> Z -> addressing   (**r Address is [r1 * scale + offset] *)
//   | Aindexed2scaled: Z -> Z -> addressing  (**r Address is [r1 + r2 * scale + offset] *)
//   | Aglobal: ident -> ptrofs -> addressing (**r Address is [symbol + offset] *)
//   | Abased: ident -> ptrofs -> addressing  (**r Address is [symbol + offset + r1] *)
//   | Abasedscaled: Z -> ident -> ptrofs -> addressing  (**r Address is [symbol + offset + r1 * scale] *)
//   | Ainstack: ptrofs -> addressing. (**r Address is [stack_pointer + offset] *)




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