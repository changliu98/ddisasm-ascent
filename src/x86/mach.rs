use csv;
use crate::ast::{Signature, Typ, Ident, Z, BuiltinArg, BuiltinRes, Positive, ExternalFunction};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition};

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


fn mkfunction(fn_sig: Signature, 
    fn_code: Vec<Instruction>, 
    fn_stacksize: u64, 
    fn_link_ofs: Ptrofs, 
    fn_retaddr_ofs: Ptrofs) -> Function {
    Function{fn_sig, fn_code, fn_stacksize, fn_link_ofs, fn_retaddr_ofs}
}
