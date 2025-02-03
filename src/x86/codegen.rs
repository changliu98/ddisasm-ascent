use ascent::ascent;

use crate::util::Either;
use crate::ast::{Signature, Typ};
use crate::x86::reg::Mreg;
use crate::ast::{Ident, MemoryChunk, ExternalFunction, BuiltinArg, BuiltinRes};
use crate::x86::op::{Addressing, Ptrofs, Operation, Condition};
use crate::x86::mach::Function as MachFunction;
use crate::x86::mach::Instruction as MachInstruction;

pub type Label = usize;

ascent! {
    pub struct Mach2Asm;

}
