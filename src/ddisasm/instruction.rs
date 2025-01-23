use std::{cell::RefCell, ops::Add, rc::Rc};

use ascent::ascent;
use crate::ddisasm::ast::{Instruction, Address, Symbol, OperandCode};

use super::ast::DatalogCFG;

ascent!{
    pub struct LiftInstruction;
    extern database DatalogCFG cfg;
    relation instruction(Address, u64, Symbol, Symbol, OperandCode, OperandCode, OperandCode, OperandCode, u64, u64) in cfg; 
    relation do_lift(Address);

    relation lift(Address, Instruction);
    lift(addr, ins) <-- 
        do_lift(addr),
        cfg.instruction(addr, size, prefix, opcode, op1, op2, op3, op4, immOffset, displacementOffset),
        let operands = [*op1, *op2, *op3, *op4],
        let ins = Instruction {
            // ea: Address,
            // size: u64,
            // prefix: Symbol,
            // opcode: Symbol,
            // operands: [OperandCode; 4],
            // imm_offset: u64,
            // displacement_offset: u64,
            ea:*addr, size:*size, prefix:prefix, opcode:opcode, operands:operands, imm_offset:*immOffset, 
            displacement_offset:*displacementOffset
        };
}

struct Foo(&u32);

pub fn lift_instruction(addr:Address, db:& DatalogCFG) {
    let dbptr = Rc::new(RefCell::new(db));
    let mut liftdb = LiftInstruction::default();
    liftdb.cfg = dbptr;

}