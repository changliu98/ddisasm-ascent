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
        cfg.instruction(addr, size, prefix, opcode, op1, op2, op3, op4, imm_offset, displacement_offset),
        let operands = [*op1, *op2, *op3, *op4],
        let ins = Instruction {
            ea:*addr, size:*size, prefix:prefix, opcode:opcode, operands:operands, imm_offset:*imm_offset, 
            displacement_offset:*displacement_offset
        };
}


pub fn lift_instruction(addr:Address, db:& DatalogCFG) {
    
    let mut inst = LiftInstruction::default();
    inst.do_lift = vec![(addr,)];
    inst.run(&db);

}