use std::hash::{Hasher, Hash};
use std::{cell::RefCell, collections::BTreeSet};
use std::rc::Rc;

use ascent::ascent;

use super::util::{leak, read_csv};

pub type Address = u64;
// pub type OpCode = &'static str;
pub type Symbol = &'static str;
pub type OperandCode = u64;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ptr<T>(pub Rc<RefCell<T>>);

impl<T> Hash for Ptr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
    
    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for piece in data {
            piece.hash(state)
        }
    }
}

// relation ID, you might need this, ID is rel_name \times u64
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RelationID(pub &'static str,  pub u64);


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Instruction {
    ea: Address,
    size: u64,
    prefix: Symbol,
    opcode: Symbol,
    operands: [OperandCode; 4],
    imm_offset: u64,
    displacement_offset: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BasicBlock {
    id: u64,
    start: Address,
    end: Address,
    instructions: Vec<Instruction>,
    prev_bbs: BTreeSet<Ptr<BasicBlock>>,
    next_bbs: BTreeSet<Ptr<BasicBlock>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CFG {
    entry: Ptr<BasicBlock>,
    bbs: BTreeSet<Ptr<BasicBlock>>,
}

// flatten version of CFG
ascent! {
    pub struct DatalogCFG;

    // (1,0), next_id -> (1, 0, RelationId("next", 0))
    relation next(Address, Address);

    // EA, Block
    relation code_in_block(Address, Address);

    // cfg edge
    relation code(Address);

    // .decl instruction(ea:address,size:unsigned,prefix:symbol,opcode:symbol,
    //     op1:operand_code,op2:operand_code,op3:operand_code,op4:operand_code,
    //     immOffset:unsigned,displacementOffset:unsigned)
    relation instruction(Address, u64, Symbol, Symbol, OperandCode, OperandCode, OperandCode, OperandCode, u64, u64);
    // instruction has id "i", i maps to a struct of instruction
    relation lifted_relation(RelationID, Instruction);
    
    // output reation, lifted
}

pub fn read_cfg(flatten_cfg: &mut DatalogCFG, data_path: &'static str) {
    let get_path = |x: &str| format!("{data_path}{x}");
    flatten_cfg.next = read_csv::<(Address, Address)>(&get_path("next.csv")).collect();
        

    flatten_cfg.code_in_block =
        read_csv::<(Address, Address)>(&get_path("code_in_block.csv")).collect();

    flatten_cfg.code = read_csv::<(Address,)>(&get_path("code.csv")).collect();

    flatten_cfg.instruction = read_csv::<(
        Address,
        u64,
        String,
        String,
        OperandCode,
        OperandCode,
        OperandCode,
        OperandCode,
        u64,
        u64,
    )>(&get_path("instruction.csv"))
    .map(
        |(ea, size, prefix, opcode, op1, op2, op3, op4, imm_offset, dispalcement_offset)| {
            (
                ea,
                size,
                leak(prefix),
                leak(opcode),
                op1,
                op2,
                op3,
                op4,
                imm_offset,
                dispalcement_offset,
            )
        },
    )
    .collect();
}
