use std::{cell::RefCell, collections::BTreeSet, rc::Rc, hash::{Hash, Hasher},};
use ascent::aggregators::count;
use ascent::ascent;
use ascent::lattice::set::Set;

use super::util::{leak, read_csv};

pub type Address = u64;
// pub type OpCode = &'static str;
pub type Symbol = &'static str;
pub type OperandCode = u64;

 
 // const *
 
 #[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
 struct Ptr<T>(Rc<RefCell<T>>);
 
 impl<T> Hash for Ptr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
       // use the hash value of the pointer
       self.0.as_ptr().hash(state);
    }
 }
 
 #[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
 struct GraphNode {
    id: u64,
    next: BTreeSet<Ptr<GraphNode>>,
    //    prev: BTreeSet<Ptr<GraphNode>>,
 }


// relation ID, you might need this, ID is rel_name \times u64
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RelationID(pub &'static str,  pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Instruction {
    pub ea: Address,
    pub size: u64,
    pub prefix: Symbol,
    pub opcode: Symbol,
    pub operands: [OperandCode; 4],
    pub imm_offset: u64,
    pub displacement_offset: u64,
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

macro_rules! ptr {
    ($x:expr) => {
       Ptr(Rc::new(RefCell::new($x)))
    };
 } 

// flatten version of CFG
ascent! {
    pub struct DatalogCFG;

    // (1,0), next_id -> (1, 0, RelationId("next", 0))
    relation next(Address, Address);

    // EA, Block
    relation code_in_block(Address, Address);

    // relation code(Address);

    relation basicblock(Address);

    // .decl instruction(ea:address,size:unsigned,prefix:symbol,opcode:symbol,
    //     op1:operand_code,op2:operand_code,op3:operand_code,op4:operand_code,
    //     immOffset:unsigned,displacementOffset:unsigned)
    relation instruction(Address, u64, Symbol, Symbol, OperandCode, OperandCode, OperandCode, OperandCode, u64, u64);
    index instruction (0,);
    // instruction has id "i", i maps to a struct of instruction
    relation lifted_relation(RelationID, Instruction);
    
    relation cfg(
        Address, Address, Symbol, Symbol,  Symbol   
    );

    relation node(Address);
    relation start_node(Address);
    relation end_node(Address);
    relation edge(Address, Address);

    lattice lifted_graph_head(Set<Ptr<GraphNode>>);
    relation outages(Address, usize);


    node(x) <-- cfg(x, _,  _, _, _);
    node(y) <-- cfg(_, y,  _, _, _);

    start_node(x) <-- node(x), !cfg(_, x, _, _, _);
    end_node(x) <-- node(x), !edge(x, _);
    outages(x, n) <-- node(x), agg n = count() in edge(x, _);


    relation do_lift(Address);
    lattice lift(Address, Set<Ptr<GraphNode>>);

    start_node(x) <-- do_lift(x);

    do_lift(y) <-- do_lift(x), edge(x, y);
    
    lift(x, Set::singleton(new_node)) <--
        do_lift(x),
        edge(x, y), end_node(y),
        let new_node = ptr!(GraphNode { id: *x, ..Default::default() });

    lift(x, Set::singleton(new_node)) <--
        do_lift(x),
        edge(x, y), lift(y, y_nodes), outages(y, n),
        if &y_nodes.0.len() == n,
        let new_node = ptr!(GraphNode { id: *x, next: y_nodes.0.clone() });

    lifted_graph_head(Set::singleton(new_node)) <-- start_node(x), lift(x, next),
        let new_node = ptr!(GraphNode { id: *x, next: next.0.clone() });
    



}

pub fn read_cfg(flatten_cfg: &mut DatalogCFG, data_path: &'static str) {
    let get_path = |x: &str| format!("{data_path}{x}");
    flatten_cfg.next = read_csv::<(Address, Address)>(&get_path("/disassembly/next.csv")).collect();
        
    flatten_cfg.code_in_block =
        read_csv::<(Address, Address)>(&get_path("/disassembly/code_in_block.csv")).collect();

    flatten_cfg.basicblock = read_csv::<(Address,)>(&get_path("/disassembly/block.csv")).collect();

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
    )>(&get_path("/disassembly/instruction.csv"))
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

    flatten_cfg.cfg = read_csv::<(
        Address,
        Address,
        String,
        String,
        String
    )>(&get_path("/function-inference/cfg_edge.csv"))
    .map(
        |(src, dest, condition, indirect, jumptype)| {(
                src, dest, leak(condition), leak(indirect), leak(jumptype)
            )},
    )
    .collect();

}
