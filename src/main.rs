// use ascent::ascent;
use std::process::Command;

use ascent::*;
use itertools::Itertools;

fn read_csv<T>(path: &str) -> impl Iterator<Item = T>
where
   for<'de> T: serde::de::Deserialize<'de>,
{
    println!("Reading CSV file: {}", path);
   csv::ReaderBuilder::new()
      .delimiter(b'\t')
      .has_headers(false)
      .double_quote(false)
      .quoting(false)
      .from_path(path)
      .unwrap()
      .into_deserialize()
      .map(|x| x.unwrap())
}


type InputReg=String;
type RegNullable=String;
type Register=String;
type Address=u64;
type OperandCode=u64;
type OperandIndex=u64;

type LimitType=String;

type AccessMode=String;
type SymbolPosition=String;

// Todo: var not bounded
type StackVar=(Register, u64);

type ConditionCode=String;
type Number=i64;

use libc::size_t;

#[link(name = "functors")]
extern "C" {
    fn functor_data_valid(ea: u64, size: size_t) -> u64;
    fn functor_data_signed(ea: u64, size: size_t) -> i64;
    fn functor_data_unsigned(ea: u64, size: size_t) -> i64;
}

ascent! {
    #![measure_rule_times]
    struct DDisasm;
    relation value_reg(Address, Register, Address, RegNullable, Number, Number, u64);
    relation base_relative_jump(Address, Address);
    relation base_relative_operand(Address, OperandIndex, Address);
    relation block_next(Address, Address, Address);
    relation cmp_defines(Address, Address, Register);
    relation compare_and_jump_immediate(Address, Address, ConditionCode, Register, i64);
    relation compare_and_jump_indirect(Address, Address, ConditionCode, OperandCode, i64);
    relation compare_and_jump_register(Address, Address, ConditionCode, Register, Register);
    relation const_value_reg_used(Address, Address, Address, Register, i64);
    relation def_used_for_address(Address, Register, SymbolPosition);
    relation flags_and_jump_pair(Address, Address, ConditionCode);
    relation got_relative_operand(Address, OperandIndex, Address);
    relation jump_table_element_access(Address, u64, Address, Register);
    relation jump_table_max(Address, Address);
    relation jump_table_signed(Address, u64);
    relation jump_table_start(Address, u64, Address, Address, i64);
    relation jump_table_target(Address, Address);
    relation last_value_reg_limit(Address, Address, Register, i64, LimitType, u64);
    relation reg_def_use_def_used(Address, Register, Address, OperandIndex);
    relation reg_def_use_live_var_at_block_end(Address, Address, Register);
    relation reg_def_use_live_var_at_prior_used(Address, Address, Register);
    relation reg_def_use_live_var_used(Address, Register, Register, Address, OperandIndex, u64);
    relation reg_def_use_return_val_used(Address, Address, Register, Address, OperandIndex);
    relation reg_has_base_image(Address, Register);
    relation reg_has_got(Address, Register);
    relation reg_reg_arithmetic_operation_defs(Address, Register, Address, Register, Address, Register, i64, i64);
    relation relative_jump_table_entry_candidate(Address, Address, u64, Address, Address, i64, i64);
    relation stack_def_use_def_used(Address, StackVar, Address, StackVar, OperandIndex);
    relation stack_def_use_live_var_at_block_end(Address, Address, StackVar);
    relation stack_def_use_live_var_at_prior_used(Address, Address, StackVar);
    relation stack_def_use_live_var_used(Address, StackVar, StackVar, Address, OperandIndex, u64);
    relation stack_def_use_live_var_used_in_block(Address, Address, StackVar, StackVar, Address, OperandIndex, u64);
    relation tls_desc_call(Address, Address, Address);
    relation tls_get_addr(Address, Address, Address);
    relation value_reg_edge(Address, Register, Address, Register, i64, i64);
    relation value_reg_limit(Address, Address, Register, i64, LimitType);
    relation value_reg_unsupported(Address, Register);
    relation adjusts_stack_in_block(Address, Address, Register, i64);
    relation after_end(Address, Address);
    relation arch_call(Address, OperandIndex);
    relation arch_cmp_operation(String);
    // .decl arch_cmp_zero_operation(Operation:symbol)
    relation arch_cmp_zero_operation(String);

    // .decl arch_condition_flags_reg(Reg:register)
    relation arch_condition_flags_reg(Register);
    
    relation arch_condition_flags_reg(Register);

    // .decl arch_conditional(EA:address,CC:condition_code)
    relation arch_conditional(Address, ConditionCode);

    // .decl arch_extend_load(EA:address,Signed:unsigned,SrcBits:unsigned)
    relation arch_extend_load(Address, u64, u64);

    // .decl arch_extend_reg(EA:address,Reg:register,Signed:unsigned,SrcBits:unsigned)
    relation arch_extend_reg(Address, Register, u64, u64);

    // .decl arch_jump(EA:address)
    relation arch_jump(Address);

    // decl arch_memory_access(AccessType:symbol,EA:address,SrcOp:operand_index,DstOp:operand_index,
    //     DirectReg:register,BaseReg:reg_nullable,IndexReg:reg_nullable,Mult:number,Offset:number)
    relation arch_memory_access(AccessMode, Address, OperandIndex, OperandIndex, Register, RegNullable, RegNullable, i64, i64);
    
    // .decl arch_move_reg_imm(EA:address,Dst:register,Imm:number,ImmIndex:operand_index)
    relation arch_move_reg_imm(Address, Register, i64, OperandIndex);

    // .decl arch_move_reg_reg(EA:address,Dst:register,Src:register)
    relation arch_move_reg_reg(Address, Register, Register);

    // .decl arch_pc_relative_addr(EA:address,Reg:register,Target:address)
    relation arch_pc_relative_addr(Address, Register, Address);

    // .decl arch_reg_arithmetic_operation(EA:address,Dst:register,Src:register,Mult:number,Offset:number)
    relation arch_reg_arithmetic_operation(Address, Register, Register, i64, i64);

    // .decl arch_reg_reg_arithmetic_operation(EA:address,Dst:register,Src1:register,Src2:register,Mult:number,Offset:number)
    relation arch_reg_reg_arithmetic_operation(Address, Register, Register, Register, i64, i64);

    // .decl arch_register_size_bytes(Reg:input_reg,Size:unsigned)
    relation arch_register_size_bytes(InputReg, u64);

    // .decl arch_return_reg(Reg:register)
    relation arch_return_reg(Register);

    // .decl arch_stack_pointer(Reg:register)
    relation arch_stack_pointer(Register);

    // .decl arch_store_immediate(EA:address,SrcOp:operand_index,DstOp:operand_index,
    //     Immediate:number,BaseReg:reg_nullable,IndexReg:reg_nullable,Mult:number,Offset:number)
    relation arch_store_immediate(Address, OperandIndex, OperandIndex, i64, RegNullable, RegNullable, i64, i64);

    // .decl base_address(ea:address)
    relation base_address(Address);

    // .decl base_relative_operation(EA_relop:address,EA:address)
    relation base_relative_operation(Address, Address);

    // .decl binary_format(Format:symbol)
    relation binary_format(String);

    // .decl block(Block:address)
    relation block(Address);

    // .decl call_tls_get_addr(Call:address,Reg:register)
    // .decl cmp_immediate_to_reg(EA:address,Reg:register,Imm_index:operand_index,Immediate:number)
    // .decl cmp_reg_to_reg(EA:address,Reg1:register,Reg2:register)
    // .decl code_in_block(EA:address,Block:address)
    // .decl conditional_jump(src:address)
    // .decl data_access(EA:address,Op_index:operand_index,RegSegment:reg_nullable,RegBase:reg_nullable,RegIndex:reg_nullable,Mult:number,Offset:number,Size:unsigned)
    // .decl data_segment(Begin:address,End:address)
    // .decl defined_symbol(ea:address,size:unsigned,type:symbol,scope:symbol,visibility:symbol,sectionIndex:unsigned,originTable:symbol,tableIndex:unsigned,name:symbol)
    // .decl direct_call(EA:address,Dest:address)
    // .decl direct_jump(src:address, dest:address)
    // .decl got_reference_pointer(EA:address)
    // .decl got_section(name:symbol)
    // .decl instruction(ea:address,size:unsigned,prefix:symbol,opcode:symbol,
    //           op1:operand_code,op2:operand_code,op3:operand_code,op4:operand_code,
    //           immOffset:unsigned,displacementOffset:unsigned)
    // .decl instruction_displacement_offset(EA:address,Index:operand_index,Offset:unsigned,Size:unsigned)
    // .decl instruction_get_dest_op(EA:address,Index:operand_index,Op:operand_code)
    // .decl instruction_get_op(ea:address, index:operand_index, operator:operand_code)
    // .decl instruction_get_src_op(EA:address,Index:operand_index,Op:operand_code)
    // .decl instruction_has_relocation(EA:address,Rel:address)
    // .decl inter_procedural_edge(Src:address,Dest:address)
    // .decl is_padding(EA:address)
    // .decl is_xor_reset(EA:address)
    // .decl limit_reg_op(EA:address,DstReg:register,SrcReg:register,Offset:number)
    // .decl limit_type_map(CC:condition_code,BranchLT:limit_type,FallthroughLT:limit_type,BranchOffset:number,FallthroughOffset:number)
    // .decl loaded_section(Beg:address,End:address,Name:symbol)
    // .decl lsda_callsite_addresses(Start:address,End:address,LandingPad:address)
    // .decl may_fallthrough(o:address,d:address)
    // .decl next(n:address,m:address)
    // .decl no_return_call_propagated(EA:address)
    // .decl block_instruction_next(Block:address,Before:address,After:address)
    // .decl block_last_instruction(Block:address,EA:address)

    relation block_instruction_next(Address, Address, Address);
    relation block_last_instruction(Address, Address);
    relation call_tls_get_addr(Address, Register);
    relation cmp_immediate_to_reg(Address, Register, OperandIndex, i64);
    relation cmp_reg_to_reg(Address, Register, Register);
    relation code_in_block(Address, Address);
    relation conditional_jump(Address);
    relation data_access(Address, OperandIndex, RegNullable, RegNullable, RegNullable, i64, i64, u64);
    relation data_segment(Address, Address);
    relation defined_symbol(Address, u64, String, String, String, u64, String, u64, String);
    relation direct_call(Address, Address);
    relation direct_jump(Address, Address);
    relation got_reference_pointer(Address);
    relation got_section(String);
    relation instruction(Address, u64, String, String, OperandCode, OperandCode, OperandCode, OperandCode, u64, u64);
    relation instruction_displacement_offset(Address, OperandIndex, u64, u64);
    relation instruction_get_dest_op(Address, OperandIndex, OperandCode);
    relation instruction_get_op(Address, OperandIndex, OperandCode);
    relation instruction_get_src_op(Address, OperandIndex, OperandCode);
    relation instruction_has_relocation(Address, Address);
    relation inter_procedural_edge(Address, Address);
    relation is_padding(Address);
    relation is_xor_reset(Address);
    relation limit_reg_op(Address, Register, Register, i64);
    relation limit_type_map(ConditionCode, LimitType, LimitType, i64, i64);
    relation loaded_section(Address, Address, String);
    relation lsda_callsite_addresses(Address, Address, Address);
    relation may_fallthrough(Address, Address);
    relation next(Address, Address);
    relation no_return_call_propagated(Address);

    // .decl no_value_reg_limit(EA_jmp:address)
    // .decl op_immediate(Code:operand_code,Offset:number,SizeBytes:unsigned)
    // .decl op_immediate_and_reg(EA:address,Operation:symbol,Reg:register,Imm_index:operand_index,Immediate:number)
    // .decl op_indirect(Code:operand_code,Reg1:input_reg,Reg2:input_reg,Reg3:input_reg,
    //         Multiplier:number,Offset:number,SizeBytes:unsigned)
    // .decl op_indirect_mapped(Op:operand_code,Reg1:reg_nullable,Reg2:reg_nullable,Reg3:reg_nullable,Mult:number,Offset:number,Size:unsigned)
    // .decl op_regdirect(Code:operand_code,RegisterName:input_reg)
    // .decl op_regdirect_contains_reg(Op:operand_code,Reg:register)
    // .decl pc_relative_operand(src:address,index:operand_index, dest:address)
    // .decl possible_rva_operand(EA:address,Index:operand_index,Dest:address)
    // .decl reg_call(Src:address,Reg:register)
    // .decl reg_def_use_block_last_def(EA:address,EA_def:address,Var:register)
    // .decl reg_def_use_def(EA:address,Var:register)
    // // .decl reg_def_use_def_used(EA_def:address,Var:register,EA_used:address,Index_used:operand_index)
    // // .input reg_def_use_def_used(filename="reg_def_use.def_used.facts")
    // .decl reg_def_use_defined_in_block(Block:address,Var:register)
    // .decl reg_def_use_flow_def(EA:address,Var:register,EA_next:address,Value:number)
    // .decl reg_def_use_live_var_def(Block:address,VarIdentity:register,LiveVar:register,EA_def:address)
    // .decl reg_def_use_ref_in_block(Block:address,Var:register)
    relation no_value_reg_limit(Address);
    relation op_immediate(OperandCode, i64, u64);
    relation op_immediate_and_reg(Address, String, Register, OperandIndex, i64);
    relation op_indirect(OperandCode, InputReg, InputReg, InputReg, i64, i64, u64);
    relation op_indirect_mapped(OperandCode, RegNullable, RegNullable, RegNullable, i64, i64, u64);
    relation op_regdirect(OperandCode, InputReg);
    relation op_regdirect_contains_reg(OperandCode, Register);
    relation pc_relative_operand(Address, OperandIndex, Address);
    relation possible_rva_operand(Address, OperandIndex, Address);
    relation reg_call(Address, Register);
    relation reg_def_use_block_last_def(Address, Address, Register);
    relation reg_def_use_def(Address, Register);
    relation reg_def_use_defined_in_block(Address, Register);
    relation reg_def_use_flow_def(Address, Register, Address, i64);
    relation reg_def_use_live_var_def(Address, Register, Register, Address);
    relation reg_def_use_ref_in_block(Address, Register);

    // .decl reg_def_use_return_block_end(Callee:address,CalleeEnd:address,Block:address,BlockEnd:address)
    // .decl reg_def_use_used(EA:address,Var:register,Index:operand_index)
    // .decl reg_def_use_used_in_block(Block:address,EA_used:address,Var:register,Index:operand_index)
    // .decl reg_jump(Src:address,Reg:register)
    // .decl reg_map(RegIn:input_reg,Reg:register)
    // .decl reg_used_for(EA:address,Reg:register,Type:symbol)
    // .decl register_access(EA:address,Register:input_reg,AccessMode:access_mode)
    // .decl relative_address(EA:address,Size:unsigned,TableStart:address,Reference:address,Dest:address,DestIsFirstOrSecond:symbol)
    // .decl relative_address_start(EA:address,Size:unsigned,Reference:address,Dest:address, DestIsFirstOrSecond:symbol)
    // .decl relocation(EA:address,Type:symbol,Name:symbol,Addend:number,SymbolIndex:unsigned,Section:symbol,RelType:symbol)
    // .decl relocation_adjustment_total(EA:address,Adjustment:number)
    // .decl simple_data_access_pattern(Address:address,Op_index:unsigned,Size:unsigned,FromWhere:address)
    // .decl stack_base_reg_move(Block:address,EA:address,Src:register,Dst:register)
    // .decl stack_def_use_block_last_def(EA:address,EA_def:address,Var:stack_var)
    // .decl stack_def_use_def(EA:address,Var:stack_var)
    // .decl stack_def_use_defined_in_block(Block:address,Var:stack_var)
    // .decl stack_def_use_live_var_def(Block:address,VarIdentity:stack_var,LiveVar:stack_var,EA_def:address)
    // .decl stack_def_use_moves_limit(Moves:unsigned)
    // .decl stack_def_use_ref_in_block(Block:address,Var:stack_var)
    // .decl stack_def_use_used(EA:address,Var:stack_var,Index:operand_index)
    // .decl stack_def_use_used_in_block(Block:address,EA_used:address,Var:stack_var,Index:operand_index)
    // .decl step_limit(Limit:unsigned)
    // .decl step_limit_small(Limit:unsigned)
    // .decl symbol(ea:address,size:unsigned,type:symbol,scope:symbol,visibility:symbol,sectionIndex:unsigned,originTable:symbol,tableIndex:unsigned,name:symbol)
    // .decl symbolic_expr_from_relocation(EA:address,Size:unsigned,Symbol:symbol,Offset:number,TargetEA:address)
    // .decl take_address(Src:address,Address_taken:address)
    // .decl tls_descriptor(EA:address,Offset:unsigned)
    // .decl tls_index(EA:address,Offset:unsigned)
    // .decl tls_segment(Start:address,End:address,Align:unsigned)
    // .decl track_register(Reg:register)
    relation reg_def_use_return_block_end(Address, Address, Address, Address);
    relation reg_def_use_used(Address, Register, OperandIndex);
    relation reg_def_use_used_in_block(Address, Address, Register, OperandIndex);
    relation reg_jump(Address, Register);
    relation reg_map(InputReg, Register);
    relation reg_used_for(Address, Register, String);
    relation register_access(Address, InputReg, AccessMode);
    relation relative_address(Address, u64, Address, Address, Address, String);
    relation relative_address_start(Address, u64, Address, Address, String);
    relation relocation(Address, String, String, i64, u64, String, String);
    relation relocation_adjustment_total(Address, i64);
    relation simple_data_access_pattern(Address, u64, u64, Address);
    relation stack_base_reg_move(Address, Address, Register, Register);
    relation stack_def_use_block_last_def(Address, Address, StackVar);
    relation stack_def_use_def(Address, StackVar);
    relation stack_def_use_defined_in_block(Address, StackVar);
    relation stack_def_use_live_var_def(Address, StackVar, StackVar, Address);
    relation stack_def_use_moves_limit(u64);
    relation stack_def_use_ref_in_block(Address, StackVar);
    relation stack_def_use_used(Address, StackVar, OperandIndex);
    relation stack_def_use_used_in_block(Address, Address, StackVar, OperandIndex);
    relation step_limit(u64);
    relation step_limit_small(u64);
    relation symbol(Address, u64, String, String, String, u64, String, u64, String);
    relation symbolic_expr_from_relocation(Address, u64, String, i64, Address);
    relation take_address(Address, Address);
    relation tls_descriptor(Address, u64);
    relation tls_index(Address, u64);
    relation tls_segment(Address, Address, u64);
    relation track_register(Register);

    // block_next(Block,EA,Block2) <-- 
    // block_last_instruction(Block,EA),
    // may_fallthrough(EA,Block2),
    // !no_return_call_propagated(EA),
    // !inter_procedural_edge(EA,Block2),
    // block(Block2).

    block_next(block, ea, block2) <-- 
        block_last_instruction(block, ea),
        may_fallthrough(ea, block2), 
        !no_return_call_propagated(ea), 
        !inter_procedural_edge(ea, block2),
        block(block2);

    // block_next(Block,EA,Block2) <-- 
    //     lsda_callsite_addresses(Beg,End,Block2),
    //     block_last_instruction(Block,EA),
    //     EA >= Beg,
    //     EA < End,
    //     block(Block2).
    block_next(block, ea, block2) <-- 
        lsda_callsite_addresses(beg, end, block2),
        block_last_instruction(block, ea),
        if ea >= beg,
        if ea < end,
        block(block2);

    // block_next(Block,EA,EA_next) <-- 
    //     block_last_instruction(Block,EA),
    //     direct_jump(EA,EA_next),
    //     !inter_procedural_edge(EA,EA_next).
    block_next(block, ea, ea_next) <-- 
        block_last_instruction(block, ea),
        direct_jump(ea, ea_next),
        !inter_procedural_edge(ea, ea_next);

    // compare_and_jump_immediate(EA,EA,CC,Reg,0) <-- 
    //     instruction(EA,_,_,Operation,_,_,_,_,_,_),
    //     arch_cmp_zero_operation(Operation),
    //     arch_jump(EA),
    //     arch_conditional(EA,CC),
    //     instruction_get_op(EA,_,Op),
    //     op_regdirect_contains_reg(Op,Reg).        
    compare_and_jump_immediate(ea, ea, cc, reg, 0) <-- 
        instruction(ea, _, _, operation, _, _, _, _, _, _),
        arch_cmp_zero_operation(operation),
        arch_jump(ea),
        arch_conditional(ea, cc),
        instruction_get_op(ea, _, op),
        op_regdirect_contains_reg(op, reg);
        
    compare_and_jump_immediate(ea, ea, cc, reg, 0) <-- 
        instruction(ea, _, _, operation, _, _, _, _, _, _),
        arch_cmp_zero_operation(operation),
        arch_jump(ea),
        arch_conditional(ea, cc),
        instruction_get_op(ea, _, op),
        register_access(ea, reg_in, "R".to_string()),
        reg_map(reg_in, reg),
        !op_regdirect_contains_reg(op, reg);

    compare_and_jump_register(ea, ea, cc, reg1, reg2) <-- 
        cmp_reg_to_reg(ea, reg1, reg2),
        arch_jump(ea),
        arch_conditional(ea, cc);

    def_used_for_address(ea, reg, "PCRelative".to_string()) <-- 
        arch_pc_relative_addr(ea, reg, _);

    got_relative_operand(ea, index, (((*target_ea as Number) + addend) + adjustment) as Address) <-- 
        symbol(target_ea, _, _, _, _, _, _, symbol_index, symbol),
        instruction_displacement_offset(ea, index, displacement_offset, _),
        let tmp_53 = ea + displacement_offset,
        relocation_adjustment_total(tmp_53, adjustment),
        relocation(tmp_53, "GOTOFF".to_string(), symbol, addend, symbol_index, _, _);

    // TODO: why mult is same as size?
    jump_table_element_access(ea, size, (*table_start as Address), (reg_index.clone() as Register)) <-- 
        data_access(ea, _, "NONE".to_string(), "NONE".to_string(), reg_index, mult, table_start, size),
        if *mult as u64 == *size,
        if reg_index != "NONE",
        data_segment(beg, end),
        if *table_start as Address >= *beg,
        if *table_start as Address <= *end;

    jump_table_element_access(ea, 1, (*table_start as Address), (reg_base_copy.to_string() as Register)) <-- 
        data_access(ea, _, "NONE".to_string(), reg_base, "NONE".to_string(), _, table_start, 1),
        if reg_base != "NONE",
        let reg_base_copy = reg_base.clone(),
        data_segment(beg, end),
        if (*table_start as Address) >= *beg,
        if (*table_start as Address) <= *end;

    reg_def_use_def_used(ea_def, var, ea_used, index) <-- 
        reg_def_use_used(ea_used, var, index),
        reg_def_use_block_last_def(ea_used, ea_def, var);

    reg_def_use_live_var_used(block, var, var, ea_used, index, 0) <-- 
        reg_def_use_used_in_block(block, ea_used, var, index),
        !reg_def_use_block_last_def(ea_used, _, var);

    reg_has_base_image(ea, reg) <-- 
        let inlined_operation_773 = "LEA",
        base_address(image_base),
        pc_relative_operand(ea, _, image_base),
        code_in_block(ea, _),
        reg_def_use_def(ea, reg),
        instruction(ea, _, _, inlined_operation_773.to_string(), _, _, _, _, _, _);

    reg_has_base_image(ea_code, reg) <-- 
        binary_format("PE".to_string()),
        base_address(image_base),
        code_in_block(ea_code, _),
        arch_memory_access("LOAD".to_string(), ea_code, _, _, _, _, _, _, _),
        pc_relative_operand(ea_code, _, ea_data),
        if unsafe {functor_data_valid(*ea_data, 8) == 1},
        if *image_base == unsafe {functor_data_signed(*ea_data, 8)} as Address,
        reg_def_use_def(ea_code, reg);

    relative_jump_table_entry_candidate(ea, table_start, 1, refr, dest, 4, 0) <-- 
        relative_address(ea, 1, table_start, refr, dest, "first".to_string()),
        if dest < table_start,
        relative_address_start(refr, 4, _, _, _),
        loaded_section(start, end, _),
        if refr >= start,
        if refr < end,
        if dest >= start,
        if dest < end;

    stack_def_use_def_used(ea_def, var, ea_used, var, index) <-- 
        stack_def_use_used(ea_used, var, index),
        stack_def_use_block_last_def(ea_used, ea_def, var);

    stack_def_use_live_var_used_in_block(block, ea_used, x, x, ea_used, index, 0) <-- 
        stack_def_use_used_in_block(block, ea_used, x, index);

    value_reg(ea, reg, ea, "NONE".to_string(), 0, (*val as Number), 1) <-- 
        arch_pc_relative_addr(ea, reg, val),
        track_register(reg);

    value_reg(ea, reg, ea, "NONE".to_string(), 0, (((ea + size) as Number) + offset), 1) <-- 
        let pc_reg = "RIP",
        code_in_block(ea, _),
        arch_reg_arithmetic_operation(ea, reg, pc_reg.to_string(), 1, offset),
        instruction(ea, size, _, _, _, _, _, _, _, _),
        !instruction_has_relocation(ea, _),
        track_register(reg);

    // Todo: <=
    // value_reg(ea, reg, ea_reg1, reg1, multiplier, offset, steps1) <= value_reg(ea, reg, ea_reg1, reg1, multiplier, offset, steps2) <-- 
    //     steps2 <= steps1;

    base_relative_jump(ea_relop, ea) <-- 
        reg_jump(ea, _),
        base_relative_operation(ea_relop, ea);

    base_relative_operand(ea_used, op_index, rva) <-- 
        reg_has_base_image(ea_def, reg),
        reg_def_use_def_used(ea_def, reg, ea_used, op_index),
        possible_rva_operand(ea_used, op_index, rva);

    base_relative_operand(ea_def2, op_index_access, rva) <-- 
        reg_has_base_image(ea_def1, reg1),
        reg_def_use_def_used(ea_def1, reg1, ea, _),
        data_access(ea_def2, op_index_access, "NONE".to_string(), "NONE".to_string(), _, 4, offset, _),
        if *offset > 0,
        reg_def_use_def_used(ea_def2, reg2, ea, _),
        possible_rva_operand(ea_def2, op_index_access, rva),
        data_access(ea, _, "NONE".to_string(), reg1, reg2, 1, _, _);

    base_relative_operand(ea_def2, op_index_access, rva) <-- 
        reg_has_base_image(ea_def1, reg1),
        reg_def_use_def_used(ea_def1, reg1, ea, _),
        data_access(ea_def2, op_index_access, "NONE".to_string(), "NONE".to_string(), _, 4, offset, _),
        if *offset > 0,
        reg_def_use_def_used(ea_def2, reg2, ea, _),
        possible_rva_operand(ea_def2, op_index_access, rva),
        data_access(ea, _, "NONE".to_string(), reg2, reg1, 1, _, _);

    base_relative_operand(ea_def1, op_index, (*value as Address)) <-- 
        reg_has_base_image(ea_def2, reg2),
        reg_def_use_def_used(ea_def2, reg2, ea, _),
        arch_reg_reg_arithmetic_operation(ea, reg1, *reg1, &*reg2, 1, 0),
        reg_def_use_def_used(ea_def1, reg1, ea, _),
        instruction_get_op(ea_def1, op_index, op),
        instruction(ea_def1, _, _, inlined_operation_41, _, _, _, _, _, _),
        if inlined_operation_41 == "LEA",
        op_indirect(op, _, _, _, _, value, _),
        if *value > 0;

    base_relative_operand(ea_def1, op_index, (*value as Address)) <-- 
        reg_has_base_image(ea_def2, reg2),
        reg_def_use_def_used(ea_def2, reg2, ea, op_index_access),
        data_access(ea, op_index_access, "NONE".to_string(), reg2, reg1, 1, 0, _),
        !instruction(ea, _, _, "LEA".to_string(), _, _, _, _, _, _),
        reg_def_use_def_used(ea_def1, reg1, ea, _),
        instruction_get_op(ea_def1, op_index, op),
        instruction(ea_def1, _, _, inlined_operation_497, _, _, _, _, _, _),
        if inlined_operation_497 == "LEA",
        op_indirect(op, _, _, _, _, value, _),
        if *value > 0;

    base_relative_operand(ea_def1, op_index, (*value as Address)) <-- 
        reg_has_base_image(ea_def2, reg2),
        reg_def_use_def_used(ea_def2, reg2, ea, op_index_access),
        data_access(ea, op_index_access, "NONE".to_string(), reg1, reg2, 1, 0, _),
        !instruction(ea, _, _, "LEA".to_string(), _, _, _, _, _, _),
        reg_def_use_def_used(ea_def1, reg1, ea, _),
        instruction_get_op(ea_def1, op_index, op),
        instruction(ea_def1, _, _, inlined_operation_498, _, _, _, _, _, _),
        if inlined_operation_498 == "LEA",
        op_indirect(op, _, _, _, _, value, _),
        if *value > 0;

    base_relative_operation(ea_relop, ea_inst) <-- 
        base_relative_operand(ea_relop, _, _),
        reg_def_use_def_used(ea_relop, _, ea_inst, _);

    base_relative_operation(ea_relop, ea_next) <-- 
        base_relative_operand(ea_relop, _, _),
        reg_def_use_def_used(ea_relop, _, ea_inst, _),
        reg_def_use_def_used(ea_inst, _, ea_next, _);

    block_next(block, ea, dest) <-- 
        block_last_instruction(block, ea),
        jump_table_target(ea, dest);

    cmp_defines(ea_jmp, ea_dst, reg) <-- 
        compare_and_jump_immediate(_, ea_jmp, "E".to_string(), reg, _),
        direct_jump(ea_jmp, ea_dst);

    cmp_defines(ea_jmp, ea_dst, reg) <-- 
        compare_and_jump_immediate(_, ea_jmp, "NE".to_string(), reg, _),
        may_fallthrough(ea_jmp, ea_dst);

    compare_and_jump_immediate(ea_cmp, ea_jmp, cc, reg, immediate) <-- 
        flags_and_jump_pair(ea_cmp, ea_jmp, cc),
        instruction(ea_cmp, _, _, operation, _, _, _, _, _, _),
        arch_cmp_operation(operation),
        cmp_immediate_to_reg(ea_cmp, reg, _, immediate);

    compare_and_jump_indirect(ea_cmp, ea_jmp, cc, indirect_op, immediate) <-- 
        flags_and_jump_pair(ea_cmp, ea_jmp, cc),
        instruction(ea_cmp, _, _, cmp_operation, _, _, _, _, _, _),
        arch_cmp_operation(cmp_operation),
        instruction_get_op(ea_cmp, _, indirect_op),
        op_indirect(indirect_op, _, _, _, _, _, _),
        instruction_get_op(ea_cmp, _, imm_op),
        op_immediate(imm_op, immediate, _);

    compare_and_jump_register(ea_cmp, ea_jmp, cc, reg1, reg2) <-- 
        flags_and_jump_pair(ea_cmp, ea_jmp, cc),
        cmp_reg_to_reg(ea_cmp, reg1, reg2);

    const_value_reg_used(used_ea, ea_def, ea_reg_def, reg, value) <-- 
        value_reg(ea_reg_def, reg, ea_def, "NONE".to_string(), 0, value, _),
        reg_def_use_def_used(ea_reg_def, reg, used_ea, _);

        def_used_for_address(ea_def, reg, type_here) <-- 
        reg_def_use_def_used(ea_def, reg, ea, _),
        reg_used_for(ea, reg, type_here);

    def_used_for_address(ea_def, reg, type_here) <-- 
        def_used_for_address(ea_used, _, type_here),
        reg_def_use_def_used(ea_def, reg, ea_used, _);

    def_used_for_address(ea_def, reg1, type_here) <-- 
        def_used_for_address(ea_load, reg2, type_here),
        arch_memory_access("LOAD".to_string(), ea_load, _, _, reg2, reg_base_load, "NONE".to_string(), _, stack_pos_load),
        arch_memory_access("STORE".to_string(), ea_store, _, _, reg1, reg_base_store, "NONE".to_string(), _, stack_pos_store),
        stack_def_use_def_used(ea_store, (reg_base_store.clone(), *stack_pos_store as u64), ea_load, (reg_base_load.clone(), *stack_pos_load as u64), _),
        reg_def_use_def_used(ea_def, reg1, ea_store, _);

    flags_and_jump_pair(ea_flags, ea_jmp, cc) <-- 
        arch_condition_flags_reg(reg),
        reg_def_use_def_used(ea_flags, reg, ea_jmp, _),
        arch_jump(ea_jmp),
        arch_conditional(ea_jmp, cc);

    got_relative_operand(ea3, index, ((*dest as Number)).try_into().unwrap()) <-- 
        reg_has_got(ea1, reg1),
        reg_def_use_def_used(ea1, reg1, ea2, _),
        arch_reg_reg_arithmetic_operation(ea2, reg3, reg1, reg2, mult, 0),
        reg_def_use_def_used(ea2, reg3, ea3, _),
        op_immediate_and_reg(ea3, _, reg3, index, _),
        value_reg(ea3, reg3, _, reg2, mult, dest, _);

    got_relative_operand(ea, index, *got + (*offset as Address)) <-- 
        reg_has_got(ea_base, reg),
        reg_def_use_def_used(ea_base, reg, ea, index),
        instruction_get_op(ea, index, op),
        op_indirect_mapped(op, "NONE".to_string(), reg, _, _, offset, _),
        if *offset != 0,
        got_reference_pointer(got);

    got_relative_operand(ea, index, got + (*offset as Address)) <-- 
        reg_has_got(ea_base, reg),
        reg_def_use_def_used(ea_base, reg, ea, index),
        instruction_get_op(ea, index, op),
        op_indirect_mapped(op, "NONE".to_string(), _, reg, 1, offset, _),
        if *offset != 0,
        got_reference_pointer(got);

    jump_table_element_access(ea, size, table_start_addr, "NONE".to_string()) <-- 
        pc_relative_operand(ea, 1, table_start_addr),
        data_access(ea, _, _, _, _, _, _, size),
        def_used_for_address(ea, _, type_here),
        if *type_here == "Jump".to_string(),
        reg_def_use_def_used(ea, reg1, ea_add, _),
        reg_def_use_def_used(ea2, reg2, ea_add, _),
        take_address(ea2, table_start_addr),
        arch_reg_reg_arithmetic_operation(ea_add, _, reg2, reg1, 1, 0),
        data_segment(beg, end),
        if table_start_addr >= beg,
        if table_start_addr <= end;

    jump_table_element_access(ea, size, table_start_addr, "NONE".to_string()) <-- 
        pc_relative_operand(ea, 1, table_start_addr),
        data_access(ea, _, _, _, _, _, _, size),
        def_used_for_address(ea, _, type_here),
        if *type_here == "Call".to_string(),
        reg_def_use_def_used(ea, reg1, ea_add, _),
        reg_def_use_def_used(ea2, reg2, ea_add, _),
        take_address(ea2, table_start_addr),
        arch_reg_reg_arithmetic_operation(ea_add, _, reg2, reg1, 1, 0),
        data_segment(beg, end),
        if table_start_addr >= beg,
        if table_start_addr <= end;

    jump_table_element_access(ea, size, table_start_u64, reg_base) <-- 
        data_access(ea, _, "NONE".to_string(), reg_base, reg_index, 1, 0, size),
        if *reg_base != "NONE".to_string(),
        if *reg_index != "NONE".to_string(),
        const_value_reg_used(ea, _, _, reg_index, table_start),
        let table_start_u64 = *table_start as Address,
        data_segment(beg, end),
        if table_start_u64 >= *beg,
        if table_start_u64 <= *end;

    jump_table_element_access(ea, size, ((base + offset) as Address), reg_index) <-- 
        data_access(ea, _, "NONE".to_string(), reg_base, reg_index, _, offset, size),
        if *reg_base != "NONE".to_string(),
        const_value_reg_used(ea, _, _, reg_base, base),
        data_segment(beg, end),
        if *base + *offset >= *beg as Number,
        if *base + *offset <= *end as Number;

    jump_table_max(table_start, *table_start + (*value as Address) * *size) <-- 
        jump_table_element_access(ea, size, table_start, reg_index),
        code_in_block(ea, block),
        !reg_def_use_block_last_def(ea, _, reg_index),
        last_value_reg_limit(_, block, reg_index, value, "MAX".to_string(), _),
        if *value >= 0;

    jump_table_max(table_start, table_start + (*value as Address) * *size) <-- 
        jump_table_element_access(ea, size, table_start, reg_index),
        code_in_block(ea, _),
        reg_def_use_block_last_def(ea, ea_def, reg_index),
        last_value_reg_limit(ea_def, _, reg_index, value, "MAX".to_string(), _),
        if *value >= 0;

    jump_table_max(table_start2, (*table_start2 as i64 + fdu) as u64) <-- 
        jump_table_element_access(ea1, size1, table_start1, _),
        jump_table_max(table_start1, table_end1),
        reg_def_use_def_used(ea1, reg, ea2, _),
        if ea1 != ea2,
        jump_table_element_access(ea2, size2, table_start2, reg),
        if table_start1 != table_start2,
        for generator_0 in (*table_start1..(table_end1 + size1)).step_by(*size1 as usize),
        if unsafe {functor_data_valid(generator_0, *size1 as size_t) } == 1,
        let fdu = unsafe { functor_data_unsigned(generator_0, *size1 as size_t) } * (*size2) as i64 ;

    jump_table_signed(table_start, signed) <-- 
        jump_table_element_access(ea, size, table_start, _),
        arch_extend_load(ea, signed, _tmp_71),
        if *_tmp_71 == 8 * size;

    jump_table_signed(table_start, signed) <-- 
        jump_table_element_access(ea, size, table_start, _),
        value_reg(ea_used, _, ea, reg, _, _, _),
        arch_extend_reg(ea_used, reg, signed, _tmp_72),
        if *_tmp_72 == 8 * size;

    jump_table_signed(table_start, 0) <-- 
        jump_table_element_access(ea, _, table_start, _),
        instruction_get_dest_op(ea, _, dst_op),
        op_regdirect(dst_op, def_reg),
        reg_map(def_reg, def_reg_mapped),
        value_reg(ea_used, _, ea, def_reg_mapped, _, _, _),
        instruction_get_src_op(ea_used, _, op),
        op_regdirect(op, used_reg),
        reg_map(used_reg, def_reg_mapped),
        arch_register_size_bytes(def_reg, def_size),
        arch_register_size_bytes(used_reg, used_size),
        if *used_size > *def_size;

    jump_table_signed(table_start, 1) <-- 
        jump_table_element_access(_, ptr_size, table_start, _),
        if *ptr_size == 8;

    jump_table_start(ea_jump, 4, image_base + value, image_base, 1) <-- 
        base_address(image_base),
        base_relative_jump(ea_base, ea_jump),
        base_relative_operand(ea_base, _, value);

    jump_table_start(ea_jump, size, table_start, (*base as Address), scale) <-- 
        jump_table_element_access(ea, size, table_start, _),
        value_reg(ea_add, reg_jump, ea, reg, scale, base, _),
        if *reg != "NONE".to_string(),
        reg_def_use_def_used(ea_add, reg_jump, ea_jump, _),
        reg_call(ea_jump, _),
        code_in_block(ea_jump, _);

    jump_table_start(ea_jump, size, table_start, (*base as Address), scale) <-- 
        jump_table_element_access(ea, size, table_start, _),
        value_reg(ea_add, reg_jump, ea, reg, scale, base, _),
        if *reg != "NONE".to_string(),
        reg_def_use_def_used(ea_add, reg_jump, ea_jump, _),
        reg_jump(ea_jump, _),
        code_in_block(ea_jump, _);
        jump_table_start(ea_jump, size, (*table_reference as Address), (*table_reference as Address), 1) <-- 
        reg_jump(ea_jump, _),
        code_in_block(ea_jump, _),
        reg_def_use_def_used(ea_base, reg, ea_jump, _),
        instruction(ea_base, _, _, inlined_operation_591, _, _, _, _, _, _),
        if *inlined_operation_591 == "ADD".to_string(),
        jump_table_element_access(ea_base, size, _tmp_73, _),
        const_value_reg_used(ea_base, _, _, reg, table_reference),
        if *_tmp_73 == (*table_reference as Address);

    jump_table_start(ea_jump, size, (*table_reference as Address), (*table_reference as Address), -1) <-- 
        reg_jump(ea_jump, _),
        code_in_block(ea_jump, _),
        reg_def_use_def_used(ea_base, reg, ea_jump, _),
        instruction(ea_base, _, _, inlined_operation_593, _, _, _, _, _, _),
        if *inlined_operation_593 == "SUB".to_string(),
        jump_table_element_access(ea_base, size, _tmp_74, _),
        const_value_reg_used(ea_base, _, _, reg, table_reference),
        if *_tmp_74 == (*table_reference as Address);

    jump_table_start(ea_jump, size, table_start, (*table_reference as Address), 1) <-- 
        reg_jump(ea_jump, _),
        code_in_block(ea_jump, _),
        reg_def_use_def_used(ea_base, reg, ea_jump, _),
        instruction(ea_base, _, _, inlined_operation_590, _, _, _, _, _, _),
        if *inlined_operation_590 == "ADD".to_string(),
        jump_table_element_access(ea_base, size, table_start, _),
        const_value_reg_used(ea_base, _, _, reg, table_reference),
        code_in_block(_tmp_153, _),
        if *_tmp_153 == (*table_reference as Address);

    jump_table_start(ea_jump, size, table_start, (*table_reference as Address), -1) <-- 
        reg_jump(ea_jump, _),
        code_in_block(ea_jump, _),
        reg_def_use_def_used(ea_base, reg, ea_jump, _),
        instruction(ea_base, _, _, inlined_operation_592, _, _, _, _, _, _),
        if *inlined_operation_592 == "SUB".to_string(),
        jump_table_element_access(ea_base, size, table_start, _),
        const_value_reg_used(ea_base, _, _, reg, table_reference),
        code_in_block(_tmp_154, _),
        if *_tmp_154 == (*table_reference as Address);

    jump_table_target(ea, dest) <-- 
        jump_table_start(ea, size, table_start, _, _),
        relative_jump_table_entry_candidate(_, table_start, size, _, dest, _, _);

    last_value_reg_limit(from, to, reg, value, limit_type, 0) <-- 
        value_reg_limit(from, to, reg, value, limit_type);

    last_value_reg_limit(block_end, block_next, propagated_reg, propagated_val, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, propagated_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_next(block, block_end, block_next),
        !reg_def_use_defined_in_block(block, propagated_reg),
        !conditional_jump(block_end);

    last_value_reg_limit(block_end, block_next, propagated_reg, propagated_val, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, propagated_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_next(block, block_end, block_next),
        !reg_def_use_defined_in_block(block, propagated_reg),
        no_value_reg_limit(block_end);

    last_value_reg_limit(block_end, block_next, propagated_reg, propagated_val, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, propagated_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_next(block, block_end, block_next),
        !reg_def_use_defined_in_block(block, propagated_reg),
        cmp_defines(block_end, block_next, reg),
        if reg != propagated_reg;

    last_value_reg_limit(block_end, block_next, propagated_reg, propagated_val, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, propagated_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_next(block, block_end, block_next),
        !reg_def_use_defined_in_block(block, propagated_reg),
        cmp_defines(block_end, defined_next, propagated_reg),
        if block_next != defined_next;

    last_value_reg_limit(block_end, block_next, propagated_reg, propagated_val, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, propagated_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_next(block, block_end, block_next),
        !reg_def_use_defined_in_block(block, propagated_reg),
        value_reg_limit(block_end, block_next, reg, _, _),
        if reg != propagated_reg;

    last_value_reg_limit(block_end, block_next, propagated_reg, propagated_val, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, propagated_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_next(block, block_end, block_next),
        !reg_def_use_defined_in_block(block, propagated_reg),
        value_reg_limit(block_end, block_next, propagated_reg, val, type_here),
        if *propagated_type == "MAX".to_string(),
        if *type_here == "MAX".to_string(),
        if propagated_val < val;

    last_value_reg_limit(block_end, block_next, propagated_reg, propagated_val, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, propagated_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_next(block, block_end, block_next),
        !reg_def_use_defined_in_block(block, propagated_reg),
        value_reg_limit(block_end, block_next, propagated_reg, val, type_here),
        if *propagated_type == "MIN".to_string(),
        if *type_here == "MIN".to_string(),
        if propagated_val > val;

    last_value_reg_limit(block_end, ea_next, dst_reg, propagated_val + offset, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, src_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_last_instruction(block, block_end),
        limit_reg_op(block_end, dst_reg, src_reg, offset),
        if ea <= block_end,
        if block_end <= block_end,
        code_in_block(block_end, block),
        block_next(block, block_end, ea_next),
        !reg_def_use_block_last_def(block_end, _, src_reg);

    last_value_reg_limit(block_end, ea_next, dst_reg, propagated_val + offset, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(from, ea, src_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_last_instruction(block, block_end),
        limit_reg_op(block_end, dst_reg, src_reg, offset),
        if ea <= block_end,
        if block_end <= block_end,
        code_in_block(block_end, block),
        block_next(block, block_end, ea_next),
        reg_def_use_block_last_def(block_end, from, src_reg);

    last_value_reg_limit(ea_mov, ea_next, dst_reg, propagated_val + offset, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(from, ea, src_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_last_instruction(block, block_end),
        limit_reg_op(ea_mov, dst_reg, src_reg, offset),
        if ea <= ea_mov,
        if ea_mov <= block_end,
        code_in_block(ea_mov, block),
        if ea_mov != block_end,
        code_in_block(ea_mov, inlined_block_193),
        may_fallthrough(ea_mov, ea_next),
        code_in_block(ea_next, inlined_block_193),
        reg_def_use_block_last_def(ea_mov, from, src_reg);

    last_value_reg_limit(ea_mov, ea_next, dst_reg, propagated_val + offset, propagated_type, steps + 1) <-- 
        step_limit_small(step_limit),
        last_value_reg_limit(_, ea, src_reg, propagated_val, propagated_type, steps),
        if steps <= step_limit,
        code_in_block(ea, block),
        block_last_instruction(block, block_end),
        limit_reg_op(ea_mov, dst_reg, src_reg, offset),
        if ea <= ea_mov,
        if ea_mov <= block_end,
        code_in_block(ea_mov, block),
        if ea_mov != block_end,
        code_in_block(ea_mov, inlined_block_194),
        may_fallthrough(ea_mov, ea_next),
        code_in_block(ea_next, inlined_block_194),
        !reg_def_use_block_last_def(ea_mov, _, src_reg);

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_immediate(_, ea_jmp, cc, _, _),
        if *cc == "O".to_string();

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_immediate(_, ea_jmp, cc, _, _),
        if *cc == "NO".to_string();

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_immediate(_, ea_jmp, cc, _, _),
        if *cc == "P".to_string();

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_immediate(_, ea_jmp, cc, _, _),
        if *cc == "PE".to_string();

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_immediate(_, ea_jmp, cc, _, _),
        if *cc == "S".to_string();

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_immediate(_, ea_jmp, cc, _, _),
        if *cc == "NS".to_string();

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_register(ea_cmp, ea_jmp, _, reg1, reg2),
        !reg_def_use_block_last_def(ea_cmp, _, reg1),
        !reg_def_use_block_last_def(ea_cmp, _, reg2);

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_register(ea_cmp, ea_jmp, _, reg1, reg2),
        reg_def_use_block_last_def(ea_cmp, ea, reg1),
        !arch_move_reg_imm(ea, reg1, _, _),
        !reg_def_use_block_last_def(ea_cmp, _, reg2);

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_register(ea_cmp, ea_jmp, _, reg1, reg2),
        reg_def_use_block_last_def(ea_cmp, ea, reg2),
        !arch_move_reg_imm(ea, reg2, _, _),
        !reg_def_use_block_last_def(ea_cmp, _, reg1);

    no_value_reg_limit(ea_jmp) <-- 
        compare_and_jump_register(ea_cmp, ea_jmp, _, reg1, reg2),
        reg_def_use_block_last_def(ea_cmp, ea, reg1),
        !arch_move_reg_imm(ea, reg1, _, _),
        reg_def_use_block_last_def(ea_cmp, _, reg2),
        !arch_move_reg_imm(ea, reg2, _, _);

    no_value_reg_limit(ea_jmp) <-- 
        flags_and_jump_pair(ea_cmp, ea_jmp, _),
        instruction(ea_cmp, _, _, operation, _, _, _, _, _, _),
        !arch_cmp_operation(operation);    

    // Todo
    // .plan 1:(3,1,2)

    reg_def_use_def_used(ea_def, var, next_ea_used, next_index) <--
        reg_def_use_live_var_at_prior_used(ea_used, next_used_block, var),
        reg_def_use_def_used(ea_def, var, ea_used, _),
        reg_def_use_live_var_used(next_used_block, var, var, next_ea_used, next_index, _);

    reg_def_use_def_used(ea_def, reg, ea_used, index) <--
        reg_def_use_return_val_used(_, callee, reg, ea_used, index),
        reg_def_use_return_block_end(callee, _, _, block_end),
        reg_def_use_block_last_def(block_end, ea_def, reg);

    reg_def_use_live_var_at_block_end(prev_block, block, var) <--
        block_next(prev_block, prev_block_end, block),
        reg_def_use_live_var_used(block, var, _, _, _, _),
        !reg_def_use_flow_def(prev_block_end, var, block, _);

    reg_def_use_live_var_at_block_end(prev_block, block_used, var) <--
        reg_def_use_live_var_at_block_end(block, block_used, var),
        !reg_def_use_ref_in_block(block, var),
        block_next(prev_block, _, block);

    reg_def_use_live_var_at_prior_used(ea_used, block_used, var) <--
        reg_def_use_live_var_at_block_end(block, block_used, var),
        reg_def_use_used_in_block(block, ea_used, var, _),
        !reg_def_use_defined_in_block(block, var);

    // Todo fix unbounded reg
    reg_def_use_live_var_used(ret_block, reg, reg, ea_used, index, 1) <--
        reg_def_use_return_block_end(callee, _, ret_block, ret_block_end),
        // !reg_def_use_block_last_def(ret_block_end, _, reg),
        reg_def_use_return_val_used(_, callee, reg, ea_used, index);

    reg_def_use_return_val_used(ea_call, callee, reg, ea_used, index_used) <--
        arch_return_reg(reg),
        reg_def_use_def_used(ea_call, reg, ea_used, index_used),
        direct_call(ea_call, callee);

    reg_has_base_image(ea, reg) <--
        reg_has_base_image(ea2, reg2),
        reg_def_use_def_used(ea2, reg2, ea, _),
        arch_move_reg_reg(ea, reg, reg2);

    reg_has_base_image(ea, reg) <--
        reg_has_base_image(ea2, reg2),
        reg_def_use_def_used(ea2, reg2, ea, _),
        arch_reg_reg_arithmetic_operation(ea, reg, reg2, _, mult, 0),
        if *mult > 1;

    reg_has_got(ea, reg) <--
        value_reg(ea, reg, _, "NONE".to_string(), _, offset, _),
        got_section(name),
        loaded_section(tmp_88, _, name),
        if *tmp_88 == (*offset as Address);

    reg_reg_arithmetic_operation_defs(ea, reg_def, ea_def1, reg1, ea_def2, reg2, mult, offset) <--
        def_used_for_address(ea, reg_def, _),
        arch_reg_reg_arithmetic_operation(ea, reg_def, reg1, reg2, mult, offset),
        if reg1 != reg2,
        reg_def_use_def_used(ea_def1, reg1, ea, _),
        if ea != ea_def1,
        reg_def_use_def_used(ea_def2, reg2, ea, _),
        if ea != ea_def2;

    relative_jump_table_entry_candidate(table_start, table_start, size, reference, (*reference + (unsafe {functor_data_unsigned(*table_start as Address, *size as size_t)} as Address) ), scale, 0) <--
        jump_table_start(_, size, table_start, reference, scale),
        jump_table_signed(table_start, inlined_signed_1118),
        if *inlined_signed_1118 == 0,
        if unsafe {functor_data_valid(*table_start as Address, *size as size_t) } == 1,
        code_in_block(tmp_318, _),
        loaded_section(section_start, section_end, _),
        if table_start >= section_start,
        if table_start < section_end,
        if *tmp_318 == (*reference + unsafe{functor_data_unsigned(*table_start, *size as size_t)} as Address );

    relative_jump_table_entry_candidate(table_start, table_start, size, reference, dest, scale, 0) <--
        jump_table_start(_, size, table_start, reference, scale),
        jump_table_signed(table_start, inlined_signed_1119),
        if *inlined_signed_1119 == 0,
        if unsafe { functor_data_valid(*table_start, *size as size_t) } == 1,
        is_padding(inlined_dest_1119),
        after_end(inlined_dest_1119, inlined_end_1119),
        after_end(dest, inlined_end_1119),
        !is_padding(dest),
        if *inlined_dest_1119 == (reference + unsafe {functor_data_unsigned(*table_start, *size as size_t)} as Address),
        code_in_block(dest, _),
        loaded_section(section_start, section_end, _),
        if table_start >= section_start,
        if table_start < section_end;

    relative_jump_table_entry_candidate(table_start, table_start, size, reference, (((*reference as Number) + ((*scale as Number) * unsafe {functor_data_signed(*table_start, *size as size_t) })) as Address), scale, 0) <--
        jump_table_start(_, size, table_start, reference, scale),
        jump_table_signed(table_start, inlined_signed_1120),
        if *inlined_signed_1120 == 1,
        if unsafe {functor_data_valid(*table_start, *size as size_t) } == 1,
        code_in_block(tmp_319, _),
        loaded_section(section_start, section_end, _),
        if table_start >= section_start,
        if table_start < section_end,
        if *tmp_319 == (((*reference as Number) + ((*scale as Number) * unsafe {functor_data_signed(*table_start, *size as size_t)} )) as Address);

    relative_jump_table_entry_candidate(table_start, table_start, size, reference, dest, scale, 0) <--
        jump_table_start(_, size, table_start, reference, scale),
        jump_table_signed(table_start, inlined_signed_1121),
        if *inlined_signed_1121 == 1,
        if unsafe {functor_data_valid(*table_start, *size as size_t)} == 1,
        is_padding(inlined_dest_1121),
        after_end(inlined_dest_1121, inlined_end_1121),
        after_end(dest, inlined_end_1121),
        if *inlined_dest_1121 == (((*reference as Number) + ((*scale as Number) * unsafe {functor_data_signed(*table_start, *size as size_t)} )) as Address),
        !is_padding(dest),
        code_in_block(dest, _),
        loaded_section(section_start, section_end, _),
        if table_start >= section_start,
        if table_start < section_end;

        relative_jump_table_entry_candidate((last_ea + size), table_start, size, reference, (*reference + unsafe { functor_data_unsigned((last_ea + size), *size as size_t) } as Address), scale, offset) <--
        relative_jump_table_entry_candidate(last_ea, table_start, size, reference, _, scale, offset),
        jump_table_max(table_start, table_end),
        if *table_end >= (last_ea + size),
        !symbol((last_ea + size), _, _, _, _, _, _, _, _),
        data_segment(beg_data, end_data),
        if beg_data <= table_start,
        if ((last_ea + size) + size) <= *end_data,
        jump_table_signed(table_start, inlined_signed_1122),
        if *inlined_signed_1122 == 0,
        if unsafe { functor_data_valid((last_ea + size), *size as size_t) } == 1,
        code_in_block(_tmp_320, _),
        if *_tmp_320 == (*reference + unsafe { functor_data_unsigned((last_ea + size), *size as size_t) } as Address);

    relative_jump_table_entry_candidate((last_ea + size), table_start, size, reference, dest, scale, offset) <--
        relative_jump_table_entry_candidate(last_ea, table_start, size, reference, _, scale, offset),
        jump_table_max(table_start, table_end),
        if *table_end >= (last_ea + size),
        !symbol((last_ea + size), _, _, _, _, _, _, _, _),
        data_segment(beg_data, end_data),
        if beg_data <= table_start,
        if ((last_ea + size) + size) <= *end_data,
        jump_table_signed(table_start, inlined_signed_1123),
        if *inlined_signed_1123 == 0,
        if unsafe { functor_data_valid((last_ea + size), *size as size_t) } == 1,
        is_padding(inlined_dest_1123),
        after_end(inlined_dest_1123, inlined_end_1123),
        after_end(dest, inlined_end_1123),
        if *inlined_dest_1123 == (reference + unsafe { functor_data_unsigned((last_ea + size), *size as size_t) } as Address),

        !is_padding(dest),
        code_in_block(dest, _);

    relative_jump_table_entry_candidate((last_ea + size), table_start, size, reference, (((*reference as Number) + ((*scale as Number) * unsafe { functor_data_signed((last_ea + size), *size as size_t) })) as Address), scale, offset) <--
        relative_jump_table_entry_candidate(last_ea, table_start, size, reference, _, scale, offset),
        jump_table_max(table_start, table_end),
        if *table_end >= (last_ea + size),
        !symbol((last_ea + size), _, _, _, _, _, _, _, _),
        data_segment(beg_data, end_data),
        if beg_data <= table_start,
        if ((last_ea + size) + size) <= *end_data,
        jump_table_signed(table_start, inlined_signed_1124),
        if *inlined_signed_1124 == 1,
        if unsafe { functor_data_valid((last_ea + size), *size as size_t) } == 1,
        code_in_block(_tmp_321, _),
        if *_tmp_321 == (((*reference as Number) + ((*scale as Number) * unsafe { functor_data_signed((last_ea + size), *size as size_t) })) as Address);

    relative_jump_table_entry_candidate((last_ea + size), table_start, size, reference, dest, scale, offset) <--
        relative_jump_table_entry_candidate(last_ea, table_start, size, reference, _, scale, offset),
        jump_table_max(table_start, table_end),
        if *table_end >= (last_ea + size),
        !symbol((last_ea + size), _, _, _, _, _, _, _, _),
        data_segment(beg_data, end_data),
        if beg_data <= table_start,
        if ((last_ea + size) + size) <= *end_data,
        jump_table_signed(table_start, inlined_signed_1125),
        if *inlined_signed_1125 == 1,
        if unsafe { functor_data_valid((last_ea + size), *size as size_t) } == 1,
        is_padding(inlined_dest_1125),
        after_end(inlined_dest_1125, inlined_end_1125),
        after_end(dest, inlined_end_1125),
        if *inlined_dest_1125 == (((*reference as Number) + ((*scale as Number) * unsafe { functor_data_signed((last_ea + size), *size as size_t) })) as Address),
        !is_padding(dest),
        code_in_block(dest, _);

    stack_def_use_def_used(ea_def, var_def, ea_used, var_used, index) <--
        stack_def_use_live_var_at_block_end(block, block_used, var),
        stack_def_use_live_var_def(block, var_def, var, ea_def),
        stack_def_use_live_var_used(block_used, var, var_used, ea_used, index, _);

    stack_def_use_def_used(ea_def, def_var, ea_used, used_var, index) <--
        stack_def_use_live_var_used_in_block(_, ea, def_var, used_var, ea_used, index, _),
        may_fallthrough(ea_def, ea),
        code_in_block(ea_def, block),
        code_in_block(ea, block),
        stack_def_use_def(ea_def, def_var);

    stack_def_use_def_used(ea_def, var_def, next_ea_used, var_used, next_index) <--
        stack_def_use_live_var_at_prior_used(ea_used, next_used_block, var),
        stack_def_use_def_used(ea_def, var_def, ea_used, var, _),
        stack_def_use_live_var_used(next_used_block, var, var_used, next_ea_used, next_index, _);

    stack_def_use_live_var_at_block_end(prev_block, block_used, (inlined_base_reg_374, inlined_stack_pos_374)) <--
        stack_def_use_live_var_at_block_end(block, block_used, (inlined_base_reg_374, inlined_stack_pos_374)),
        !stack_def_use_ref_in_block(block, (inlined_base_reg_374, inlined_stack_pos_374)),
        !reg_def_use_defined_in_block(block, inlined_base_reg_374),
        block_next(prev_block, _, block);

    stack_def_use_live_var_at_block_end(prev_block, block, var) <--
        block_next(prev_block, _, block),
        stack_def_use_live_var_used(block, var, _, _, _, _);

    stack_def_use_live_var_at_prior_used(ea_used, block_used, (inlined_base_reg_375, inlined_stack_pos_375)) <--
        stack_def_use_live_var_at_block_end(block, block_used, (inlined_base_reg_375, inlined_stack_pos_375)),
        stack_def_use_used_in_block(block, ea_used, (*inlined_base_reg_375, inlined_stack_pos_375), _),
        !reg_def_use_defined_in_block(block, inlined_base_reg_375),
        !stack_def_use_defined_in_block(block, (inlined_base_reg_375, inlined_stack_pos_375));

    stack_def_use_live_var_used(block, live_var, used_var, ea_used, index, moves) <--
        stack_def_use_live_var_used_in_block(block, block, live_var, used_var, ea_used, index, moves);

    stack_def_use_live_var_used_in_block(block, next_ea, (*base_reg, stack_pos), var_used, ea_used, index, (moves + 1)) <--
        adjusts_stack_in_block(block, _, base_reg, _),
        stack_def_use_live_var_at_block_end(block, block_used, (base_reg, stack_pos)),
        stack_def_use_live_var_used(block_used, (base_reg, stack_pos), var_used, ea_used, index, moves),
        stack_def_use_moves_limit(moves_limit),
        if moves <= moves_limit,
        block_last_instruction(block, last_ea),
        next(last_ea, next_ea);

    stack_def_use_live_var_used_in_block(block, next_ea, (*base_reg, stack_pos), var_used, ea_used, index, (moves + 1)) <--
        stack_base_reg_move(block, _, _, base_reg),
        stack_def_use_live_var_at_block_end(block, block_used, (base_reg, stack_pos)),
        stack_def_use_live_var_used(block_used, (base_reg, stack_pos), var_used, ea_used, index, moves),
        stack_def_use_moves_limit(moves_limit),
        if moves <= moves_limit,
        block_last_instruction(block, last_ea),
        next(last_ea, next_ea);

    stack_def_use_live_var_used_in_block(block, ea, (base_reg, stack_pos), (final_base_reg, final_stack_pos), ea_used, index, moves) <--
        stack_def_use_live_var_used_in_block(block, next, (base_reg, stack_pos), (final_base_reg, final_stack_pos), ea_used, index, moves),
        block_instruction_next(block, ea, next),
        !reg_def_use_def(ea, base_reg),
        !stack_def_use_def(ea, (base_reg, stack_pos));

    stack_def_use_live_var_used_in_block(block, ea, (*base_reg, (stack_pos + offset)), used_var, ea_used, index, moves) <--
        stack_def_use_live_var_used_in_block(block, next, (*base_reg, stack_pos), used_var, ea_used, index, moves),
        block_instruction_next(block, ea, next),
        adjusts_stack_in_block(_, ea, base_reg, offset),
        !stack_def_use_def(ea, (base_reg, stack_pos)),
        arch_stack_pointer(base_reg),
        if (stack_pos + offset) >= 0;

    stack_def_use_live_var_used_in_block(block, ea, (*base_reg, (stack_pos + offset)), used_var, ea_used, index, moves) <--
        stack_def_use_live_var_used_in_block(block, next, (base_reg, stack_pos), used_var, ea_used, index, moves),
        block_instruction_next(block, ea, next),
        adjusts_stack_in_block(_, ea, base_reg, offset),
        !stack_def_use_def(ea, (base_reg, stack_pos)),
        !arch_stack_pointer(base_reg);

    stack_def_use_live_var_used_in_block(block, ea, (*src_base_reg, stack_pos), used_var, ea_used, index, moves) <--

        stack_def_use_live_var_used_in_block(block, next, (dst_base_reg, stack_pos), used_var, ea_used, index, moves),
        block_instruction_next(block, ea, next),
        stack_base_reg_move(_, ea, src_base_reg, dst_base_reg);

    tls_desc_call(load, call, (start + offset)) <--
        tls_segment(start, _, _),
        tls_descriptor(ea, offset),
        pc_relative_operand(load, _, ea),
        arch_call(call, _),
        const_value_reg_used(call, load, _, _, _tmp_137),
        if *_tmp_137 == (*ea as Number);

    tls_desc_call(load, call, (start + offset)) <--
        tls_segment(start, _, _),
        tls_descriptor(ea, offset),
        got_relative_operand(load, _, ea),
        arch_call(call, _),
        const_value_reg_used(call, load, _, _, _tmp_138),
        if *_tmp_138 == (*ea as Number);

    tls_get_addr(load, call, (start + offset)) <--
        binary_format("ELF".to_string()),
        pc_relative_operand(load, _, ea),
        tls_index(ea, offset),
        reg_def_use_def_used(load, reg, call, _),
        call_tls_get_addr(call, reg),
        tls_segment(start, _, _);

    value_reg(ea, reg, ea, "None".to_string(), 0, immediate, 1) <--
        def_used_for_address(ea, reg, _),
        arch_move_reg_imm(ea, reg, immediate, _),
        !instruction_has_relocation(ea, _);

    value_reg(ea, reg, ea, "None".to_string(), 0, 0, 1) <--
        def_used_for_address(ea, reg, _),
        is_xor_reset(ea);

    value_reg(ea, reg, ea, "None".to_string(), 0, immediate, 1) <--
        def_used_for_address(ea, reg, _),
        reg_def_use_flow_def(ea, reg, _, immediate);

    value_reg(ea, reg, ea, reg, 1, 0, 1) <--
        def_used_for_address(ea, reg, _),
        value_reg_unsupported(ea, reg);

    value_reg(ea, reg, ea, reg, 1, 0, 1) <--
        def_used_for_address(ea, reg, _),
        value_reg_unsupported(ea, reg);

    value_reg(ea, reg, ea_from, "Unknown".to_string(), immediate, base, (steps + 1)) <--
        step_limit(step_limit),
        value_reg(ea, reg, ea_from, "None".to_string(), 0, base, steps),
        if *steps <= (step_limit - 2),
        value_reg_edge(ea, reg, ea, reg, 1, immediate),
        if *immediate != 0;

    value_reg(ea1, reg1, ea3, reg3, (multiplier * multiplier2), ((offset2 * multiplier) + offset), (steps + 1)) <--
        step_limit(step_limit),
        value_reg(ea2, reg2, ea3, reg3, multiplier2, offset2, steps),
        if *steps <= (step_limit - 2),
        value_reg_edge(ea1, reg1, ea2, reg2, multiplier, offset),
        if ea1 > ea2;

    value_reg(ea1, reg1, ea3, reg3, (multiplier * multiplier2), ((offset2 * multiplier) + offset), (steps + 5)) <--
        step_limit(step_limit),
        value_reg(ea2, reg2, ea3, reg3, multiplier2, offset2, steps),
        if *steps <= (step_limit - 6),
        value_reg_edge(ea1, reg1, ea2, reg2, multiplier, offset),
        if ea1 < ea2;

    value_reg(ea, reg, ea, "None".to_string(), 0, (*address as Number), 1) <--
        arch_return_reg(reg),
        tls_get_addr(_, ea, address);

    value_reg(ea, reg, ea, "None".to_string(), 0, (*address as Number), 1) <--
        arch_return_reg(reg),
        tls_desc_call(_, ea, address);

        value_reg(ea, reg, ea, "NONE".to_string(), 0, (*address as Number), 1) <--
        def_used_for_address(ea, reg, _),
        instruction_has_relocation(ea, ea_rel),
        symbolic_expr_from_relocation(ea_rel, _, _, _, address);

    value_reg(ea, reg, ea, "NONE".to_string(), 0, (*offset as Number), 1) <--
        binary_format("ELF".to_string()),
        got_relative_operand(ea, 1, offset),
        instruction(ea, _, _, "LEA".to_string(), _, op2, 0, 0, _, _),
        op_regdirect_contains_reg(op2, reg),
        track_register(reg);

    value_reg(ea, reg_def, ea_third, reg3, (mult1 + (mult * mult2)), ((offset + offset1) + (offset2 * mult)), (max(*steps1, *steps2) + 2)) <--
        step_limit(step_limit),
        reg_reg_arithmetic_operation_defs(ea, reg_def, ea_def1, reg1, ea_def2, reg2, mult, offset),
        value_reg(ea_def1, reg1, ea_third, reg3, mult1, offset1, steps1),
        if *steps1 <= (*step_limit - 3),
        if ea != ea_third,
        value_reg(ea_def2, reg2, ea_third, reg3, mult2, offset2, steps2),
        if *steps2 <= (*step_limit - 3);

    value_reg(ea, reg_def, ea_third, reg3, (mult * mult2), ((offset + offset1) + (offset2 * mult)), (max(*steps1, *steps2) + 2)) <--
        step_limit(step_limit),
        reg_reg_arithmetic_operation_defs(ea, reg_def, ea_def1, reg1, ea_def2, reg2, mult, offset),
        value_reg(ea_def1, reg1, _, "NONE".to_string(), _, offset1, steps1),
        if *steps1 <= (*step_limit - 3),
        value_reg(ea_def2, reg2, ea_third, reg3, mult2, offset2, steps2),
        if *steps2 <= (*step_limit - 3),
        if ea != ea_third,
        if *reg3 != "NONE".to_string();

    // Todo: add max
    value_reg(ea, reg_def, ea_third, reg3, mult1, ((offset + offset1) + (offset2 * mult)), (max(steps1, steps2) + 2)) <--
        step_limit(step_limit),
        reg_reg_arithmetic_operation_defs(ea, reg_def, ea_def1, reg1, ea_def2, reg2, mult, offset),
        value_reg(ea_def2, reg2, _, "NONE".to_string(), _, offset2, steps2),
        if *steps2 <= (*step_limit - 3),
        value_reg(ea_def1, reg1, ea_third, reg3, mult1, offset1, steps1),
        if *steps1 <= (*step_limit - 3),
        if *reg3 != "NONE".to_string(),
        if ea != ea_third;

    value_reg(ea_load, reg2, ea_load, "NONE".to_string(), 0, immediate, 1) <--
        arch_store_immediate(ea_store, _, _, immediate, reg_base_store, "NONE".to_string(), _, stack_pos_store),
        stack_def_use_def_used(ea_store, (*reg_base_store, *stack_pos_store as Address), ea_load, (*reg_base_load, stack_pos_load), _),
        arch_memory_access("LOAD".to_string(), ea_load, _, _, reg2, reg_base_load, "NONE".to_string(), _, stack_pos_load),
        def_used_for_address(ea_load, reg2, _);

    value_reg(ea, reg, ea, "NONE".to_string(), 0, (*target_addr as Number), 1) <--
        def_used_for_address(ea, reg, _),
        arch_memory_access("LOAD".to_string(), ea, src_op, _, reg, _, _, _, _),
        simple_data_access_pattern(mem_addr, src_op, size, ea),
        if 4 <= *size,
        if *size <= 8,
        symbolic_expr_from_relocation(mem_addr, size, symbol, _, target_addr),
        defined_symbol(_, _, _, _, _, _, _, _, symbol),
        if (*target_addr as Number) >= 0;

    value_reg(ea, reg, ea, "NONE".to_string(), 0, unsafe {functor_data_signed(*mem_addr, *size as size_t)}, 1) <--
        def_used_for_address(ea, reg, _),
        arch_memory_access("LOAD".to_string(), ea, src_op, _, reg, _, _, _, _),
        simple_data_access_pattern(mem_addr, src_op, size, ea),
        if 4 <= *size,
        if *size <= 8,
        if unsafe {functor_data_valid(*mem_addr, *size as size_t)} == 1,
        !symbolic_expr_from_relocation(mem_addr, _, _, _, _),
        if unsafe {functor_data_signed(*mem_addr, *size as size_t)} >= 0;

    value_reg_edge(ea, reg, ea_prev, reg_origin, 1, 0) <--
        def_used_for_address(ea_prev, reg_origin, _),
        reg_def_use_def_used(ea_prev, reg_origin, ea, _),
        arch_move_reg_reg(ea, reg, reg_origin),
        track_register(reg),
        if ea != ea_prev;

    value_reg_edge(ea, dst, ea_prev, src, mult, immediate) <--
        def_used_for_address(ea_prev, src, _),
        reg_def_use_def_used(ea_prev, src, ea, _),
        arch_reg_arithmetic_operation(ea, dst, src, mult, immediate),
        track_register(dst);

    value_reg_edge(ea_load, reg2, ea_prev, reg1, 1, 0) <--
        stack_def_use_def_used(ea_store, (*reg_base_store as Register, *stack_pos_store as Address), ea_load, 
        (*reg_base_load as Register, *stack_pos_load as Address), _),
            arch_memory_access("STORE".to_string(), ea_store, _, _, reg1, reg_base_store, "NONE".to_string(), _, stack_pos_store),
            arch_memory_access("LOAD".to_string(), ea_load, _, _, reg2, reg_base_load, "NONE".to_string(), _, stack_pos_load),
            reg_def_use_def_used(ea_prev, reg1, ea_store, _);

    value_reg_limit(ea_jmp, ea_branch, reg, (immediate + branch_offset), branch_lt) <--
        compare_and_jump_immediate(_, ea_jmp, cc, reg, immediate),
        track_register(reg),
        limit_type_map(cc, branch_lt, _, branch_offset, _),
        direct_jump(ea_jmp, ea_branch),
        may_fallthrough(ea_jmp, _);

    value_reg_limit(ea_jmp, ea_fallthrough, reg, (immediate + fallthrough_offset), fallthrough_lt) <--
        compare_and_jump_immediate(_, ea_jmp, cc, reg, immediate),
        track_register(reg),
        limit_type_map(cc, _, fallthrough_lt, _, fallthrough_offset),
        direct_jump(ea_jmp, _),
        may_fallthrough(ea_jmp, ea_fallthrough);

    value_reg_limit(ea_jmp, ea_branch, reg2, (immediate + offset1), lt1) <--
        compare_and_jump_register(ea_cmp, ea_jmp, cc, reg1, reg2),
        limit_type_map(cc, lt1, _, offset1, _),
        reg_def_use_block_last_def(ea_cmp, ea_regdef, reg1),
        arch_move_reg_imm(ea_regdef, reg1, immediate, _),
        track_register(reg2),
        direct_jump(ea_jmp, ea_branch),
        may_fallthrough(ea_jmp, _);

    value_reg_limit(ea_jmp, ea_branch, reg1, (immediate + offset2), lt2) <--
        compare_and_jump_register(ea_cmp, ea_jmp, cc, reg1, reg2),
        limit_type_map(cc, _, lt2, _, offset2),
        reg_def_use_block_last_def(ea_cmp, ea_regdef, reg2),
        arch_move_reg_imm(ea_regdef, reg2, immediate, _),
        track_register(reg1),
        direct_jump(ea_jmp, ea_branch),
        may_fallthrough(ea_jmp, _);

    value_reg_limit(ea_jmp, ea_fallthrough, reg2, (immediate + offset2), lt2) <--
        compare_and_jump_register(ea_cmp, ea_jmp, cc, reg1, reg2),
        limit_type_map(cc, _, lt2, _, offset2),
        reg_def_use_block_last_def(ea_cmp, ea_regdef, reg1),
        arch_move_reg_imm(ea_regdef, reg1, immediate, _),
        track_register(reg2),
        direct_jump(ea_jmp, _),
        may_fallthrough(ea_jmp, ea_fallthrough);

    value_reg_limit(ea_jmp, ea_fallthrough, reg1, (immediate + offset1), lt1) <--
        compare_and_jump_register(ea_cmp, ea_jmp, cc, reg1, reg2),
        limit_type_map(cc, lt1, _, offset1, _),
        reg_def_use_block_last_def(ea_cmp, ea_regdef, reg2),
        arch_move_reg_imm(ea_regdef, reg2, immediate, _),
        track_register(reg1),
        direct_jump(ea_jmp, _),
        may_fallthrough(ea_jmp, ea_fallthrough);

    value_reg_limit(ea_target, ea_limited, reg, (immediate + branch_offset), branch_lt) <--
        compare_and_jump_indirect(ea_cmp, ea_jmp, cc, indirect_op, immediate),
        limit_type_map(cc, branch_lt, _, branch_offset, _),
        next(ea_cmp, ea_jmp),
        direct_jump(ea_jmp, ea_target),
        arch_memory_access("LOAD".to_string(), ea_target, _, _, reg, _, _, _, _),
        track_register(reg),
        instruction_get_op(ea_target, _, indirect_op),
        code_in_block(ea_target, inlined_block_887),
        may_fallthrough(ea_target, ea_limited),
        code_in_block(ea_limited, inlined_block_887);

    value_reg_limit(ea_target, ea_limited, reg, (immediate + fallthrough_offset), fallthrough_lt) <--
        compare_and_jump_indirect(ea_cmp, ea_jmp, cc, indirect_op, immediate),
        limit_type_map(cc, _, fallthrough_lt, _, fallthrough_offset),
        next(ea_cmp, ea_jmp),
        may_fallthrough(ea_jmp, ea_target),
        arch_memory_access("LOAD".to_string(), ea_target, _, _, reg, _, _, _, _),
        track_register(reg),
        instruction_get_op(ea_target, _, indirect_op),
        code_in_block(ea_target, inlined_block_888),
        may_fallthrough(ea_target, ea_limited),
        code_in_block(ea_limited, inlined_block_888);

    value_reg_unsupported(ea, reg) <--
        def_used_for_address(ea, reg, _),
        arch_move_reg_reg(ea, dst, src),
        track_register(dst),
        !track_register(src);

    value_reg_unsupported(ea, reg) <--
        def_used_for_address(ea, reg, _),
        arch_call(ea, _);

    value_reg_unsupported(ea, reg) <--
        def_used_for_address(ea, reg, _),
        arch_memory_access("LOAD".to_string(), ea, _, _, reg, reg_base, _, _, _),
        if reg_base != "NONE".to_string();

    value_reg_unsupported(ea, reg) <--
        def_used_for_address(ea, reg, _),
        arch_memory_access("LOAD".to_string(), ea, _, _, reg, _, reg_index, _, _),
        if reg_index != "NONE".to_string();

    // Todo solve this
    // value_reg(EA,Reg,EA_reg1,Reg1,Multiplier,Offset,Steps1) <= value_reg(EA,Reg,EA_reg1,Reg1,Multiplier,Offset,Steps2) :- 
    // Steps2 <= Steps1.
    // value_reg(EA,Reg,EA_reg1,Reg1,Multiplier,Offset,Steps1) <= value_reg(EA,Reg,EA_reg1,Reg1,Multiplier,Offset,Steps2) :- 
    //     Steps2 <= Steps1.
    // value_reg(EA,Reg,EA_reg1,Reg1,Multiplier,Offset,Steps1) <= value_reg(EA,Reg,EA_reg1,Reg1,Multiplier,Offset,Steps2) :- 
    //     Steps2 <= Steps1.    

}

fn main() {
    let mut program = DDisasm::default();

    // arguments
    // dd_ascent <db_dir> <binary_path>
    let db_dir = std::env::args().nth(1).unwrap();
    let binary_path = std::env::args().nth(2).unwrap();
    // run command using rust's std::process::Command
    // ddisasm -j 12 --debug-dir <db_dir> <binary_path>
    let _output = Command::new("ddisasm")
        .arg("-j")
        .arg("12")
        .arg("--debug-dir")
        .arg(&db_dir)
        .arg(&binary_path)
        .output()
        .expect("failed to execute process");

    let path = format!("{}/disassembly/", db_dir);
    let get_path = |x: &str| format!("{path}{x}");

    program.adjusts_stack_in_block = read_csv::<(Address, Address, Register, i64)>(&get_path("adjusts_stack_in_block.facts"))
        .map(|(x, y, z, xx)| (x, y, z, xx))
        .collect_vec();
    program.after_end = read_csv::<(Address, Address)>(&get_path("after_end.facts"))
        .map(|(x, y)| (x, y))
        .collect_vec();

    // Attention on the path
    program.arch_call = read_csv::<(Address, OperandIndex)>(&get_path("arch.call.facts"))
        .map(|(x, y)| (x, y))
        .collect_vec();

    // .input arch_cmp_operation(filename="arch.cmp_operation.facts")
    program.arch_cmp_operation = read_csv::<(String,)>(&get_path("arch.cmp_operation.facts"))
        .map(|(x,)| (x,))
        .collect_vec();


    // .input arch_cmp_zero_operation(filename="arch.cmp_zero_operation.facts")
    program.arch_cmp_zero_operation = read_csv::<(String,)>(&get_path("arch.cmp_zero_operation.facts"))
        .map(|(x,)| (x,))
        .collect_vec();

    // arch_cmp_zero_operation("")<--
    //     false.
    // Todo

    // .input arch_conditional(filename="arch.conditional.facts")
    program.arch_conditional = read_csv::<(Address, ConditionCode)>(&get_path("arch.conditional.facts"))
        .map(|(x, y)| (x, y))
        .collect_vec();

    // .input arch_condition_flags_reg(filename="arch.condition_flags_reg.facts")
    program.arch_condition_flags_reg = read_csv::<(Register,)>(&get_path("arch.condition_flags_reg.facts"))
        .map(|(x,)| (x,))
        .collect_vec();

    // .input arch_extend_load(filename="arch.extend_load.facts")
    program.arch_extend_load = read_csv::<(Address, u64, u64)>(&get_path("arch.extend_load.facts"))
        .map(|(x, y, z)| (x, y, z))
        .collect_vec();

    // .input arch_extend_reg(filename="arch.extend_reg.facts")
    program.arch_extend_reg = read_csv::<(Address, Register, u64, u64)>(&get_path("arch.extend_reg.facts"))
        .map(|(x, y, z, xx)| (x, y, z, xx))
        .collect_vec();

    // .input arch_jump(filename="arch.jump.facts")
    program.arch_jump = read_csv::<(Address,)>(&get_path("arch.jump.facts"))
        .map(|(x,)| (x,))
        .collect_vec();

    // .input arch_memory_access(filename="arch.memory_access.facts")
    program.arch_memory_access = read_csv::<(AccessMode, Address, OperandIndex, OperandIndex, Register, RegNullable, RegNullable, i64, i64)>(&get_path("arch.memory_access.facts"))
        .map(|(a, b, c, d, e, f, g, h, i)| (a, b, c, d, e, f, g, h, i))
        .collect_vec();

    // .input arch_move_reg_imm(filename="arch.move_reg_imm.facts")
    program.arch_move_reg_imm = read_csv::<(Address, Register, i64, OperandIndex)>(&get_path("arch.move_reg_imm.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    // .input arch_move_reg_reg(filename="arch.move_reg_reg.facts")
    program.arch_move_reg_reg = read_csv::<(Address, Register, Register)>(&get_path("arch.move_reg_reg.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();


    // arch_pc_relative_addr(0,"",0)<-- false.
    // Todo

    // .input arch_reg_arithmetic_operation(filename="arch.reg_arithmetic_operation.facts")
    program.arch_reg_arithmetic_operation = read_csv::<(Address, Register, Register, i64, i64)>(&get_path("arch.reg_arithmetic_operation.facts"))
        .map(|(a, b, c, d, e)| (a, b, c, d, e))
        .collect_vec();

    // .input arch_reg_reg_arithmetic_operation(filename="arch.reg_reg_arithmetic_operation.facts")
    program.arch_reg_reg_arithmetic_operation = read_csv::<(Address, Register, Register, Register, i64, i64)>(&get_path("arch.reg_reg_arithmetic_operation.facts"))
        .map(|(a, b, c, d, e, f)| (a, b, c, d, e, f))
        .collect_vec();

    // .input arch_register_size_bytes(filename="arch.register_size_bytes.facts")
    program.arch_register_size_bytes = read_csv::<(InputReg, u64)>(&get_path("arch.register_size_bytes.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    // .input arch_return_reg(filename="arch.return_reg.facts")
    program.arch_return_reg = read_csv::<(Register,)>(&get_path("arch.return_reg.facts"))
        .map(|(a,)| (a,))
        .collect_vec();


    // .input arch_stack_pointer(filename="arch.stack_pointer.facts")
    program.arch_stack_pointer = read_csv::<(Register,)>(&get_path("arch.stack_pointer.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    // .input arch_store_immediate(filename="arch.store_immediate.facts")
    program.arch_store_immediate = read_csv::<(Address, OperandIndex, OperandIndex, i64, RegNullable, RegNullable, i64, i64)>(&get_path("arch.store_immediate.facts"))
        .map(|(a, b, c, d, e, f, g, h)| (a, b, c, d, e, f, g, h))
        .collect_vec();

    // arch_store_immediate(0,0,0,0,"NONE","NONE",0,0)<-- false.
    // Todo

    // .input base_address
    program.base_address = read_csv::<(Address,)>(&get_path("base_address.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    // .input base_relative_operation
    program.base_relative_operation = read_csv::<(Address, Address)>(&get_path("base_relative_operation.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    // .input binary_format
    program.binary_format = read_csv::<(String,)>(&get_path("binary_format.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    // .input block
    program.block = read_csv::<(Address,)>(&get_path("block.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.block_last_instruction = read_csv::<(Address, Address)>(&get_path("block_last_instruction.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.block_instruction_next = read_csv::<(Address, Address, Address)>(&get_path("block_instruction_next.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.call_tls_get_addr = read_csv::<(Address, Register)>(&get_path("call_tls_get_addr.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.cmp_immediate_to_reg = read_csv::<(Address, Register, OperandIndex, i64)>(&get_path("cmp_immediate_to_reg.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.cmp_reg_to_reg = read_csv::<(Address, Register, Register)>(&get_path("cmp_reg_to_reg.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.code_in_block = read_csv::<(Address, Address)>(&get_path("code_in_block.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.conditional_jump = read_csv::<(Address,)>(&get_path("conditional_jump.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.data_access = read_csv::<(Address, OperandIndex, RegNullable, RegNullable, RegNullable, i64, i64, u64)>(&get_path("data_access.facts"))
        .map(|(a, b, c, d, e, f, g, h)| (a, b, c, d, e, f, g, h))
        .collect_vec();

    program.data_segment = read_csv::<(Address, Address)>(&get_path("data_segment.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.defined_symbol = read_csv::<(Address, u64, String, String, String, u64, String, u64, String)>(&get_path("defined_symbol.facts"))
        .map(|(a, b, c, d, e, f, g, h, i)| (a, b, c, d, e, f, g, h, i))
        .collect_vec();

    program.direct_call = read_csv::<(Address, Address)>(&get_path("direct_call.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.direct_jump = read_csv::<(Address, Address)>(&get_path("direct_jump.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.got_reference_pointer = read_csv::<(Address,)>(&get_path("got_reference_pointer.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.got_section = read_csv::<(String,)>(&get_path("got_section.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.instruction = read_csv::<(Address, u64, String, String, OperandCode, OperandCode, OperandCode, OperandCode, u64, u64)>(&get_path("instruction.facts"))
        .map(|(a, b, c, d, e, f, g, h, i, j)| (a, b, c, d, e, f, g, h, i, j))
        .collect_vec();

    program.instruction_displacement_offset = read_csv::<(Address, OperandIndex, u64, u64)>(&get_path("instruction_displacement_offset.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.instruction_get_dest_op = read_csv::<(Address, OperandIndex, OperandCode)>(&get_path("instruction_get_dest_op.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.instruction_get_op = read_csv::<(Address, OperandIndex, OperandCode)>(&get_path("instruction_get_op.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.instruction_get_src_op = read_csv::<(Address, OperandIndex, OperandCode)>(&get_path("instruction_get_src_op.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.instruction_has_relocation = read_csv::<(Address, Address)>(&get_path("instruction_has_relocation.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.inter_procedural_edge = read_csv::<(Address, Address)>(&get_path("inter_procedural_edge.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.is_padding = read_csv::<(Address,)>(&get_path("is_padding.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.is_xor_reset = read_csv::<(Address,)>(&get_path("is_xor_reset.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.limit_reg_op = read_csv::<(Address, Register, Register, i64)>(&get_path("limit_reg_op.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.limit_type_map = read_csv::<(ConditionCode, LimitType, LimitType, i64, i64)>(&get_path("limit_type_map.facts"))
        .map(|(a, b, c, d, e)| (a, b, c, d, e))
        .collect_vec();

    program.loaded_section = read_csv::<(Address, Address, String)>(&get_path("loaded_section.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.lsda_callsite_addresses = read_csv::<(Address, Address, Address)>(&get_path("lsda_callsite_addresses.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.may_fallthrough = read_csv::<(Address, Address)>(&get_path("may_fallthrough.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.next = read_csv::<(Address, Address)>(&get_path("next.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.no_return_call_propagated = read_csv::<(Address,)>(&get_path("no_return_call_propagated.facts"))
        .map(|(a,)| (a,))
        .collect_vec();
    
    program.no_value_reg_limit = read_csv::<(Address,)>(&get_path("no_value_reg_limit.facts"))
    .map(|(a,)| (a,))
    .collect_vec();

    program.op_immediate = read_csv::<(OperandCode, i64, u64)>(&get_path("op_immediate.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.op_immediate_and_reg = read_csv::<(Address, String, Register, OperandIndex, i64)>(&get_path("op_immediate_and_reg.facts"))
        .map(|(a, b, c, d, e)| (a, b, c, d, e))
        .collect_vec();

    program.op_indirect = read_csv::<(OperandCode, InputReg, InputReg, InputReg, i64, i64, u64)>(&get_path("op_indirect.facts"))
        .map(|(a, b, c, d, e, f, g)| (a, b, c, d, e, f, g))
        .collect_vec();

    program.op_indirect_mapped = read_csv::<(OperandCode, RegNullable, RegNullable, RegNullable, i64, i64, u64)>(&get_path("op_indirect_mapped.facts"))
        .map(|(a, b, c, d, e, f, g)| (a, b, c, d, e, f, g))
        .collect_vec();

    program.op_regdirect = read_csv::<(OperandCode, InputReg)>(&get_path("op_regdirect.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.op_regdirect_contains_reg = read_csv::<(OperandCode, Register)>(&get_path("op_regdirect_contains_reg.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.pc_relative_operand = read_csv::<(Address, OperandIndex, Address)>(&get_path("pc_relative_operand.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.possible_rva_operand = read_csv::<(Address, OperandIndex, Address)>(&get_path("possible_rva_operand.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.reg_call = read_csv::<(Address, Register)>(&get_path("reg_call.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.reg_def_use_block_last_def = read_csv::<(Address, Address, Register)>(&get_path("reg_def_use.block_last_def.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.reg_def_use_def = read_csv::<(Address, Register)>(&get_path("reg_def_use.def.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.reg_def_use_defined_in_block = read_csv::<(Address, Register)>(&get_path("reg_def_use.defined_in_block.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.reg_def_use_flow_def = read_csv::<(Address, Register, Address, i64)>(&get_path("reg_def_use.flow_def.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.reg_def_use_live_var_def = read_csv::<(Address, Register, Register, Address)>(&get_path("reg_def_use.live_var_def.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.reg_def_use_ref_in_block = read_csv::<(Address, Register)>(&get_path("reg_def_use.ref_in_block.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();
    
    program.reg_def_use_return_block_end = read_csv::<(Address, Address, Address, Address)>(&get_path("reg_def_use.return_block_end.facts"))
    .map(|(a, b, c, d)| (a, b, c, d))
    .collect_vec();

    program.reg_def_use_used = read_csv::<(Address, Register, OperandIndex)>(&get_path("reg_def_use.used.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.reg_def_use_used_in_block = read_csv::<(Address, Address, Register, OperandIndex)>(&get_path("reg_def_use.used_in_block.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.reg_jump = read_csv::<(Address, Register)>(&get_path("reg_jump.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.reg_map = read_csv::<(InputReg, Register)>(&get_path("reg_map.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.reg_used_for = read_csv::<(Address, Register, String)>(&get_path("reg_used_for.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.register_access = read_csv::<(Address, InputReg, AccessMode)>(&get_path("register_access.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.relative_address = read_csv::<(Address, u64, Address, Address, Address, String)>(&get_path("relative_address.facts"))
        .map(|(a, b, c, d, e, f)| (a, b, c, d, e, f))
        .collect_vec();

    program.relative_address_start = read_csv::<(Address, u64, Address, Address, String)>(&get_path("relative_address_start.facts"))
        .map(|(a, b, c, d, e)| (a, b, c, d, e))
        .collect_vec();

    program.relocation = read_csv::<(Address, String, String, i64, u64, String, String)>(&get_path("relocation.facts"))
        .map(|(a, b, c, d, e, f, g)| (a, b, c, d, e, f, g))
        .collect_vec();

    program.relocation_adjustment_total = read_csv::<(Address, i64)>(&get_path("relocation_adjustment_total.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.simple_data_access_pattern = read_csv::<(Address, u64, u64, Address)>(&get_path("simple_data_access_pattern.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.stack_base_reg_move = read_csv::<(Address, Address, Register, Register)>(&get_path("stack_base_reg_move.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.stack_def_use_block_last_def = read_csv::<(Address, Address, StackVar)>(&get_path("stack_def_use.block_last_def.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.stack_def_use_def = read_csv::<(Address, StackVar)>(&get_path("stack_def_use.def.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.stack_def_use_defined_in_block = read_csv::<(Address, StackVar)>(&get_path("stack_def_use.defined_in_block.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.stack_def_use_live_var_def = read_csv::<(Address, StackVar, StackVar, Address)>(&get_path("stack_def_use.live_var_def.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.stack_def_use_moves_limit = read_csv::<(u64,)>(&get_path("stack_def_use.moves_limit.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.stack_def_use_ref_in_block = read_csv::<(Address, StackVar)>(&get_path("stack_def_use.ref_in_block.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.stack_def_use_used = read_csv::<(Address, StackVar, OperandIndex)>(&get_path("stack_def_use.used.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.stack_def_use_used_in_block = read_csv::<(Address, Address, StackVar, OperandIndex)>(&get_path("stack_def_use.used_in_block.facts"))
        .map(|(a, b, c, d)| (a, b, c, d))
        .collect_vec();

    program.step_limit = read_csv::<(u64,)>(&get_path("step_limit.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.symbol = read_csv::<(Address, u64, String, String, String, u64, String, u64, String)>(&get_path("symbol.facts"))
        .map(|(a, b, c, d, e, f, g, h, i)| (a, b, c, d, e, f, g, h, i))
        .collect_vec();

    program.symbolic_expr_from_relocation = read_csv::<(Address, u64, String, i64, Address)>(&get_path("symbolic_expr_from_relocation.facts"))
        .map(|(a, b, c, d, e)| (a, b, c, d, e))
        .collect_vec();

    program.take_address = read_csv::<(Address, Address)>(&get_path("take_address.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.tls_descriptor = read_csv::<(Address, u64)>(&get_path("tls_descriptor.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.tls_index = read_csv::<(Address, u64)>(&get_path("tls_index.facts"))
        .map(|(a, b)| (a, b))
        .collect_vec();

    program.tls_segment = read_csv::<(Address, Address, u64)>(&get_path("tls_segment.facts"))
        .map(|(a, b, c)| (a, b, c))
        .collect_vec();

    program.track_register = read_csv::<(Register,)>(&get_path("track_register.facts"))
        .map(|(a,)| (a,))
        .collect_vec();

    program.run();

    println!("{:?}", program.scc_times_summary());
    println!("{:?}", program.relation_sizes_summary());

    let reg_jump_size = program.reg_jump.len();
    println!("reg_jump size: {:?}", reg_jump_size);

    let base_relative_operation_size = program.base_relative_operation.len();
    println!("base_relative_operation size: {:?}", base_relative_operation_size);

    let value_reg_size = program.value_reg.len();
    println!("value_reg size: {:?}", value_reg_size);
    let base_relative_jump_size = program.base_relative_jump.len();
    println!("base_relative_jump size: {:?}", base_relative_jump_size);
    let base_relative_operand_size = program.base_relative_operand.len();
    println!("base_relative_operand size: {:?}", base_relative_operand_size);
    let block_next_size = program.block_next.len();
    println!("block_next size: {:?}", block_next_size);
    let cmp_defines_size = program.cmp_defines.len();
    println!("cmp_defines size: {:?}", cmp_defines_size);
    let compare_and_jump_immediate_size = program.compare_and_jump_immediate.len();
    println!("compare_and_jump_immediate size: {:?}", compare_and_jump_immediate_size);
    let compare_and_jump_indirect_size = program.compare_and_jump_indirect.len();
    println!("compare_and_jump_indirect size: {:?}", compare_and_jump_indirect_size);
    let compare_and_jump_register_size = program.compare_and_jump_register.len();
    println!("compare_and_jump_register size: {:?}", compare_and_jump_register_size);
    let const_value_reg_used_size = program.const_value_reg_used.len();
    println!("const_value_reg_used size: {:?}", const_value_reg_used_size);
    let def_used_for_address_size = program.def_used_for_address.len();
    println!("def_used_for_address size: {:?}", def_used_for_address_size);
    let flags_and_jump_pair_size = program.flags_and_jump_pair.len();
    println!("flags_and_jump_pair size: {:?}", flags_and_jump_pair_size);
    let got_relative_operand_size = program.got_relative_operand.len();
    println!("got_relative_operand size: {:?}", got_relative_operand_size);
    let jump_table_element_access_size = program.jump_table_element_access.len();
    println!("jump_table_element_access size: {:?}", jump_table_element_access_size);
    let jump_table_max_size = program.jump_table_max.len();
    println!("jump_table_max size: {:?}", jump_table_max_size);
    let jump_table_signed_size = program.jump_table_signed.len();
    println!("jump_table_signed size: {:?}", jump_table_signed_size);
    let jump_table_start_size = program.jump_table_start.len();
    println!("jump_table_start size: {:?}", jump_table_start_size);
    let jump_table_target_size = program.jump_table_target.len();
    println!("jump_table_target size: {:?}", jump_table_target_size);
    let last_value_reg_limit_size = program.last_value_reg_limit.len();
    println!("last_value_reg_limit size: {:?}", last_value_reg_limit_size);
    let reg_def_use_def_used_size = program.reg_def_use_def_used.len();
    println!("reg_def_use_def_used size: {:?}", reg_def_use_def_used_size);
    let reg_def_use_live_var_at_block_end_size = program.reg_def_use_live_var_at_block_end.len();
    println!("reg_def_use_live_var_at_block_end size: {:?}", reg_def_use_live_var_at_block_end_size);
    let reg_def_use_live_var_at_prior_used_size = program.reg_def_use_live_var_at_prior_used.len();
    println!("reg_def_use_live_var_at_prior_used size: {:?}", reg_def_use_live_var_at_prior_used_size);
    let reg_def_use_live_var_used_size = program.reg_def_use_live_var_used.len();
    println!("reg_def_use_live_var_used size: {:?}", reg_def_use_live_var_used_size);
    let reg_def_use_return_val_used_size = program.reg_def_use_return_val_used.len();
    println!("reg_def_use_return_val_used size: {:?}", reg_def_use_return_val_used_size);
    let reg_has_base_image_size = program.reg_has_base_image.len();
    println!("reg_has_base_image size: {:?}", reg_has_base_image_size);
    let reg_has_got_size = program.reg_has_got.len();
    println!("reg_has_got size: {:?}", reg_has_got_size);
    let reg_reg_arithmetic_operation_defs_size = program.reg_reg_arithmetic_operation_defs.len();
    println!("reg_reg_arithmetic_operation_defs size: {:?}", reg_reg_arithmetic_operation_defs_size);
    let relative_jump_table_entry_candidate_size = program.relative_jump_table_entry_candidate.len();
    println!("relative_jump_table_entry_candidate size: {:?}", relative_jump_table_entry_candidate_size);
    let stack_def_use_def_used_size = program.stack_def_use_def_used.len();
    println!("stack_def_use_def_used size: {:?}", stack_def_use_def_used_size);
    let stack_def_use_live_var_at_block_end_size = program.stack_def_use_live_var_at_block_end.len();
    println!("stack_def_use_live_var_at_block_end size: {:?}", stack_def_use_live_var_at_block_end_size);
    let stack_def_use_live_var_at_prior_used_size = program.stack_def_use_live_var_at_prior_used.len();
    println!("stack_def_use_live_var_at_prior_used size: {:?}", stack_def_use_live_var_at_prior_used_size);
    let stack_def_use_live_var_used_size = program.stack_def_use_live_var_used.len();
    println!("stack_def_use_live_var_used size: {:?}", stack_def_use_live_var_used_size);
    let stack_def_use_live_var_used_in_block_size = program.stack_def_use_live_var_used_in_block.len();
    println!("stack_def_use_live_var_used_in_block size: {:?}", stack_def_use_live_var_used_in_block_size);
    let tls_desc_call_size = program.tls_desc_call.len();
    println!("tls_desc_call size: {:?}", tls_desc_call_size);
    let tls_get_addr_size = program.tls_get_addr.len();
    println!("tls_get_addr size: {:?}", tls_get_addr_size);
    let value_reg_edge_size = program.value_reg_edge.len();
    println!("value_reg_edge size: {:?}", value_reg_edge_size);
    let value_reg_limit_size = program.value_reg_limit.len();
    println!("value_reg_limit size: {:?}", value_reg_limit_size);
    let value_reg_unsupported_size = program.value_reg_unsupported.len();
    println!("value_reg_unsupported size: {:?}", value_reg_unsupported_size);    

}
