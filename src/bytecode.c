#include "bytecode.h"

IR_Instr** get_bucket_instr(BucketList* bucket_list, size_t index)
{
    return (IR_Instr**)bucket_list_get_elem(bucket_list, index);
}

IR_Instr** add_bucket_instr(BucketList* bucket_list, Allocator* arena, IR_Instr* instr)
{
    return (IR_Instr**)bucket_list_add_elem(bucket_list, arena, instr);
}

IR_Instr* new_ir_instr(Allocator* arena, IR_OpCode opcode, IR_Operand op_r, IR_Operand op_a, IR_Operand op_b)
{
    IR_Instr* instr = alloc_type(arena, IR_Instr, false);

    instr->opcode = opcode;
    instr->operand_r = op_r;
    instr->operand_a = op_a;
    instr->operand_b = op_b;

    return instr;
}
