#include "bytecode.h"

// NOTE: This assumes that a valid index is always provided!!!
// Also assumes that all buckets, except the last, are full.
IR_Var** get_bucket_var(BucketList* bucket_list, size_t index)
{
    return (IR_Var**)bucket_list_get_elem_packed(bucket_list, index);
}

// NOTE: This assumes that a valid index is always provided!!!
// Also assumes that all buckets, except the last, are full.
IR_Proc** get_bucket_proc(BucketList* bucket_list, size_t index)
{
    return (IR_Proc**)bucket_list_get_elem_packed(bucket_list, index);
}

IR_Instr** get_bucket_instr(BucketList* bucket_list, size_t index)
{
    return (IR_Instr**)bucket_list_get_elem(bucket_list, index);
}

IR_Instr** add_bucket_instr(BucketList* bucket_list, Allocator* arena, IR_Instr* instr)
{
    return (IR_Instr**)bucket_list_add_elem(bucket_list, arena, instr);
}

IR_Proc** add_bucket_proc(BucketList* bucket_list, Allocator* arena, IR_Proc* proc)
{
    return (IR_Proc**)bucket_list_add_elem(bucket_list, arena, proc);
}

IR_Var** add_bucket_var(BucketList* bucket_list, Allocator* arena, IR_Var* var)
{
    return (IR_Var**)bucket_list_add_elem(bucket_list, arena, var);
}

IR_Proc* new_ir_proc(Allocator* arena, DeclProc* decl, Type* type)
{
    IR_Proc* proc = alloc_type(arena, IR_Proc, false);

    proc->arena = arena;
    proc->decl = decl;
    proc->type = type;

    bucket_list_init(&proc->instrs, arena, IR_INSTRS_PER_BUCKET);
    bucket_list_init(&proc->local_vars, arena, IR_VARS_PER_BUCKET);

    return proc;
}

IR_Var* new_ir_var(Allocator* arena, DeclVar* decl, Type* type, s64 offset)
{
    IR_Var* var = alloc_type(arena, IR_Var, false);

    var->decl = decl;
    var->type = type;
    var->offset = offset;

    return var;
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
