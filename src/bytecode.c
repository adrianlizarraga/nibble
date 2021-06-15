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

IR_Proc* new_ir_proc(Allocator* arena, DeclProc* decl)
{
    IR_Proc* proc = alloc_type(arena, IR_Proc, false);

    proc->arena = arena;
    proc->decl = decl;

    bucket_list_init(&proc->instrs, arena, IR_INSTRS_PER_BUCKET);
    bucket_list_init(&proc->local_vars, arena, IR_VARS_PER_BUCKET);

    return proc;
}
