#include "bytecode.h"

#include "bytecode/vars.c"
#include "bytecode/procs.c"


void IR_gen_bytecode(Allocator* arena, Allocator* tmp_arena, BucketList* vars, BucketList* procs, BucketList* str_lits,
                     BucketList* float_lits, TypeCache* type_cache)
{
    IR_build_vars(arena, tmp_arena, vars, str_lits, float_lits, type_cache);
    IR_build_procs(arena, tmp_arena, procs, str_lits, float_lits, type_cache);
}
