#include "bytecode/module.h"

void IR_gen_bytecode(Allocator* arena, Allocator* tmp_arena, GlobalData* vars, BucketList* procs, GlobalData* str_lits,
                     GlobalData* float_lits, TypeCache* type_cache, HMap* float_lit_map)
{
    IR_build_vars(arena, tmp_arena, vars, str_lits, float_lits, type_cache, float_lit_map);
    IR_build_procs(arena, tmp_arena, procs, str_lits, float_lits, type_cache, float_lit_map);
}
