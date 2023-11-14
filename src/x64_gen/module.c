#include "nibble.h"
#include "stream.h"
#include "x64_gen/module.h"
#include "x64_gen/nasm_gen.h"
#include "x64_gen/print_xir.h"

bool x64_gen_module(Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars, BucketList* procs, const Symbol* main_proc, GlobalData* str_lits,
                    GlobalData* float_lits, BucketList* foreign_procs, const char* output_file)
{
    bool ret = X64_nasm_gen_module(gen_mem, tmp_mem, vars, procs, str_lits, float_lits, foreign_procs, output_file);
    if (!ret) {
        NIBBLE_FATAL_EXIT("X64_nasm_gen_module failed!!!!");
    }

#if 1
    x64_gen_elf(gen_mem, tmp_mem, vars, procs, main_proc, str_lits, float_lits, foreign_procs, output_file);
#endif
    return true;
}
