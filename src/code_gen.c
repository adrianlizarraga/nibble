#include "code_gen.h"
#include "x64_gen/gen.h"

typedef bool GenModuleProc(Allocator* gen_mem, Allocator* tmp_mem, BucketList* vars, BucketList* procs,
                           BucketList* str_lits, const char* output_file);

static GenModuleProc* target_gen_module;

bool init_code_gen(OS target_os, Arch target_arch)
{
    if (target_arch != ARCH_X64)
        return false;

    if (init_x64_target(target_os)) {
        target_gen_module = x64_gen_module;
        return true;
    }

    return false;
}

bool gen_module(Allocator* gen_mem, Allocator* tmp_mem, BucketList* vars, BucketList* procs, BucketList* str_lits,
                const char* output_file)
{
    return target_gen_module(gen_mem, tmp_mem, vars, procs, str_lits, output_file);
}
