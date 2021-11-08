#ifndef NIBBLE_X64_GEN_GEN_H
#define NIBBLE_X64_GEN_GEN_H
#include "x64_gen/regs.h"

bool x64_gen_module(Allocator* gen_mem, Allocator* tmp_mem, BucketList* vars, BucketList* procs, HMap* str_lit_map,
                    const char* output_file);

#endif
