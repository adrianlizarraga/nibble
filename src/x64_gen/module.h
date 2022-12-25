#ifndef NIBBLE_X64_GEN_GEN_H
#define NIBBLE_X64_GEN_GEN_H
#include "compiler.h"

bool x64_gen_module(Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars, BucketList* procs, GlobalData* str_lits,
                    GlobalData* float_lits, BucketList* foreign_procs, const char* output_file);

#endif
