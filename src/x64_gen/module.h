#ifndef NIBBLE_X64_GEN_GEN_H
#define NIBBLE_X64_GEN_GEN_H
#include "ast/module.h"

void x64_init_target(OS target_os);

bool x64_gen_elf(Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars, BucketList* procs, const Symbol* main_proc,
                 GlobalData* str_lits, GlobalData* float_lits, BucketList* foreign_procs, const char* output_file);

bool x64_gen_nasm(Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars, BucketList* procs, GlobalData* str_lits,
                  GlobalData* float_lits, BucketList* foreign_procs, const char* output_file);
#endif
