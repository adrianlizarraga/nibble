#ifndef NIBBLE_X64_NASM_GEN_H
#define NIBBLE_X64_NASM_GEN_H
#include "nibble.h"
#include "compiler.h"
#include "allocator.h"
#include "stream.h"
#include "regs.h"

extern const char* x64_sext_ax_into_dx[X64_MAX_INT_REG_SIZE + 1];
extern const char* x64_condition_codes[];

bool X64_nasm_gen_module(Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars, BucketList* procs, GlobalData* str_lits,
                         GlobalData* float_lits, BucketList* foreign_procs, const char* output_file);
#endif
