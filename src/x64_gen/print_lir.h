#ifndef NIBBLE_X64_PRINT_LIR_H
#define NIBBLE_X64_PRINT_LIR_H
#include "nibble.h"
#include "allocator.h"
#include "x64_gen/lir.h"

void LIR_dump_proc_dot(Allocator* arena, const char* proc_name, size_t num_xbblocks, X64_BBlock** xbblocks);
char* LIR_print_instr(Allocator* arena, const X64_Instr* instr);
#endif
