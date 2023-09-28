#ifndef NIBBLE_X64_PRINT_XIR_H
#define NIBBLE_X64_PRINT_XIR_H
#include "nibble.h"
#include "allocator.h"
#include "x64_gen/xir.h"

void XIR_dump_proc_dot(Allocator* arena, const char* proc_name, size_t num_xbblocks, XIR_BBlock** xbblocks);
char* XIR_print_instr(Allocator* arena, const XIR_Instr* instr);
#endif
