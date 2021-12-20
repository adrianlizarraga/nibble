#ifndef NIBBLE_PRINT_NIR_H
#define NIBBLE_PRINT_NIR_H
#include "ir.h"

char* NIR_print_instr(Allocator* arena, Instr* instr);
void NIR_print_out_proc(Allocator* arena, Symbol* sym);
#endif
