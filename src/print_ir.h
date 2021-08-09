#ifndef NIBBLE_PRINT_IR_H
#define NIBBLE_PRINT_IR_H
#include "bytecode.h"

char* IR_print_instr(IR_Instr* instr, Allocator* arena);
#endif
