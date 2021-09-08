#ifndef NIBBLE_X64_GEN_H
#define NIBBLE_X64_GEN_H
#include "bytecode.h"

bool init_code_gen(OS target_os, Arch target_arch);
bool gen_module(Allocator* gen_mem, Allocator* tmp_mem, IR_Module* module, const char* output_file);

#endif
