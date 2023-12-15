#ifndef NIBBLE_X64_GEN_XIR_TO_X64_H
#define NIBBLE_X64_GEN_XIR_TO_X64_H

#include "x64_instrs.h"
#include "allocator.h"
#include "ast/module.h"


X64_Instrs X64_gen_proc_instrs(Allocator* gen_mem, Allocator* tmp_mem, Symbol* proc_sym);

#endif // defined(NIBBLE_X64_GEN_XIR_TO_X64_H)
