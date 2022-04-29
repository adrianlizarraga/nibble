#ifndef NIBBLE_X64_REG_ALLOC_H
#define NIBBLE_X64_REG_ALLOC_H
#include "ast.h"
#include "x64_gen/regs.h"
#include "x64_gen/lir.h"

typedef struct X64_RegAllocResult {
    u32 stack_offset; // In bytes
    u32 free_regs;
    u32 used_callee_regs;
    bool success;
} X64_RegAllocResult;

// Modified linear scan register allocation adapted from Poletto et al (1999)
X64_RegAllocResult X64_linear_scan_reg_alloc(X64_LIRBuilder* builder, X64_ScratchRegs (*scratch_regs)[X64_REG_CLASS_COUNT],
                                             u32 init_stack_offset);

#endif
