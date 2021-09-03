#ifndef NIBBLE_X64_REG_ALLOC_H
#define NIBBLE_X64_REG_ALLOC_H
#include "ast.h"
#include "x64_gen/regs.h"

typedef struct X64_RegAllocResult {
    u32 stack_offset; // In bytes
    u32 free_regs;
    u32 used_callee_regs;
} X64_RegAllocResult;

// Linear scan register allocation from Poletto et al (1999)
X64_RegAllocResult X64_linear_scan_reg_alloc(Allocator* arena, u32 num_vregs, LifetimeInterval* vreg_intervals, X64_VRegLoc* vreg_locs,
                                             u32 init_stack_offset);
#endif
