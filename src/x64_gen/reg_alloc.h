#ifndef NIBBLE_X64_REG_ALLOC_H
#define NIBBLE_X64_REG_ALLOC_H
#include "ast.h"
#include "x64_gen/regs.h"

typedef struct X64_RegAllocResult {
    u32 stack_offset; // In bytes
    u32 free_regs;
    u32 used_callee_regs;
} X64_RegAllocResult;

typedef struct X64_VRegInterval {
    LifetimeInterval interval;
    u32 index;

    struct X64_VRegInterval* next;
    struct X64_VRegInterval* prev;
} X64_VRegInterval;

typedef struct X64_VRegIntervalList {
    X64_VRegInterval sentinel;
    u32 count;
    Allocator* arena;
} X64_VRegIntervalList;

// Linear scan register allocation from Poletto et al (1999)
X64_RegAllocResult X64_linear_scan_reg_alloc(Allocator* arena, u32 num_vregs, LifetimeInterval* vreg_intervals,
                                             X64_VRegLoc* vreg_locs, u32 num_x64_regs, X64_Reg* x64_scratch_regs,
                                             u32 init_stack_offset);

void X64_vreg_interval_list_rm(X64_VRegIntervalList* list, X64_VRegInterval* node);
void X64_vreg_interval_list_add(X64_VRegIntervalList* list, LifetimeInterval* interval, u32 index);
#endif
