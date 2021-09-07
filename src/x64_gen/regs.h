#ifndef NIBBLE_X64_REGS_H
#define NIBBLE_X64_REGS_H
#include "nibble.h"

#define X64_MAX_INT_REG_SIZE 8
#define X64_STACK_ALIGN 16
#define X64_STACK_WORD_SIZE 8
#define X64_NUM_ARG_REGS 6

// Bit is 1 for caller saved registers: RAX, RCX, RDX, _, _, _, RSI, RDI, R8, R9, R10, R11, _, _, _, _
#define X64_CALLER_SAVED_REG_MASK 0x0FC7

// Bit is 1 for arg registers: _, RCX, RDX, _, _, _, RSI, RDI, R8, R9, _, _, _, _, _, _
#define X64_ARG_REG_MASK 0x03C6

typedef enum X64_Reg {
    X64_RAX = 0,
    X64_RCX,
    X64_RDX,
    X64_RBX,
    X64_RSP,
    X64_RBP,
    X64_RSI,
    X64_RDI,
    X64_R8,
    X64_R9,
    X64_R10,
    X64_R11,
    X64_R12,
    X64_R13,
    X64_R14,
    X64_R15,
    X64_REG_COUNT,
} X64_Reg;

extern X64_Reg x64_leaf_scratch_regs[];
extern X64_Reg x64_nonleaf_scratch_regs[];
extern X64_Reg x64_arg_regs[];

bool X64_is_reg_caller_saved(X64_Reg reg);
bool X64_is_reg_callee_saved(X64_Reg reg);

// Data structures used to track the "location" of a virtual IR register.
// A virtual register could be assigned to a physical register, or could be assigned
// to a stack offset.
typedef enum X64_VRegLocKind {
    X64_VREG_LOC_UNASSIGNED = 0,
    X64_VREG_LOC_REG,
    X64_VREG_LOC_STACK,
} X64_VRegLocKind;

typedef struct X64_VRegLoc {
    X64_VRegLocKind kind;

    union {
        X64_Reg reg;
        s32 offset;
    };
} X64_VRegLoc;

#endif
