#ifndef NIBBLE_X64_REGS_H
#define NIBBLE_X64_REGS_H
#include "nibble.h"

#define X64_MAX_INT_REG_SIZE 8
#define X64_STACK_ALIGN 16
#define X64_STACK_WORD_SIZE 8

#define X64_WINDOWS_SHADOW_SPACE 32

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

typedef struct X64_Target {
    u32 num_arg_regs;
    X64_Reg* arg_regs;

    u32 num_leaf_scratch_regs;
    X64_Reg* leaf_scratch_regs;

    u32 num_nonleaf_scratch_regs;
    X64_Reg* nonleaf_scratch_regs;

    u32 caller_saved_reg_mask;
    u32 arg_reg_mask;

    const char* startup_code;

    OS os;
} X64_Target;

extern X64_Target x64_target;

bool init_x64_target(OS target_os);
bool X64_is_caller_saved_reg(X64_Reg reg);
bool X64_is_callee_saved_reg(X64_Reg reg);
bool X64_is_arg_reg(X64_Reg reg);

// Data structures used to track the "location" of a virtual IR register.
// A virtual register could be assigned to a physical register, or could be assigned
// to a stack offset.
typedef enum X64_LRegLocKind {
    X64_LREG_LOC_UNASSIGNED = 0,
    X64_LREG_LOC_REG,
    X64_LREG_LOC_STACK,
} X64_LRegLocKind;

typedef struct X64_LRegLoc {
    X64_LRegLocKind kind;

    union {
        X64_Reg reg;
        s32 offset;
    };
} X64_LRegLoc;

#endif
