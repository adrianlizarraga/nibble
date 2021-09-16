#include "cstring.h"
#include "x64_gen/regs.h"

// Linux System V ABI
static X64_Reg x64_linux_leaf_scratch_regs[] = {
    X64_R10, X64_R11, X64_RAX, X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9, // NOTE: Caller saved
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, // NOTE: Callee saved
};

static X64_Reg x64_linux_nonleaf_scratch_regs[] = {
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, // NOTE: Callee saved
    X64_R10, X64_R11, X64_RAX, X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9, // NOTE: Caller saved
};

static X64_Reg x64_linux_arg_regs[] = {X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9};

// Bit is 1 for caller saved registers: RAX, RCX, RDX, _, _, _, RSI, RDI, R8, R9, R10, R11, _, _, _, _
static const u32 x64_linux_caller_saved_reg_mask = 0x0FC7;

// Bit is 1 for arg registers: _, RCX, RDX, _, _, _, RSI, RDI, R8, R9, _, _, _, _, _, _
static const u32 x64_linux_arg_reg_mask = 0x03C6;

// Windows ABI
static X64_Reg x64_windows_leaf_scratch_regs[] = {
    X64_R10, X64_R11, X64_RAX, X64_RCX, X64_RDX, X64_R8,  X64_R9, // NOTE: Caller saved
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, X64_RSI, X64_RDI, // NOTE: Callee saved
};

static X64_Reg x64_windows_nonleaf_scratch_regs[] = {
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, X64_RSI, X64_RDI, // NOTE: Callee saved
    X64_R10, X64_R11, X64_RAX, X64_RCX, X64_RDX, X64_R8,  X64_R9, // NOTE: Caller saved
};

static X64_Reg x64_windows_arg_regs[] = {X64_RCX, X64_RDX, X64_R8, X64_R9};

// RAX, RCX, RDX, _, _, _, _, _, R8, R9, R10, R11, _, _, _, _
static const u32 x64_windows_caller_saved_reg_mask = 0x0F07;

// _, RCX, RDX, _, _, _, _, _, R8, R9, _, _, _, _, _, _
static const u32 x64_windows_arg_reg_mask = 0x0306;

X64_Target x64_target;

bool init_x64_target(OS target_os)
{
    x64_target.os = target_os;

    switch (target_os) {
    case OS_LINUX:
        x64_target.num_arg_regs = ARRAY_LEN(x64_linux_arg_regs);
        x64_target.arg_regs = x64_linux_arg_regs;

        x64_target.num_leaf_scratch_regs = ARRAY_LEN(x64_linux_leaf_scratch_regs);
        x64_target.leaf_scratch_regs = x64_linux_leaf_scratch_regs;

        x64_target.num_nonleaf_scratch_regs = ARRAY_LEN(x64_linux_nonleaf_scratch_regs);
        x64_target.nonleaf_scratch_regs = x64_linux_nonleaf_scratch_regs;

        x64_target.caller_saved_reg_mask = x64_linux_caller_saved_reg_mask;
        x64_target.arg_reg_mask = x64_linux_arg_reg_mask;
        return true;
    case OS_WIN32:
        x64_target.num_arg_regs = ARRAY_LEN(x64_windows_arg_regs);
        x64_target.arg_regs = x64_windows_arg_regs;

        x64_target.num_leaf_scratch_regs = ARRAY_LEN(x64_windows_leaf_scratch_regs);
        x64_target.leaf_scratch_regs = x64_windows_leaf_scratch_regs;

        x64_target.num_nonleaf_scratch_regs = ARRAY_LEN(x64_windows_nonleaf_scratch_regs);
        x64_target.nonleaf_scratch_regs = x64_windows_nonleaf_scratch_regs;

        x64_target.caller_saved_reg_mask = x64_windows_caller_saved_reg_mask;
        x64_target.arg_reg_mask = x64_windows_arg_reg_mask;
        return true;
    default:
        return false;
    }
}

bool X64_is_caller_saved_reg(X64_Reg reg)
{
    return u32_is_bit_set(x64_target.caller_saved_reg_mask, reg);
}

bool X64_is_callee_saved_reg(X64_Reg reg)
{
    return !u32_is_bit_set(x64_target.caller_saved_reg_mask, reg);
}

bool X64_is_arg_reg(X64_Reg reg)
{
    return u32_is_bit_set(x64_target.arg_reg_mask, reg);
}
