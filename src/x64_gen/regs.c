#include "cstring.h"
#include "bytecode/module.h"
#include "x64_gen/regs.h"
#include "x64_gen/startup_linux.c"

// Linux System V ABI
static X64_Reg x64_linux_leaf_scratch_int_regs[] = {
    X64_R10, X64_R11, X64_RAX, X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9, // NOTE: Caller saved
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, // NOTE: Callee saved
};
static X64_Reg x64_linux_leaf_scratch_flt_regs[] = {
    X64_XMM0,  X64_XMM1,  X64_XMM2,  X64_XMM3,  X64_XMM4,  X64_XMM5,  X64_XMM6, X64_XMM7, X64_XMM8, X64_XMM9, // NOTE: FP Caller-saved
    X64_XMM10, X64_XMM11, X64_XMM12, X64_XMM13, X64_XMM14, X64_XMM15, // NOTE: FP Caller-saved
};
static X64_ScratchRegs x64_linux_leaf_scratch_regs[X64_REG_CLASS_COUNT] = {
    [X64_REG_CLASS_INT] = {.num_regs = ARRAY_LEN(x64_linux_leaf_scratch_int_regs), .regs = x64_linux_leaf_scratch_int_regs},
    [X64_REG_CLASS_FLOAT] = {.num_regs = ARRAY_LEN(x64_linux_leaf_scratch_flt_regs), .regs = x64_linux_leaf_scratch_flt_regs}};

static X64_Reg x64_linux_nonleaf_scratch_int_regs[] = {
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX, // NOTE: Callee saved
    X64_R10, X64_R11, X64_RAX, X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9, // NOTE: Caller saved
};
static X64_Reg x64_linux_nonleaf_scratch_flt_regs[] = {
    X64_XMM10, X64_XMM11, X64_XMM12, X64_XMM13, X64_XMM14, X64_XMM15, // NOTE: FP Caller-saved
    X64_XMM0,  X64_XMM1,  X64_XMM2,  X64_XMM3,  X64_XMM4,  X64_XMM5,  X64_XMM6, X64_XMM7, X64_XMM8, X64_XMM9, // NOTE: FP Caller-saved
};
static X64_ScratchRegs x64_linux_nonleaf_scratch_regs[X64_REG_CLASS_COUNT] = {
    [X64_REG_CLASS_INT] = {.num_regs = ARRAY_LEN(x64_linux_nonleaf_scratch_int_regs), .regs = x64_linux_nonleaf_scratch_int_regs},
    [X64_REG_CLASS_FLOAT] = {.num_regs = ARRAY_LEN(x64_linux_nonleaf_scratch_flt_regs), .regs = x64_linux_nonleaf_scratch_flt_regs}};

static X64_Reg x64_linux_arg_int_regs[] = {X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9};
static X64_Reg x64_linux_arg_flt_regs[] = {X64_XMM0, X64_XMM1, X64_XMM2, X64_XMM3, X64_XMM4, X64_XMM5, X64_XMM6, X64_XMM7};
static X64_ScratchRegs x64_linux_arg_regs[X64_REG_CLASS_COUNT] = {
    [X64_REG_CLASS_INT] = {.num_regs = ARRAY_LEN(x64_linux_arg_int_regs), .regs = x64_linux_arg_int_regs},
    [X64_REG_CLASS_FLOAT] = {.num_regs = ARRAY_LEN(x64_linux_arg_flt_regs), .regs = x64_linux_arg_flt_regs}};

static X64_Reg x64_linux_ret_int_regs[] = {X64_RAX, X64_RDX};
static X64_Reg x64_linux_ret_flt_regs[] = {X64_XMM0, X64_XMM1};
static X64_ScratchRegs x64_linux_ret_regs[X64_REG_CLASS_COUNT] = {
    [X64_REG_CLASS_INT] = {.num_regs = ARRAY_LEN(x64_linux_ret_int_regs), .regs = x64_linux_ret_int_regs},
    [X64_REG_CLASS_FLOAT] = {.num_regs = ARRAY_LEN(x64_linux_ret_flt_regs), .regs = x64_linux_ret_flt_regs},
};

// Bit is 1 for caller saved registers: RAX, RCX, RDX, _, _, _, RSI, RDI, R8, R9, R10, R11, _, _, _, _, XMM0, ..., XMM15
static const u32 x64_linux_caller_saved_reg_mask = 0xFFFF0FC7;

// Bit is 1 for arg registers: _, RCX, RDX, _, _, _, RSI, RDI, R8, R9, _, _, _, _, _, _, XMM0, ... , XMM7
static const u32 x64_linux_arg_reg_mask = 0x00FF03C6;

// Bit is 1 for ret registers: RAX, _, RDX, _, _, _, _, _, _, _, _, _, _, _, _, _, XMM0, XMM1, _, _, ...
static const u32 x64_linux_ret_reg_mask = 0x00030005;

X64_Target x64_target;

void x64_init_target(OS target_os)
{
    x64_target.os = target_os;

    // RAX, RCX, RDX, RBX, _, _, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15
    x64_target.scratch_reg_mask = 0xFFCF; // TODO: Not sync'd with actual scratch register arrays.

    switch (target_os) {
    case OS_LINUX:
        x64_target.arg_regs = &x64_linux_arg_regs;
        x64_target.ret_regs = &x64_linux_ret_regs;
        x64_target.leaf_scratch_regs = &x64_linux_leaf_scratch_regs;
        x64_target.nonleaf_scratch_regs = &x64_linux_nonleaf_scratch_regs;

        x64_target.caller_saved_reg_mask = x64_linux_caller_saved_reg_mask;
        x64_target.arg_reg_mask = x64_linux_arg_reg_mask;
        x64_target.ret_reg_mask = x64_linux_ret_reg_mask;

        x64_target.startup_code = x64_linux_startup_code_text;
        break;
    default:
        NIBBLE_FATAL_EXIT("Unsupported x86_64 OS kind '%d'", target_os);
        break;
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

X64_RegClass X64_obj_reg_class(const Type* type)
{
    return type_agg_has_non_float(type) ? X64_REG_CLASS_INT : X64_REG_CLASS_FLOAT;
}
