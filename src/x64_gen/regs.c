#include "cstring.h"
#include "x64_gen/regs.h"

X64_Reg x64_scratch_regs[] = {
    X64_R10, X64_R11, X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9, X64_RAX, // NOTE: Caller saved
    X64_R12, X64_R13, X64_R14, X64_R15, X64_RBX,                                   // NOTE: Callee saved
};

X64_Reg x64_arg_regs[] = {X64_RDI, X64_RSI, X64_RDX, X64_RCX, X64_R8, X64_R9};

bool X64_is_reg_caller_saved(X64_Reg reg)
{
    return u32_is_bit_set(X64_CALLER_SAVED_REG_MASK, reg);    
}

bool X64_is_reg_callee_saved(X64_Reg reg)
{
    return !u32_is_bit_set(X64_CALLER_SAVED_REG_MASK, reg);    
}

