#ifndef NIBBLE_X64_REGS_H
#define NIBBLE_X64_REGS_H
#include "nibble.h"

#define X64_MAX_INT_REG_SIZE 8
#define X64_STACK_ALIGN 16
#define X64_STACK_WORD_SIZE 8
#define X64_STACK_ARG_RBP_OFFSET 0x10
#define X64_MAX_SIBD_SCALE 8

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
    X64_XMM0,
    X64_XMM1,
    X64_XMM2,
    X64_XMM3,
    X64_XMM4,
    X64_XMM5,
    X64_XMM6,
    X64_XMM7,
    X64_XMM8,
    X64_XMM9,
    X64_XMM10,
    X64_XMM11,
    X64_XMM12,
    X64_XMM13,
    X64_XMM14,
    X64_XMM15,
    X64_REG_COUNT,
} X64_Reg;

typedef enum X64_RegClass {
    X64_REG_CLASS_INT = 0,
    X64_REG_CLASS_FLOAT,
    X64_REG_CLASS_COUNT,
} X64_RegClass;

extern const X64_RegClass x64_reg_classes[X64_REG_COUNT];
extern const char* x64_flt_reg_names[X64_REG_COUNT];

typedef struct X64_ScratchRegs {
    unsigned num_regs;
    X64_Reg* regs;
} X64_ScratchRegs;

typedef struct X64_Target {
    u32 num_arg_regs;
    X64_Reg* arg_regs;

    X64_ScratchRegs (*leaf_scratch_regs)[X64_REG_CLASS_COUNT];
    X64_ScratchRegs (*nonleaf_scratch_regs)[X64_REG_CLASS_COUNT];

    u32 caller_saved_reg_mask;
    u32 arg_reg_mask;

    u32 scratch_reg_mask;

    const char* startup_code;

    OS os;
} X64_Target;

extern X64_Target x64_target;

bool init_x64_target(OS target_os);
bool X64_is_caller_saved_reg(X64_Reg reg);
bool X64_is_callee_saved_reg(X64_Reg reg);
bool X64_is_arg_reg(X64_Reg reg);

#define X64_linux_is_obj_retarg_large(s) ((s) > (X64_MAX_INT_REG_SIZE << 1))
#define X64_windows_is_obj_retarg_large(s) (((s) > X64_MAX_INT_REG_SIZE) || !IS_POW2(s))
bool X64_is_obj_retarg_large(size_t size);

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

typedef struct X64_LRegRangeList {
    List ranges;
    ListNode lnode;
} X64_LRegRangeList;

typedef enum X64_RegAllocControlKind {
    X64_REG_ALLOC_CTRL_NONE = 0,
    X64_REG_ALLOC_CTRL_FORCE_REG, // Used for required operand registers (e.g., rcx for shift) and SIBD addr registers
    X64_REG_ALLOC_CTRL_FORCE_ANY_REG,
    X64_REG_ALLOC_CTRL_FORCE_REG_OR_SPILL, // Used for procedure arguments
    X64_REG_ALLOC_CTRL_HINT_LIR_REG, // Used for register to register moves
    X64_REG_ALLOC_CTRL_HINT_PHYS_REG, // Used for register to register moves
} X64_RegAllocControlKind;

typedef struct X64_LRegRange {
    u32 lreg;
    long start;
    long end;

    X64_RegClass reg_class;
    X64_RegAllocControlKind ra_ctrl_kind;
    union {
        X64_Reg preg;
        u32 lreg;
        u32 preg_mask;
    } ra_ctrl;

    X64_LRegLoc loc;
    ListNode lnode;
} X64_LRegRange;

typedef struct X64_StackArgsInfo {
    u64 size;
    u64 offset;
} X64_StackArgsInfo;

extern const char* x64_mem_size_label[X64_MAX_INT_REG_SIZE + 1];
extern const char* x64_data_size_label[X64_MAX_INT_REG_SIZE + 1];
extern const char* x64_sext_ax_into_dx[X64_MAX_INT_REG_SIZE + 1];
extern const char* x64_condition_codes[];
#endif
