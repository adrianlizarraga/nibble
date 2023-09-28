#ifndef NIBBLE_XIR_H
#define NIBBLE_XIR_H
#include "allocator.h"
#include "hash_map.h"
#include "bytecode/module.h"
#include "x64_gen/regs.h"

#define XIR_REG_COUNT 0xFFFFFFFF

typedef struct XIR_BBlock XIR_BBlock;
typedef struct XIR_Instr XIR_Instr;
typedef struct XIR_RegLoc XIR_RegLoc;
typedef struct XIR_RegRangeList XIR_RegRangeList;
typedef struct XIR_RegRange XIR_RegRange;

typedef enum XIR_InstrKind {
    XIR_INSTR_KIND_NONE = 0,

    // Addition
    XIR_InstrAdd_R_R_KIND,
    XIR_InstrAdd_R_I_KIND,
    XIR_InstrAdd_R_M_KIND,

    // f32 add
    XIR_InstrAddSS_R_R_KIND,
    XIR_InstrAddSS_R_M_KIND,

    // f64 add
    XIR_InstrAddSD_R_R_KIND,
    XIR_InstrAddSD_R_M_KIND,

    // Subtraction
    XIR_InstrSub_R_R_KIND,
    XIR_InstrSub_R_I_KIND,
    XIR_InstrSub_R_M_KIND,

    // f32 sub
    XIR_InstrSubSS_R_R_KIND,
    XIR_InstrSubSS_R_M_KIND,

    // f64 sub
    XIR_InstrSubSD_R_R_KIND,
    XIR_InstrSubSD_R_M_KIND,

    // Multiplication
    XIR_InstrIMul_R_R_KIND,
    XIR_InstrIMul_R_I_KIND,
    XIR_InstrIMul_R_M_KIND,

    // f32 mul
    XIR_InstrMulSS_R_R_KIND,
    XIR_InstrMulSS_R_M_KIND,

    // f64 mul
    XIR_InstrMulSD_R_R_KIND,
    XIR_InstrMulSD_R_M_KIND,

    // Unsigned division
    XIR_InstrDiv_R_KIND,
    XIR_InstrDiv_M_KIND,

    // Signed division
    XIR_InstrIDiv_R_KIND,
    XIR_InstrIDiv_M_KIND,

    // f32 div
    XIR_InstrDivSS_R_R_KIND,
    XIR_InstrDivSS_R_M_KIND,

    // f64 div
    XIR_InstrDivSD_R_R_KIND,
    XIR_InstrDivSD_R_M_KIND,

    // Bitwise AND
    XIR_InstrAnd_R_R_KIND,
    XIR_InstrAnd_R_I_KIND,
    XIR_InstrAnd_R_M_KIND,

    // Bitwise OR
    XIR_InstrOr_R_R_KIND,
    XIR_InstrOr_R_I_KIND,
    XIR_InstrOr_R_M_KIND,

    // Bitwise XOR
    XIR_InstrXor_R_R_KIND,
    XIR_InstrXor_R_I_KIND,
    XIR_InstrXor_R_M_KIND,

    // Arithmetic shift right
    XIR_InstrSar_R_R_KIND,
    XIR_InstrSar_R_I_KIND,

    // Shift left
    XIR_InstrShl_R_R_KIND,
    XIR_InstrShl_R_I_KIND,

    // Bitwise NOT
    XIR_InstrNot_KIND,

    // Two's complement negation.
    XIR_InstrNeg_KIND,

    XIR_InstrMov_R_R_KIND, // Register copy
    XIR_InstrMov_R_I_KIND, // Load imm
    XIR_InstrMov_R_M_KIND, // Load memory

    XIR_InstrMov_R_RH_KIND, // Copy high byte of register to another register.

    // Store into memory.
    XIR_InstrMov_M_R_KIND,
    XIR_InstrMov_M_I_KIND,

    // Zero-extend
    XIR_InstrMovZX_R_R_KIND,
    XIR_InstrMovZX_R_M_KIND,

    // Sign-extend
    XIR_InstrMovSX_R_R_KIND,
    XIR_InstrMovSX_R_M_KIND,
    XIR_InstrSExtAxToDx_KIND,

    // f32 mov
    XIR_InstrMovSS_R_R_KIND,
    XIR_InstrMovSS_R_M_KIND,
    XIR_InstrMovSS_M_R_KIND,

    // f64 mov
    XIR_InstrMovSD_R_R_KIND,
    XIR_InstrMovSD_R_M_KIND,
    XIR_InstrMovSD_M_R_KIND,

    // Flt to Flt
    XIR_InstrCvtSS2SD_R_R_KIND,
    XIR_InstrCvtSS2SD_R_M_KIND,
    XIR_InstrCvtSD2SS_R_R_KIND,
    XIR_InstrCvtSD2SS_R_M_KIND,

    // Flt to Int
    XIR_InstrCvtSS2SI_R_R_KIND,
    XIR_InstrCvtSS2SI_R_M_KIND,
    XIR_InstrCvtSD2SI_R_R_KIND,
    XIR_InstrCvtSD2SI_R_M_KIND,

    // Int to Flt
    XIR_InstrCvtSI2SS_R_R_KIND,
    XIR_InstrCvtSI2SS_R_M_KIND,
    XIR_InstrCvtSI2SD_R_R_KIND,
    XIR_InstrCvtSI2SD_R_M_KIND,

    // Load an address computation into a register.
    XIR_InstrLEA_KIND,

    // Compare two values and set condition flags
    XIR_InstrCmp_R_R_KIND,
    XIR_InstrCmp_R_I_KIND,
    XIR_InstrCmp_R_M_KIND,
    XIR_InstrCmp_M_R_KIND,
    XIR_InstrCmp_M_I_KIND,

    // Compare floating-point values and set condition flags.
    XIR_InstrUComiSS_R_R_KIND,
    XIR_InstrUComiSS_R_M_KIND,
    XIR_InstrUComiSD_R_R_KIND,
    XIR_InstrUComiSD_R_M_KIND,

    // Jump to instruction index
    XIR_InstrJmp_KIND,

    // Jump to instruction index based on condition
    XIR_InstrJmpCC_KIND,

    // Set a byte (0 or 1) based on condition
    XIR_InstrSetCC_KIND,

    XIR_InstrRepMovsb_KIND,
    XIR_InstrRepStosb_KIND,
    XIR_InstrSyscall_KIND,

    // Return value in specifed register
    XIR_InstrRet_KIND,

    // Call a procedure directly
    XIR_InstrCall_KIND,

    // Call a procedure indirectly (register has procedure address)
    XIR_InstrCall_R_KIND,

} XIR_InstrKind;

// TODO: Look into removing link-list support and storing instrs in an array (reduce memory costs).
struct XIR_Instr {
    XIR_InstrKind kind;
    u32 ino;
    XIR_Instr* next;
};

typedef enum XIR_MemAddrKind {
    XIR_ADDR_GLOBAL_SYM,
    XIR_ADDR_SIBD,
    XIR_ADDR_STR_LIT,
    XIR_ADDR_FLOAT_LIT,
} XIR_MemAddrKind;

typedef struct XIR_MemAddr {
    XIR_MemAddrKind kind;

    union {
        Symbol* global; // TODO: Should be able to add disp to global.
        struct {
            u32 base_reg;
            u32 index_reg;
            s32 disp;
            u8 scale;
        } sibd;
        StrLit* str_lit;
        FloatLit* float_lit;
    };
} XIR_MemAddr;

typedef struct XIR_InstrBinary_R_R {
    XIR_Instr super;
    u8 size;
    u32 dst;
    u32 src;
} XIR_InstrBinary_R_R;

typedef XIR_InstrBinary_R_R XIR_InstrAdd_R_R;
typedef XIR_InstrBinary_R_R XIR_InstrSub_R_R;
typedef XIR_InstrBinary_R_R XIR_InstrIMul_R_R;
typedef XIR_InstrBinary_R_R XIR_InstrAnd_R_R;
typedef XIR_InstrBinary_R_R XIR_InstrOr_R_R;
typedef XIR_InstrBinary_R_R XIR_InstrXor_R_R;

typedef struct XIR_InstrBinary_R_I {
    XIR_Instr super;
    u8 size;
    u32 dst;
    Scalar src;
} XIR_InstrBinary_R_I;

typedef XIR_InstrBinary_R_I XIR_InstrAdd_R_I;
typedef XIR_InstrBinary_R_I XIR_InstrSub_R_I;
typedef XIR_InstrBinary_R_I XIR_InstrIMul_R_I;
typedef XIR_InstrBinary_R_I XIR_InstrAnd_R_I;
typedef XIR_InstrBinary_R_I XIR_InstrOr_R_I;
typedef XIR_InstrBinary_R_I XIR_InstrXor_R_I;

typedef struct XIR_InstrBinary_R_M {
    XIR_Instr super;
    u8 size;
    u32 dst;
    XIR_MemAddr src;
} XIR_InstrBinary_R_M;

typedef XIR_InstrBinary_R_M XIR_InstrAdd_R_M;
typedef XIR_InstrBinary_R_M XIR_InstrSub_R_M;
typedef XIR_InstrBinary_R_M XIR_InstrIMul_R_M;
typedef XIR_InstrBinary_R_M XIR_InstrAnd_R_M;
typedef XIR_InstrBinary_R_M XIR_InstrOr_R_M;
typedef XIR_InstrBinary_R_M XIR_InstrXor_R_M;

typedef struct XIR_InstrBinaryFlt_R_R {
    XIR_Instr super;
    u32 dst;
    u32 src;
} XIR_InstrBinaryFlt_R_R;

typedef XIR_InstrBinaryFlt_R_R XIR_InstrAddSS_R_R;
typedef XIR_InstrBinaryFlt_R_R XIR_InstrAddSD_R_R;
typedef XIR_InstrBinaryFlt_R_R XIR_InstrSubSS_R_R;
typedef XIR_InstrBinaryFlt_R_R XIR_InstrSubSD_R_R;
typedef XIR_InstrBinaryFlt_R_R XIR_InstrMulSS_R_R;
typedef XIR_InstrBinaryFlt_R_R XIR_InstrMulSD_R_R;
typedef XIR_InstrBinaryFlt_R_R XIR_InstrDivSS_R_R;
typedef XIR_InstrBinaryFlt_R_R XIR_InstrDivSD_R_R;

typedef struct XIR_InstrBinaryFlt_R_M {
    XIR_Instr super;
    u32 dst;
    XIR_MemAddr src;
} XIR_InstrBinaryFlt_R_M;

typedef XIR_InstrBinaryFlt_R_M XIR_InstrAddSS_R_M;
typedef XIR_InstrBinaryFlt_R_M XIR_InstrAddSD_R_M;
typedef XIR_InstrBinaryFlt_R_M XIR_InstrSubSS_R_M;
typedef XIR_InstrBinaryFlt_R_M XIR_InstrSubSD_R_M;
typedef XIR_InstrBinaryFlt_R_M XIR_InstrMulSS_R_M;
typedef XIR_InstrBinaryFlt_R_M XIR_InstrMulSD_R_M;
typedef XIR_InstrBinaryFlt_R_M XIR_InstrDivSS_R_M;
typedef XIR_InstrBinaryFlt_R_M XIR_InstrDivSD_R_M;

typedef struct XIR_InstrBaseDiv_R {
    XIR_Instr super;
    u8 size;
    u32 rdx;
    u32 rax;
    u32 src;
} XIR_InstrBaseDiv_R;

typedef XIR_InstrBaseDiv_R XIR_InstrDiv_R;
typedef XIR_InstrBaseDiv_R XIR_InstrIDiv_R;

typedef struct XIR_InstrBaseDiv_M {
    XIR_Instr super;
    u8 size;
    u32 rdx;
    u32 rax;
    XIR_MemAddr src;
} XIR_InstrBaseDiv_M;

typedef XIR_InstrBaseDiv_M XIR_InstrDiv_M;
typedef XIR_InstrBaseDiv_M XIR_InstrIDiv_M;

typedef struct XIR_InstrShift_R_R {
    XIR_Instr super;
    u8 size;
    u32 dst;
    u32 src;
} XIR_InstrShift_R_R;

typedef XIR_InstrShift_R_R XIR_InstrSar_R_R;
typedef XIR_InstrShift_R_R XIR_InstrShl_R_R;

typedef struct XIR_InstrShift_R_I {
    XIR_Instr super;
    u8 size;
    u32 dst;
    Scalar src;
} XIR_InstrShift_R_I;

typedef XIR_InstrShift_R_I XIR_InstrSar_R_I;
typedef XIR_InstrShift_R_I XIR_InstrShl_R_I;

typedef struct XIR_InstrUnary {
    XIR_Instr super;
    u8 size;
    u32 dst;
} XIR_InstrUnary;

typedef XIR_InstrUnary XIR_InstrNot;
typedef XIR_InstrUnary XIR_InstrNeg;

typedef struct XIR_InstrMov_R_R {
    XIR_Instr super;
    u8 size;
    u32 dst;
    u32 src;
} XIR_InstrMov_R_R;

typedef struct XIR_InstrMov_R_I {
    XIR_Instr super;
    u8 size;
    u32 dst;
    Scalar src;
} XIR_InstrMov_R_I;

typedef struct XIR_InstrMov_R_M {
    XIR_Instr super;
    u8 size;
    u32 dst;
    XIR_MemAddr src;
} XIR_InstrMov_R_M;

typedef struct XIR_InstrMov_R_RH {
    XIR_Instr super;
    u32 dst;
    u32 src;
} XIR_InstrMov_R_RH;

typedef struct XIR_InstrMov_M_R {
    XIR_Instr super;
    u8 size;
    XIR_MemAddr dst;
    u32 src;
} XIR_InstrMov_M_R;

typedef struct XIR_InstrMov_M_I {
    XIR_Instr super;
    u8 size;
    XIR_MemAddr dst;
    Scalar src;
} XIR_InstrMov_M_I;

typedef struct XIR_InstrConvert_R_R {
    XIR_Instr super;
    u8 dst_size;
    u8 src_size;
    u32 dst;
    u32 src;
} XIR_InstrConvert_R_R;

typedef XIR_InstrConvert_R_R XIR_InstrMovZX_R_R;
typedef XIR_InstrConvert_R_R XIR_InstrMovSX_R_R;

typedef struct XIR_InstrConvert_R_M {
    XIR_Instr super;
    u8 dst_size;
    u8 src_size;
    u32 dst;
    XIR_MemAddr src;
} XIR_InstrConvert_R_M;

typedef XIR_InstrConvert_R_M XIR_InstrMovZX_R_M;
typedef XIR_InstrConvert_R_M XIR_InstrMovSX_R_M;

typedef struct XIR_InstrSExtAxToDx {
    XIR_Instr super;
    u8 size;
    u32 rdx;
    u32 rax;
} XIR_InstrSExtAxToDx;

typedef struct XIR_InstrMovFlt_R_R {
    XIR_Instr super;
    u32 dst;
    u32 src;
} XIR_InstrMovFlt_R_R;

typedef XIR_InstrMovFlt_R_R XIR_InstrMovSS_R_R;
typedef XIR_InstrMovFlt_R_R XIR_InstrMovSD_R_R;

typedef struct XIR_InstrMovFlt_R_M {
    XIR_Instr super;
    u32 dst;
    XIR_MemAddr src;
} XIR_InstrMovFlt_R_M;

typedef XIR_InstrMovFlt_R_M XIR_InstrMovSS_R_M;
typedef XIR_InstrMovFlt_R_M XIR_InstrMovSD_R_M;

typedef struct XIR_InstrMovFlt_M_R {
    XIR_Instr super;
    XIR_MemAddr dst;
    u32 src;
} XIR_InstrMovFlt_M_R;

typedef XIR_InstrMovFlt_M_R XIR_InstrMovSS_M_R;
typedef XIR_InstrMovFlt_M_R XIR_InstrMovSD_M_R;

typedef struct XIR_InstrFlt2Flt_R_R {
    XIR_Instr super;
    u32 dst;
    u32 src;
} XIR_InstrFlt2Flt_R_R;

typedef XIR_InstrFlt2Flt_R_R XIR_InstrCvtSS2SD_R_R;
typedef XIR_InstrFlt2Flt_R_R XIR_InstrCvtSD2SS_R_R;

typedef struct XIR_InstrFlt2Flt_R_M {
    XIR_Instr super;
    u32 dst;
    XIR_MemAddr src;
} XIR_InstrFlt2Flt_R_M;

typedef XIR_InstrFlt2Flt_R_M XIR_InstrCvtSS2SD_R_M;
typedef XIR_InstrFlt2Flt_R_M XIR_InstrCvtSD2SS_R_M;

typedef struct XIR_InstrFlt2Int_R_R {
    XIR_Instr super;
    u8 dst_size;
    u32 dst;
    u32 src;
} XIR_InstrFlt2Int_R_R;

typedef XIR_InstrFlt2Int_R_R XIR_InstrCvtSS2SI_R_R;
typedef XIR_InstrFlt2Int_R_R XIR_InstrCvtSD2SI_R_R;

typedef struct XIR_InstrFlt2Int_R_M {
    XIR_Instr super;
    u8 dst_size;
    u32 dst;
    XIR_MemAddr src;
} XIR_InstrFlt2Int_R_M;

typedef XIR_InstrFlt2Int_R_M XIR_InstrCvtSS2SI_R_M;
typedef XIR_InstrFlt2Int_R_M XIR_InstrCvtSD2SI_R_M;

typedef struct XIR_InstrInt2Flt_R_R {
    XIR_Instr super;
    u8 src_size;
    u32 dst;
    u32 src;
} XIR_InstrInt2Flt_R_R;

typedef XIR_InstrInt2Flt_R_R XIR_InstrCvtSI2SS_R_R;
typedef XIR_InstrInt2Flt_R_R XIR_InstrCvtSI2SD_R_R;

typedef struct XIR_InstrInt2Flt_R_M {
    XIR_Instr super;
    u8 src_size;
    u32 dst;
    XIR_MemAddr src;
} XIR_InstrInt2Flt_R_M;

typedef XIR_InstrInt2Flt_R_M XIR_InstrCvtSI2SS_R_M;
typedef XIR_InstrInt2Flt_R_M XIR_InstrCvtSI2SD_R_M;

typedef struct XIR_InstrRepMovsb {
    XIR_Instr super;
    u32 rdi;
    u32 rsi;
    u32 rcx;
} XIR_InstrRepMovsb;

typedef struct XIR_InstrRepStosb {
    XIR_Instr super;
    u32 rdi;
    u32 rax;
    u32 rcx;
} XIR_InstrRepStosb;

typedef struct XIR_InstrLEA {
    XIR_Instr super;
    u32 dst;
    XIR_MemAddr mem;
} XIR_InstrLEA;

typedef struct XIR_InstrCmp_R_R {
    XIR_Instr super;
    u8 size;
    u32 op1;
    u32 op2;
} XIR_InstrCmp_R_R;

typedef struct XIR_InstrCmp_R_I {
    XIR_Instr super;
    u8 size;
    u32 op1;
    Scalar op2;
} XIR_InstrCmp_R_I;

typedef struct XIR_InstrCmp_R_M {
    XIR_Instr super;
    u8 size;
    u32 op1;
    XIR_MemAddr op2;
} XIR_InstrCmp_R_M;

typedef struct XIR_InstrCmp_M_R {
    XIR_Instr super;
    u8 size;
    XIR_MemAddr op1;
    u32 op2;
} XIR_InstrCmp_M_R;

typedef struct XIR_InstrCmp_M_I {
    XIR_Instr super;
    u8 size;
    XIR_MemAddr op1;
    Scalar op2;
} XIR_InstrCmp_M_I;

typedef struct XIR_InstrCmpFlt_R_R {
    XIR_Instr super;
    u32 op1;
    u32 op2;
} XIR_InstrCmpFlt_R_R;

typedef XIR_InstrCmpFlt_R_R XIR_InstrUComiSS_R_R;
typedef XIR_InstrCmpFlt_R_R XIR_InstrUComiSD_R_R;

typedef struct XIR_InstrCmpFlt_R_M {
    XIR_Instr super;
    u32 op1;
    XIR_MemAddr op2;
} XIR_InstrCmpFlt_R_M;

typedef XIR_InstrCmpFlt_R_M XIR_InstrUComiSS_R_M;
typedef XIR_InstrCmpFlt_R_M XIR_InstrUComiSD_R_M;

typedef struct XIR_InstrJmpCC {
    XIR_Instr super;
    ConditionKind cond;
    XIR_BBlock* from;
    XIR_BBlock* true_bb;
    XIR_BBlock* false_bb;
} XIR_InstrJmpCC;

typedef struct XIR_InstrJmp {
    XIR_Instr super;
    XIR_BBlock* from;
    XIR_BBlock* target;
} XIR_InstrJmp;

typedef struct XIR_InstrSetCC {
    XIR_Instr super;
    ConditionKind cond;
    u32 dst;
} XIR_InstrSetCC;

typedef struct XIR_InstrRet {
    XIR_Instr super;
    u32 rax;
    u32 rdx;
} XIR_InstrRet;

typedef struct XIR_PrimArgSlot {
    bool in_reg;

    union {
        u32 sp_offset;
        X64_Reg preg;
    };
} XIR_PrimArgSlot;

typedef struct XIR_ObjArgSlot {
    unsigned num_regs;
    union {
        u32 sp_offset;
        X64_Reg pregs[2];
    };

    // NOTE: Only used for Windows calling convention.
    bool as_ptr;
    u32 ptr_sp_offset;
} XIR_ObjArgSlot;

typedef union XIR_CallValue {
    u32 reg;
    XIR_MemAddr addr;
} XIR_CallValue;

typedef struct XIR_InstrCallArg {
    Type* type;

    XIR_CallValue val;

    // Required location before call instruction
    union {
        XIR_PrimArgSlot prim;
        XIR_ObjArgSlot obj;
    } slot;
} XIR_InstrCallArg;

typedef struct XIR_InstrCall {
    XIR_Instr super;
    Symbol* sym;
    XIR_CallValue dst;
    u32 num_args;
    XIR_InstrCallArg* args;
    X64_StackArgsInfo stack_info;
    unsigned save_reg_mask;
} XIR_InstrCall;

typedef struct XIR_InstrCall_R {
    XIR_Instr super;
    Type* proc_type;
    u32 proc_loc;
    XIR_CallValue dst;
    u32 num_args;
    XIR_InstrCallArg* args;
    X64_StackArgsInfo stack_info;
    unsigned save_reg_mask;
} XIR_InstrCall_R;

typedef struct XIR_InstrSyscall {
    XIR_Instr super;
    u32 rax; // return
    u32 rcx; // clobbered
    u32 r11; // clobbered
    u8 num_args; // At most 6
    u32* args;
} XIR_InstrSyscall;

struct XIR_BBlock {
    long id;
    u32 flags;

    // Doubly-linked list of instructions.
    size_t num_instrs;
    XIR_Instr* first;
    XIR_Instr* last;
};

typedef struct XIR_Builder {
    Allocator* arena;

    u32 num_regs;
    XIR_RegRange* lreg_ranges; // Stretchy buf
    XIR_Instr** call_sites; // Stretchy buf

    size_t num_instrs;
    size_t num_bblocks;
    XIR_BBlock** bblocks;

    u32* reg_map; // Map IR reg -> LIR reg; size: num_iregs
    u32 lreg_rbp;
    u32 stack_reg_mask;

    // Disjoint Set Union data structure for register renaming/aliasing.
    u32* lreg_aliases; // Root alias node for each lir reg. size: num_lirregs
    u32* lreg_sizes; // Size for each lir reg aliasing set. size: num_lirregs
} XIR_Builder;

// Data structures used to track the "location" of a virtual IR register.
// A virtual register could be assigned to a physical register, or could be assigned
// to a stack offset.
typedef enum XIR_RegLocKind {
    XIR_LREG_LOC_UNASSIGNED = 0,
    XIR_LREG_LOC_REG,
    XIR_LREG_LOC_STACK,
} XIR_RegLocKind;

struct XIR_RegLoc {
    XIR_RegLocKind kind;

    union {
        X64_Reg reg;
        s32 offset;
    };
};

struct XIR_RegRangeList {
    List ranges;
    ListNode lnode;
};

typedef enum XIR_RegAllocControlKind {
    XIR_REG_ALLOC_CTRL_NONE = 0,
    XIR_REG_ALLOC_CTRL_FORCE_REG, // Used for required operand registers (e.g., rcx for shift) and SIBD addr registers
    XIR_REG_ALLOC_CTRL_FORCE_ANY_REG,
    XIR_REG_ALLOC_CTRL_FORCE_REG_OR_SPILL, // Used for procedure arguments
    XIR_REG_ALLOC_CTRL_HINT_LIR_REG, // Used for register to register moves
    XIR_REG_ALLOC_CTRL_HINT_PHYS_REG, // Used for register to register moves
} XIR_RegAllocControlKind;

struct XIR_RegRange {
    u32 lreg;
    long start;
    long end;

    X64_RegClass reg_class;
    XIR_RegAllocControlKind ra_ctrl_kind;
    union {
        X64_Reg preg;
        u32 lreg;
        u32 preg_mask;
    } ra_ctrl;

    XIR_RegLoc loc;
    ListNode lnode;
};

u32 XIR_find_alias_reg(XIR_Builder* builder, u32 r);
void XIR_emit_instrs(XIR_Builder* builder, size_t num_iregs, size_t num_bblocks, BBlock** bblocks);

void XIR_emit_instr_add_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_add_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void XIR_emit_instr_add_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_sub_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_sub_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void XIR_emit_instr_sub_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_imul_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_imul_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void XIR_emit_instr_imul_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_and_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_and_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void XIR_emit_instr_and_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_or_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_or_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void XIR_emit_instr_or_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_xor_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_xor_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void XIR_emit_instr_xor_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_addss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_addss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_addsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_addsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_subss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_subss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_subsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_subsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_mulss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_mulss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_mulsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_mulsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_divss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_divss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_divsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_divsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);

void XIR_emit_instr_sar_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_sar_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void XIR_emit_instr_shl_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_shl_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);

typedef void (*XIR_EmitInstrIntDiv_R_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src);
typedef void (*XIR_EmitInstrIntDiv_M_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, XIR_MemAddr src);
void XIR_emit_instr_div_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src);
void XIR_emit_instr_div_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, XIR_MemAddr src);
void XIR_emit_instr_idiv_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src);
void XIR_emit_instr_idiv_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, XIR_MemAddr src);

void XIR_emit_instr_neg(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst);
void XIR_emit_instr_not(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst);

typedef void (*XIR_EmitInstrMovXX_R_R_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src);
typedef void (*XIR_EmitInstrMovXX_R_M_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size,
                                            XIR_MemAddr src);
void XIR_emit_instr_movzx_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src);
void XIR_emit_instr_movzx_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, XIR_MemAddr src);
void XIR_emit_instr_movsx_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src);
void XIR_emit_instr_movsx_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, XIR_MemAddr src);

typedef void (*XIR_EmitInstrFlt2Flt_R_R_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
typedef void (*XIR_EmitInstrFlt2Flt_R_M_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_cvtss2sd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_cvtss2sd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_cvtsd2ss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_cvtsd2ss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);

typedef void (*XIR_EmitInstrFlt2Int_R_R_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u32 src);
typedef void (*XIR_EmitInstrFlt2Int_R_M_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_cvtss2si_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u32 src);
void XIR_emit_instr_cvtss2si_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_cvtsd2si_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u32 src);
void XIR_emit_instr_cvtsd2si_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, XIR_MemAddr src);

typedef void (*XIR_EmitInstrInt2Flt_R_R_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, u32 src);
typedef void (*XIR_EmitInstrInt2Flt_R_M_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, XIR_MemAddr src);
void XIR_emit_instr_cvtsi2ss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, u32 src);
void XIR_emit_instr_cvtsi2ss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, XIR_MemAddr src);
void XIR_emit_instr_cvtsi2sd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, u32 src);
void XIR_emit_instr_cvtsi2sd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, XIR_MemAddr src);

void XIR_emit_instr_mov_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src);
void XIR_emit_instr_mov_r_rh(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_mov_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_mov_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void XIR_emit_instr_mov_m_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, XIR_MemAddr dst, u32 src);
void XIR_emit_instr_mov_m_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, XIR_MemAddr dst, Scalar src);

typedef void (*XIR_EmitInstrMovFlt_R_R_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
typedef void (*XIR_EmitInstrMovFlt_R_M_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
typedef void (*XIR_EmitInstrMovFlt_M_R_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, XIR_MemAddr dst, u32 src);
void XIR_emit_instr_movss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_movss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_movss_m_r(XIR_Builder* builder, XIR_BBlock* xbblock, XIR_MemAddr dst, u32 src);
void XIR_emit_instr_movsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src);
void XIR_emit_instr_movsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src);
void XIR_emit_instr_movsd_m_r(XIR_Builder* builder, XIR_BBlock* xbblock, XIR_MemAddr dst, u32 src);

void XIR_emit_instr_sext_ax_to_dx(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax);
void XIR_emit_instr_lea(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr mem);

void XIR_emit_instr_cmp_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 op1, u32 op2);
void XIR_emit_instr_cmp_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 op1, Scalar op2);
void XIR_emit_instr_cmp_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 op1, XIR_MemAddr op2);
void XIR_emit_instr_cmp_m_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, XIR_MemAddr op1, u32 op2);
void XIR_emit_instr_cmp_m_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, XIR_MemAddr op1, Scalar op2);

typedef void (*XIR_EmitInstrFltCmp_R_R_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, u32 op2);
typedef void (*XIR_EmitInstrFltCmp_R_M_Func)(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, XIR_MemAddr op2);
void XIR_emit_instr_ucomiss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, u32 op2);
void XIR_emit_instr_ucomiss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, XIR_MemAddr op2);
void XIR_emit_instr_ucomisd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, u32 op2);
void XIR_emit_instr_ucomisd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, XIR_MemAddr op2);

void XIR_emit_instr_jmp(XIR_Builder* builder, XIR_BBlock* xbblock, XIR_BBlock* target);
void XIR_emit_instr_jmpcc(XIR_Builder* builder, XIR_BBlock* xbblock, ConditionKind cond, XIR_BBlock* true_bb, XIR_BBlock* false_bb);
void XIR_emit_instr_setcc(XIR_Builder* builder, XIR_BBlock* xbblock, ConditionKind cond, u32 dst);
void XIR_emit_instr_rep_movsb(XIR_Builder* builder, XIR_BBlock* xbblock, u32 rdi, u32 rsi, u32 rcx);
void XIR_emit_instr_rep_stosb(XIR_Builder* builder, XIR_BBlock* xbblock, u32 rdi, u32 rax, u32 rcx);
void XIR_emit_instr_syscall(XIR_Builder* builder, XIR_BBlock* xbblock, u32 rax, u8 num_args, u32* args, u32 rcx, u32 r11);
void XIR_emit_instr_ret(XIR_Builder* builder, XIR_BBlock* xbblock, u32 rax, u32 rdx);
XIR_Instr* XIR_emit_instr_call(XIR_Builder* builder, XIR_BBlock* xbblock, Symbol* sym, XIR_CallValue dst, u32 num_args,
                               XIR_InstrCallArg* args, X64_StackArgsInfo stack_info);
XIR_Instr* XIR_emit_instr_call_r(XIR_Builder* builder, XIR_BBlock* xbblock, Type* proc_type, u32 proc_loc, XIR_CallValue dst,
                                 u32 num_args, XIR_InstrCallArg* args, X64_StackArgsInfo stack_info);
#endif
