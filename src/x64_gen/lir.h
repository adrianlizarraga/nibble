#ifndef NIBBLE_X64_LIR_H
#define NIBBLE_X64_LIR_H
#include "allocator.h"
#include "hash_map.h"
#include "x64_gen/regs.h"

#define X64_LIR_REG_COUNT 0xFFFFFFFF

typedef struct X64_BBlock X64_BBlock;
typedef struct X64_Instr X64_Instr;

typedef enum X64_InstrKind {
    X64_INSTR_KIND_NONE = 0,

    // Addition
    X64_InstrAdd_R_R_KIND,
    X64_InstrAdd_R_I_KIND,
    X64_InstrAdd_R_M_KIND,

    // f32 add
    X64_InstrAddSS_R_R_KIND,
    X64_InstrAddSS_R_M_KIND,

    // f64 add
    X64_InstrAddSD_R_R_KIND,
    X64_InstrAddSD_R_M_KIND,

    // Subtraction
    X64_InstrSub_R_R_KIND,
    X64_InstrSub_R_I_KIND,
    X64_InstrSub_R_M_KIND,

    // f32 sub
    X64_InstrSubSS_R_R_KIND,
    X64_InstrSubSS_R_M_KIND,

    // f64 sub
    X64_InstrSubSD_R_R_KIND,
    X64_InstrSubSD_R_M_KIND,

    // Multiplication
    X64_InstrIMul_R_R_KIND,
    X64_InstrIMul_R_I_KIND,
    X64_InstrIMul_R_M_KIND,

    // f32 mul
    X64_InstrMulSS_R_R_KIND,
    X64_InstrMulSS_R_M_KIND,

    // f64 mul
    X64_InstrMulSD_R_R_KIND,
    X64_InstrMulSD_R_M_KIND,

    // Unsigned division
    X64_InstrDiv_R_KIND,
    X64_InstrDiv_M_KIND,

    // Signed division
    X64_InstrIDiv_R_KIND,
    X64_InstrIDiv_M_KIND,

    // f32 div
    X64_InstrDivSS_R_R_KIND,
    X64_InstrDivSS_R_M_KIND,

    // f64 div
    X64_InstrDivSD_R_R_KIND,
    X64_InstrDivSD_R_M_KIND,

    // Bitwise AND
    X64_InstrAnd_R_R_KIND,
    X64_InstrAnd_R_I_KIND,
    X64_InstrAnd_R_M_KIND,

    // Bitwise OR
    X64_InstrOr_R_R_KIND,
    X64_InstrOr_R_I_KIND,
    X64_InstrOr_R_M_KIND,

    // Bitwise XOR
    X64_InstrXor_R_R_KIND,
    X64_InstrXor_R_I_KIND,
    X64_InstrXor_R_M_KIND,

    // Arithmetic shift right
    X64_InstrSar_R_R_KIND,
    X64_InstrSar_R_I_KIND,

    // Shift left
    X64_InstrShl_R_R_KIND,
    X64_InstrShl_R_I_KIND,

    // Bitwise NOT
    X64_InstrNot_KIND,

    // Two's complement negation.
    X64_InstrNeg_KIND,

    X64_InstrMov_R_R_KIND, // Register copy
    X64_InstrMov_R_I_KIND, // Load imm
    X64_InstrMov_R_M_KIND, // Load memory

    X64_InstrMov_R_RH_KIND, // Copy high byte of register to another register.

    // Store into memory.
    X64_InstrMov_M_R_KIND,
    X64_InstrMov_M_I_KIND,

    // Zero-extend
    X64_InstrMovZX_R_R_KIND,
    X64_InstrMovZX_R_M_KIND,

    // Sign-extend
    X64_InstrMovSX_R_R_KIND,
    X64_InstrMovSX_R_M_KIND,
    X64_InstrSExtAxToDx_KIND,

    // f32 mov
    X64_InstrMovSS_R_R_KIND,
    X64_InstrMovSS_R_M_KIND,
    X64_InstrMovSS_M_R_KIND,

    // f64 mov
    X64_InstrMovSD_R_R_KIND,
    X64_InstrMovSD_R_M_KIND,
    X64_InstrMovSD_M_R_KIND,

    // Flt to Flt
    X64_InstrCvtSS2SD_R_R_KIND,
    X64_InstrCvtSS2SD_R_M_KIND,
    X64_InstrCvtSD2SS_R_R_KIND,
    X64_InstrCvtSD2SS_R_M_KIND,

    // Flt to Int
    X64_InstrCvtSS2SI_R_R_KIND,
    X64_InstrCvtSS2SI_R_M_KIND,
    X64_InstrCvtSD2SI_R_R_KIND,
    X64_InstrCvtSD2SI_R_M_KIND,

    // Int to Flt
    X64_InstrCvtSI2SS_R_R_KIND,
    X64_InstrCvtSI2SS_R_M_KIND,
    X64_InstrCvtSI2SD_R_R_KIND,
    X64_InstrCvtSI2SD_R_M_KIND,

    // Load an address computation into a register.
    X64_InstrLEA_KIND,

    // Compare two values and set condition flags
    X64_InstrCmp_R_R_KIND,
    X64_InstrCmp_R_I_KIND,
    X64_InstrCmp_R_M_KIND,
    X64_InstrCmp_M_R_KIND,
    X64_InstrCmp_M_I_KIND,

    // Compare floating-point values and set condition flags.
    X64_InstrUComiSS_R_R_KIND,
    X64_InstrUComiSS_R_M_KIND,
    X64_InstrUComiSD_R_R_KIND,
    X64_InstrUComiSD_R_M_KIND,

    // Jump to instruction index
    X64_InstrJmp_KIND,

    // Jump to instruction index based on condition
    X64_InstrJmpCC_KIND,

    // Set a byte (0 or 1) based on condition
    X64_InstrSetCC_KIND,

    X64_InstrRepMovsb_KIND,
    X64_InstrRepStosb_KIND,
    X64_InstrSyscall_KIND,

    // Return value in specifed register
    X64_InstrRet_KIND,

    // Call a procedure directly
    X64_InstrCall_KIND,

    // Call a procedure indirectly (register has procedure address)
    X64_InstrCall_R_KIND,

} X64_InstrKind;

// TODO: Look into removing link-list support and storing instrs in an array (reduce memory costs).
struct X64_Instr {
    X64_InstrKind kind;
    u32 ino;
    X64_Instr* next;
};

typedef enum X64_MemAddrKind {
    X64_ADDR_GLOBAL_SYM,
    X64_ADDR_SIBD,
    X64_ADDR_STR_LIT,
    X64_ADDR_FLOAT_LIT,
} X64_MemAddrKind;

typedef struct X64_MemAddr {
    X64_MemAddrKind kind;

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
} X64_MemAddr;

typedef struct X64_InstrBinary_R_R {
    X64_Instr super;
    u8 size;
    u32 dst;
    u32 src;
} X64_InstrBinary_R_R;

typedef X64_InstrBinary_R_R X64_InstrAdd_R_R;
typedef X64_InstrBinary_R_R X64_InstrSub_R_R;
typedef X64_InstrBinary_R_R X64_InstrIMul_R_R;
typedef X64_InstrBinary_R_R X64_InstrAnd_R_R;
typedef X64_InstrBinary_R_R X64_InstrOr_R_R;
typedef X64_InstrBinary_R_R X64_InstrXor_R_R;

typedef struct X64_InstrBinary_R_I {
    X64_Instr super;
    u8 size;
    u32 dst;
    Scalar src;
} X64_InstrBinary_R_I;

typedef X64_InstrBinary_R_I X64_InstrAdd_R_I;
typedef X64_InstrBinary_R_I X64_InstrSub_R_I;
typedef X64_InstrBinary_R_I X64_InstrIMul_R_I;
typedef X64_InstrBinary_R_I X64_InstrAnd_R_I;
typedef X64_InstrBinary_R_I X64_InstrOr_R_I;
typedef X64_InstrBinary_R_I X64_InstrXor_R_I;

typedef struct X64_InstrBinary_R_M {
    X64_Instr super;
    u8 size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrBinary_R_M;

typedef X64_InstrBinary_R_M X64_InstrAdd_R_M;
typedef X64_InstrBinary_R_M X64_InstrSub_R_M;
typedef X64_InstrBinary_R_M X64_InstrIMul_R_M;
typedef X64_InstrBinary_R_M X64_InstrAnd_R_M;
typedef X64_InstrBinary_R_M X64_InstrOr_R_M;
typedef X64_InstrBinary_R_M X64_InstrXor_R_M;

typedef struct X64_InstrBinaryFlt_R_R {
    X64_Instr super;
    u32 dst;
    u32 src;
} X64_InstrBinaryFlt_R_R;

typedef X64_InstrBinaryFlt_R_R X64_InstrAddSS_R_R;
typedef X64_InstrBinaryFlt_R_R X64_InstrAddSD_R_R;
typedef X64_InstrBinaryFlt_R_R X64_InstrSubSS_R_R;
typedef X64_InstrBinaryFlt_R_R X64_InstrSubSD_R_R;
typedef X64_InstrBinaryFlt_R_R X64_InstrMulSS_R_R;
typedef X64_InstrBinaryFlt_R_R X64_InstrMulSD_R_R;
typedef X64_InstrBinaryFlt_R_R X64_InstrDivSS_R_R;
typedef X64_InstrBinaryFlt_R_R X64_InstrDivSD_R_R;

typedef struct X64_InstrBinaryFlt_R_M {
    X64_Instr super;
    u32 dst;
    X64_MemAddr src;
} X64_InstrBinaryFlt_R_M;

typedef X64_InstrBinaryFlt_R_M X64_InstrAddSS_R_M;
typedef X64_InstrBinaryFlt_R_M X64_InstrAddSD_R_M;
typedef X64_InstrBinaryFlt_R_M X64_InstrSubSS_R_M;
typedef X64_InstrBinaryFlt_R_M X64_InstrSubSD_R_M;
typedef X64_InstrBinaryFlt_R_M X64_InstrMulSS_R_M;
typedef X64_InstrBinaryFlt_R_M X64_InstrMulSD_R_M;
typedef X64_InstrBinaryFlt_R_M X64_InstrDivSS_R_M;
typedef X64_InstrBinaryFlt_R_M X64_InstrDivSD_R_M;

typedef struct X64_InstrBaseDiv_R {
    X64_Instr super;
    u8 size;
    u32 rdx;
    u32 rax;
    u32 src;
} X64_InstrBaseDiv_R;

typedef X64_InstrBaseDiv_R X64_InstrDiv_R;
typedef X64_InstrBaseDiv_R X64_InstrIDiv_R;

typedef struct X64_InstrBaseDiv_M {
    X64_Instr super;
    u8 size;
    u32 rdx;
    u32 rax;
    X64_MemAddr src;
} X64_InstrBaseDiv_M;

typedef X64_InstrBaseDiv_M X64_InstrDiv_M;
typedef X64_InstrBaseDiv_M X64_InstrIDiv_M;

typedef struct X64_InstrShift_R_R {
    X64_Instr super;
    u8 size;
    u32 dst;
    u32 src;
} X64_InstrShift_R_R;

typedef X64_InstrShift_R_R X64_InstrSar_R_R;
typedef X64_InstrShift_R_R X64_InstrShl_R_R;

typedef struct X64_InstrShift_R_I {
    X64_Instr super;
    u8 size;
    u32 dst;
    Scalar src;
} X64_InstrShift_R_I;

typedef X64_InstrShift_R_I X64_InstrSar_R_I;
typedef X64_InstrShift_R_I X64_InstrShl_R_I;

typedef struct X64_InstrUnary {
    X64_Instr super;
    u8 size;
    u32 dst;
} X64_InstrUnary;

typedef X64_InstrUnary X64_InstrNot;
typedef X64_InstrUnary X64_InstrNeg;

typedef struct X64_InstrMov_R_R {
    X64_Instr super;
    u8 size;
    u32 dst;
    u32 src;
} X64_InstrMov_R_R;

typedef struct X64_InstrMov_R_I {
    X64_Instr super;
    u8 size;
    u32 dst;
    Scalar src;
} X64_InstrMov_R_I;

typedef struct X64_InstrMov_R_M {
    X64_Instr super;
    u8 size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrMov_R_M;

typedef struct X64_InstrMov_R_RH {
    X64_Instr super;
    u32 dst;
    u32 src;
} X64_InstrMov_R_RH;

typedef struct X64_InstrMov_M_R {
    X64_Instr super;
    u8 size;
    X64_MemAddr dst;
    u32 src;
} X64_InstrMov_M_R;

typedef struct X64_InstrMov_M_I {
    X64_Instr super;
    u8 size;
    X64_MemAddr dst;
    Scalar src;
} X64_InstrMov_M_I;

typedef struct X64_InstrConvert_R_R {
    X64_Instr super;
    u8 dst_size;
    u8 src_size;
    u32 dst;
    u32 src;
} X64_InstrConvert_R_R;

typedef X64_InstrConvert_R_R X64_InstrMovZX_R_R;
typedef X64_InstrConvert_R_R X64_InstrMovSX_R_R;

typedef struct X64_InstrConvert_R_M {
    X64_Instr super;
    u8 dst_size;
    u8 src_size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrConvert_R_M;

typedef X64_InstrConvert_R_M X64_InstrMovZX_R_M;
typedef X64_InstrConvert_R_M X64_InstrMovSX_R_M;

typedef struct X64_InstrSExtAxToDx {
    X64_Instr super;
    u8 size;
    u32 rdx;
    u32 rax;
} X64_InstrSExtAxToDx;

typedef struct X64_InstrMovFlt_R_R {
    X64_Instr super;
    u32 dst;
    u32 src;
} X64_InstrMovFlt_R_R;

typedef X64_InstrMovFlt_R_R X64_InstrMovSS_R_R;
typedef X64_InstrMovFlt_R_R X64_InstrMovSD_R_R;

typedef struct X64_InstrMovFlt_R_M {
    X64_Instr super;
    u32 dst;
    X64_MemAddr src;
} X64_InstrMovFlt_R_M;

typedef X64_InstrMovFlt_R_M X64_InstrMovSS_R_M;
typedef X64_InstrMovFlt_R_M X64_InstrMovSD_R_M;

typedef struct X64_InstrMovFlt_M_R {
    X64_Instr super;
    X64_MemAddr dst;
    u32 src;
} X64_InstrMovFlt_M_R;

typedef X64_InstrMovFlt_M_R X64_InstrMovSS_M_R;
typedef X64_InstrMovFlt_M_R X64_InstrMovSD_M_R;

typedef struct X64_InstrFlt2Flt_R_R {
    X64_Instr super;
    u32 dst;
    u32 src;
} X64_InstrFlt2Flt_R_R;

typedef X64_InstrFlt2Flt_R_R X64_InstrCvtSS2SD_R_R;
typedef X64_InstrFlt2Flt_R_R X64_InstrCvtSD2SS_R_R;

typedef struct X64_InstrFlt2Flt_R_M {
    X64_Instr super;
    u32 dst;
    X64_MemAddr src;
} X64_InstrFlt2Flt_R_M;

typedef X64_InstrFlt2Flt_R_M X64_InstrCvtSS2SD_R_M;
typedef X64_InstrFlt2Flt_R_M X64_InstrCvtSD2SS_R_M;

typedef struct X64_InstrFlt2Int_R_R {
    X64_Instr super;
    u8 dst_size;
    u32 dst;
    u32 src;
} X64_InstrFlt2Int_R_R;

typedef X64_InstrFlt2Int_R_R X64_InstrCvtSS2SI_R_R;
typedef X64_InstrFlt2Int_R_R X64_InstrCvtSD2SI_R_R;

typedef struct X64_InstrFlt2Int_R_M {
    X64_Instr super;
    u8 dst_size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrFlt2Int_R_M;

typedef X64_InstrFlt2Int_R_M X64_InstrCvtSS2SI_R_M;
typedef X64_InstrFlt2Int_R_M X64_InstrCvtSD2SI_R_M;

typedef struct X64_InstrInt2Flt_R_R {
    X64_Instr super;
    u8 src_size;
    u32 dst;
    u32 src;
} X64_InstrInt2Flt_R_R;

typedef X64_InstrInt2Flt_R_R X64_InstrCvtSI2SS_R_R;
typedef X64_InstrInt2Flt_R_R X64_InstrCvtSI2SD_R_R;

typedef struct X64_InstrInt2Flt_R_M {
    X64_Instr super;
    u8 src_size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrInt2Flt_R_M;

typedef X64_InstrInt2Flt_R_M X64_InstrCvtSI2SS_R_M;
typedef X64_InstrInt2Flt_R_M X64_InstrCvtSI2SD_R_M;

typedef struct X64_InstrRepMovsb {
    X64_Instr super;
    u32 rdi;
    u32 rsi;
    u32 rcx;
} X64_InstrRepMovsb;

typedef struct X64_InstrRepStosb {
    X64_Instr super;
    u32 rdi;
    u32 rax;
    u32 rcx;
} X64_InstrRepStosb;

typedef struct X64_InstrLEA {
    X64_Instr super;
    u32 dst;
    X64_MemAddr mem;
} X64_InstrLEA;

typedef struct X64_InstrCmp_R_R {
    X64_Instr super;
    u8 size;
    u32 op1;
    u32 op2;
} X64_InstrCmp_R_R;

typedef struct X64_InstrCmp_R_I {
    X64_Instr super;
    u8 size;
    u32 op1;
    Scalar op2;
} X64_InstrCmp_R_I;

typedef struct X64_InstrCmp_R_M {
    X64_Instr super;
    u8 size;
    u32 op1;
    X64_MemAddr op2;
} X64_InstrCmp_R_M;

typedef struct X64_InstrCmp_M_R {
    X64_Instr super;
    u8 size;
    X64_MemAddr op1;
    u32 op2;
} X64_InstrCmp_M_R;

typedef struct X64_InstrCmp_M_I {
    X64_Instr super;
    u8 size;
    X64_MemAddr op1;
    Scalar op2;
} X64_InstrCmp_M_I;

typedef struct X64_InstrCmpFlt_R_R {
    X64_Instr super;
    u32 op1;
    u32 op2;
} X64_InstrCmpFlt_R_R;

typedef X64_InstrCmpFlt_R_R X64_InstrUComiSS_R_R;
typedef X64_InstrCmpFlt_R_R X64_InstrUComiSD_R_R;

typedef struct X64_InstrCmpFlt_R_M {
    X64_Instr super;
    u32 op1;
    X64_MemAddr op2;
} X64_InstrCmpFlt_R_M;

typedef X64_InstrCmpFlt_R_M X64_InstrUComiSS_R_M;
typedef X64_InstrCmpFlt_R_M X64_InstrUComiSD_R_M;

typedef struct X64_InstrJmpCC {
    X64_Instr super;
    ConditionKind cond;
    X64_BBlock* from;
    X64_BBlock* true_bb;
    X64_BBlock* false_bb;
} X64_InstrJmpCC;

typedef struct X64_InstrJmp {
    X64_Instr super;
    X64_BBlock* from;
    X64_BBlock* target;
} X64_InstrJmp;

typedef struct X64_InstrSetCC {
    X64_Instr super;
    ConditionKind cond;
    u32 dst;
} X64_InstrSetCC;

typedef struct X64_InstrRet {
    X64_Instr super;
    u32 rax;
    u32 rdx;
} X64_InstrRet;

typedef struct X64_PrimArgSlot {
    bool in_reg;

    union {
        u32 sp_offset;
        X64_Reg preg;
    };
} X64_PrimArgSlot;

typedef struct X64_ObjArgSlot {
    unsigned num_regs;
    union {
        u32 sp_offset;
        X64_Reg pregs[2];
    };

    // NOTE: Only used for Windows calling convention.
    bool as_ptr;
    u32 ptr_sp_offset;
} X64_ObjArgSlot;

typedef union X64_CallValue {
    u32 reg;
    X64_MemAddr addr;
} X64_CallValue;

typedef struct X64_InstrCallArg {
    Type* type;

    X64_CallValue val;

    // Required location before call instruction
    union {
        X64_PrimArgSlot prim;
        X64_ObjArgSlot obj;
    } slot;
} X64_InstrCallArg;

typedef struct X64_InstrCall {
    X64_Instr super;
    Symbol* sym;
    X64_CallValue dst;
    u32 num_args;
    X64_InstrCallArg* args;
    X64_StackArgsInfo stack_info;
    unsigned save_reg_mask;
} X64_InstrCall;

typedef struct X64_InstrCall_R {
    X64_Instr super;
    Type* proc_type;
    u32 proc_loc;
    X64_CallValue dst;
    u32 num_args;
    X64_InstrCallArg* args;
    X64_StackArgsInfo stack_info;
    unsigned save_reg_mask;
} X64_InstrCall_R;

typedef struct X64_InstrSyscall {
    X64_Instr super;
    u32 rax; // return
    u32 rcx; // clobbered
    u32 r11; // clobbered
    u8 num_args; // At most 6
    u32* args;
} X64_InstrSyscall;

struct X64_BBlock {
    long id;
    u32 flags;

    // Doubly-linked list of instructions.
    size_t num_instrs;
    X64_Instr* first;
    X64_Instr* last;
};

typedef struct X64_LIRBuilder {
    Allocator* arena;

    u32 num_regs;
    X64_LRegRange* lreg_ranges; // Stretchy buf
    X64_Instr** call_sites; // Stretchy buf

    size_t num_instrs;
    size_t num_bblocks;
    X64_BBlock** bblocks;

    u32* reg_map; // Map IR reg -> LIR reg; size: num_iregs
    u32 lreg_rbp;
    u32 stack_reg_mask;

    // Disjoint Set Union data structure for register renaming/aliasing.
    u32* lreg_aliases; // Root alias node for each lir reg. size: num_lirregs
    u32* lreg_sizes; // Size for each lir reg aliasing set. size: num_lirregs
} X64_LIRBuilder;

u32 X64_find_alias_reg(X64_LIRBuilder* builder, u32 r);

void X64_emit_instr_add_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_add_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void X64_emit_instr_add_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src);

void X64_emit_instr_sub_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_sub_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void X64_emit_instr_sub_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src);

void X64_emit_instr_imul_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_imul_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void X64_emit_instr_imul_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src);

void X64_emit_instr_and_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_and_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void X64_emit_instr_and_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src);

void X64_emit_instr_or_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_or_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void X64_emit_instr_or_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src);

void X64_emit_instr_xor_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_xor_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void X64_emit_instr_xor_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src);

void X64_emit_instr_addss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_addss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
void X64_emit_instr_addsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_addsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);

void X64_emit_instr_subss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_subss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
void X64_emit_instr_subsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_subsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);

void X64_emit_instr_mulss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_mulss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
void X64_emit_instr_mulsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_mulsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);

void X64_emit_instr_divss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_divss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
void X64_emit_instr_divsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_divsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);

void X64_emit_instr_sar_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_sar_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void X64_emit_instr_shl_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_shl_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);

typedef void (*X64_EmitInstrIntDiv_R_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src);
typedef void (*X64_EmitInstrIntDiv_M_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, X64_MemAddr src);
void X64_emit_instr_div_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src);
void X64_emit_instr_div_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, X64_MemAddr src);
void X64_emit_instr_idiv_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src);
void X64_emit_instr_idiv_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, X64_MemAddr src);

void X64_emit_instr_neg(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst);
void X64_emit_instr_not(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst);

typedef void (*X64_EmitInstrMovXX_R_R_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src);
typedef void (*X64_EmitInstrMovXX_R_M_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size,
                                            X64_MemAddr src);
void X64_emit_instr_movzx_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src);
void X64_emit_instr_movzx_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, X64_MemAddr src);
void X64_emit_instr_movsx_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src);
void X64_emit_instr_movsx_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, X64_MemAddr src);

typedef void (*X64_EmitInstrFlt2Flt_R_R_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
typedef void (*X64_EmitInstrFlt2Flt_R_M_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
void X64_emit_instr_cvtss2sd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_cvtss2sd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
void X64_emit_instr_cvtsd2ss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_cvtsd2ss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);

typedef void (*X64_EmitInstrFlt2Int_R_R_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u32 src);
typedef void (*X64_EmitInstrFlt2Int_R_M_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, X64_MemAddr src);
void X64_emit_instr_cvtss2si_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u32 src);
void X64_emit_instr_cvtss2si_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, X64_MemAddr src);
void X64_emit_instr_cvtsd2si_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u32 src);
void X64_emit_instr_cvtsd2si_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, X64_MemAddr src);

typedef void (*X64_EmitInstrInt2Flt_R_R_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, u32 src);
typedef void (*X64_EmitInstrInt2Flt_R_M_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, X64_MemAddr src);
void X64_emit_instr_cvtsi2ss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, u32 src);
void X64_emit_instr_cvtsi2ss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, X64_MemAddr src);
void X64_emit_instr_cvtsi2sd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, u32 src);
void X64_emit_instr_cvtsi2sd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, X64_MemAddr src);

void X64_emit_instr_mov_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src);
void X64_emit_instr_mov_r_rh(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_mov_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src);
void X64_emit_instr_mov_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src);
void X64_emit_instr_mov_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr dst, u32 src);
void X64_emit_instr_mov_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr dst, Scalar src);

typedef void (*X64_EmitInstrMovFlt_R_R_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
typedef void (*X64_EmitInstrMovFlt_R_M_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
typedef void (*X64_EmitInstrMovFlt_M_R_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_MemAddr dst, u32 src);
void X64_emit_instr_movss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_movss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
void X64_emit_instr_movss_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_MemAddr dst, u32 src);
void X64_emit_instr_movsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_movsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src);
void X64_emit_instr_movsd_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_MemAddr dst, u32 src);

void X64_emit_instr_sext_ax_to_dx(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax);
void X64_emit_instr_lea(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr mem);

void X64_emit_instr_cmp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, u32 op2);
void X64_emit_instr_cmp_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, Scalar op2);
void X64_emit_instr_cmp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, X64_MemAddr op2);
void X64_emit_instr_cmp_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr op1, u32 op2);
void X64_emit_instr_cmp_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr op1, Scalar op2);

typedef void (*X64_EmitInstrFltCmp_R_R_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, u32 op2);
typedef void (*X64_EmitInstrFltCmp_R_M_Func)(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, X64_MemAddr op2);
void X64_emit_instr_ucomiss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, u32 op2);
void X64_emit_instr_ucomiss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, X64_MemAddr op2);
void X64_emit_instr_ucomisd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, u32 op2);
void X64_emit_instr_ucomisd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, X64_MemAddr op2);

void X64_emit_instr_jmp(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_BBlock* target);
void X64_emit_instr_jmpcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, X64_BBlock* true_bb, X64_BBlock* false_bb);
void X64_emit_instr_setcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, u32 dst);
void X64_emit_instr_rep_movsb(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rdi, u32 rsi, u32 rcx);
void X64_emit_instr_rep_stosb(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rdi, u32 rax, u32 rcx);
void X64_emit_instr_syscall(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rax, u8 num_args, u32* args, u32 rcx, u32 r11);
void X64_emit_instr_ret(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rax, u32 rdx);
X64_Instr* X64_emit_instr_call(X64_LIRBuilder* builder, X64_BBlock* xbblock, Symbol* sym, X64_CallValue dst, u32 num_args,
                               X64_InstrCallArg* args, X64_StackArgsInfo stack_info);
X64_Instr* X64_emit_instr_call_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, Type* proc_type, u32 proc_loc, X64_CallValue dst,
                                 u32 num_args, X64_InstrCallArg* args, X64_StackArgsInfo stack_info);
#endif
