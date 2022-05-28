#ifndef NIBBLE_X64_LIR_H
#define NIBBLE_X64_LIR_H
#include "allocator.h"
#include "hash_map.h"
#include "x64_gen/regs.h"

#define X64_LIR_REG_COUNT 0xFFFFFFFF

typedef struct X64_BBlock X64_BBlock;
typedef struct X64_Instr X64_Instr;

typedef enum X64_InstrKind
{
    X64_INSTR_NONE = 0,

    // Addition
    X64_INSTR_ADD_R_R,
    X64_INSTR_ADD_R_I,
    X64_INSTR_ADD_R_M,

    // f32 add
    X64_INSTR_ADDSS_R_R,
    X64_INSTR_ADDSS_R_M,

    // f64 add
    X64_INSTR_ADDSD_R_R,
    X64_INSTR_ADDSD_R_M,

    // Subtraction
    X64_INSTR_SUB_R_R,
    X64_INSTR_SUB_R_I,
    X64_INSTR_SUB_R_M,

    // f32 sub
    X64_INSTR_SUBSS_R_R,
    X64_INSTR_SUBSS_R_M,

    // f64 sub
    X64_INSTR_SUBSD_R_R,
    X64_INSTR_SUBSD_R_M,

    // Multiplication
    X64_INSTR_IMUL_R_R,
    X64_INSTR_IMUL_R_I,
    X64_INSTR_IMUL_R_M,

    // Unsigned division
    X64_INSTR_DIV_R,
    X64_INSTR_DIV_M,

    // Signed division
    X64_INSTR_IDIV_R,
    X64_INSTR_IDIV_M,

    // Bitwise AND
    X64_INSTR_AND_R_R,
    X64_INSTR_AND_R_I,
    X64_INSTR_AND_R_M,

    // Bitwise OR
    X64_INSTR_OR_R_R,
    X64_INSTR_OR_R_I,
    X64_INSTR_OR_R_M,

    // Bitwise XOR
    X64_INSTR_XOR_R_R,
    X64_INSTR_XOR_R_I,
    X64_INSTR_XOR_R_M,

    // Arithmetic shift right
    X64_INSTR_SAR_R_R,
    X64_INSTR_SAR_R_I,

    // Shift left
    X64_INSTR_SHL_R_R,
    X64_INSTR_SHL_R_I,

    // Bitwise NOT
    X64_INSTR_NOT,

    // Two's complement negation.
    X64_INSTR_NEG,

    X64_INSTR_MOV_R_I, // Load imm
    X64_INSTR_MOV_R_R, // Register copy
    X64_INSTR_MOV_R_M, // Load memory

    X64_INSTR_MOV_R_RH, // Copy high byte of register to another register.

    // Store into memory.
    X64_INSTR_MOV_M_R,
    X64_INSTR_MOV_M_I,

    // Zero-extend
    X64_INSTR_MOVZX_R_R,
    X64_INSTR_MOVZX_R_M,

    // Sign-extend
    X64_INSTR_MOVSX_R_R,
    X64_INSTR_MOVSX_R_M,
    X64_INSTR_SEXT_AX_TO_DX,

    // f32 mov
    X64_INSTR_MOVSS_R_R,
    X64_INSTR_MOVSS_R_M,
    X64_INSTR_MOVSS_M_R,

    // f64 mov
    X64_INSTR_MOVSD_R_R,
    X64_INSTR_MOVSD_R_M,
    X64_INSTR_MOVSD_M_R,

    // Fp to Fp
    X64_INSTR_CVTSS2SD_R_R,
    X64_INSTR_CVTSS2SD_R_M,
    X64_INSTR_CVTSD2SS_R_R,
    X64_INSTR_CVTSD2SS_R_M,

    // Fp to Int
    X64_INSTR_CVTTSS2SI_R_R,
    X64_INSTR_CVTTSS2SI_R_M,
    X64_INSTR_CVTTSD2SI_R_R,
    X64_INSTR_CVTTSD2SI_R_M,

    // Int to Fp
    X64_INSTR_CVTSI2SS_R_R,
    X64_INSTR_CVTSI2SS_R_M,
    X64_INSTR_CVTSI2SD_R_R,
    X64_INSTR_CVTSI2SD_R_M,

    // Load an address computation into a register.
    X64_INSTR_LEA,

    // Compare two values and set condition flags
    X64_INSTR_CMP_R_R,
    X64_INSTR_CMP_R_I,
    X64_INSTR_CMP_R_M,
    X64_INSTR_CMP_M_R,
    X64_INSTR_CMP_M_I,

    // Jump to instruction index
    X64_INSTR_JMP,

    // Jump to instruction index based on condition
    X64_INSTR_JMPCC,

    // Set a byte (0 or 1) based on condition
    X64_INSTR_SETCC,

    // Return value in specifed register
    X64_INSTR_RET,

    // Call a procedure directly
    X64_INSTR_CALL,

    // Call a procedure indirectly (register has procedure address)
    X64_INSTR_CALL_R,

    X64_INSTR_REP_MOVSB,
    X64_INSTR_REP_STOSB,
} X64_InstrKind;

typedef enum X64_MemAddrKind
{
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
    size_t size;
    u32 dst;
    u32 src;
} X64_InstrBinary_R_R;

typedef struct X64_InstrBinary_R_I {
    size_t size;
    u32 dst;
    Scalar src;
} X64_InstrBinary_R_I;

typedef struct X64_InstrBinary_R_M {
    size_t size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrBinary_R_M;

typedef struct X64_InstrBinaryFP_R_R {
    u32 dst;
    u32 src;
} X64_InstrBinaryFP_R_R;

typedef struct X64_InstrBinaryFP_R_M {
    u32 dst;
    X64_MemAddr src;
} X64_InstrBinaryFP_R_M;

typedef struct X64_InstrShift_R_R {
    size_t size;
    u32 dst;
    u32 src;
} X64_InstrShift_R_R;

typedef struct X64_InstrShift_R_I {
    size_t size;
    u32 dst;
    Scalar src;
} X64_InstrShift_R_I;

typedef struct X64_InstrDiv_R {
    size_t size;
    u32 rdx;
    u32 rax;
    u32 src;
} X64_InstrDiv_R;

typedef struct X64_InstrDiv_M {
    size_t size;
    u32 rdx;
    u32 rax;
    X64_MemAddr src;
} X64_InstrDiv_M;

typedef struct X64_InstrUnary {
    size_t size;
    u32 dst;
} X64_InstrUnary;

typedef struct X64_InstrMov_R_RH {
    u32 dst;
    u32 src;
} X64_InstrMov_R_RH;

typedef struct X64_InstrMov_R_R {
    size_t size;
    u32 dst;
    u32 src;
} X64_InstrMov_R_R;

typedef struct X64_InstrMov_R_M {
    size_t size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrMov_R_M;

typedef struct X64_InstrMov_R_I {
    size_t size;
    u32 dst;
    Scalar src;
} X64_InstrMov_R_I;

typedef struct X64_InstrMov_M_R {
    size_t size;
    X64_MemAddr dst;
    u32 src;
} X64_InstrMov_M_R;

typedef struct X64_InstrMov_M_I {
    size_t size;
    X64_MemAddr dst;
    Scalar src;
} X64_InstrMov_M_I;

typedef struct X64_InstrMovfp_R_R {
    u32 dst;
    u32 src;
} X64_InstrMovfp_R_R;

typedef struct X64_InstrMovfp_R_M {
    u32 dst;
    X64_MemAddr src;
} X64_InstrMovfp_R_M;

typedef struct X64_InstrMovfp_M_R {
    X64_MemAddr dst;
    u32 src;
} X64_InstrMovfp_M_R;

typedef struct X64_InstrFp2Fp_R_R {
    u32 dst;
    u32 src;
} X64_InstrFp2Fp_R_R;

typedef struct X64_InstrFp2Fp_R_M {
    u32 dst;
    X64_MemAddr src;
} X64_InstrFp2Fp_R_M;

typedef struct X64_InstrFp2Int_R_R {
    size_t dst_size;
    u32 dst;
    u32 src;
} X64_InstrFp2Int_R_R;

typedef struct X64_InstrFp2Int_R_M {
    size_t dst_size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrFp2Int_R_M;

typedef struct X64_InstrInt2Fp_R_R {
    size_t src_size;
    u32 dst;
    u32 src;
} X64_InstrInt2Fp_R_R;

typedef struct X64_InstrInt2Fp_R_M {
    size_t src_size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrInt2Fp_R_M;

typedef struct X64_InstrRepMovsb {
    u32 rdi;
    u32 rsi;
    u32 rcx;
} X64_InstrRepMovsb;

typedef struct X64_InstrRepStosb {
    u32 rdi;
    u32 rax;
    u32 rcx;
} X64_InstrRepStosb;

typedef struct X64_InstrConvert_R_R {
    size_t dst_size;
    size_t src_size;
    u32 dst;
    u32 src;
} X64_InstrConvert_R_R;

typedef struct X64_InstrConvert_R_M {
    size_t dst_size;
    size_t src_size;
    u32 dst;
    X64_MemAddr src;
} X64_InstrConvert_R_M;

typedef struct X64_InstrSExtAxToDx {
    size_t size;
    u32 rdx;
    u32 rax;
} X64_InstrSExtAxToDx;

typedef struct X64_InstrLEA {
    u32 dst;
    X64_MemAddr mem;
} X64_InstrLEA;

typedef struct X64_InstrCmp_R_R {
    size_t size;
    u32 op1;
    u32 op2;
} X64_InstrCmp_R_R;

typedef struct X64_InstrCmp_R_I {
    size_t size;
    u32 op1;
    Scalar op2;
} X64_InstrCmp_R_I;

typedef struct X64_InstrCmp_R_M {
    size_t size;
    u32 op1;
    X64_MemAddr op2;
} X64_InstrCmp_R_M;

typedef struct X64_InstrCmp_M_R {
    size_t size;
    X64_MemAddr op1;
    u32 op2;
} X64_InstrCmp_M_R;

typedef struct X64_InstrCmp_M_I {
    size_t size;
    X64_MemAddr op1;
    Scalar op2;
} X64_InstrCmp_M_I;

typedef struct X64_InstrJmpCC {
    ConditionKind cond;
    X64_BBlock* from;
    X64_BBlock* true_bb;
    X64_BBlock* false_bb;
} X64_InstrJmpCC;

typedef struct X64_InstrJmp {
    X64_BBlock* from;
    X64_BBlock* target;
} X64_InstrJmp;

typedef struct X64_InstrSetCC {
    ConditionKind cond;
    u32 dst;
} X64_InstrSetCC;

typedef struct X64_InstrRet {
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
    Symbol* sym;
    X64_CallValue dst;
    u32 num_args;
    X64_InstrCallArg* args;
    X64_StackArgsInfo stack_info;
    unsigned save_reg_mask;
} X64_InstrCall;

typedef struct X64_InstrCall_R {
    Type* proc_type;
    u32 proc_loc;
    X64_CallValue dst;
    u32 num_args;
    X64_InstrCallArg* args;
    X64_StackArgsInfo stack_info;
    unsigned save_reg_mask;
} X64_InstrCall_R;

struct X64_Instr {
    X64_InstrKind kind;
    long ino;
    bool is_leader;

    union {
        X64_InstrBinary_R_R binary_r_r;
        X64_InstrBinary_R_I binary_r_i;
        X64_InstrBinary_R_M binary_r_m;

        X64_InstrBinaryFP_R_R binary_fp_r_r;
        X64_InstrBinaryFP_R_M binary_fp_r_m;

        X64_InstrShift_R_R shift_r_r;
        X64_InstrShift_R_I shift_r_i;

        X64_InstrDiv_R div_r;
        X64_InstrDiv_M div_m;

        X64_InstrUnary unary;

        X64_InstrMov_R_R mov_r_rh;
        X64_InstrMov_R_R mov_r_r;
        X64_InstrMov_R_M mov_r_m;
        X64_InstrMov_R_I mov_r_i;
        X64_InstrMov_M_R mov_m_r;
        X64_InstrMov_M_I mov_m_i;

        X64_InstrMovfp_R_R movfp_r_r;
        X64_InstrMovfp_R_M movfp_r_m;
        X64_InstrMovfp_M_R movfp_m_r;

        X64_InstrFp2Fp_R_R fp2fp_r_r;
        X64_InstrFp2Fp_R_M fp2fp_r_m;

        X64_InstrFp2Int_R_R fp2int_r_r;
        X64_InstrFp2Int_R_M fp2int_r_m;

        X64_InstrInt2Fp_R_R int2fp_r_r;
        X64_InstrInt2Fp_R_M int2fp_r_m;

        X64_InstrRepMovsb rep_movsb;
        X64_InstrRepStosb rep_stosb;

        X64_InstrConvert_R_R convert_r_r;
        X64_InstrConvert_R_M convert_r_m;

        X64_InstrSExtAxToDx sext_ax_to_dx;

        X64_InstrLEA lea;

        X64_InstrCmp_R_R cmp_r_r;
        X64_InstrCmp_R_I cmp_r_i;
        X64_InstrCmp_R_M cmp_r_m;
        X64_InstrCmp_M_R cmp_m_r;
        X64_InstrCmp_M_I cmp_m_i;

        X64_InstrJmp jmp;

        X64_InstrJmpCC jmpcc;

        X64_InstrSetCC setcc;

        X64_InstrRet ret;

        X64_InstrCall call;

        X64_InstrCall_R call_r;
    };

    X64_Instr* prev;
    X64_Instr* next;
};

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
    X64_Instr** call_sites;     // Stretchy buf

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

void X64_emit_instr_binary_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, u32 src);
void X64_emit_instr_binary_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, Scalar src);
void X64_emit_instr_binary_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, X64_MemAddr src);

void X64_emit_instr_addfp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, u32 src);
void X64_emit_instr_addfp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, X64_MemAddr src);

void X64_emit_instr_shift_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, u32 src);
void X64_emit_instr_shift_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, Scalar src);
void X64_emit_instr_div_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 rdx, u32 rax, u32 src);
void X64_emit_instr_div_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 rdx, u32 rax,
                          X64_MemAddr src);
void X64_emit_instr_unary(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst);
void X64_emit_instr_mov_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 dst, u32 src);
void X64_emit_instr_mov_r_rh(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src);
void X64_emit_instr_mov_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 dst, X64_MemAddr src);
void X64_emit_instr_mov_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 dst, Scalar src);
void X64_emit_instr_mov_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, X64_MemAddr dst, u32 src);
void X64_emit_instr_mov_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, X64_MemAddr dst, Scalar src);

void X64_emit_instr_movfp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, u32 src);
void X64_emit_instr_movfp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, X64_MemAddr src);
void X64_emit_instr_movfp_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, X64_MemAddr dst, u32 src);

void X64_emit_instr_fp2fp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind dst_kind, u32 dst, FloatKind src_kind, u32 src);
void X64_emit_instr_fp2fp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind dst_kind, u32 dst, FloatKind src_kind, X64_MemAddr src);

void X64_emit_instr_fp2int_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t dst_size, u32 dst, FloatKind src_kind, u32 src);
void X64_emit_instr_fp2int_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t dst_size, u32 dst, FloatKind src_kind, X64_MemAddr src);

void X64_emit_instr_int2fp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind dst_kind, u32 dst, size_t src_size, u32 src);
void X64_emit_instr_int2fp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind dst_kind, u32 dst, size_t src_size, X64_MemAddr src);

void X64_emit_instr_convert_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t dst_size, u32 dst,
                                size_t src_size, u32 src);
void X64_emit_instr_convert_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t dst_size, u32 dst,
                                size_t src_size, X64_MemAddr src);
void X64_emit_instr_sext_ax_to_dx(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 rdx, u32 rax);
void X64_emit_instr_lea(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr mem);
void X64_emit_instr_cmp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 op1, u32 op2);
void X64_emit_instr_cmp_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 op1, Scalar op2);
void X64_emit_instr_cmp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 op1, X64_MemAddr op2);
void X64_emit_instr_cmp_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, X64_MemAddr op1, u32 op2);
void X64_emit_instr_cmp_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, X64_MemAddr op1, Scalar op2);
void X64_emit_instr_jmp(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_BBlock* target);
void X64_emit_instr_jmpcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, X64_BBlock* true_bb,
                          X64_BBlock* false_bb);
void X64_emit_instr_setcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, u32 dst);
void X64_emit_instr_rep_movsb(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rdi, u32 rsi, u32 rcx);
void X64_emit_instr_ret(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rax, u32 rdx);
X64_Instr* X64_emit_instr_call(X64_LIRBuilder* builder, X64_BBlock* xbblock, Symbol* sym, X64_CallValue dst, u32 num_args, X64_InstrCallArg* args,
                               X64_StackArgsInfo stack_info);
X64_Instr* X64_emit_instr_call_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, Type* proc_type, u32 proc_loc, X64_CallValue dst, u32 num_args,
                                 X64_InstrCallArg* args, X64_StackArgsInfo stack_info);
#endif
