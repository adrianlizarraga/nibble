#ifndef NIBBLE_X64_LIR_H
#define NIBBLE_X64_LIR_H
#include "allocator.h"
#include "hash_map.h"
#include "x64_gen/regs.h"

typedef enum X64_InstrKind {
    X64_INSTR_NONE = 0,

    // Addition
    X64_INSTR_ADD_R_R,
    X64_INSTR_ADD_R_I,

    // Subtraction
    X64_INSTR_SUB_R_R,
    X64_INSTR_SUB_R_I,

    // Multiplication
    X64_INSTR_IMUL_R_R,
    X64_INSTR_IMUL_R_I,

    // Unsigned division
    X64_INSTR_DIV,

    // Signed division
    X64_INSTR_IDIV,

    // Bitwise AND
    X64_INSTR_AND_R_R,
    X64_INSTR_AND_R_I,

    // Bitwise OR
    X64_INSTR_OR_R_R,
    X64_INSTR_OR_R_I,

    // Bitwise XOR
    X64_INSTR_XOR_R_R,
    X64_INSTR_XOR_R_I,

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

    // Store into memory.
    X64_INSTR_MOV_M_R,
    X64_INSTR_MOV_M_I,

    // Zero-extend
    X64_INSTR_MOVZX_R_R,

    // Sign-extend
    X64_INSTR_MOVSX_R_R,
    X64_INSTR_SEXT_AX_TO_DX,

    // Load an address computation into a register.
    X64_INSTR_LEA,

    // Compare two values and set condition flags
    X64_INSTR_CMP_R_R,
    X64_INSTR_CMP_R_I,

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

    X64_INSTR_REP_MOVSB
} X64_InstrKind;

typedef enum X64_MemAddrKind {
    X64_ADDR_GLOBAL,
    X64_ADDR_LOCAL,
    X64_ADDR_STR_LIT,
} X64_MemAddrKind;

typedef struct X64_MemAddr {
    X64_MemAddrKind kind;

    union {
        Symbol* global;
        struct {
            u32 base_reg;
            u32 index_reg;
            s32 disp;
            u8 scale;
        } local;
        StrLit* str_lit;
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

typedef struct X64_InstrDiv {
    size_t size;
    u32 src;
} X64_InstrDiv;

typedef struct X64_InstrUnary {
    size_t size;
    u32 dst;
} X64_InstrUnary;

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

typedef struct X64_InstrConvert_R_R {
    size_t dst_size;
    size_t src_size;
    u32 dst;
    u32 src;
} X64_InstrConvert_R_R;

typedef struct X64_InstrSExtAxToDx {
    size_t size;
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

typedef struct X64_InstrJmpCC {
    u32 jmp_target;
    ConditionKind cond;
} X64_InstrJmpCC;

typedef struct X64_InstrJmp {
    u32 jmp_target;
} X64_InstrJmp;

typedef struct X64_InstrSetCC {
    ConditionKind cond;
    u32 dst;
} X64_InstrSetCC;

typedef struct X64_InstrCallArg {
    Type* type;
    bool in_reg;
    u32 loc;
    u32 sp_offset;
} X64_InstrCallArg;

typedef struct X64_InstrCall {
    Symbol* sym;
    u32 dst;
    u32 num_args;
    X64_InstrCallArg* args;
    X64_StackArgsInfo stack_info;
} X64_InstrCall;

typedef struct X64_InstrCall_R {
    Type* proc_type;
    u32 proc_loc;
    u32 dst;
    u32 num_args;
    X64_InstrCallArg* args;
    X64_StackArgsInfo stack_info;
} X64_InstrCall_R;

typedef struct X64_Instr {
    X64_InstrKind kind;
    bool is_jmp_target;

    union {
        X64_InstrBinary_R_R binary_r_r;
        X64_InstrBinary_R_I binary_r_i;

        X64_InstrShift_R_R shift_r_r;
        X64_InstrShift_R_I shift_r_i;

        X64_InstrDiv div;

        X64_InstrUnary unary;

        X64_InstrMov_R_R mov_r_r;
        X64_InstrMov_R_M mov_r_m;
        X64_InstrMov_R_I mov_r_i;
        X64_InstrMov_M_R mov_m_r;
        X64_InstrMov_M_I mov_m_i;

        X64_InstrConvert_R_R convert_r_r;

        X64_InstrSExtAxToDx sext_ax_to_dx;

        X64_InstrLEA lea;

        X64_InstrCmp_R_R cmp_r_r;
        X64_InstrCmp_R_I cmp_r_i;

        X64_InstrJmp jmp;

        X64_InstrJmpCC jmpcc;

        X64_InstrSetCC setcc;

        X64_InstrCall call;

        X64_InstrCall_R call_r;
    };

    ListNode lnode;
} X64_Instr;

typedef struct X64_LIRBuilder {
    Allocator* arena;

    // List of X64 LIR instructions.
    size_t num_instrs;
    List instrs;

    X64_LRegRange* lreg_ranges; // Stretchy buf
    u32* reg_map; // Map IR reg -> LIR reg; size: num_iregs

    u32 lreg_rbp;

    HMap jmp_map;
    bool next_instr_is_jmp_target;

    u32* call_sites; // lir call sites. Used for reg allocation

    // Disjoint Set Union data structure for register renaming/aliasing.
    u32* lreg_aliases; // Root alias node for each lir reg. size: num_lirregs
    u32* lreg_sizes;   // Size for each lir reg aliasing set. size: num_lirregs
} X64_LIRBuilder;

u32 X64_find_alias_reg(X64_LIRBuilder* builder, u32 r);

void X64_emit_instr_binary_r_r(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst, u32 src);
void X64_emit_instr_binary_r_i(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst, Scalar src);
void X64_emit_instr_shift_r_r(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst, u32 src);
void X64_emit_instr_shift_r_i(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst, Scalar src);
void X64_emit_instr_div(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 src);
void X64_emit_instr_unary(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst);
void X64_emit_instr_mov_r_r(X64_LIRBuilder* builder, size_t size, u32 dst, u32 src);
void X64_emit_instr_mov_r_m(X64_LIRBuilder* builder, size_t size, u32 dst, X64_MemAddr src);
void X64_emit_instr_mov_r_i(X64_LIRBuilder* builder, size_t size, u32 dst, Scalar src);
void X64_emit_instr_mov_m_r(X64_LIRBuilder* builder, size_t size, X64_MemAddr dst, u32 src);
void X64_emit_instr_convert_r_r(X64_LIRBuilder* builder, X64_InstrKind kind, size_t dst_size, u32 dst, size_t src_size, u32 src);
void X64_emit_instr_sext_ax_to_dx(X64_LIRBuilder* builder, size_t size);
void X64_emit_instr_lea(X64_LIRBuilder* builder, u32 dst, X64_MemAddr mem);
void X64_emit_instr_cmp_r_r(X64_LIRBuilder* builder, size_t size, u32 op1, u32 op2);
void X64_emit_instr_cmp_r_i(X64_LIRBuilder* builder, size_t size, u32 op1, Scalar op2);
void X64_emit_instr_jmp(X64_LIRBuilder* builder, u32 target);
void X64_emit_instr_jmpcc(X64_LIRBuilder* builder, ConditionKind cond, u32 target);
void X64_emit_instr_setcc(X64_LIRBuilder* builder, ConditionKind cond, u32 dst);
void X64_emit_instr_rep_movsb(X64_LIRBuilder* builder);
void X64_emit_instr_ret(X64_LIRBuilder* builder);
void X64_emit_instr_call(X64_LIRBuilder* builder, Symbol* sym, u32 dst, u32 num_args, X64_InstrCallArg* args,
                         X64_StackArgsInfo stack_info);
void X64_emit_instr_call_r(X64_LIRBuilder* builder, Type* proc_type, u32 proc_loc, u32 dst, u32 num_args, X64_InstrCallArg* args,
                           X64_StackArgsInfo stack_info);
#endif
