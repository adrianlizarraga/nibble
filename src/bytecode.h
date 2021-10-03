#ifndef NIBBLE_IR_H
#define NIBBLE_IR_H
#include "stream.h"
#include "ast.h"

#define IR_STACK_ALIGN 16
#define IR_INSTRS_PER_BUCKET 64
#define IR_PROCS_PER_BUCKET 16
#define IR_REG_COUNT 0xFFFFFFFF

typedef u32 IR_Reg;

typedef enum IR_InstrKind {
    IR_INSTR_NONE = 0,

    // Addition
    IR_INSTR_ADD_R_R,
    IR_INSTR_ADD_R_M,
    IR_INSTR_ADD_R_I,

    // Subtraction
    IR_INSTR_SUB_R_R,
    IR_INSTR_SUB_R_M,
    IR_INSTR_SUB_R_I,

    // Multiplication
    IR_INSTR_MUL_R_R,
    IR_INSTR_MUL_R_M,
    IR_INSTR_MUL_R_I,

    // Unsigned division
    IR_INSTR_UDIV_R_R,
    IR_INSTR_UDIV_R_M,
    IR_INSTR_UDIV_R_I,

    // Signed division
    IR_INSTR_SDIV_R_R,
    IR_INSTR_SDIV_R_M,
    IR_INSTR_SDIV_R_I,

    // TODO: Unsigned/Signed MOD

    // Arithmetic shift right
    IR_INSTR_SAR_R_R,
    IR_INSTR_SAR_R_M,
    IR_INSTR_SAR_R_I,

    // Bitwise NOT
    IR_INSTR_NOT,

    // Two's complement negation.
    IR_INSTR_NEG,

    // Load immediate into register.
    IR_INSTR_LIMM,

    // Truncate
    IR_INSTR_TRUNC_R_R,
    IR_INSTR_TRUNC_R_M,

    // Zero-extend
    IR_INSTR_ZEXT_R_R,
    IR_INSTR_ZEXT_R_M,

    // Sign-extend
    IR_INSTR_SEXT_R_R,
    IR_INSTR_SEXT_R_M,

    // Load an address computation into a register.
    IR_INSTR_LADDR,

    // Load contents of memory into register.
    IR_INSTR_LOAD,

    // Store into memory.
    IR_INSTR_STORE_R,
    IR_INSTR_STORE_I,

    // Compare two values and set condition flags
    IR_INSTR_CMP_R_R,
    IR_INSTR_CMP_R_M,
    IR_INSTR_CMP_R_I,
    IR_INSTR_CMP_M_R,
    IR_INSTR_CMP_M_I,

    // Jump to instruction index
    IR_INSTR_JMP,

    // Jump to instruction index based on condition
    IR_INSTR_JMPCC,

    // Set a byte (0 or 1) based on condition
    IR_INSTR_SETCC,

    // Return value in specifed register
    IR_INSTR_RET,

    // Call a procedure directly
    IR_INSTR_CALL,

    // Call a procedure indirectly (register has procedure address)
    IR_INSTR_CALL_R,
} IR_InstrKind;

typedef enum IR_MemBaseKind {
    IR_MEM_BASE_NONE = 0,
    IR_MEM_BASE_REG,
    IR_MEM_BASE_SYM,
} IR_MemBaseKind;

typedef struct IR_MemAddr {
    IR_MemBaseKind base_kind;
    union {
        IR_Reg reg;
        Symbol* sym;
    } base;

    IR_Reg index_reg;
    u8 scale;
    u32 disp;
} IR_MemAddr;

typedef struct IR_InstrBinary_R_R {
    Type* type;
    IR_Reg dst;
    IR_Reg src;
} IR_InstrBinary_R_R;

typedef struct IR_InstrBinary_R_M {
    Type* type;
    IR_Reg dst;
    IR_MemAddr src;
} IR_InstrBinary_R_M;

typedef struct IR_InstrBinary_R_I {
    Type* type;
    IR_Reg dst;
    Scalar src;
} IR_InstrBinary_R_I;

typedef struct IR_InstrUnary {
    Type* type;
    IR_Reg dst;
} IR_InstrUnary;

typedef struct IR_InstrLImm {
    Type* type;
    IR_Reg dst;
    Scalar src;
} IR_InstrLImm;

typedef struct IR_InstrLoad {
    Type* type;
    IR_Reg dst;
    IR_MemAddr src;
} IR_InstrLoad;

typedef struct IR_InstrStore_R {
    Type* type;
    IR_MemAddr dst;
    IR_Reg src;
} IR_InstrStore_R;

typedef struct IR_InstrStore_I {
    Type* type;
    IR_MemAddr dst;
    Scalar src;
} IR_InstrStore_I;

typedef struct IR_InstrLAddr {
    IR_Reg dst;
    Type* type; // Type of the thing we would be loading. Not necessary??
    IR_MemAddr mem;
} IR_InstrLAddr;

typedef struct IR_InstrRet {
    Type* type;
    IR_Reg src;
} IR_InstrRet;

typedef struct IR_InstrConvert_R_R {
    Type* dst_type;
    Type* src_type;
    IR_Reg dst;
    IR_Reg src;
} IR_InstrConvert_R_R;

typedef struct IR_InstrConvert_R_M {
    Type* dst_type;
    Type* src_type;
    IR_Reg dst;
    IR_MemAddr src;
} IR_InstrConvert_R_M;

typedef struct IR_InstrCmp_R_R {
    Type* type;
    IR_Reg op1;
    IR_Reg op2;
} IR_InstrCmp_R_R;

typedef struct IR_InstrCmp_R_M {
    Type* type;
    IR_Reg op1;
    IR_MemAddr op2;
} IR_InstrCmp_R_M;

typedef struct IR_InstrCmp_R_I {
    Type* type;
    IR_Reg op1;
    Scalar op2;
} IR_InstrCmp_R_I;

typedef struct IR_InstrCmp_M_R {
    Type* type;
    IR_MemAddr op1;
    IR_Reg op2;
} IR_InstrCmp_M_R;

typedef struct IR_InstrCmp_M_I {
    Type* type;
    IR_MemAddr op1;
    Scalar op2;
} IR_InstrCmp_M_I;

typedef enum IR_ConditionKind {
    IR_COND_U_LT,
    IR_COND_S_LT,
    IR_COND_U_LTEQ,
    IR_COND_S_LTEQ,
    IR_COND_U_GT,
    IR_COND_S_GT,
    IR_COND_U_GTEQ,
    IR_COND_S_GTEQ,
    IR_COND_EQ,
    IR_COND_NEQ,
} IR_ConditionKind;

typedef struct IR_InstrJmpCC {
    u32 jmp_target;
    IR_ConditionKind cond;
} IR_InstrJmpCC;

typedef struct IR_InstrJmp {
    u32 jmp_target;
} IR_InstrJmp;

typedef struct IR_InstrSetCC {
    IR_ConditionKind cond;
    IR_Reg dst;
} IR_InstrSetCC;

typedef struct IR_InstrCallArg {
    Type* type;
    IR_Reg loc; // TODO: Support stack args
} IR_InstrCallArg;

typedef struct IR_InstrCall {
    Symbol* sym;
    IR_Reg dst;
    u32 num_args;
    IR_InstrCallArg* args;
} IR_InstrCall;

typedef struct IR_InstrCall_R {
    Type* proc_type;
    IR_Reg proc_loc;
    IR_Reg dst;
    u32 num_args;
    IR_InstrCallArg* args;
} IR_InstrCall_R;

typedef struct IR_Instr {
    IR_InstrKind kind;
    bool is_jmp_target;

    union {
        // Addition
        IR_InstrBinary_R_R add_r_r;
        IR_InstrBinary_R_M add_r_m;
        IR_InstrBinary_R_I add_r_i;

        // Subtraction
        IR_InstrBinary_R_R sub_r_r;
        IR_InstrBinary_R_M sub_r_m;
        IR_InstrBinary_R_I sub_r_i;

        // Multiplication
        IR_InstrBinary_R_R mul_r_r;
        IR_InstrBinary_R_M mul_r_m;
        IR_InstrBinary_R_I mul_r_i;

        // Division
        IR_InstrBinary_R_R div_r_r;
        IR_InstrBinary_R_M div_r_m;
        IR_InstrBinary_R_I div_r_i;

        // Arithmetic shift right
        IR_InstrBinary_R_R sar_r_r;
        IR_InstrBinary_R_M sar_r_m;
        IR_InstrBinary_R_I sar_r_i;

        // Two's complement negation
        IR_InstrUnary neg;

        // Bitwise NOT
        IR_InstrUnary not ;

        // Load immediate into register
        IR_InstrLImm limm;

        // Load an address computation into register
        IR_InstrLAddr laddr;

        // Truncate integer
        IR_InstrConvert_R_R trunc_r_r;
        IR_InstrConvert_R_M trunc_r_m;

        // Zero-extend integer
        IR_InstrConvert_R_R zext_r_r;
        IR_InstrConvert_R_M zext_r_m;

        // Sign-extend integer
        IR_InstrConvert_R_R sext_r_r;
        IR_InstrConvert_R_M sext_r_m;

        // Load contents of memory into register
        IR_InstrLoad load;

        // Store register contents into memory
        IR_InstrStore_R store_r;

        // Store immediate into memory
        IR_InstrStore_I store_i;

        // Compare two values and set condition flags
        IR_InstrCmp_R_R cmp_r_r;
        IR_InstrCmp_R_M cmp_r_m;
        IR_InstrCmp_R_I cmp_r_i;
        IR_InstrCmp_M_R cmp_m_r;
        IR_InstrCmp_M_I cmp_m_i;

        // Jump to instruction index
        IR_InstrJmp jmp;

        // Jump to instruction index based on condition
        IR_InstrJmpCC jmpcc;

        // Set a byte (1 or 0) based on condition
        IR_InstrSetCC setcc;

        // Return value in specified register
        IR_InstrRet ret;

        // Call a procedure directly
        IR_InstrCall call;

        // Call a procedure indirectly (register contains procedure address)
        IR_InstrCall_R call_r;
    };
} IR_Instr;

typedef struct IR_Module {
    u32 num_vars;
    u32 num_procs;
    Symbol** vars;
    Symbol** procs;
} IR_Module;

IR_Module* IR_build_module(Allocator* arena, Allocator* tmp_arena, Scope* global_scope, TypeCache* type_cache);

#endif
