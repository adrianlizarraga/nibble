#ifndef NIBBLE_IR_NEW_H
#define NIBBLE_IR_NEW_H

#include "ast.h"

#define NIR_STACK_ALIGN 16
#define NIR_INSTRS_PER_BUCKET 64
#define NIR_PROCS_PER_BUCKET 16
#define NIR_REG_COUNT 0xFFFFFFFF

typedef u32 NIR_Reg;

typedef enum InstrKind {
    INSTR_NONE = 0,
    INSTR_ADD,
    INSTR_SUB,
    INSTR_MUL,
    INSTR_UDIV,
    INSTR_SDIV,
    INSTR_SAR,
    INSTR_SHL,
    INSTR_AND,
    INSTR_OR,
    INSTR_XOR,
    INSTR_NOT,
    INSTR_NEG,
    INSTR_LIMM,
    INSTR_TRUNC,
    INSTR_ZEXT,
    INSTR_SEXT,
    INSTR_LOAD,
    INSTR_STORE,
    INSTR_CMPCC,
    INSTR_JMP,
    INSTR_RET,
    INSTR_CALL,
    INSTR_MEMCPY,
} InstrKind;

typedef enum MemBaseKind {
    MEM_BASE_NONE = 0,
    MEM_BASE_REG,
    MEM_BASE_SYM,
    MEM_BASE_STR_LIT,
} MemBaseKind;

typedef struct MemAddr {
    MemBaseKind base_kind;

    union {
        NIR_Reg reg;
        Symbol* sym;
        StrLit* str_lit;
    } base;

    NIR_Reg index_reg;
    u8 scale;
    u32 disp;
} MemAddr;

typedef struct InstrBinary {
    Type* type;
    NIR_Reg r;
    NIR_Reg a;
    NIR_Reg b;
} InstrBinary;

typedef struct InstrUnary {
    Type* type;
    NIR_Reg r;
    NIR_Reg a;
} InstrUnary;

typedef struct InstrConvert {
    Type* dst_type;
    Type* src_type;
    NIR_Reg r;
    NIR_Reg a;
} InstrConvert;

typedef struct InstrLImm {
    u8 size;
    NIR_Reg r;
    Scalar imm;
} InstrLImm;

typedef struct InstrLAddr {
    Type* type;
    NIR_Reg r;
    MemAddr addr;
} InstrLAddr;

typedef struct InstrLoad {
    Type* type;
    NIR_Reg r;
    MemAddr addr;
} InstrLoad;

typedef struct InstrStore {
    Type* type;
    MemAddr addr;
    NIR_Reg a;
} InstrStore;

typedef struct InstrRet {
    Type* type;
    NIR_Reg a;
} InstrRet;

typedef enum ConditionKind {
    COND_U_LT,
    COND_S_LT,
    COND_U_LTEQ,
    COND_S_LTEQ,
    COND_U_GT,
    COND_S_GT,
    COND_U_GTEQ,
    COND_S_GTEQ,
    COND_EQ,
    COND_NEQ,
} ConditionKind;

typedef struct InstrCmp {
    Type* type;
    ConditionKind cond;
    NIR_Reg r;
    NIR_Reg a;
    NIR_Reg b;
} InstrCmp;

typedef struct InstrJmp {
    u32* jmp_target;
    NIR_Reg a;
} InstrJmp;

typedef struct InstrCallArg {
    Type* type;
    NIR_Reg loc;
} InstrCallArg;

typedef struct InstrCall {
    Symbol* sym;
    NIR_Reg r;
    u32 num_args;
    InstrCallArg* args;
} InstrCall;

typedef struct InstrCallIndirect {
    Type* proc_type;
    NIR_Reg loc;
    NIR_Reg r;
    u32 num_args;
    InstrCallArg* args;
} InstrCallIndirect;

typedef struct InstrMemcpy {
    Type* type;
    MemAddr dst;
    MemAddr src;
} InstrMemcpy;

typedef struct Instr {
    InstrKind kind;
    bool is_jmp_target;

    union {
        InstrBinary binary;
        InstrUnary unary;
        InstrConvert convert;
        InstrLImm limm;
        InstrLAddr laddr;
        InstrLoad load;
        InstrStore store;
        InstrRet ret;
        InstrCmp cmp;
        InstrJmp jmp;
        InstrCall call;
        InstrCallIndirect calli;
        InstrMemcpy memcpy;
    } u;
} Instr;

#endif
