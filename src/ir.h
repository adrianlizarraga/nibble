#ifndef NIBBLE_IR_NEW_H
#define NIBBLE_IR_NEW_H

#include "ast.h"

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
        int reg;
        Symbol* sym;
        StrLit* str_lit;
    } base;

    int index_reg;
    u8 scale;
    u32 disp;
} MemAddr;

typedef struct InstrBinary {
    Type* type;
    int r;
    int a;
    int b;
} InstrIBinary;

typedef struct InstrUnary {
    Type* type;
    int r;
    int a;
} InstrIUnary;

typedef struct InstrLImm {
    u8 size;
    int r;
    Scalar imm;
} InstrLImm;

typedef struct InstrLoad {
    Type* type;
    int r;
    MemAddr addr;
} InstrLoad;

typedef struct InstrStore {
    Type* type;
    MemAddr addr;
    int a;
} InstrStore;

typedef struct InstrRet {
    Type* type;
    int a;
} InstrRet;

typedef struct InstrConvert {
    Type* dst_type;
    Type* src_type;
    int r;
    int a;
} InstrConvert;

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
    int r;
    int a;
    int b;
} InstrCmp;

typedef struct InstrJmp {
    u32* jmp_target;
} InstrJmp;

typedef struct InstrCallArg {
    Type* type;
    int loc; // TODO: Support stack args
} InstrCallArg;

typedef struct InstrCall {
    Symbol* sym;
    int r;
    u32 num_args;
    InstrCallArg* args;
} InstrCall;

typedef struct InstrCallIndirect {
    Type* proc_type;
    int loc;
    int r;
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
} Instr;

#endif
