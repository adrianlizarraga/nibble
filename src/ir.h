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


#endif
