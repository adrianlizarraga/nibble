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
        Reg reg;
        Symbol* sym;
        StrLit* str_lit;
    } base;

    Reg index_reg;
    u8 scale;
    u32 disp;
} MemAddr;

typedef struct InstrIBinary {
    u8 size;
    Reg r;
    Reg a;
    Reg b;
} InstrIBinary;

typedef struct InstrIUnary {
    u8 size;
    Reg r;
    Reg a;
} InstrIUnary;

typedef struct InstrLImm {
    u8 size;
    Reg r;
    Scalar imm;
} InstrLImm;

typedef struct InstrLoad {
    u8 size;
    Reg r;
    MemAddr addr;
} InstrLoad;

typedef struct InstrStore {
    u8 size;
    MemAddr addr;
    Reg a;
} InstrStore;

#endif
