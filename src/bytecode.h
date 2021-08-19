#ifndef NIBBLE_IR_H
#define NIBBLE_IR_H
#include "stream.h"
#include "ast.h"

#define IR_STACK_ALIGN 16
#define IR_INSTRS_PER_BUCKET 64
#define IR_PROCS_PER_BUCKET 16
#define IR_REG_COUNT 32

typedef struct IR_Instr IR_Instr;
typedef struct IR_Module IR_Module;

typedef struct IR_SIBDAddr IR_SIBDAddr;
typedef struct IR_MemAddr IR_MemAddr;
typedef struct IR_OpRMI IR_OpRMI;
typedef struct IR_OpRM IR_OpRM;
typedef struct IR_OpRI IR_OpRI;

typedef struct IR_InstrBinary IR_InstrBinary;
typedef struct IR_InstrUnary IR_InstrUnary;
typedef struct IR_InstrMov IR_InstrMov;
typedef struct IR_InstrLoad IR_InstrLoad;
typedef struct IR_InstrStore IR_InstrStore;
typedef struct IR_InstrLAddr IR_InstrLAddr;
typedef struct IR_InstrConvert IR_InstrConvert;
typedef struct IR_InstrRet IR_InstrRet;
typedef struct IR_InstrCmp IR_InstrCmp;
typedef struct IR_InstrJmpCC IR_InstrJmpCC;
typedef struct IR_InstrJmp IR_InstrJmp;
typedef struct IR_InstrSetCC IR_InstrSetCC;

typedef u8 IR_Reg;

typedef enum IR_InstrKind {
    IR_INSTR_NONE = 0,

    // Binary instructions.
    IR_INSTR_ADD,
    IR_INSTR_SUB,
    IR_INSTR_MULT,
    IR_INSTR_UDIV,
    IR_INSTR_SDIV,
    IR_INSTR_UMOD,
    IR_INSTR_SMOD,

    // Binary bitwise instructions.
    IR_INSTR_SHL,
    IR_INSTR_SHR,
    IR_INSTR_SAR,
    IR_INSTR_AND,
    IR_INSTR_OR,
    IR_INSTR_XOR,

    // Unary instructions.
    IR_INSTR_NEG,
    IR_INSTR_NOT,

    IR_INSTR_LADDR, // Load an address computation into a register.
    IR_INSTR_MOV,   // Copy a reg/imm to a reg
    IR_INSTR_LOAD,  // Load a contents of memory into register.
    IR_INSTR_STORE, // Store a reg/imm to a memory location.

    // Conversion instructions
    IR_INSTR_TRUNC,
    IR_INSTR_ZEXT,
    IR_INSTR_SEXT,

    IR_INSTR_JMP,   // Jump to label
    IR_INSTR_JMPCC, // Jump to label based on condition
    IR_INSTR_CMP,   // Compare two values and set condition flags
    IR_INSTR_SETCC, // Set a byte on condition

    IR_INSTR_RET,
    IR_INSTR_ARG, // Set argument
    IR_INSTR_CALL, // Pushes a "Call Record" into a stack of call records.
} IR_InstrKind;

struct IR_SIBDAddr {
    IR_Reg base_reg;
    IR_Reg index_reg;
    u8 scale;
    u32 disp;
};

typedef enum IR_MemAddrKind {
    IR_MEM_ADDR_SIBD,
    IR_MEM_ADDR_SYM,
} IR_MemAddrKind;

struct IR_MemAddr {
    IR_MemAddrKind kind;

    union {
        IR_SIBDAddr sibd;
        Symbol* sym;
    };
};

typedef enum IR_OpKind {
    IR_OP_REG,
    IR_OP_IMM,
    IR_OP_MEM,
} IR_OpKind;

struct IR_OpRMI {
    IR_OpKind kind;
    union {
        IR_Reg reg;
        Scalar imm;
        IR_MemAddr mem;
    };
};

struct IR_OpRM {
    IR_OpKind kind;
    union {
        IR_Reg reg;
        IR_MemAddr mem;
    };
};

struct IR_OpRI {
    IR_OpKind kind;
    union {
        IR_Reg reg;
        Scalar imm;
    };
};

struct IR_InstrBinary {
    Type* type;
    IR_OpRM dst;
    IR_OpRMI src;
};

struct IR_InstrUnary {
    Type* type;
    IR_OpRM dst;
};

struct IR_InstrMov {
    Type* type;
    IR_Reg dst;
    IR_OpRI src;
};

struct IR_InstrLoad {
    Type* type;
    IR_Reg dst;
    IR_MemAddr src;
};

struct IR_InstrStore {
    Type* type;
    IR_MemAddr dst;
    IR_OpRI src;
};

struct IR_InstrLAddr {
    IR_Reg dst;
    IR_MemAddr mem;
};

struct IR_InstrRet {
    Type* type;
    IR_Reg src;
};

struct IR_InstrConvert {
    Type* dst_type;
    Type* src_type;
    IR_Reg dst;
    IR_OpRM src;
};

struct IR_InstrCmp {
    Type* type;
    IR_OpRM op1;
    IR_OpRMI op2;
};

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

struct IR_InstrJmpCC {
    u32 jmp_target;
    IR_ConditionKind cond;
};

struct IR_InstrJmp {
    u32 jmp_target;
};

struct IR_InstrSetCC {
    IR_ConditionKind cond;
    IR_OpRM dst;
};

struct IR_Instr {
    IR_InstrKind kind;

    union {
        IR_InstrBinary _add;
        IR_InstrBinary _sub;
        IR_InstrBinary _shr;
        IR_InstrUnary _neg;
        IR_InstrUnary _not;
        IR_InstrMov _mov;
        IR_InstrLoad _load;
        IR_InstrStore _store;
        IR_InstrLAddr _laddr;
        IR_InstrRet _ret;
        IR_InstrCmp _cmp;
        IR_InstrJmpCC _jmpcc;
        IR_InstrJmp _jmp;
        IR_InstrSetCC _setcc;
        IR_InstrConvert _trunc;
        IR_InstrConvert _zext;
        IR_InstrConvert _sext;
    };
};

struct IR_Module {
    u32 num_vars;
    u32 num_procs;
    Symbol** vars;
    Symbol** procs;
};

IR_Module* IR_build_module(Allocator* arena, Allocator* tmp_arena, Scope* global_scope);

#endif
