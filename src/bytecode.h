#ifndef NIBBLE_IR_H
#define NIBBLE_IR_H
#include "stream.h"
#include "ast.h"

#define IR_STACK_ALIGN 16
#define IR_INSTRS_PER_BUCKET 64
#define IR_PROCS_PER_BUCKET 16

typedef struct IR_SIBDAddr IR_SIBDAddr;
typedef struct IR_InstrArg IR_InstrArg;
typedef struct IR_Instr IR_Instr;
typedef struct IR_Module IR_Module;
typedef struct IR_Builder IR_Builder;

typedef struct IR_InstrBinary IR_InstrBinary;
typedef struct IR_InstrUnary IR_InstrUnary;
typedef struct IR_InstrStore IR_InstrStore;
typedef struct IR_InstrLoad IR_InstrLoad;
typedef struct IR_InstrLAddr IR_InstrLAddr;
typedef struct IR_InstrLAddrVar IR_InstrLAddrVar;
typedef struct IR_InstrRet IR_InstrRet;
typedef struct IR_InstrCmp IR_InstrCmp;
typedef struct IR_InstrCJmp IR_InstrCJmp;
typedef struct IR_InstrJmp IR_InstrJmp;

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
    IR_INSTR_NOT,

    IR_INSTR_LADDR,     // Load an address computation into a register.
    IR_INSTR_LADDR_VAR, // Load a variable's address into a register.
    IR_INSTR_LIMM,

    // Memory instructions
    IR_INSTR_LOAD,
    IR_INSTR_STORE,

    // Conversion instructions
    IR_INSTR_TRUNC,
    IR_INSTR_ZEXT,
    IR_INSTR_SEXT,
    IR_INSTR_PTR_TO_INT,
    IR_INSTR_INT_TO_PTR,
    IR_INSTR_BITCAST,

    IR_INSTR_JMP,  // Jump to label

    // Comparison style (no flags) that uses registers.
    // Multiple comparison instructions that set a result register to true/false.
    // One conditional jump instruction that takes an input register and two labels.
    IR_INSTR_CJMP,
    IR_INSTR_CMP,

    IR_INSTR_RET,
    IR_INSTR_ARG, // Set argument
    IR_INSTR_CALL, // Pushes a "Call Record" into a stack of call records.
} IR_InstrKind;

typedef enum IR_InstrArgKind {
    IR_ARG_NONE,
    IR_ARG_REG,
    IR_ARG_IMM,
} IR_InstrArgKind;

typedef struct IR_InstrArg {
    IR_InstrArgKind kind;
    union {
        IR_Reg reg;
        Scalar imm;
    };
} IR_InstrArg;

struct IR_SIBDAddr {
    IR_Reg base_reg;
    IR_Reg index_reg;
    u64 disp;
    u8 scale;
};

struct IR_InstrBinary {
    Type* type;
    IR_Reg out_reg;
    IR_InstrArg arg1;
    IR_InstrArg arg2;
};

struct IR_InstrUnary {
    Type* type;
    IR_Reg out_reg;
    IR_InstrArg arg;
};

struct IR_InstrStore {
    Type* type;
    IR_SIBDAddr addr;
    IR_InstrArg arg;
};

struct IR_InstrLoad {
    Type* type;
    IR_Reg out_reg;
    IR_SIBDAddr addr;
};

struct IR_InstrLAddr {
    Type* type;
    IR_Reg out_reg;
    IR_SIBDAddr addr;
};

struct IR_InstrLAddrVar {
    IR_Reg out_reg;
    Symbol* sym;
};

struct IR_InstrRet {
    Type* type;
    IR_InstrArg ret_arg;
};

typedef enum IR_ConditionKind {
    IR_COND_U_LT,
    IR_COND_S_LT,
    IR_COND_U_LTEQ,
    IR_COND_S_LTEQ,
    IR_COND_EQ,
    IR_COND_NEQ,
} IR_ConditionKind;

struct IR_InstrCmp {
    IR_ConditionKind kind;
    Type* type;
    IR_Reg out_reg;
    IR_InstrArg arg1;
    IR_InstrArg arg2;
};

struct IR_InstrCJmp {
    u32 jmp_target;
    IR_Reg cond_reg;
};

struct IR_InstrJmp {
    u32 jmp_target;
};

struct IR_Instr {
    IR_InstrKind kind;

    union {
        IR_InstrBinary _add;
        IR_InstrBinary _sub;
        IR_InstrBinary _shr;
        IR_InstrStore _store;
        IR_InstrLoad _load;
        IR_InstrLAddr _laddr;
        IR_InstrLAddrVar _laddr_var;
        IR_InstrRet _ret;
        IR_InstrCmp _cmp;
        IR_InstrCJmp _cjmp;
        IR_InstrJmp _jmp;
    };
};

struct IR_Module {
    u32 num_vars;
    u32 num_procs;
    Symbol** vars;
    Symbol** procs;
};

struct IR_Builder {
    Allocator* arena;
    Allocator* tmp_arena;
    IR_Module* module;
    Symbol* curr_proc;
    Scope* curr_scope;
};

IR_Module* IR_build_module(Allocator* arena, Allocator* tmp_arena, Scope* global_scope);

#endif
