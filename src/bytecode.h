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
    IR_INSTR_CMP_ULT,
    IR_INSTR_CMP_SLT,
    IR_INSTR_CMP_ULE,
    IR_INSTR_CMP_SLE,
    IR_INSTR_CMP_EQ,
    IR_INSTR_CMP_UGE,
    IR_INSTR_CMP_SGE,
    IR_INSTR_CMP_UGT,
    IR_INSTR_CMP_SGT,
    IR_INSTR_CMP_NE,

    IR_INSTR_RET,
    IR_INSTR_ARG, // Set argument
    IR_INSTR_CALL, // Pushes a "Call Record" into a stack of call records.
} IR_InstrKind;

typedef enum IR_Type {
    IR_TYPE_VOID,
    IR_TYPE_INT8,
    IR_TYPE_INT16,
    IR_TYPE_INT32,
    IR_TYPE_INT64,
    IR_TYPE_F32,
    IR_TYPE_F64,
    IR_TYPE_PTR,
} IR_Type;

typedef enum IR_InstrArgKind {
    IR_ARG_REG,
    IR_ARG_IMM,
} IR_InstrArgKind;

typedef struct IR_InstrArg {
    IR_InstrArgKind kind;
    union {
        struct {
            IR_Reg reg0;
            IR_Reg reg1;
        };
        Scalar imm;
    };
} IR_InstrArg;

struct IR_Instr {
    u16 kind;

    // Optional values vary per instruction kind.
    union {
        u8 bytes[2];
        u16 val;
    } option;

    // Result register
    IR_Reg r;

    // First input (register/immediate).
    IR_InstrArg a;

    // Second input (register/immediate).
    IR_InstrArg b;
};

enum IR_VarFlag {
    IR_VAR_IS_LOCAL = 0x1,
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

struct IR_SIBDAddr {
    IR_Reg base_reg;
    IR_Reg index_reg;
    u64 disp;
    u8 scale;
};

IR_Module* IR_build_module(Allocator* arena, Allocator* tmp_arena, Scope* global_scope);

#endif
