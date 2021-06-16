#ifndef NIBBLE_IR_H
#define NIBBLE_IR_H
#include "ast.h"
#include "stream.h"

#define IR_INSTRS_PER_BUCKET 64
#define IR_VARS_PER_BUCKET 32
#define IR_PROCS_PER_BUCKET 16

typedef struct IR_Program IR_Program;
typedef struct IR_Proc IR_Proc;
typedef struct IR_Var IR_Var;
typedef struct IR_Instr IR_Instr;
typedef struct IR_Operand IR_Operand;

typedef enum IR_OpCode {
    IR_OPCODE_NONE = 0,
    IR_OPCODE_ADD,
    IR_OPCODE_SUB,
    IR_OPCODE_MULT,
    IR_OPCODE_DIV,

    IR_OPCODE_LSHIFT,
    IR_OPCODE_RSHIFT,

    IR_OPCODE_AND,
    IR_OPCODE_OR,
    IR_OPCODE_XOR,

    IR_OPCODE_LOAD,  // Load var (when used in expr)
    IR_OPCODE_LOADI, // Load imm to register

    IR_OPCODE_STORE, // Store var (when assigned)

    IR_OPCODE_JMP,  // Jump to label
    IR_OPCODE_JMPR, // Jump to register

    // Comparison style that sets a global flag
    // One comparison instruction, and multiple conditional jump instructions
    // that read a flag.
    IR_OPCODE_CMP,
    IR_OPCODE_CJMP_LT,
    IR_OPCODE_CJMP_LE,
    IR_OPCODE_CJMP_EQ,
    IR_OPCODE_CJMP_GE,
    IR_OPCODE_CJMP_GT,
    IR_OPCODE_CJMP_NE,

    // Comparison style (no flags) that uses registers.
    // Multiple comparison instructions that set a result register to true/false.
    // One conditional jump instruction that takes an input register and two labels.
    IR_OPCODE_CJMP,
    IR_OPCODE_CMP_LT,
    IR_OPCODE_CMP_LE,
    IR_OPCODE_CMP_EQ,
    IR_OPCODE_CMP_GE,
    IR_OPCODE_CMP_GT,
    IR_OPCODE_CMP_NE,

    IR_OPCODE_CALL,
    IR_OPCODE_CALLR,
} IR_OpCode;

typedef enum IR_OperandKind {
    IR_OPERAND_NONE,
    IR_OPERAND_IMM,
    IR_OPERAND_REG,
    IR_OPERAND_LOCAL_VAR,
    IR_OPERAND_GLOBAL_VAR,
    IR_OPERAND_PROC,
    IR_OPERAND_LABEL,
} IR_OperandKind;

#define IR_REG_BYTE_SIZE(r) (((r) >> 56) & 0xFF)
#define IR_REG_ID(r) ((r) & 0x00FFFFFFFFFFFFFF)

struct IR_Operand {
    IR_OperandKind kind;

    union {
        struct {
            Scalar value;
            Type* type;
        } imm;
        u64 reg;        // Top 8 bits for reg size. Bottom 7 bytes for reg id
        u64 local_var;  // Index into procedure's local_vars array
        u64 global_var; // Index into program's global_var array.
        u64 proc;       // Index into program's procedures array.
        u64 label;      // Index into procedure's instructions array
    };
};

struct IR_Instr {
    IR_OpCode opcode;
    IR_Operand operand_r;
    IR_Operand operand_a;
    IR_Operand operand_b;
};

struct IR_Var {
    Type* type;
    DeclVar* decl;
    s64 offset;
};

struct IR_Proc {
    BucketList local_vars; // NOTE: BucketList of IR_Var* elems.
    BucketList instrs;     // NOTE: BucketList of IR_Instr* elems.

    Type* type;
    DeclProc* decl;
    Allocator* arena;
};

struct IR_Program {
    BucketList global_vars; // NOTE: BucketList of IR_Var* elems.
    BucketList procs;       // NOTE: BucketList of IR_Proc* elems
};

IR_Instr* new_ir_instr(Allocator* arena, IR_OpCode opcode, IR_Operand op_r, IR_Operand op_a, IR_Operand op_b);
IR_Instr** get_bucket_instr(BucketList* bucket_list, size_t index);
IR_Instr** add_bucket_instr(BucketList* bucket_list, Allocator* arena, IR_Instr* instr);

IR_Proc* new_ir_proc(Allocator* arena, DeclProc* decl, Type* type);
IR_Proc** get_bucket_proc(BucketList* bucket_list, size_t index);
IR_Proc** add_bucket_proc(BucketList* bucket_list, Allocator* arena, IR_Proc* proc);

IR_Var* new_ir_var(Allocator* arena, DeclVar* decl, Type* type, s64 offset);
IR_Var** get_bucket_var(BucketList* bucket_list, size_t index);
IR_Var** add_bucket_var(BucketList* bucket_list, Allocator* arena, IR_Var* var);

#endif
