#ifndef NIBBLE_IR_H
#define NIBBLE_IR_H
#include "ast.h"
#include "stream.h"

#define IR_INSTRS_PER_BUCKET 64
#define IR_PROCS_PER_BUCKET 16

typedef struct IR_InstrArg IR_InstrArg;
typedef struct IR_Instr IR_Instr;
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

    IR_INSTR_LADDR,
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
    IR_INSTR_CALL,
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
            s32 reg0;
            s32 reg1;
        };
        Scalar imm;
    };
} IR_InstrArg;

struct IR_Instr {
    s16 kind;
    s16 option; // Can be a type (add), a scale (lea), or arg index
    s32 r;      // Typically the result register
    IR_InstrArg a;
    IR_InstrArg b;
};

struct IR_Builder {
    BucketList* instrs;
    Allocator* arena;
};

IR_Instr* IR_new_instr(Allocator* arena, IR_OpCode opcode, IR_Operand op_s, IR_Operand op_d);
IR_Instr** IR_get_bucket_instr(BucketList* bucket_list, size_t index);
IR_Instr** IR_add_bucket_instr(BucketList* bucket_list, Allocator* arena, IR_Instr* instr);

u32 IR_init_free_regs(IR_Builder* builder);
IR_Instr* IR_push_instr(IR_Builder* builder, IR_Opcode opcode, IR_Operand* src_op, IR_Operand* dst_op);

void IR_emit_add(IR_Builder* builder, IR_Operand* src_op, IR_Operand* dst_op);
void IR_emit_sub(IR_Builder* builder, IR_Operand* src_op, IR_Operand* dst_op);
void IR_free_op(IR_Operand* op);

void IR_ensure_op_in_reg(IR_Builder* builder, IR_Operand* op);
void IR_emit_op_to_reg(IR_Operand* src_op, IR_RegID reg, IR_Operand* dst_op);
IR_RegID IR_next_reg(IR_Builder* builder);
void IR_free_reg(IR_Builder* builder, IR_RegID reg);
void IR_alloc_reg(IR_Builder* builder, IR_RegID reg);
bool IR_try_alloc_reg(IR_Builder* builder, IR_RegID reg);
#endif
