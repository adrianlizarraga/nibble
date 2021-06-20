#ifndef NIBBLE_IR_H
#define NIBBLE_IR_H
#include "ast.h"
#include "stream.h"

#define IR_INSTRS_PER_BUCKET 64
#define IR_PROCS_PER_BUCKET 16

typedef struct IR_Instr IR_Instr;
typedef struct IR_Operand IR_Operand;
typedef struct IR_Builder IR_Builder;

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
    IR_OPCODE_R2R,   // Copy register

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
    IR_OPERAND_VAR,
    IR_OPERAND_PROC,
    IR_OPERAND_LABEL,
} IR_OperandKind;

#define IR_REG_ID_MASK 0x00FFFFFFFFFFFFFF
#define IR_REG_BYTE_SIZE(r) (((r) >> 56) & 0xFF)
#define IR_REG_ID(r) ((r) & IR_REG_ID_MASK)

enum IR_OperandFlag {
    IR_OPERAND_IS_L_VALUE = 0x1,
};

struct IR_Operand {
    IR_OperandKind kind;
    u32 flags;

    union {
        struct {
            Type* type;
            Scalar value;
        } _imm;
        struct {
            Type* type;
            u64 reg;   // Top 8 bits for reg size. Bottom 7 bytes for reg id
        } _reg;
        Symbol* _var;
        Symbol* _proc;
        u64 _label;
    };
};

struct IR_Instr {
    IR_OpCode opcode;
    IR_Operand operand_s;
    IR_Operand operand_d;
};

#define IR_NUM_ARG_REGS 6
#define IR_NUM_TMP_REGS 6
#define IR_NUM_RET_REGS 2

typedef enum IR_RegID {
    IR_REG0 = 0,
    IR_REG1,
    IR_REG2,
    IR_REG3,
    IR_REG4,
    IR_REG5,
    IR_ARG_REG0,
    IR_ARG_REG1,
    IR_ARG_REG2,
    IR_ARG_REG3,
    IR_ARG_REG4,
    IR_ARG_REG5,
    IR_RET_REG0,
    IR_RET_REG1,
    IR_REG_COUNT,
    IR_REG_INVALID = IR_REG_COUNT
} IR_RegID;

extern const char* IR_reg_names[IR_REG_COUNT];
extern IR_RegID IR_tmp_regs[IR_NUM_TMP_REGS];
extern IR_RegID IR_arg_regs[IR_NUM_ARG_REGS];
extern IR_RegID IR_ret_regs[IR_NUM_RET_REGS];

struct IR_Builder {
    BucketList* instrs;
    u32 free_regs;
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
