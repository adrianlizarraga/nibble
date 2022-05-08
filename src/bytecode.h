#ifndef NIBBLE_IR_NEW_H
#define NIBBLE_IR_NEW_H

#include "ast.h"

#define IR_STACK_ALIGN 16
#define IR_INSTRS_PER_BUCKET 64
#define IR_PROCS_PER_BUCKET 16
#define IR_REG_COUNT 0xFFFFFFFF

typedef u32 IR_Reg;
typedef struct OpRI OpRI;
typedef struct OpRA OpRA;
typedef struct OpRIA OpRIA;
typedef struct MemObj MemObj;
typedef struct MemAddr MemAddr;
typedef struct Instr Instr;

// Instruction operand that is either a register (R) or an immediate (I).
struct OpRI {
    bool is_imm;

    union {
        IR_Reg reg;
        Scalar imm;
    };
};

typedef enum InstrKind {
    INSTR_NONE = 0,
    INSTR_ADD,
    INSTR_SUB,
    INSTR_MUL,
    INSTR_DIV,
    INSTR_MOD,
    INSTR_DIVMOD,
    INSTR_SAR,
    INSTR_SHL,
    INSTR_AND,
    INSTR_OR,
    INSTR_XOR,
    INSTR_NOT,
    INSTR_NEG,
    INSTR_TRUNC,
    INSTR_ZEXT,
    INSTR_SEXT,
    INSTR_FP2INT,
    INSTR_INT2FP,
    INSTR_FP2FP,
    INSTR_LIMM,
    INSTR_LOAD,
    INSTR_LADDR,
    INSTR_STORE,
    INSTR_CMP,
    INSTR_JMP,
    INSTR_COND_JMP,
    INSTR_RET,
    INSTR_CALL,
    INSTR_CALL_INDIRECT,
    INSTR_MEMCPY,
    INSTR_MEMSET,
    INSTR_PHI,
    INSTR_KIND_COUNT
} InstrKind;

typedef enum MemBaseKind {
    MEM_BASE_NONE = 0,
    MEM_BASE_REG,
    MEM_BASE_MEM_OBJ,
    MEM_BASE_STR_LIT,
    MEM_BASE_FLOAT_LIT,
} MemBaseKind;

struct MemAddr {
    MemBaseKind base_kind;

    union {
        IR_Reg reg;
        MemObj* obj;
        StrLit* str_lit;
        FloatLit* float_lit;
    } base;

    IR_Reg index_reg;
    u8 scale;
    u32 disp;
};

// Instruction operand that is either a register (R) or an address (A).
struct OpRA {
    bool is_addr;

    union {
        IR_Reg reg;
        MemAddr addr;
    };
};

typedef enum OpRIAKind {
    OP_RIA_NONE = 0,
    OP_RIA_REG,
    OP_RIA_IMM,
    OP_RIA_ADDR
} OpRIAKind;

// Instruction operand that is either a register (R), an immediate (I), or an address (A).
struct OpRIA {
    OpRIAKind kind;

    union {
        IR_Reg reg;
        Scalar imm;
        MemAddr addr;
    };
};

typedef enum MemObjKind {
    MEM_OBJ_NONE = 0,
    MEM_OBJ_ANON_OBJ,
    MEM_OBJ_SYM,
    MEM_OBJ_ADDR,
    MEM_OBJ_ALIAS,
} MemObjKind;

struct MemObj {
    MemObjKind kind;

    union {
        AnonObj* anon_obj;
        Symbol* sym;
        MemObj* alias;
        MemAddr addr;
    };
};

typedef struct InstrBinary {
    Type* type;
    IR_Reg r;
    OpRIA a;
    OpRIA b;
} InstrBinary;

typedef struct InstrDivmod {
    Type* type;
    IR_Reg q; // quotient
    IR_Reg r; // remainder
    OpRIA a; // dividend
    OpRIA b; // divisor
} InstrDivmod;

typedef struct InstrShift {
    Type* type;
    IR_Reg r;
    OpRIA a;
    OpRIA b;
} InstrShift;

typedef struct InstrUnary {
    Type* type;
    IR_Reg r;
    IR_Reg a;
} InstrUnary;

typedef struct InstrConvert {
    Type* dst_type;
    Type* src_type;
    IR_Reg r;
    IR_Reg a;
} InstrConvert;

typedef struct InstrFpCast {
    u16 dst_size;
    u16 src_size;
    IR_Reg r;
    OpRA a;
} InstrFpCast;

typedef struct InstrLImm {
    Type* type;
    IR_Reg r;
    Scalar imm;
} InstrLImm;

typedef struct InstrLoad {
    Type* type;
    IR_Reg r;
    MemAddr addr;
} InstrLoad;

typedef struct InstrLAddr {
    Type* type;
    IR_Reg r;
    MemAddr addr;
} InstrLAddr;

typedef struct InstrStore {
    Type* type;
    MemAddr addr;
    OpRI a;
} InstrStore;

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
    IR_Reg r;
    OpRIA a;
    OpRIA b;
} InstrCmp;

typedef struct InstrJmp {
    BBlock* from;
    BBlock* target;
} InstrJmp;

typedef struct InstrCondJmp {
    BBlock* from;
    BBlock* true_bb;
    BBlock* false_bb;
    IR_Reg a;
} InstrCondJmp;

typedef struct IR_Value {
    Type* type;

    union {
        IR_Reg reg;
        MemAddr addr;
    };
} IR_Value;

typedef struct InstrCall {
    Symbol* sym;
    IR_Value r;
    u32 num_args;
    IR_Value* args;
} InstrCall;

typedef struct InstrCallIndirect {
    Type* proc_type;
    IR_Reg loc;
    IR_Value r;
    u32 num_args;
    IR_Value* args;
} InstrCallIndirect;

typedef struct InstrRet {
    IR_Value val;
} InstrRet;

typedef struct InstrMemcpy {
    MemAddr dst;
    MemAddr src;
    OpRI size;
} InstrMemcpy;

typedef struct InstrMemset {
    MemAddr dst;
    OpRI value;
    OpRI size;
} InstrMemset;

typedef struct PhiArg {
    BBlock* bblock;
    IR_Reg ireg;
} PhiArg;

typedef struct InstrPhi {
    Type* type;
    IR_Reg r;
    size_t num_args;
    PhiArg* args;
} InstrPhi;

struct Instr {
    InstrKind kind;
    long ino; // Instruction number
    bool is_leader;

    union {
        InstrBinary binary;
        InstrDivmod divmod;
        InstrShift shift;
        InstrUnary unary;
        InstrConvert convert;
        InstrFpCast fp_cast;
        InstrLImm limm;
        InstrLoad load;
        InstrStore store;
        InstrLAddr laddr;
        InstrCmp cmp;
        InstrJmp jmp;
        InstrCondJmp cond_jmp;
        InstrCall call;
        InstrCallIndirect calli;
        InstrRet ret;
        InstrMemcpy memcpy;
        InstrMemset memset;
        InstrPhi phi;
    };

    Instr* prev;
    Instr* next;
};

enum BBlockFlags {
    BBLOCK_IS_START    = 0x1,
    BBLOCK_IS_LOOP_HDR = 0x2
};

struct BBlock {
    long id;
    u32 flags;

    // Doubly-linked list of instructions.
    size_t num_instrs;
    Instr* first;
    Instr* last;

    BBlock** preds; // Stretchy buffer of predecessor basic blocks.

    bool closed; // Currently used for debugging. A BBlock is closed once the final jmp/ret instruction has been added.
};

void IR_gen_bytecode(Allocator* arena, Allocator* tmp_arena, BucketList* vars, BucketList* procs, BucketList* str_lits,
                     BucketList* float_lits, TypeCache* type_cache);
#endif
