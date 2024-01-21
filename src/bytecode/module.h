#ifndef NIBBLE_BYTECODE_MODULE_H
#define NIBBLE_BYTECODE_MODULE_H

#include "ast/module.h"

#define IR_STACK_ALIGN 16
#define IR_INSTRS_PER_BUCKET 64
#define IR_PROCS_PER_BUCKET 16
#define IR_REG_COUNT 0xFFFFFFFF

typedef u32 IR_Reg;

// Instruction operand that is either a register (R) or an immediate (I).
typedef struct OpRI {
    bool is_imm;

    union {
        IR_Reg reg;
        Scalar imm;
    };
} OpRI;

typedef enum MemBaseKind {
    MEM_BASE_NONE = 0,
    MEM_BASE_REG,
    MEM_BASE_MEM_OBJ,
    MEM_BASE_STR_LIT,
    MEM_BASE_FLOAT_LIT,
} MemBaseKind;

typedef struct MemAddr {
    MemBaseKind base_kind;

    union {
        IR_Reg reg;
        struct MemObj* obj;
        StrLit* str_lit;
        FloatLit* float_lit;
    } base;

    IR_Reg index_reg;
    u8 scale;
    u32 disp;
} MemAddr;

// Instruction operand that is either a register (R) or an address (A).
typedef struct OpRA {
    bool is_addr;

    union {
        IR_Reg reg;
        MemAddr addr;
    };
} OpRA;

typedef enum OpRIAKind {
    OP_RIA_NONE = 0,
    OP_RIA_REG,
    OP_RIA_IMM,
    OP_RIA_ADDR
} OpRIAKind;

// Instruction operand that is either a register (R), an immediate (I), or an address (A).
typedef struct OpRIA {
    OpRIAKind kind;

    union {
        IR_Reg reg;
        Scalar imm;
        MemAddr addr;
    };
} OpRIA;

typedef enum MemObjKind {
    MEM_OBJ_NONE = 0,
    MEM_OBJ_ANON_OBJ,
    MEM_OBJ_SYM,
    MEM_OBJ_ADDR,
    MEM_OBJ_ALIAS,
} MemObjKind;

typedef struct MemObj {
    MemObjKind kind;

    union {
        AnonObj* anon_obj;
        Symbol* sym;
        struct MemObj* alias;
        MemAddr addr;
    };
} MemObj;

typedef enum IR_InstrKind {
    IR_INSTR_KIND_NONE = 0,
    IR_InstrIntAdd_KIND,
    IR_InstrFltAdd_KIND,
    IR_InstrIntSub_KIND,
    IR_InstrFltSub_KIND,
    IR_InstrIntMul_KIND,
    IR_InstrFltMul_KIND,
    IR_InstrIntDiv_KIND,
    IR_InstrFltDiv_KIND,
    IR_InstrMod_KIND,
    IR_InstrDivMod_KIND,
    IR_InstrSar_KIND,
    IR_InstrShl_KIND,
    IR_InstrAnd_KIND,
    IR_InstrOr_KIND,
    IR_InstrXor_KIND,
    IR_InstrNot_KIND,
    IR_InstrNeg_KIND,
    IR_InstrTrunc_KIND,
    IR_InstrZExt_KIND,
    IR_InstrSExt_KIND,
    IR_InstrFlt2Int_KIND,
    IR_InstrInt2Flt_KIND,
    IR_InstrFlt2Flt_KIND,
    IR_InstrLImm_KIND,
    IR_InstrLoad_KIND,
    IR_InstrLAddr_KIND,
    IR_InstrStore_KIND,
    IR_InstrIntCmp_KIND,
    IR_InstrFltCmp_KIND,
    IR_InstrJmp_KIND,
    IR_InstrCondJmp_KIND,
    IR_InstrRet_KIND,
    IR_InstrCall_KIND,
    IR_InstrCallIndirect_KIND,
    IR_InstrMemcpy_KIND,
    IR_InstrMemset_KIND,
    IR_InstrSyscall_KIND,
    IR_InstrPhi_KIND,
    IR_INSTR_KIND_COUNT
} IR_InstrKind;

// TODO: Look into removing link-list support and storing instrs in an array (reduce memory costs).
typedef struct IR_Instr {
    IR_InstrKind kind;
    u32 ino; // Instruction number in procedure
    struct IR_Instr* next;
} IR_Instr;

typedef struct IR_InstrIntBinary {
    IR_Instr super;
    Type* type;
    IR_Reg r;
    OpRIA a;
    OpRIA b;
} IR_InstrIntBinary;

typedef IR_InstrIntBinary IR_InstrIntAdd;
typedef IR_InstrIntBinary IR_InstrIntSub;
typedef IR_InstrIntBinary IR_InstrIntMul;
typedef IR_InstrIntBinary IR_InstrIntDiv;
typedef IR_InstrIntBinary IR_InstrMod;
typedef IR_InstrIntBinary IR_InstrAnd;
typedef IR_InstrIntBinary IR_InstrOr;
typedef IR_InstrIntBinary IR_InstrXor;

typedef struct IR_InstrFltBinary {
    IR_Instr super;
    FloatKind fkind;
    IR_Reg r;
    OpRA a;
    OpRA b;
} IR_InstrFltBinary;

typedef IR_InstrFltBinary IR_InstrFltAdd;
typedef IR_InstrFltBinary IR_InstrFltSub;
typedef IR_InstrFltBinary IR_InstrFltMul;
typedef IR_InstrFltBinary IR_InstrFltDiv;

typedef struct IR_InstrShift {
    IR_Instr super;
    Type* type;
    IR_Reg r;
    OpRIA a;
    OpRIA b;
} IR_InstrShift;

typedef IR_InstrShift IR_InstrSar;
typedef IR_InstrShift IR_InstrShl;

typedef struct IR_InstrDivMod {
    IR_Instr super;
    Type* type;
    IR_Reg q; // quotient
    IR_Reg r; // remainder
    OpRIA a; // dividend
    OpRIA b; // divisor
} IR_InstrDivMod;

typedef struct IR_InstrUnary {
    IR_Instr super;
    Type* type;
    IR_Reg r;
    IR_Reg a;
} IR_InstrUnary;

typedef IR_InstrUnary IR_InstrNot;
typedef IR_InstrUnary IR_InstrNeg;

typedef struct IR_InstrConvert {
    IR_Instr super;
    Type* dst_type;
    Type* src_type;
    IR_Reg r;
    IR_Reg a;
} IR_InstrConvert;

typedef IR_InstrConvert IR_InstrTrunc;
typedef IR_InstrConvert IR_InstrZExt;
typedef IR_InstrConvert IR_InstrSExt;

typedef struct IR_InstrFlt2Int {
    IR_Instr super;
    FloatKind src_kind;
    IntegerKind dst_kind;
    IR_Reg dst;
    OpRA src;
} IR_InstrFlt2Int;

typedef struct IR_InstrInt2Flt {
    IR_Instr super;
    IntegerKind src_kind;
    FloatKind dst_kind;
    IR_Reg dst;
    OpRA src;
} IR_InstrInt2Flt;

typedef struct IR_InstrFlt2Flt {
    IR_Instr super;
    FloatKind src_kind;
    FloatKind dst_kind;
    IR_Reg dst;
    OpRA src;
} IR_InstrFlt2Flt;

typedef struct IR_InstrLImm {
    IR_Instr super;
    Type* type;
    IR_Reg r;
    Scalar imm;
} IR_InstrLImm;

typedef struct IR_InstrLoad {
    IR_Instr super;
    Type* type;
    IR_Reg r;
    MemAddr addr;
} IR_InstrLoad;

typedef struct IR_InstrLAddr {
    IR_Instr super;
    Type* type;
    IR_Reg r;
    MemAddr addr;
} IR_InstrLAddr;

typedef struct IR_InstrStore {
    IR_Instr super;
    Type* type;
    MemAddr addr;
    OpRI a;
} IR_InstrStore;

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

typedef struct IR_InstrIntCmp {
    IR_Instr super;
    Type* type;
    ConditionKind cond;
    IR_Reg r;
    OpRIA a;
    OpRIA b;
} IR_InstrIntCmp;

typedef struct IR_InstrFltCmp {
    IR_Instr super;
    FloatKind fkind;
    ConditionKind cond;
    IR_Reg r;
    IR_Reg a;
    OpRA b;
} IR_InstrFltCmp;

typedef struct IR_InstrJmp {
    IR_Instr super;
    BBlock* from;
    BBlock* target;
} IR_InstrJmp;

typedef struct IR_InstrCondJmp {
    IR_Instr super;
    BBlock* from;
    BBlock* true_bb;
    BBlock* false_bb;
    IR_Reg a;
} IR_InstrCondJmp;

typedef struct IR_CaseRange {
    u32 start;
    u32 end;
    u32 index;
}IR_CaseRange;

typedef struct IR_InstrSwitchCaseJmp {
    IR_Instr super;
    OpRIA val;
    BBlock* from;
    BBlock** targets;
    u32 num_targets;

    // Sorted sparse case value ranges (start value, end value, index into targets).
    // Missing entries jump to the last target (could be default case or not).
    u32 num_case_ranges;
    IR_CaseRange* case_ranges;
} IR_InstrSwitchCaseJmp;

typedef struct IR_Value {
    Type* type;

    union {
        IR_Reg reg;
        MemAddr addr;
    };
} IR_Value;

typedef struct IR_InstrRet {
    IR_Instr super;
    IR_Value val;
} IR_InstrRet;

typedef struct IR_InstrCall {
    IR_Instr super;
    Symbol* sym;
    IR_Value r;
    u32 num_args;
    IR_Value* args;
} IR_InstrCall;

typedef struct IR_InstrCallIndirect {
    IR_Instr super;
    Type* proc_type;
    IR_Reg loc;
    IR_Value r;
    u32 num_args;
    IR_Value* args;
} IR_InstrCallIndirect;

typedef struct IR_InstrMemcpy {
    IR_Instr super;
    MemAddr dst;
    MemAddr src;
    OpRI size;
} IR_InstrMemcpy;

typedef struct IR_InstrMemset {
    IR_Instr super;
    MemAddr dst;
    OpRI value;
    OpRI size;
} IR_InstrMemset;

typedef struct IR_InstrSyscall {
    IR_Instr super;
    OpRIA nr;
    u8 count; // [1 - 6]
    OpRIA* args;
    IR_Reg r;
} IR_InstrSyscall;

typedef struct PhiArg {
    BBlock* bblock;
    IR_Reg ireg;
} PhiArg;

typedef struct IR_InstrPhi {
    IR_Instr super;
    Type* type;
    IR_Reg r;
    size_t num_args;
    PhiArg* args;
} IR_InstrPhi;

enum BBlockFlags {
    BBLOCK_IS_START    = 0x1,
    BBLOCK_IS_LOOP_HDR = 0x2
};

struct BBlock {
    long id;
    u32 flags;

    // Doubly-linked list of instructions.
    size_t num_instrs;
    IR_Instr* first;
    IR_Instr* last;

    BBlock** preds; // Stretchy buffer of predecessor basic blocks.

    bool closed; // Currently used for debugging. A BBlock is closed once the final jmp/ret instruction has been added.
};

void IR_gen_bytecode(Allocator* arena, Allocator* tmp_arena, GlobalData* vars, BucketList* procs, GlobalData* str_lits,
                     GlobalData* float_lits, TypeCache* type_cache, HMap* float_lit_map);
void IR_build_procs(Allocator* arena, Allocator* tmp_arena, BucketList* procs, GlobalData* str_lits, GlobalData* float_lits,
                    TypeCache* type_cache, HMap* float_lit_map);
void IR_build_vars(Allocator* arena, Allocator* tmp_arena, GlobalData* vars, GlobalData* str_lits, GlobalData* float_lits,
                   TypeCache* type_cache, HMap* float_lit_map);

char* IR_print_instr(Allocator* arena, IR_Instr* instr);
void IR_print_out_proc(Allocator* arena, Symbol* sym);
void IR_dump_proc_dot(Allocator* arena, Symbol* sym);
#endif
