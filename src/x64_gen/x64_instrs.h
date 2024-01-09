#ifndef NIBBLE_X64_GEN_X64_INSTRS_H
#define NIBBLE_X64_GEN_X64_INSTRS_H

#include "basics.h"
#include "bytecode/module.h"
#include "x64_gen/regs.h"

typedef enum X64_SIBD_Addr_Kind {
    X64_SIBD_ADDR_GLOBAL,
    X64_SIBD_ADDR_LOCAL,
    X64_SIBD_ADDR_STR_LIT,
    X64_SIBD_ADDR_FLOAT_LIT,
} X64_SIBD_Addr_Kind;

typedef struct X64_SIBD_Addr {
    X64_SIBD_Addr_Kind kind;

    union {
        Symbol* global;
        struct {
            u8 base_reg;
            u8 index_reg;
            u8 scale;
            s32 disp;
        } local;
        StrLit* str_lit;
        FloatLit* float_lit;
    };
} X64_SIBD_Addr;

#define LIST_OF_X64_INSTR_KINDS \
    X_MACRO(NOOP)               \
    X_MACRO(PUSH)               \
    X_MACRO(POP)                \
    X_MACRO(JMP)                \
    X_MACRO(JMP_TO_RET)         \
    X_MACRO(JMPCC)              \
    X_MACRO(SETCC_R)            \
    X_MACRO(SETCC_M)            \
    X_MACRO(RET)                \
    X_MACRO(CALL)               \
    X_MACRO(CALL_R)             \
    X_MACRO(CALL_M)             \
    X_MACRO(REP_MOVSB)          \
    X_MACRO(REP_STOSB)          \
    X_MACRO(SYSCALL)            \
    X_MACRO(ADD_RR)             \
    X_MACRO(ADD_RM)             \
    X_MACRO(ADD_MR)             \
    X_MACRO(ADD_RI)             \
    X_MACRO(ADD_MI)             \
    X_MACRO(SUB_RR)             \
    X_MACRO(SUB_RM)             \
    X_MACRO(SUB_MR)             \
    X_MACRO(SUB_RI)             \
    X_MACRO(SUB_MI)             \
    X_MACRO(IMUL_RR)            \
    X_MACRO(IMUL_RM)            \
    X_MACRO(IMUL_RI)            \
    X_MACRO(IMUL_MI)            \
    X_MACRO(IMUL_R)             \
    X_MACRO(IMUL_M)             \
    X_MACRO(MUL_R)              \
    X_MACRO(MUL_M)              \
    X_MACRO(AND_RR)             \
    X_MACRO(AND_RM)             \
    X_MACRO(AND_MR)             \
    X_MACRO(AND_RI)             \
    X_MACRO(AND_MI)             \
    X_MACRO(OR_RR)              \
    X_MACRO(OR_RM)              \
    X_MACRO(OR_MR)              \
    X_MACRO(OR_RI)              \
    X_MACRO(OR_MI)              \
    X_MACRO(XOR_RR)             \
    X_MACRO(XOR_RM)             \
    X_MACRO(XOR_MR)             \
    X_MACRO(XOR_RI)             \
    X_MACRO(XOR_MI)             \
    X_MACRO(ADD_FLT_RR)         \
    X_MACRO(ADD_FLT_RM)         \
    X_MACRO(SUB_FLT_RR)         \
    X_MACRO(SUB_FLT_RM)         \
    X_MACRO(MUL_FLT_RR)         \
    X_MACRO(MUL_FLT_RM)         \
    X_MACRO(DIV_FLT_RR)         \
    X_MACRO(DIV_FLT_RM)         \
    X_MACRO(NEG_R)              \
    X_MACRO(NEG_M)              \
    X_MACRO(NOT_R)              \
    X_MACRO(NOT_M)              \
    X_MACRO(SAR_RR)             \
    X_MACRO(SAR_MR)             \
    X_MACRO(SAR_RI)             \
    X_MACRO(SAR_MI)             \
    X_MACRO(SHL_RR)             \
    X_MACRO(SHL_MR)             \
    X_MACRO(SHL_RI)             \
    X_MACRO(SHL_MI)             \
    X_MACRO(DIV_R)              \
    X_MACRO(DIV_M)              \
    X_MACRO(IDIV_R)             \
    X_MACRO(IDIV_M)             \
    X_MACRO(CWD)                \
    X_MACRO(CDQ)                \
    X_MACRO(CQO)                \
    X_MACRO(MOV_RR)             \
    X_MACRO(MOV_RM)             \
    X_MACRO(MOV_MR)             \
    X_MACRO(MOV_RI)             \
    X_MACRO(MOV_MI)             \
    X_MACRO(MOVSX_RR)           \
    X_MACRO(MOVSX_RM)           \
    X_MACRO(MOVSXD_RR)          \
    X_MACRO(MOVSXD_RM)          \
    X_MACRO(MOVZX_RR)           \
    X_MACRO(MOVZX_RM)           \
    X_MACRO(MOV_FLT_RR)         \
    X_MACRO(MOV_FLT_MR)         \
    X_MACRO(MOV_FLT_RM)         \
    X_MACRO(CVTSS2SD_RR)        \
    X_MACRO(CVTSS2SD_RM)        \
    X_MACRO(CVTSD2SS_RR)        \
    X_MACRO(CVTSD2SS_RM)        \
    X_MACRO(CVTTSS2SI_RR)       \
    X_MACRO(CVTTSS2SI_RM)       \
    X_MACRO(CVTTSD2SI_RR)       \
    X_MACRO(CVTTSD2SI_RM)       \
    X_MACRO(CVTSI2SS_RR)        \
    X_MACRO(CVTSI2SS_RM)        \
    X_MACRO(CVTSI2SD_RR)        \
    X_MACRO(CVTSI2SD_RM)        \
    X_MACRO(MOVDQU_MR)          \
    X_MACRO(MOVDQU_RM)          \
    X_MACRO(CMP_RR)             \
    X_MACRO(CMP_RM)             \
    X_MACRO(CMP_MR)             \
    X_MACRO(CMP_RI)             \
    X_MACRO(CMP_MI)             \
    X_MACRO(UCOMISS_RR)         \
    X_MACRO(UCOMISS_RM)         \
    X_MACRO(UCOMISD_RR)         \
    X_MACRO(UCOMISD_RM)         \
    X_MACRO(LEA)

#define X_MACRO(name) X64_Instr_Kind_##name,
typedef enum X64_Instr_Kind {
    LIST_OF_X64_INSTR_KINDS X64_Instr_Kind_COUNT
} X64_Instr_Kind;
#undef X_MACRO

extern StringView x64_instr_kind_names[X64_Instr_Kind_COUNT];

#define X64_INSTR_KIND_MASK ((1 << 9) - 1)
#define X64_INSTR_MOV_SRC_RH_MASK (1 << 9)
#define X64_INSTR_CVT_FLT_SI_INT64_MASK (1 << 9)
static_assert(X64_Instr_Kind_COUNT <= X64_INSTR_KIND_MASK + 1, "Must have at most 512 X64_Instr_Kinds");

typedef struct X64_Instr {
    u32 flags; // [31] : is_jmp_target; [30:9] instr-specific flags; [8:0]: kind

    union {
        struct {
            u8 reg;
        } push;

        struct {
            u8 reg;
        } pop;

        struct {
            u32 target;
        } jmp; // For _JMP and _JMP_TO_RET

        struct {
            u32 target;
            ConditionKind cond;
        } jmpcc; // For jb, jl, je, ...

        struct {
            ConditionKind cond;
            u8 dst;
        } setcc_r;

        struct {
            ConditionKind cond;
            X64_SIBD_Addr dst;
        } setcc_m;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } add_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } add_rm;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } add_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } add_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u32 imm;
        } add_mi;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } sub_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } sub_rm;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } sub_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } sub_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u32 imm;
        } sub_mi;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } imul_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } imul_rm;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } imul_ri;

        struct {
            u8 size;
            u8 src;
        } imul_r;

        struct {
            u8 size;
            X64_SIBD_Addr src;
        } imul_m;

        struct {
            u8 size;
            u8 src;
        } mul_r;

        struct {
            u8 size;
            X64_SIBD_Addr src;
        } mul_m;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u32 imm;
        } imul_mi;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } and_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } and_rm;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } and_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } and_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u32 imm;
        } and_mi;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } or_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } or_rm;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } or_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } or_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u32 imm;
        } or_mi;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } xor_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } xor_rm;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } xor_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } xor_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u32 imm;
        } xor_mi;

        struct {
            FloatKind kind;
            u8 dst;
            u8 src;
        } add_flt_rr;

        struct {
            FloatKind kind;
            u8 dst;
            X64_SIBD_Addr src;
        } add_flt_rm;

        struct {
            FloatKind kind;
            u8 dst;
            u8 src;
        } sub_flt_rr;

        struct {
            FloatKind kind;
            u8 dst;
            X64_SIBD_Addr src;
        } sub_flt_rm;

        struct {
            FloatKind kind;
            u8 dst;
            u8 src;
        } mul_flt_rr;

        struct {
            FloatKind kind;
            u8 dst;
            X64_SIBD_Addr src;
        } mul_flt_rm;

        struct {
            FloatKind kind;
            u8 dst;
            u8 src;
        } div_flt_rr;

        struct {
            FloatKind kind;
            u8 dst;
            X64_SIBD_Addr src;
        } div_flt_rm;

        struct {
            u8 size;
            u8 dst;
        } neg_r;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
        } neg_m;

        struct {
            u8 size;
            u8 dst;
        } not_r;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
        } not_m;

        struct {
            u8 size;
            u8 dst;
            // src must be rcx
        } sar_rr;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            // src must be rcx
        } sar_mr;

        struct {
            u8 size;
            u8 dst;
            u8 imm;
        } sar_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 imm;
        } sar_mi;

        struct {
            u8 size;
            u8 dst;
            // src must be rcx
        } shl_rr;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            // src must be rcx
        } shl_mr;

        struct {
            u8 size;
            u8 dst;
            u8 imm;
        } shl_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 imm;
        } shl_mi;

        struct {
            u8 size;
            u8 src;
        } div_r;

        struct {
            u8 size;
            X64_SIBD_Addr src;
        } div_m;

        struct {
            u8 size;
            u8 src;
        } idiv_r;

        struct {
            u8 size;
            X64_SIBD_Addr src;
        } idiv_m;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } mov_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } mov_rm;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } mov_mr;

        struct {
            u8 size;
            u8 dst;
            u64 imm; // Only mov can load a 64-bit immediate into an integer register.
        } mov_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u32 imm;
        } mov_mi;

        struct {
            u8 dst_size;
            u8 src_size;
            u8 dst;
            u8 src;
        } movsx_rr;

        struct {
            u8 dst_size;
            u8 src_size;
            u8 dst;
            X64_SIBD_Addr src;
        } movsx_rm;

        struct {
            u8 dst_size;
            u8 src_size;
            u8 dst;
            u8 src;
        } movsxd_rr;

        struct {
            u8 dst_size;
            u8 src_size;
            u8 dst;
            X64_SIBD_Addr src;
        } movsxd_rm;

        struct {
            u8 dst_size;
            u8 src_size;
            u8 dst;
            u8 src;
        } movzx_rr;

        struct {
            u8 dst_size;
            u8 src_size;
            u8 dst;
            X64_SIBD_Addr src;
        } movzx_rm;

        struct {
            FloatKind kind;
            u8 dst;
            u8 src;
        } mov_flt_rr;

        struct {
            FloatKind kind;
            X64_SIBD_Addr dst;
            u8 src;
        } mov_flt_mr;

        struct {
            FloatKind kind;
            u8 dst;
            X64_SIBD_Addr src;
        } mov_flt_rm;

        struct {
            u8 dst;
            u8 src;
        } cvtss2sd_rr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } cvtss2sd_rm;

        struct {
            u8 dst;
            u8 src;
        } cvtsd2ss_rr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } cvtsd2ss_rm;

        struct {
            u8 dst; // flag [9] is 1 if dst is 8-byte int (else 4-byte)
            u8 src;
        } cvttss2si_rr;

        struct {
            u8 dst; // flag [9] is 1 if dst is 8-byte int (else 4-byte)
            X64_SIBD_Addr src;
        } cvttss2si_rm;

        struct {
            u8 dst; // flag [9] is 1 if dst is 8-byte int (else 4-byte)
            u8 src;
        } cvttsd2si_rr;

        struct {
            u8 dst; // flag [9] is 1 if dst is 8-byte int (else 4-byte)
            X64_SIBD_Addr src;
        } cvttsd2si_rm;

        struct {
            u8 dst;
            u8 src; // flag [9] is 1 if src is 8-byte int (else 4-byte)
        } cvtsi2ss_rr;

        struct {
            u8 dst;
            X64_SIBD_Addr src; // flag [9] is 1 if src is 8-byte int (else 4-byte)
        } cvtsi2ss_rm;

        struct {
            u8 dst;
            u8 src; // flag [9] is 1 if src is 8-byte int (else 4-byte)
        } cvtsi2sd_rr;

        struct {
            u8 dst;
            X64_SIBD_Addr src; // flag [9] is 1 if src is 8-byte int (else 4-byte)
        } cvtsi2sd_rm;

        struct {
            X64_SIBD_Addr dst;
            u8 src;
        } movdqu_mr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } movdqu_rm;

        struct {
            u8 size;
            u8 dst;
            u8 src;
        } cmp_rr;

        struct {
            u8 size;
            u8 dst;
            X64_SIBD_Addr src;
        } cmp_rm;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u8 src;
        } cmp_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } cmp_ri;

        struct {
            u8 size;
            X64_SIBD_Addr dst;
            u32 imm;
        } cmp_mi;

        struct {
            u8 dst;
            u8 src;
        } ucomiss_rr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } ucomiss_rm;

        struct {
            u8 dst;
            u8 src;
        } ucomisd_rr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } ucomisd_rm;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } lea;

        struct {
            const Symbol* proc_sym;
        } call;

        struct {
            u8 reg;
        } call_r;

        struct {
            X64_SIBD_Addr mem;
        } call_m;
    };

    struct X64_Instr* next; // Next instruction in list
} X64_Instr;

typedef struct X64_BBlock {
    u32 num_instrs;
    X64_Instr* head;
    X64_Instr* tail;
} X64_BBlock;

typedef struct X64_Intrs {
    u32 num_instrs; // Total number of instructions in all basic blocks.

    // Stretchy array of basic blocks (end with a jump instruction)
    // Blocks are in final source-code order.
    Array(X64_BBlock) bblocks;
} X64_Instrs;

static inline X64_Instr_Kind X64_get_instr_kind(X64_Instr* instr)
{
    return (X64_Instr_Kind)(instr->flags & X64_INSTR_KIND_MASK);
}

static inline void X64_set_instr_kind(X64_Instr* instr, X64_Instr_Kind kind)
{
    instr->flags |= (kind & X64_INSTR_KIND_MASK);
}

void X64_emit_instr_ret(X64_Instrs* instrs);
void X64_emit_instr_call(X64_Instrs* instrs, const Symbol* proc_sym);
void X64_emit_instr_call_r(X64_Instrs* instrs, u8 reg);
void X64_emit_instr_call_m(X64_Instrs* instrs, X64_SIBD_Addr mem);
void X64_emit_instr_jmp(X64_Instrs* instrs, u32 target);
void X64_emit_instr_jmp_to_ret(X64_Instrs* instrs);
void X64_emit_instr_jmpcc(X64_Instrs* instrs, ConditionKind cond_kind, u32 target);
void X64_emit_instr_setcc_r(X64_Instrs* instrs, ConditionKind cond_kind, u8 dst);
void X64_emit_instr_setcc_m(X64_Instrs* instrs, ConditionKind cond_kind, X64_SIBD_Addr dst);
void X64_emit_instr_push(X64_Instrs* instrs, X64_Reg reg);
void X64_emit_instr_pop(X64_Instrs* instrs, X64_Reg reg);

void X64_emit_instr_add_rr(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src);
void X64_emit_instr_add_rm(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_add_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_add_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm);
void X64_emit_instr_add_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);

void X64_emit_instr_sub_rr(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src);
void X64_emit_instr_sub_rm(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_sub_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_sub_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm);
void X64_emit_instr_sub_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);

void X64_emit_instr_imul_rr(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src);
void X64_emit_instr_imul_rm(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_imul_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm);
void X64_emit_instr_imul_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);
void X64_emit_instr_imul_r(X64_Instrs* instrs, u8 size, X64_Reg src);
void X64_emit_instr_imul_m(X64_Instrs* instrs, u8 size, X64_SIBD_Addr src);
void X64_emit_instr_mul_r(X64_Instrs* instrs, u8 size, X64_Reg src);
void X64_emit_instr_mul_m(X64_Instrs* instrs, u8 size, X64_SIBD_Addr src);

void X64_emit_instr_and_rr(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src);
void X64_emit_instr_and_rm(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_and_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_and_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm);
void X64_emit_instr_and_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);

void X64_emit_instr_or_rr(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src);
void X64_emit_instr_or_rm(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_or_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_or_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm);
void X64_emit_instr_or_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);

void X64_emit_instr_xor_rr(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src);
void X64_emit_instr_xor_rm(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_xor_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_xor_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm);
void X64_emit_instr_xor_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);

void X64_emit_instr_add_flt_rr(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
void X64_emit_instr_add_flt_rm(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_sub_flt_rr(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
void X64_emit_instr_sub_flt_rm(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_mul_flt_rr(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
void X64_emit_instr_mul_flt_rm(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_div_flt_rr(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
void X64_emit_instr_div_flt_rm(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_neg_r(X64_Instrs* instrs, u8 size, X64_Reg dst);
void X64_emit_instr_neg_m(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst);
void X64_emit_instr_not_r(X64_Instrs* instrs, u8 size, X64_Reg dst);
void X64_emit_instr_not_m(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst);

void X64_emit_instr_sar_rr(X64_Instrs* instrs, u8 size, X64_Reg dst);
void X64_emit_instr_sar_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst);
void X64_emit_instr_sar_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u8 imm);
void X64_emit_instr_sar_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u8 imm);

void X64_emit_instr_shl_rr(X64_Instrs* instrs, u8 size, X64_Reg dst);
void X64_emit_instr_shl_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst);
void X64_emit_instr_shl_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u8 imm);
void X64_emit_instr_shl_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u8 imm);

void X64_emit_instr_div_r(X64_Instrs* instrs, u8 size, X64_Reg src);
void X64_emit_instr_div_m(X64_Instrs* instrs, u8 size, X64_SIBD_Addr src);

void X64_emit_instr_idiv_r(X64_Instrs* instrs, u8 size, X64_Reg src);
void X64_emit_instr_idiv_m(X64_Instrs* instrs, u8 size, X64_SIBD_Addr src);

void X64_emit_instr_sext_ax_into_dx(X64_Instrs* instrs, u8 size);

void X64_emit_instr_mov_rr(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src);
void X64_emit_instr_mov_rm(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_mov_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_mov_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);
void X64_emit_instr_mov_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u64 imm);
void X64_emit_instr_mov_rrh(X64_Instrs* instrs, X64_Reg dst, X64_Reg src);
void X64_emit_instr_mov_mrh(X64_Instrs* instrs, X64_SIBD_Addr dst, X64_Reg src);

void X64_emit_instr_movsx_rr(X64_Instrs* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src);
void X64_emit_instr_movsx_rm(X64_Instrs* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src);

void X64_emit_instr_movsxd_rr(X64_Instrs* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src);
void X64_emit_instr_movsxd_rm(X64_Instrs* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src);

void X64_emit_instr_movzx_rr(X64_Instrs* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src);
void X64_emit_instr_movzx_rm(X64_Instrs* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src);

void X64_emit_instr_mov_flt_rr(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
void X64_emit_instr_mov_flt_rm(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_mov_flt_mr(X64_Instrs* instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src);

void X64_emit_instr_cvtss2sd_rr(X64_Instrs* instrs, X64_Reg dst, X64_Reg src);
void X64_emit_instr_cvtss2sd_rm(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_cvtsd2ss_rr(X64_Instrs* instrs, X64_Reg dst, X64_Reg src);
void X64_emit_instr_cvtsd2ss_rm(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_cvttss2si_rr(X64_Instrs* instrs, X64_Reg dst, bool dst_8bytes, X64_Reg src);
void X64_emit_instr_cvttss2si_rm(X64_Instrs* instrs, X64_Reg dst, bool dst_8bytes, X64_SIBD_Addr src);

void X64_emit_instr_cvttsd2si_rr(X64_Instrs* instrs, X64_Reg dst, bool dst_8bytes, X64_Reg src);
void X64_emit_instr_cvttsd2si_rm(X64_Instrs* instrs, X64_Reg dst, bool dst_8bytes, X64_SIBD_Addr src);

void X64_emit_instr_cvtsi2ss_rr(X64_Instrs* instrs, X64_Reg dst, X64_Reg src, bool src_8bytes);
void X64_emit_instr_cvtsi2ss_rm(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src, bool src_8bytes);

void X64_emit_instr_cvtsi2sd_rr(X64_Instrs* instrs, X64_Reg dst, X64_Reg src, bool src_8bytes);
void X64_emit_instr_cvtsi2sd_rm(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src, bool src_8bytes);

void X64_emit_instr_movdqu_mr(X64_Instrs* instrs, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_movdqu_rm(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_cmp_rr(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src);
void X64_emit_instr_cmp_rm(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_cmp_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_cmp_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm);
void X64_emit_instr_cmp_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);

void X64_emit_instr_ucomiss_rr(X64_Instrs* instrs, X64_Reg dst, X64_Reg src);
void X64_emit_instr_ucomiss_rm(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_ucomisd_rr(X64_Instrs* instrs, X64_Reg dst, X64_Reg src);
void X64_emit_instr_ucomisd_rm(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src);

void X64_emit_instr_lea(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_rep_movsb(X64_Instrs* instrs);
void X64_emit_instr_rep_stosb(X64_Instrs* instrs);
void X64_emit_instr_syscall(X64_Instrs* instrs);
X64_Instr* X64_emit_instr_placeholder(X64_Instrs* instrs, X64_Instr_Kind kind);

#endif
