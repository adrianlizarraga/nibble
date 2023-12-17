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

typedef enum X64_Instr_Kind {
    X64_Instr_Kind_NOOP = 0,
    X64_Instr_Kind_PUSH,
    X64_Instr_Kind_POP,
    X64_Instr_Kind_ADD_RR,
    X64_Instr_Kind_ADD_RM,
    X64_Instr_Kind_ADD_MR,
    X64_Instr_Kind_ADD_RI,
    X64_Instr_Kind_ADD_MI,
    X64_Instr_Kind_SUB_RR,
    X64_Instr_Kind_SUB_RM,
    X64_Instr_Kind_SUB_MR,
    X64_Instr_Kind_SUB_RI,
    X64_Instr_Kind_SUB_MI,
    X64_Instr_Kind_IMUL_RR,
    X64_Instr_Kind_IMUL_RM,
    X64_Instr_Kind_IMUL_MR,
    X64_Instr_Kind_IMUL_RI,
    X64_Instr_Kind_IMUL_MI,
    X64_Instr_Kind_AND_RR,
    X64_Instr_Kind_AND_RM,
    X64_Instr_Kind_AND_MR,
    X64_Instr_Kind_AND_RI,
    X64_Instr_Kind_AND_MI,
    X64_Instr_Kind_OR_RR,
    X64_Instr_Kind_OR_RM,
    X64_Instr_Kind_OR_MR,
    X64_Instr_Kind_OR_RI,
    X64_Instr_Kind_OR_MI,
    X64_Instr_Kind_XOR_RR,
    X64_Instr_Kind_XOR_RM,
    X64_Instr_Kind_XOR_MR,
    X64_Instr_Kind_XOR_RI,
    X64_Instr_Kind_XOR_MI,
    X64_Instr_Kind_ADD_FLT_RR,
    X64_Instr_Kind_ADD_FLT_RM,
    X64_Instr_Kind_ADD_FLT_MR,
    X64_Instr_Kind_SUB_FLT_RR,
    X64_Instr_Kind_SUB_FLT_RM,
    X64_Instr_Kind_SUB_FLT_MR,
    X64_Instr_Kind_MUL_FLT_RR,
    X64_Instr_Kind_MUL_FLT_RM,
    X64_Instr_Kind_MUL_FLT_MR,
    X64_Instr_Kind_DIV_FLT_RR,
    X64_Instr_Kind_DIV_FLT_RM,
    X64_Instr_Kind_DIV_FLT_MR,
    X64_Instr_Kind_NEG_R,
    X64_Instr_Kind_NEG_M,
    X64_Instr_Kind_NOT_R,
    X64_Instr_Kind_NOT_M,
    X64_Instr_Kind_SAR_RR,
    X64_Instr_Kind_SAR_MR,
    X64_Instr_Kind_SAR_RI,
    X64_Instr_Kind_SAR_MI,
    X64_Instr_Kind_SHL_RR,
    X64_Instr_Kind_SHL_MR,
    X64_Instr_Kind_SHL_RI,
    X64_Instr_Kind_SHL_MI,
    X64_Instr_Kind_DIV_R,
    X64_Instr_Kind_DIV_M,
    X64_Instr_Kind_IDIV_R,
    X64_Instr_Kind_IDIV_M,
    X64_Instr_Kind_CWD, // 2-byte sign extend ax into dx
    X64_Instr_Kind_CDQ, // 4-byte sign extend ax into dx
    X64_Instr_Kind_CQO, // 8-byte sign extend ax into dx
    X64_Instr_Kind_MOV_RR,
    X64_Instr_Kind_MOV_RM,
    X64_Instr_Kind_MOV_MR,
    X64_Instr_Kind_MOV_RI,
    X64_Instr_Kind_MOV_MI,
    X64_Instr_Kind_MOVSX_RR,
    X64_Instr_Kind_MOVSX_RM,
    X64_Instr_Kind_MOVSXD_RR,
    X64_Instr_Kind_MOVSXD_RM,
    X64_Instr_Kind_MOVZX_RR,
    X64_Instr_Kind_MOVZX_RM,
    X64_Instr_Kind_MOV_FLT_RR,
    X64_Instr_Kind_MOV_FLT_MR,
    X64_Instr_Kind_MOV_FLT_RM,
    X64_Instr_Kind_CVTSS2SD_RR, // f32 to f64
    X64_Instr_Kind_CVTSS2SD_RM, // f32 (M) to f64
    X64_Instr_Kind_CVTSD2SS_RR, // f64 to f32
    X64_Instr_Kind_CVTSD2SS_RM, // f64 (M) to f32
    X64_Instr_Kind_CVTTSS2SI_RR, // f32 to int
    X64_Instr_Kind_CVTTSS2SI_RM, // f32 (M) to int
    X64_Instr_Kind_CVTTSD2SI_RR, // f64 to int
    X64_Instr_Kind_CVTTSD2SI_RM, // f64 (M) to int
    X64_Instr_Kind_CVTSI2SS_RR, // int to f32
    X64_Instr_Kind_CVTSI2SS_RM, // int (M) to f32
    X64_Instr_Kind_CVTSI2SD_RR, // int to f64
    X64_Instr_Kind_CVTSI2SD_RM, // int (M) to f64
    X64_Instr_Kind_MOVDQU_MR,
    X64_Instr_Kind_MOVDQU_RM,
    X64_Instr_Kind_CMP_RR,
    X64_Instr_Kind_CMP_RM,
    X64_Instr_Kind_CMP_MR,
    X64_Instr_Kind_CMP_RI,
    X64_Instr_Kind_CMP_MI,
    X64_Instr_Kind_UCOMISS_RR, // cmp f32s
    X64_Instr_Kind_UCOMISS_RM, // cmp f32s (src in mem)
    X64_Instr_Kind_UCOMISD_RR, // cmp f64s
    X64_Instr_Kind_UCOMISD_RM, // cmp f64s (src in mem)
    X64_Instr_Kind_LEA,
    X64_Instr_Kind_REP_MOVSB,
    X64_Instr_Kind_REP_STOSB,
    X64_Instr_Kind_SYSCALL,
    X64_Instr_Kind_JMP,
    X64_Instr_Kind_JMP_TO_RET, // Doesn't correspond to an actual X64 instruction. Jumps to ret label.
    X64_Instr_Kind_JMPCC,
    X64_Instr_Kind_SETCC_R,
    X64_Instr_Kind_SETCC_M,
    X64_Instr_Kind_RET,
    X64_Instr_Kind_CALL,
    X64_Instr_Kind_CALL_R,
    X64_Instr_Kind_CALL_M,

    X64_Instr_Kind_COUNT
} X64_Instr_Kind;

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
            X64_SIBD_Addr dst;
            u8 src;
        } imul_mr;

        struct {
            u8 size;
            u8 dst;
            u32 imm;
        } imul_ri;

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
            X64_SIBD_Addr dst;
            u8 src;
        } add_flt_mr;

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
            X64_SIBD_Addr dst;
            u8 src;
        } sub_flt_mr;

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
            X64_SIBD_Addr dst;
            u8 src;
        } mul_flt_mr;

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
            FloatKind kind;
            X64_SIBD_Addr dst;
            u8 src;
        } div_flt_mr;

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
void X64_emit_instr_imul_mr(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
void X64_emit_instr_imul_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm);
void X64_emit_instr_imul_mi(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);

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
void X64_emit_instr_add_flt_mr(X64_Instrs* instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src);

void X64_emit_instr_sub_flt_rr(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
void X64_emit_instr_sub_flt_rm(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_sub_flt_mr(X64_Instrs* instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src);

void X64_emit_instr_mul_flt_rr(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
void X64_emit_instr_mul_flt_rm(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_mul_flt_mr(X64_Instrs* instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src);

void X64_emit_instr_div_flt_rr(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
void X64_emit_instr_div_flt_rm(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);
void X64_emit_instr_div_flt_mr(X64_Instrs* instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src);

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
