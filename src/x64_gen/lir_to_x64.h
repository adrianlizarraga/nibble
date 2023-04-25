#ifndef NIBBLE_X64_GEN_LIR_TO_X64_H
#define NIBBLE_X64_GEN_LIR_TO_X64_H

#include "bytecode/module.h"
#include "nibble.h"
#include "allocator.h"
#include "array.h"
#include "ast/module.h"

typedef enum X64_SIBD_Addr_Kind {
    X64__SIBD_ADDR_GLOBAL,
    X64__SIBD_ADDR_LOCAL,
    X64__SIBD_ADDR_STR_LIT,
    X64__SIBD_ADDR_FLOAT_LIT,
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
    X64_Instr_Kind_MOVSS_RR,
    X64_Instr_Kind_MOVSS_MR,
    X64_Instr_Kind_MOVSS_RM,
    X64_Instr_Kind_MOVSD_RR,
    X64_Instr_Kind_MOVSD_MR,
    X64_Instr_Kind_MOVSD_RM,
    X64_Instr_Kind_MOVDQU_MR,
    X64_Instr_Kind_MOVDQU_RM,
    X64_Instr_Kind_CMP_RR,
    X64_Instr_Kind_CMP_RM,
    X64_Instr_Kind_CMP_MR,
    X64_Instr_Kind_CMP_RI,
    X64_Instr_Kind_CMP_MI,
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
#define X64_INSTR_IS_JMP_TARGET_MASK 0x80000000
static_assert(X64_Instr_Kind_COUNT <= X64_INSTR_KIND_MASK + 1, "Must have at most 512 X64_Instr_Kinds");

typedef struct X64__Instr {
    u32 flags; // [31] : is_jmp_target; [8:0]: kind

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
            u64 imm;  // Only mov can load a 64-bit immediate into an integer register.
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
            u8 dst;
            u8 src;
        } movss_rr;

        struct {
            X64_SIBD_Addr dst;
            u8 src;
        } movss_mr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } movss_rm;

        struct {
            u8 dst;
            u8 src;
        } movsd_rr;

        struct {
            X64_SIBD_Addr dst;
            u8 src;
        } movsd_mr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } movsd_rm;

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
} X64__Instr;

static inline X64_Instr_Kind X64__get_instr_kind(X64__Instr* instr) {
    return (X64_Instr_Kind)(instr->flags & X64_INSTR_KIND_MASK);
}

static inline void X64__set_instr_kind(X64__Instr* instr, X64_Instr_Kind kind) {
    instr->flags |= (kind & X64_INSTR_KIND_MASK);
}

static inline bool X64__is_instr_jmp_target(X64__Instr* instr) {
    return instr->flags & X64_INSTR_IS_JMP_TARGET_MASK;
}

static inline void X64__mark_instr_as_jmp_target(X64__Instr* instr) {
    instr->flags |= X64_INSTR_IS_JMP_TARGET_MASK;
}

Array(X64__Instr) X64__gen_proc_instrs(Allocator* gen_mem, Allocator* tmp_mem, Symbol* proc_sym);

#endif // defined(NIBBLE_X64_GEN_LIR_TO_X64_H)
