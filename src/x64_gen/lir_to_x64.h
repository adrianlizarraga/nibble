#ifndef NIBBLE_X64_GEN_LIR_TO_X64_H
#define NIBBLE_X64_GEN_LIR_TO_X64_H

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
    X64_Instr_Kind_RET,
    X64_Instr_Kind_JMP,
    X64_Instr_Kind_JMP_TO_RET, // Doesn't correspond to an actual X64 instruction. Jumps to ret label.
    X64_Instr_Kind_PUSH,
    X64_Instr_Kind_POP,
    X64_Instr_Kind_ADD_RR,
    X64_Instr_Kind_ADD_RM,
    X64_Instr_Kind_ADD_MR,
    X64_Instr_Kind_ADD_RI,
    X64_Instr_Kind_SUB_RR,
    X64_Instr_Kind_SUB_RM,
    X64_Instr_Kind_SUB_MR,
    X64_Instr_Kind_SUB_RI,
    X64_Instr_Kind_IMUL_RR,
    X64_Instr_Kind_IMUL_RM,
    X64_Instr_Kind_IMUL_MR,
    X64_Instr_Kind_IMUL_RI,
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
    X64_Instr_Kind_MOVSS_MR,
    X64_Instr_Kind_MOVSS_RM,
    X64_Instr_Kind_MOVSD_MR,
    X64_Instr_Kind_MOVSD_RM,
    X64_Instr_Kind_MOVDQU_MR,
    X64_Instr_Kind_MOVDQU_RM,

    X64_Instr_Kind_COUNT
} X64_Instr_Kind;

typedef struct X64__Instr {
    X64_Instr_Kind kind;

    union {
        struct {
            u8 reg;
        } push;

        struct {
            u8 reg;
        } pop;

        struct {
            size_t target;
        } jmp; // Also for JMP_TO_RET

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
            X64_SIBD_Addr dst;
            u8 src;
        } movss_mr;

        struct {
            u8 dst;
            X64_SIBD_Addr src;
        } movss_rm;

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
    };
} X64__Instr;

Array(X64__Instr) X64__gen_proc_instrs(Allocator* gen_mem, Allocator* tmp_mem, Symbol* proc_sym);

#endif // defined(NIBBLE_X64_GEN_LIR_TO_X64_H)
