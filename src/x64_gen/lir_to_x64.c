#include "x64_gen/lir_to_x64.h"
#include "allocator.h"
#include "array.h"
#include "ast/module.h"
#include "bytecode/module.h"
#include "hash_map.h"
#include "nibble.h"
#include "stream.h"
#include "x64_gen/regs.h"
#include "x64_gen/lir.h"
#include "x64_gen/reg_alloc.h"
#include "x64_gen/print_lir.h"

static inline X64_SIBD_Addr X64__get_rbp_offset_addr(s32 offset)
{
    X64_SIBD_Addr addr = {
        .kind = X64__SIBD_ADDR_LOCAL,
        .local = {
            .base_reg = X64_RBP,
            .index_reg = X64_REG_COUNT,
            .disp = offset
        }
    };

    return addr;
}

static inline X64_SIBD_Addr X64__get_rsp_offset_addr(s32 offset)
{
    X64_SIBD_Addr addr = {
        .kind = X64__SIBD_ADDR_LOCAL,
        .local = {
            .base_reg = X64_RSP,
            .index_reg = X64_REG_COUNT,
            .disp = offset
        }
    };

    return addr;
}

static void X64__emit_instr_ret(Array(X64__Instr) * instrs)
{
    X64__Instr ret_instr = {.flags = X64_Instr_Kind_RET};
    array_push(*instrs, ret_instr);
}

static void X64__emit_instr_call(Array(X64__Instr)* instrs, const Symbol* proc_sym)
{
    X64__Instr call_instr = {.flags = X64_Instr_Kind_CALL, .call.proc_sym = proc_sym};
    array_push(*instrs, call_instr);
}

static void X64__emit_instr_call_r(Array(X64__Instr)* instrs, u8 reg)
{
    X64__Instr call_instr = {.flags = X64_Instr_Kind_CALL_R, .call_r.reg = reg};
    array_push(*instrs, call_instr);
}

static void X64__emit_instr_call_m(Array(X64__Instr)* instrs, X64_SIBD_Addr mem)
{
    X64__Instr call_instr = {.flags = X64_Instr_Kind_CALL_M, .call_m.mem = mem};
    array_push(*instrs, call_instr);
}

static void X64__emit_instr_jmp(Array(X64__Instr)* instrs, u32 target)
{
    X64__Instr instr = {.flags = X64_Instr_Kind_JMP, .jmp.target = target};
    array_push(*instrs, instr);
}

static void X64__emit_instr_jmp_to_ret(Array(X64__Instr)* instrs)
{
    X64__Instr instr = {.flags = X64_Instr_Kind_JMP_TO_RET, .jmp.target = X64_REG_COUNT};
    array_push(*instrs, instr);
}

static void X64__emit_instr_jmpcc(Array(X64__Instr)* instrs, ConditionKind cond_kind, u32 target)
{
    X64__Instr instr = {.flags = X64_Instr_Kind_JMPCC, .jmpcc.target = target, .jmpcc.cond = cond_kind};
    array_push(*instrs, instr);
}

static void X64__emit_instr_setcc_r(Array(X64__Instr)* instrs, ConditionKind cond_kind, u8 dst)
{
    X64__Instr instr = {.flags = X64_Instr_Kind_SETCC_R, .setcc_r.cond = cond_kind, .setcc_r.dst = dst};
    array_push(*instrs, instr);
}

static void X64__emit_instr_setcc_m(Array(X64__Instr)* instrs, ConditionKind cond_kind, X64_SIBD_Addr dst)
{
    X64__Instr instr = {.flags = X64_Instr_Kind_SETCC_M, .setcc_m.cond = cond_kind, .setcc_m.dst = dst};
    array_push(*instrs, instr);
}

static void X64__emit_instr_push(Array(X64__Instr) * instrs, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64__Instr push_instr = {
        .flags = X64_Instr_Kind_PUSH,
        .push.reg = reg,
    };

    array_push(*instrs, push_instr);
}

static void X64__emit_instr_pop(Array(X64__Instr) * instrs, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64__Instr pop_instr = {
        .flags = X64_Instr_Kind_POP,
        .pop.reg = reg,
    };

    array_push(*instrs, pop_instr);
}

static void X64__emit_instr_add_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr add_rr_instr = {
        .flags = X64_Instr_Kind_ADD_RR,
        .add_rr.size = size,
        .add_rr.dst = dst,
        .add_rr.src = src,
    };

    array_push(*instrs, add_rr_instr);
}

static void X64__emit_instr_add_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr add_rm_instr = {
        .flags = X64_Instr_Kind_ADD_RM,
        .add_rm.size = size,
        .add_rm.dst = dst,
        .add_rm.src = src,
    };

    array_push(*instrs, add_rm_instr);
}

static void X64__emit_instr_add_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr add_mr_instr = {
        .flags = X64_Instr_Kind_ADD_MR,
        .add_mr.size = size,
        .add_mr.dst = dst,
        .add_mr.src = src,
    };

    array_push(*instrs, add_mr_instr);
}

static void X64__emit_instr_add_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr add_ri_instr = {
        .flags = X64_Instr_Kind_ADD_RI,
        .add_ri.size = size,
        .add_ri.dst = dst,
        .add_ri.imm = imm,
    };

    array_push(*instrs, add_ri_instr);
}

static void X64__emit_instr_add_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr add_mi_instr = {
        .flags = X64_Instr_Kind_ADD_MI,
        .add_mi.size = size,
        .add_mi.dst = dst,
        .add_mi.imm = imm,
    };

    array_push(*instrs, add_mi_instr);
}

static void X64__emit_instr_sub_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr sub_rr_instr = {
        .flags = X64_Instr_Kind_SUB_RR,
        .sub_rr.size = size,
        .sub_rr.dst = dst,
        .sub_rr.src = src,
    };

    array_push(*instrs, sub_rr_instr);
}

static void X64__emit_instr_sub_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr sub_rm_instr = {
        .flags = X64_Instr_Kind_SUB_RM,
        .sub_rm.size = size,
        .sub_rm.dst = dst,
        .sub_rm.src = src,
    };

    array_push(*instrs, sub_rm_instr);
}

static void X64__emit_instr_sub_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr sub_mr_instr = {
        .flags = X64_Instr_Kind_SUB_MR,
        .sub_mr.size = size,
        .sub_mr.dst = dst,
        .sub_mr.src = src,
    };

    array_push(*instrs, sub_mr_instr);
}

static void X64__emit_instr_sub_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr sub_ri_instr = {
        .flags = X64_Instr_Kind_SUB_RI,
        .sub_ri.size = size,
        .sub_ri.dst = dst,
        .sub_ri.imm = imm,
    };

    array_push(*instrs, sub_ri_instr);
}

static void X64__emit_instr_sub_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr sub_mi_instr = {
        .flags = X64_Instr_Kind_SUB_MI,
        .sub_mi.size = size,
        .sub_mi.dst = dst,
        .sub_mi.imm = imm,
    };

    array_push(*instrs, sub_mi_instr);
}

static void X64__emit_instr_imul_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr imul_rr_instr = {
        .flags = X64_Instr_Kind_IMUL_RR,
        .imul_rr.size = size,
        .imul_rr.dst = dst,
        .imul_rr.src = src,
    };

    array_push(*instrs, imul_rr_instr);
}

static void X64__emit_instr_imul_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr imul_rm_instr = {
        .flags = X64_Instr_Kind_IMUL_RM,
        .imul_rm.size = size,
        .imul_rm.dst = dst,
        .imul_rm.src = src,
    };

    array_push(*instrs, imul_rm_instr);
}

static void X64__emit_instr_imul_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr imul_mr_instr = {
        .flags = X64_Instr_Kind_IMUL_MR,
        .imul_mr.size = size,
        .imul_mr.dst = dst,
        .imul_mr.src = src,
    };

    array_push(*instrs, imul_mr_instr);
}

static void X64__emit_instr_imul_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr imul_ri_instr = {
        .flags = X64_Instr_Kind_IMUL_RI,
        .imul_ri.size = size,
        .imul_ri.dst = dst,
        .imul_ri.imm = imm,
    };

    array_push(*instrs, imul_ri_instr);
}

static void X64__emit_instr_imul_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr imul_mi_instr = {
        .flags = X64_Instr_Kind_IMUL_MI,
        .imul_mi.size = size,
        .imul_mi.dst = dst,
        .imul_mi.imm = imm,
    };

    array_push(*instrs, imul_mi_instr);
}

static void X64__emit_instr_and_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr and_rr_instr = {
        .flags = X64_Instr_Kind_AND_RR,
        .and_rr.size = size,
        .and_rr.dst = dst,
        .and_rr.src = src,
    };

    array_push(*instrs, and_rr_instr);
}

static void X64__emit_instr_and_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr and_rm_instr = {
        .flags = X64_Instr_Kind_AND_RM,
        .and_rm.size = size,
        .and_rm.dst = dst,
        .and_rm.src = src,
    };

    array_push(*instrs, and_rm_instr);
}

static void X64__emit_instr_and_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr and_mr_instr = {
        .flags = X64_Instr_Kind_AND_MR,
        .and_mr.size = size,
        .and_mr.dst = dst,
        .and_mr.src = src,
    };

    array_push(*instrs, and_mr_instr);
}

static void X64__emit_instr_and_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr and_ri_instr = {
        .flags = X64_Instr_Kind_AND_RI,
        .and_ri.size = size,
        .and_ri.dst = dst,
        .and_ri.imm = imm,
    };

    array_push(*instrs, and_ri_instr);
}

static void X64__emit_instr_and_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr and_mi_instr = {
        .flags = X64_Instr_Kind_AND_MI,
        .and_mi.size = size,
        .and_mi.dst = dst,
        .and_mi.imm = imm,
    };

    array_push(*instrs, and_mi_instr);
}

static void X64__emit_instr_or_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr or_rr_instr = {
        .flags = X64_Instr_Kind_OR_RR,
        .or_rr.size = size,
        .or_rr.dst = dst,
        .or_rr.src = src,
    };

    array_push(*instrs, or_rr_instr);
}

static void X64__emit_instr_or_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr or_rm_instr = {
        .flags = X64_Instr_Kind_OR_RM,
        .or_rm.size = size,
        .or_rm.dst = dst,
        .or_rm.src = src,
    };

    array_push(*instrs, or_rm_instr);
}

static void X64__emit_instr_or_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr or_mr_instr = {
        .flags = X64_Instr_Kind_OR_MR,
        .or_mr.size = size,
        .or_mr.dst = dst,
        .or_mr.src = src,
    };

    array_push(*instrs, or_mr_instr);
}

static void X64__emit_instr_or_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr or_ri_instr = {
        .flags = X64_Instr_Kind_OR_RI,
        .or_ri.size = size,
        .or_ri.dst = dst,
        .or_ri.imm = imm,
    };

    array_push(*instrs, or_ri_instr);
}

static void X64__emit_instr_or_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr or_mi_instr = {
        .flags = X64_Instr_Kind_OR_MI,
        .or_mi.size = size,
        .or_mi.dst = dst,
        .or_mi.imm = imm,
    };

    array_push(*instrs, or_mi_instr);
}

static void X64__emit_instr_xor_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr xor_rr_instr = {
        .flags = X64_Instr_Kind_XOR_RR,
        .xor_rr.size = size,
        .xor_rr.dst = dst,
        .xor_rr.src = src,
    };

    array_push(*instrs, xor_rr_instr);
}

static void X64__emit_instr_xor_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr xor_rm_instr = {
        .flags = X64_Instr_Kind_XOR_RM,
        .xor_rm.size = size,
        .xor_rm.dst = dst,
        .xor_rm.src = src,
    };

    array_push(*instrs, xor_rm_instr);
}

static void X64__emit_instr_xor_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr xor_mr_instr = {
        .flags = X64_Instr_Kind_XOR_MR,
        .xor_mr.size = size,
        .xor_mr.dst = dst,
        .xor_mr.src = src,
    };

    array_push(*instrs, xor_mr_instr);
}

static void X64__emit_instr_xor_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr xor_ri_instr = {
        .flags = X64_Instr_Kind_XOR_RI,
        .xor_ri.size = size,
        .xor_ri.dst = dst,
        .xor_ri.imm = imm,
    };

    array_push(*instrs, xor_ri_instr);
}

static void X64__emit_instr_xor_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr xor_mi_instr = {
        .flags = X64_Instr_Kind_XOR_MI,
        .xor_mi.size = size,
        .xor_mi.dst = dst,
        .xor_mi.imm = imm,
    };

    array_push(*instrs, xor_mi_instr);
}

static void X64__emit_instr_add_flt_rr(Array(X64__Instr) * instrs, FloatKind kind, X64_Reg dst, X64_Reg src)
{
    X64__Instr add_flt_rr_instr = {
        .flags = X64_Instr_Kind_ADD_FLT_RR,
        .add_flt_rr.kind = kind,
        .add_flt_rr.dst = dst,
        .add_flt_rr.src = src,
    };

    array_push(*instrs, add_flt_rr_instr);
}

static void X64__emit_instr_add_flt_rm(Array(X64__Instr) * instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr add_flt_rm_instr = {
        .flags = X64_Instr_Kind_ADD_FLT_RM,
        .add_flt_rm.kind = kind,
        .add_flt_rm.dst = dst,
        .add_flt_rm.src = src,
    };

    array_push(*instrs, add_flt_rm_instr);
}

static void X64__emit_instr_add_flt_mr(Array(X64__Instr) * instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr add_flt_mr_instr = {
        .flags = X64_Instr_Kind_ADD_FLT_MR,
        .add_flt_mr.kind = kind,
        .add_flt_mr.dst = dst,
        .add_flt_mr.src = src,
    };

    array_push(*instrs, add_flt_mr_instr);
}

static void X64__emit_instr_neg_r(Array(X64__Instr) * instrs, u8 size, X64_Reg dst)
{
    X64__Instr neg_r_instr = {
        .flags = X64_Instr_Kind_NEG_R,
        .neg_r.size = size,
        .neg_r.dst = dst,
    };

    array_push(*instrs, neg_r_instr);
}

static void X64__emit_instr_neg_m(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst)
{
    X64__Instr neg_m_instr = {
        .flags = X64_Instr_Kind_NEG_M,
        .neg_m.size = size,
        .neg_m.dst = dst,
    };

    array_push(*instrs, neg_m_instr);
}

static void X64__emit_instr_not_r(Array(X64__Instr) * instrs, u8 size, X64_Reg dst)
{
    X64__Instr not_r_instr = {
        .flags = X64_Instr_Kind_NOT_R,
        .not_r.size = size,
        .not_r.dst = dst,
    };

    array_push(*instrs, not_r_instr);
}

static void X64__emit_instr_not_m(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst)
{
    X64__Instr not_m_instr = {
        .flags = X64_Instr_Kind_NOT_M,
        .not_m.size = size,
        .not_m.dst = dst,
    };

    array_push(*instrs, not_m_instr);
}

static void X64__emit_instr_sar_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst)
{
    X64__Instr instr = {
        .flags = X64_Instr_Kind_SAR_RR,
        .sar_rr.size = size,
        .sar_rr.dst = dst,
    };

    array_push(*instrs, instr);
}

static void X64__emit_instr_sar_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst)
{
    X64__Instr instr = {
        .flags = X64_Instr_Kind_SAR_MR,
        .sar_mr.size = size,
        .sar_mr.dst = dst,
    };

    array_push(*instrs, instr);
}

static void X64__emit_instr_sar_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u8 imm)
{
    X64__Instr sar_ri_instr = {
        .flags = X64_Instr_Kind_SAR_RI,
        .sar_ri.size = size,
        .sar_ri.dst = dst,
        .sar_ri.imm = imm,
    };

    array_push(*instrs, sar_ri_instr);
}

static void X64__emit_instr_sar_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u8 imm)
{
    X64__Instr sar_mi_instr = {
        .flags = X64_Instr_Kind_SAR_MI,
        .sar_mi.size = size,
        .sar_mi.dst = dst,
        .sar_mi.imm = imm,
    };

    array_push(*instrs, sar_mi_instr);
}

static void X64__emit_instr_shl_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst)
{
    X64__Instr instr = {
        .flags = X64_Instr_Kind_SHL_RR,
        .shl_rr.size = size,
        .shl_rr.dst = dst,
    };

    array_push(*instrs, instr);
}

static void X64__emit_instr_shl_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst)
{
    X64__Instr instr = {
        .flags = X64_Instr_Kind_SHL_MR,
        .shl_mr.size = size,
        .shl_mr.dst = dst,
    };

    array_push(*instrs, instr);
}

static void X64__emit_instr_shl_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u8 imm)
{
    X64__Instr shl_ri_instr = {
        .flags = X64_Instr_Kind_SHL_RI,
        .shl_ri.size = size,
        .shl_ri.dst = dst,
        .shl_ri.imm = imm,
    };

    array_push(*instrs, shl_ri_instr);
}

static void X64__emit_instr_shl_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u8 imm)
{
    X64__Instr shl_mi_instr = {
        .flags = X64_Instr_Kind_SHL_MI,
        .shl_mi.size = size,
        .shl_mi.dst = dst,
        .shl_mi.imm = imm,
    };

    array_push(*instrs, shl_mi_instr);
}

static void X64__emit_instr_div_r(Array(X64__Instr) * instrs, u8 size, X64_Reg src)
{
    X64__Instr div_r_instr = {
        .flags = X64_Instr_Kind_DIV_R,
        .div_r.size = size,
        .div_r.src = src,
    };

    array_push(*instrs, div_r_instr);
}

static void X64__emit_instr_div_m(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr src)
{
    X64__Instr div_m_instr = {
        .flags = X64_Instr_Kind_DIV_M,
        .div_m.size = size,
        .div_m.src = src,
    };

    array_push(*instrs, div_m_instr);
}

static void X64__emit_instr_idiv_r(Array(X64__Instr) * instrs, u8 size, X64_Reg src)
{
    X64__Instr idiv_r_instr = {
        .flags = X64_Instr_Kind_IDIV_R,
        .idiv_r.size = size,
        .idiv_r.src = src,
    };

    array_push(*instrs, idiv_r_instr);
}

static void X64__emit_instr_idiv_m(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr src)
{
    X64__Instr idiv_m_instr = {
        .flags = X64_Instr_Kind_IDIV_M,
        .idiv_m.size = size,
        .idiv_m.src = src,
    };

    array_push(*instrs, idiv_m_instr);
}

static void X64__emit_instr_sext_ax_into_dx(Array(X64__Instr)* instrs, u8 size)
{
    X64_Instr_Kind kind = X64_Instr_Kind_NOOP;

    switch (size) {
    case 2:
        kind = X64_Instr_Kind_CWD;
        break;
    case 4:
        kind = X64_Instr_Kind_CDQ;
        break;
    case 8:
        kind = X64_Instr_Kind_CQO;
        break;
    default:
        NIBBLE_FATAL_EXIT("Unhandled size %d for X64__emit_instr_sext_ax_into_dx()", size);
        break;
    }

    X64__Instr instr = {.flags = kind};
    array_push(*instrs, instr);
}

static void X64__emit_instr_mov_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr mov_rr_instr = {
        .flags = X64_Instr_Kind_MOV_RR,
        .mov_rr.size = size,
        .mov_rr.dst = dst,
        .mov_rr.src = src,
    };

    array_push(*instrs, mov_rr_instr);
}

static void X64__emit_instr_mov_rrh(Array(X64__Instr) * instrs, X64_Reg dst, X64_Reg src)
{
    X64__Instr mov_rr_instr = {
        .flags = X64_INSTR_MOV_SRC_RH_MASK | (X64_Instr_Kind_MOV_RR & X64_INSTR_KIND_MASK),
        .mov_rr.size = 1,
        .mov_rr.dst = dst,
        .mov_rr.src = src,
    };

    array_push(*instrs, mov_rr_instr);
}

static void X64__emit_instr_mov_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr mov_rm_instr = {
        .flags = X64_Instr_Kind_MOV_RM,
        .mov_rm.size = size,
        .mov_rm.dst = dst,
        .mov_rm.src = src,
    };

    array_push(*instrs, mov_rm_instr);
}

static void X64__emit_instr_mov_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr mov_mr_instr = {
        .flags = X64_Instr_Kind_MOV_MR,
        .mov_mr.size = size,
        .mov_mr.dst = dst,
        .mov_mr.src = src,
    };

    array_push(*instrs, mov_mr_instr);
}

static void X64__emit_instr_mov_mrh(Array(X64__Instr) * instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr mov_mr_instr = {
        .flags = X64_INSTR_MOV_SRC_RH_MASK | (X64_Instr_Kind_MOV_MR & X64_INSTR_KIND_MASK),
        .mov_mr.size = 1,
        .mov_mr.dst = dst,
        .mov_mr.src = src,
    };

    array_push(*instrs, mov_mr_instr);
}

static void X64__emit_instr_mov_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u64 imm)
{
    X64__Instr mov_ri_instr = {
        .flags = X64_Instr_Kind_MOV_RI,
        .mov_ri.size = size,
        .mov_ri.dst = dst,
        .mov_ri.imm = imm,
    };

    array_push(*instrs, mov_ri_instr);
}

static void X64__emit_instr_mov_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr mov_mi_instr = {
        .flags = X64_Instr_Kind_MOV_MI,
        .mov_mi.size = size,
        .mov_mi.dst = dst,
        .mov_mi.imm = imm,
    };

    array_push(*instrs, mov_mi_instr);
}

static void X64__emit_instr_movsx_rr(Array(X64__Instr) * instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src)
{
    X64__Instr movsx_rr_instr = {
        .flags = X64_Instr_Kind_MOVSX_RR,
        .movsx_rr.dst_size = dst_size,
        .movsx_rr.src_size = src_size,
        .movsx_rr.dst = dst,
        .movsx_rr.src = src,
    };

    array_push(*instrs, movsx_rr_instr);
}

static void X64__emit_instr_movsx_rm(Array(X64__Instr) * instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src)
{
    X64__Instr movsx_rm_instr = {
        .flags = X64_Instr_Kind_MOVSX_RM,
        .movsx_rm.dst_size = dst_size,
        .movsx_rm.src_size = src_size,
        .movsx_rm.dst = dst,
        .movsx_rm.src = src,
    };

    array_push(*instrs, movsx_rm_instr);
}

static void X64__emit_instr_movsxd_rr(Array(X64__Instr) * instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src)
{
    X64__Instr movsxd_rr_instr = {
        .flags = X64_Instr_Kind_MOVSXD_RR,
        .movsxd_rr.dst_size = dst_size,
        .movsxd_rr.src_size = src_size,
        .movsxd_rr.dst = dst,
        .movsxd_rr.src = src,
    };

    array_push(*instrs, movsxd_rr_instr);
}

static void X64__emit_instr_movsxd_rm(Array(X64__Instr) * instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src)
{
    X64__Instr movsxd_rm_instr = {
        .flags = X64_Instr_Kind_MOVSXD_RM,
        .movsxd_rm.dst_size = dst_size,
        .movsxd_rm.src_size = src_size,
        .movsxd_rm.dst = dst,
        .movsxd_rm.src = src,
    };

    array_push(*instrs, movsxd_rm_instr);
}

static void X64__emit_instr_movzx_rr(Array(X64__Instr) * instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src)
{
    X64__Instr movzx_rr_instr = {
        .flags = X64_Instr_Kind_MOVZX_RR,
        .movzx_rr.dst_size = dst_size,
        .movzx_rr.src_size = src_size,
        .movzx_rr.dst = dst,
        .movzx_rr.src = src,
    };

    array_push(*instrs, movzx_rr_instr);
}

static void X64__emit_instr_movzx_rm(Array(X64__Instr) * instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src)
{
    X64__Instr movzx_rm_instr = {
        .flags = X64_Instr_Kind_MOVZX_RM,
        .movzx_rm.dst_size = dst_size,
        .movzx_rm.src_size = src_size,
        .movzx_rm.dst = dst,
        .movzx_rm.src = src,
    };

    array_push(*instrs, movzx_rm_instr);
}

static void X64__emit_instr_mov_flt_rr(Array(X64__Instr) * instrs, FloatKind kind, X64_Reg dst, X64_Reg src)
{
    X64__Instr mov_flt_rr_instr = {
        .flags = X64_Instr_Kind_MOV_FLT_RR,
        .mov_flt_rr.kind = kind,
        .mov_flt_rr.dst = dst,
        .mov_flt_rr.src = src,
    };

    array_push(*instrs, mov_flt_rr_instr);
}

static void X64__emit_instr_mov_flt_mr(Array(X64__Instr) * instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr mov_flt_mr_instr = {
        .flags = X64_Instr_Kind_MOV_FLT_MR,
        .mov_flt_mr.kind = kind,
        .mov_flt_mr.dst = dst,
        .mov_flt_mr.src = src,
    };

    array_push(*instrs, mov_flt_mr_instr);
}

static void X64__emit_instr_mov_flt_rm(Array(X64__Instr) * instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr mov_flt_rm_instr = {
        .flags = X64_Instr_Kind_MOV_FLT_RM,
        .mov_flt_rm.kind = kind,
        .mov_flt_rm.dst = dst,
        .mov_flt_rm.src = src,
    };

    array_push(*instrs, mov_flt_rm_instr);
}

static void X64__emit_instr_movdqu_mr(Array(X64__Instr) * instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    assert(x64_reg_classes[src] == X64_REG_CLASS_FLOAT);
    X64__Instr movdqu_mr_instr = {
        .flags = X64_Instr_Kind_MOVDQU_MR,
        .movdqu_mr.dst = dst,
        .movdqu_mr.src = src,
    };

    array_push(*instrs, movdqu_mr_instr);
}

static void X64__emit_instr_movdqu_rm(Array(X64__Instr) * instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    assert(x64_reg_classes[dst] == X64_REG_CLASS_FLOAT);
    X64__Instr movdqu_rm_instr = {
        .flags = X64_Instr_Kind_MOVDQU_RM,
        .movdqu_rm.dst = dst,
        .movdqu_rm.src = src,
    };

    array_push(*instrs, movdqu_rm_instr);
}

static void X64__emit_instr_cmp_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr cmp_rr_instr = {
        .flags = X64_Instr_Kind_CMP_RR,
        .cmp_rr.size = size,
        .cmp_rr.dst = dst,
        .cmp_rr.src = src,
    };

    array_push(*instrs, cmp_rr_instr);
}

static void X64__emit_instr_cmp_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr cmp_rm_instr = {
        .flags = X64_Instr_Kind_CMP_RM,
        .cmp_rm.size = size,
        .cmp_rm.dst = dst,
        .cmp_rm.src = src,
    };

    array_push(*instrs, cmp_rm_instr);
}

static void X64__emit_instr_cmp_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr cmp_mr_instr = {
        .flags = X64_Instr_Kind_CMP_MR,
        .cmp_mr.size = size,
        .cmp_mr.dst = dst,
        .cmp_mr.src = src,
    };

    array_push(*instrs, cmp_mr_instr);
}

static void X64__emit_instr_cmp_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr cmp_ri_instr = {
        .flags = X64_Instr_Kind_CMP_RI,
        .cmp_ri.size = size,
        .cmp_ri.dst = dst,
        .cmp_ri.imm = imm,
    };

    array_push(*instrs, cmp_ri_instr);
}

static void X64__emit_instr_cmp_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr cmp_mi_instr = {
        .flags = X64_Instr_Kind_CMP_MI,
        .cmp_mi.size = size,
        .cmp_mi.dst = dst,
        .cmp_mi.imm = imm,
    };

    array_push(*instrs, cmp_mi_instr);
}

static void X64__emit_instr_lea(Array(X64__Instr) * instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr lea_instr = {
        .flags = X64_Instr_Kind_LEA,
        .lea.dst = dst,
        .lea.src = src,
    };

    array_push(*instrs, lea_instr);
}

static void X64__emit_instr_rep_movsb(Array(X64__Instr)* instrs)
{
    X64__Instr instr = {.flags = X64_Instr_Kind_REP_MOVSB};
    array_push(*instrs, instr);
}

static void X64__emit_instr_rep_stosb(Array(X64__Instr)* instrs)
{
    X64__Instr instr = {.flags = X64_Instr_Kind_REP_STOSB};
    array_push(*instrs, instr);
}

static void X64__emit_instr_syscall(Array(X64__Instr)* instrs)
{
    X64__Instr instr = {.flags = X64_Instr_Kind_SYSCALL};
    array_push(*instrs, instr);
}

static size_t X64__emit_instr_placeholder(Array(X64__Instr) * instrs, X64_Instr_Kind kind)
{
    X64__Instr instr = {.flags = kind};
    array_push(*instrs, instr);
    return array_len(*instrs) - 1;
}

static void X64__push_reg_to_stack(Array(X64__Instr)* instrs, X64_Reg reg)
{
    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        X64__emit_instr_push(instrs, reg);
    }
    else {
        X64__emit_instr_sub_ri(instrs, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Make room for 16 bytes on the stack.
        X64__emit_instr_movdqu_mr(instrs, X64__get_rsp_offset_addr(0), reg); // movdqu oword [rsp], reg
    }
}

static void X64__pop_reg_from_stack(Array(X64__Instr)* instrs, X64_Reg reg)
{
    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        X64__emit_instr_pop(instrs, reg);
    }
    else {
        X64__emit_instr_movdqu_rm(instrs, reg, X64__get_rsp_offset_addr(0)); // movdqu reg, oword [rsp]
        X64__emit_instr_add_ri(instrs, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Clean up 16 bytes from stack.
    }
}

static void X64__load_prim_from_mem(Array(X64__Instr)* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    const X64_RegClass reg_class = x64_reg_classes[dst];

    if (reg_class == X64_REG_CLASS_INT) {
        X64__emit_instr_mov_rm(instrs, size, dst, src);
    }
    else {
        assert(reg_class == X64_REG_CLASS_FLOAT);
        X64__emit_instr_mov_flt_rm(instrs, size == 8 ? FLOAT_F64 : FLOAT_F32, dst, src);
    }
}

static void X64__save_prim_to_mem(Array(X64__Instr)* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    const X64_RegClass reg_class = x64_reg_classes[src];
    if (reg_class == X64_REG_CLASS_INT) {
        X64__emit_instr_mov_mr(instrs, size, dst, src);
    }
    else {
        assert(reg_class == X64_REG_CLASS_FLOAT);
        X64__emit_instr_mov_flt_mr(instrs, size == 8 ? FLOAT_F64 : FLOAT_F32, dst, src);
    }
}

typedef void X64_Emit_Bin_Int_RR_Func (Array(X64__Instr)* instrs, u8 size, X64_Reg dst, X64_Reg src);
typedef void X64_Emit_Bin_Int_RM_Func (Array(X64__Instr)* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
typedef void X64_Emit_Bin_Int_RI_Func (Array(X64__Instr)* instrs, u8 size, X64_Reg dst, u32 imm);
typedef void X64_Emit_Bin_Int_MR_Func (Array(X64__Instr)* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
typedef void X64_Emit_Bin_Int_MI_Func (Array(X64__Instr)* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);
typedef void X64_Emit_Mov_Ext_RR_Func (Array(X64__Instr)* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src);
typedef void X64_Emit_Mov_Ext_RM_Func (Array(X64__Instr)* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src);

static inline X64_Emit_Bin_Int_RR_Func* x64_bin_int_rr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_RR: return X64__emit_instr_add_rr;
    case X64_Instr_Kind_SUB_RR: return X64__emit_instr_sub_rr;
    case X64_Instr_Kind_IMUL_RR: return X64__emit_instr_imul_rr;
    case X64_Instr_Kind_AND_RR: return X64__emit_instr_and_rr;
    case X64_Instr_Kind_OR_RR: return X64__emit_instr_or_rr;
    case X64_Instr_Kind_XOR_RR: return X64__emit_instr_xor_rr;
    case X64_Instr_Kind_MOV_RR: return X64__emit_instr_mov_rr;
    case X64_Instr_Kind_CMP_RR: return X64__emit_instr_cmp_rr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_rr_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Int_RM_Func* x64_bin_int_rm_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_RM: return X64__emit_instr_add_rm;
    case X64_Instr_Kind_SUB_RM: return X64__emit_instr_sub_rm;
    case X64_Instr_Kind_IMUL_RM: return X64__emit_instr_imul_rm;
    case X64_Instr_Kind_AND_RM: return X64__emit_instr_and_rm;
    case X64_Instr_Kind_OR_RM: return X64__emit_instr_or_rm;
    case X64_Instr_Kind_XOR_RM: return X64__emit_instr_xor_rm;
    case X64_Instr_Kind_MOV_RM: return X64__emit_instr_mov_rm;
    case X64_Instr_Kind_CMP_RM: return X64__emit_instr_cmp_rm;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_rm_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Int_RI_Func* x64_bin_int_ri_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_RI: return X64__emit_instr_add_ri;
    case X64_Instr_Kind_SUB_RI: return X64__emit_instr_sub_ri;
    case X64_Instr_Kind_IMUL_RI: return X64__emit_instr_imul_ri;
    case X64_Instr_Kind_AND_RI: return X64__emit_instr_and_ri;
    case X64_Instr_Kind_OR_RI: return X64__emit_instr_or_ri;
    case X64_Instr_Kind_XOR_RI: return X64__emit_instr_xor_ri;
    case X64_Instr_Kind_CMP_RI: return X64__emit_instr_cmp_ri;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_ri_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Int_MR_Func* x64_bin_int_mr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_MR: return X64__emit_instr_add_mr;
    case X64_Instr_Kind_SUB_MR: return X64__emit_instr_sub_mr;
    case X64_Instr_Kind_IMUL_MR: return X64__emit_instr_imul_mr;
    case X64_Instr_Kind_AND_MR: return X64__emit_instr_and_mr;
    case X64_Instr_Kind_OR_MR: return X64__emit_instr_or_mr;
    case X64_Instr_Kind_XOR_MR: return X64__emit_instr_xor_mr;
    case X64_Instr_Kind_MOV_MR: return X64__emit_instr_mov_mr;
    case X64_Instr_Kind_CMP_MR: return X64__emit_instr_cmp_mr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_mr_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Int_MI_Func* x64_bin_int_mi_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_MI: return X64__emit_instr_add_mi;
    case X64_Instr_Kind_SUB_MI: return X64__emit_instr_sub_mi;
    case X64_Instr_Kind_IMUL_MI: return X64__emit_instr_imul_mi;
    case X64_Instr_Kind_AND_MI: return X64__emit_instr_and_mi;
    case X64_Instr_Kind_OR_MI: return X64__emit_instr_or_mi;
    case X64_Instr_Kind_XOR_MI: return X64__emit_instr_xor_mi;
    case X64_Instr_Kind_MOV_MI: return X64__emit_instr_mov_mi;
    case X64_Instr_Kind_CMP_MI: return X64__emit_instr_cmp_mi;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_mi_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Mov_Ext_RR_Func* x64_mov_ext_rr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_MOVSX_RR: return X64__emit_instr_movsx_rr;
    case X64_Instr_Kind_MOVSXD_RR: return X64__emit_instr_movsxd_rr;
    case X64_Instr_Kind_MOVZX_RR: return X64__emit_instr_movzx_rr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_mov_ext_rr_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Mov_Ext_RM_Func* x64_mov_ext_rm_emit_funcs(X64_Instr_Kind kind) {
    switch (kind) {
    case X64_Instr_Kind_MOVSX_RM: return X64__emit_instr_movsx_rm;
    case X64_Instr_Kind_MOVSXD_RM: return X64__emit_instr_movsxd_rm;
    case X64_Instr_Kind_MOVZX_RM: return X64__emit_instr_movzx_rm;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_mov_ext_rm_emit_funcs()", kind);
        return NULL;
    }
}

typedef void X64_Emit_Bin_Flt_RR_Func (Array(X64__Instr)* instrs, FloatKind kind, X64_Reg dst, X64_Reg src);
typedef void X64_Emit_Bin_Flt_RM_Func (Array(X64__Instr)* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);
typedef void X64_Emit_Bin_Flt_MR_Func (Array(X64__Instr)* instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src);

static inline X64_Emit_Bin_Flt_RR_Func* x64_bin_flt_rr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_FLT_RR: return X64__emit_instr_add_flt_rr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_flt_rr_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Flt_RM_Func* x64_bin_flt_rm_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_FLT_RM: return X64__emit_instr_add_flt_rm;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_flt_rm_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Flt_MR_Func* x64_bin_flt_mr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_FLT_MR: return X64__emit_instr_add_flt_mr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_flt_mr_emit_funcs()", kind);
        return NULL;
    }
}

typedef struct X64_Proc_State {
    Allocator* gen_mem;
    Allocator* tmp_mem;
    Symbol* sym; // Procedure symbol.
    X64_LIRBuilder* builder;
    X64_ScratchRegs (*scratch_regs)[X64_REG_CLASS_COUNT];
    Array(X64__Instr) instrs;
} X64_Proc_State;

#define IS_LREG_IN_REG(k) ((k) == X64_LREG_LOC_REG)
#define IS_LREG_IN_STACK(k) ((k) == X64_LREG_LOC_STACK)
static X64_LRegLoc X64__lreg_loc(X64_Proc_State* proc_state, u32 lreg)
{
    u32 rng_idx = X64_find_alias_reg(proc_state->builder, lreg);
    X64_LRegLoc reg_loc = proc_state->builder->lreg_ranges[rng_idx].loc;

    assert(reg_loc.kind != X64_LREG_LOC_UNASSIGNED);

    return reg_loc;
}

typedef struct X64_Tmp_Reg {
    X64_Reg reg;
    s32 offset;
    u64 size;
    bool store;
    struct X64_Tmp_Reg* next;
} X64_Tmp_Reg;

typedef struct X64_Reg_Group {
    X64_Proc_State* proc_state;
    u32 num_tmp_regs;
    X64_Tmp_Reg* first_tmp_reg;
    u32 used_tmp_reg_mask;
} X64_Reg_Group;

static X64_Reg_Group X64__begin_reg_group(X64_Proc_State* proc_state)
{
    X64_Reg_Group group = {
        .proc_state = proc_state,
    };

    return group;
}

static X64_Reg X64__get_reg(X64_Reg_Group* group, X64_RegClass reg_class, u32 lreg, u32 size, bool store, u32 banned_regs)
{
    X64_LRegLoc lreg_loc = X64__lreg_loc(group->proc_state, lreg);

    // If this virtual register was not spilled during allocation, just return its assigned
    // physical register.
    if (IS_LREG_IN_REG(lreg_loc.kind)) {
        return lreg_loc.reg;
    }

    // This virtual register was spilled during allocation, so use a temporary physical register which will have
    // to be restored later.

    X64_Proc_State* proc_state = group->proc_state;
    X64_ScratchRegs* x64_scratch_regs = &(*proc_state->scratch_regs)[reg_class];
    u32 num_scratch_regs = x64_scratch_regs->num_regs;
    X64_Reg* scratch_regs = x64_scratch_regs->regs;

    assert(IS_LREG_IN_STACK(lreg_loc.kind));
    assert(group->num_tmp_regs < num_scratch_regs);

    X64_Reg x64_reg = X64_REG_COUNT;

    // Try to use a scratch register that is not currently being used as a tmp register and is not banned.
    for (u32 r = 0; r < num_scratch_regs; r += 1) {
        X64_Reg reg = scratch_regs[r];

        bool is_used_as_tmp = u32_is_bit_set(group->used_tmp_reg_mask, reg);
        bool is_banned = u32_is_bit_set(banned_regs, reg);

        if (!is_used_as_tmp && !is_banned) {
            x64_reg = reg;
            break;
        }
    }

    assert(x64_reg != X64_REG_COUNT);
    assert(reg_class == x64_reg_classes[x64_reg]);

    // Record register in group
    group->num_tmp_regs += 1;
    u32_set_bit(&group->used_tmp_reg_mask, x64_reg);

    Allocator* tmp_mem = proc_state->tmp_mem;

    X64_Tmp_Reg* tmp_reg = alloc_type(tmp_mem, X64_Tmp_Reg, true);
    tmp_reg->reg = x64_reg;
    tmp_reg->offset = lreg_loc.offset;
    tmp_reg->size = size;
    tmp_reg->store = store;

    X64__push_reg_to_stack(&proc_state->instrs, tmp_reg->reg);
    X64__load_prim_from_mem(&proc_state->instrs, size, tmp_reg->reg, X64__get_rbp_offset_addr(lreg_loc.offset));

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    return tmp_reg->reg;
}

static void X64__save_reg_to_group(X64_Reg_Group* group, X64_Reg reg)
{
    assert(reg != X64_REG_COUNT);
    assert(!u32_is_bit_set(group->used_tmp_reg_mask, reg));

    Allocator* tmp_mem = group->proc_state->tmp_mem;

    X64_Tmp_Reg* tmp_reg = alloc_type(tmp_mem, X64_Tmp_Reg, true);
    tmp_reg->reg = reg;

    X64__push_reg_to_stack(&group->proc_state->instrs, tmp_reg->reg);

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    // Record register in group
    u32_set_bit(&group->used_tmp_reg_mask, tmp_reg->reg);

    group->num_tmp_regs += 1;
}

static void X64__end_reg_group(X64_Reg_Group* group)
{
    if (!group->num_tmp_regs)
        return;

    X64_Proc_State* proc_state = group->proc_state;

    X64_Tmp_Reg* it = group->first_tmp_reg;

    while (it) {
        if (it->store) {
            // Save to memory if the temporary register was holding a value for a variable that lives on the stack.
            X64__save_prim_to_mem(&proc_state->instrs, it->size, X64__get_rbp_offset_addr(it->offset), it->reg);
        }

        // Restore temporary register's original value.
        X64__pop_reg_from_stack(&proc_state->instrs, it->reg);
        it = it->next;
    }
}

typedef struct X64_Stack_Params_Info {
    u64 stack_spill_size; // Spill size below rsp
    List* local_var_iter; // Iterator pointing to the first local variable (if any) of the proc
} X64_Stack_Params_Info;

typedef struct X64_Stack_Spill_State {
    u64 stack_spill_size;
    u64 stack_arg_offset;
} X64_Stack_Spill_State;

static s32 X64__consume_stack_arg(u64* stack_arg_offset, u64 arg_size, u64 arg_align)
{
    s32 offset = (s32)*stack_arg_offset;

    *stack_arg_offset += arg_size;
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, arg_align);
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, X64_STACK_WORD_SIZE);

    return offset;
}

static s32 X64__spill_reg(Array(X64__Instr) * instrs, X64_Stack_Spill_State* state, u64 size, u64 align, X64_Reg preg)
{
    state->stack_spill_size += size;
    state->stack_spill_size = ALIGN_UP(state->stack_spill_size, align);
    s32 offset = -state->stack_spill_size;

    X64__save_prim_to_mem(instrs, size, X64__get_rbp_offset_addr(offset), preg);

    return offset;
}

static void X64__assign_proc_param_offsets(Array(X64__Instr) * instrs, const Symbol* sproc, X64_Stack_Params_Info* stack_params_info)
{
    const DeclProc* dproc = (const DeclProc*)sproc->decl;
    const Type* ret_type = sproc->type->as_proc.ret;

    u32 index = 0;
    u32 arg_reg_indices[X64_REG_CLASS_COUNT] = {0};
    X64_Stack_Spill_State state = {.stack_arg_offset = 0x10};

    // For procs that return a large struct by value:
    // Spill the first argument, which contains a pointer to the return value's memory address, into the stack.
    // We need to spill (remember) this address so that the procedure can return it, as per the X64 calling conventions.
    if (type_is_obj_like(ret_type) && X64_is_obj_retarg_large(ret_type->size)) {
        X64_ScratchRegs arg_int_regs = (*x64_target.arg_regs)[X64_REG_CLASS_INT];

        X64__spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, X64_MAX_INT_REG_SIZE,
                       arg_int_regs.regs[arg_reg_indices[X64_REG_CLASS_INT]]);
        arg_reg_indices[X64_REG_CLASS_INT] += 1;
    }

    Scope* scope = dproc->scope;
    List* head = &scope->sym_list;
    List* it = head->next;

    while (it != head) {
        // Only process params. Local variables are not processed here.
        if (index >= dproc->num_params)
            break;

        Symbol* sym = list_entry(it, Symbol, lnode);

        // Assign stack offsets to procedure params.
        assert(sym->kind == SYMBOL_VAR);

        Type* arg_type = sym->type;
        u64 arg_size = arg_type->size;
        u64 arg_align = arg_type->align;

        if (type_is_obj_like(arg_type)) {
            X64_RegClass reg_class = X64_obj_reg_class(arg_type);
            X64_ScratchRegs arg_regs = (*x64_target.arg_regs)[reg_class];
            u32* arg_reg_index = &arg_reg_indices[reg_class];

            u32 rem_regs = arg_regs.num_regs - *arg_reg_index;

            if ((arg_size <= X64_MAX_INT_REG_SIZE) && (rem_regs >= 1)) {
                X64_Reg arg_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                sym->as_var.offset = X64__spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, arg_reg);
            }
            else if ((arg_size <= (X64_MAX_INT_REG_SIZE << 1)) && (rem_regs >= 2)) {
                X64_Reg low_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;
                X64_Reg high_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                X64__spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, high_reg);
                sym->as_var.offset = X64__spill_reg(instrs, &state, X64_MAX_INT_REG_SIZE, arg_align, low_reg);
            }
            else {
                sym->as_var.offset = X64__consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }
        else {
            X64_RegClass reg_class = arg_type->kind == TYPE_FLOAT ? X64_REG_CLASS_FLOAT : X64_REG_CLASS_INT;
            X64_ScratchRegs arg_regs = (*x64_target.arg_regs)[reg_class];
            u32* arg_reg_index = &arg_reg_indices[reg_class];

            // Spill argument register below rsp
            if (*arg_reg_index < arg_regs.num_regs) {
                X64_Reg arg_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                sym->as_var.offset = X64__spill_reg(instrs, &state, arg_size, arg_align, arg_reg);
            }
            else {
                sym->as_var.offset = X64__consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }

        index += 1;
        it = it->next;
    }

    stack_params_info->stack_spill_size = state.stack_spill_size;
    stack_params_info->local_var_iter = it;
}

static u64 X64__assign_scope_stack_offsets(Scope* scope, u64 offset)
{
    u64 stack_size = offset;

    //
    // Sum sizes of local variables declared in this scope.
    //
    {
        List* head = &scope->sym_list;
        List* it = head->next;

        while (it != head) {
            Symbol* sym = list_entry(it, Symbol, lnode);

            if (sym->kind == SYMBOL_VAR) {
                stack_size += sym->type->size;
                stack_size = ALIGN_UP(stack_size, sym->type->align);
                sym->as_var.offset = -stack_size;
            }

            it = it->next;
        }
    }

    //
    // Sum sizes of anonymous objects in this scope.
    //
    {
        List* head = &scope->obj_list;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

            it = it->next;
        }
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //
    {
        List* head = &scope->children;
        List* it = head->next;
        u64 child_offset = stack_size;

        while (it != head) {
            Scope* child_scope = list_entry(it, Scope, lnode);
            u64 child_size = X64__assign_scope_stack_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static u64 X64__assign_proc_stack_offsets(X64_Proc_State* proc_state)
{
    Symbol* sproc = proc_state->sym;
    DeclProc* dproc = (DeclProc*)sproc->decl;
    Scope* scope = dproc->scope;

    //
    // Spill procedure params into the stack (assign stack offsets to params).
    //

    X64_Stack_Params_Info stack_params_info = {0};
    X64__assign_proc_param_offsets(&proc_state->instrs, sproc, &stack_params_info);

    u64 stack_size = stack_params_info.stack_spill_size;

    //
    // Assign stack offsets to local variables declared in the procedure's top scope.
    //

    {
        List* it = stack_params_info.local_var_iter;
        List* head = &scope->sym_list;

        while (it != head) {
            Symbol* sym = list_entry(it, Symbol, lnode);

            // Assign stack offsets to local variables in procedure.
            if (sym->kind == SYMBOL_VAR) {
                stack_size += sym->type->size;
                stack_size = ALIGN_UP(stack_size, sym->type->align);
                sym->as_var.offset = -stack_size;
            }

            it = it->next;
        }
    }

    //
    // Sum sizes of `TEMPORARY` anonymous objects in the procedure's top scope.
    //
    {
        List* head = &sproc->as_proc.tmp_objs;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

            it = it->next;
        }
    }

    //
    // Sum sizes of anonymous objects in the procedure's top scope.
    //
    {
        List* head = &scope->obj_list;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

            it = it->next;
        }
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //

    {
        List* head = &scope->children;
        List* it = head->next;
        u64 child_offset = stack_size;

        while (it != head) {
            Scope* child_scope = list_entry(it, Scope, lnode);
            u32 child_size = X64__assign_scope_stack_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static u32 X64__get_sibd_addr(X64_Proc_State* proc_state, X64_SIBD_Addr* sibd_addr, const X64_MemAddr* vaddr)
{
    u32 used_regs = 0;

    if (vaddr->kind == X64_ADDR_GLOBAL_SYM) {
        sibd_addr->kind = X64__SIBD_ADDR_GLOBAL;
        sibd_addr->global = vaddr->global;
    }
    else if (vaddr->kind == X64_ADDR_STR_LIT) {
        sibd_addr->kind = X64__SIBD_ADDR_STR_LIT;
        sibd_addr->str_lit = vaddr->str_lit;
    }
    else if (vaddr->kind == X64_ADDR_FLOAT_LIT) {
        sibd_addr->kind = X64__SIBD_ADDR_FLOAT_LIT;
        sibd_addr->float_lit = vaddr->float_lit;
    }
    else {
        assert(vaddr->kind == X64_ADDR_SIBD);
        bool has_base = vaddr->sibd.base_reg != (u32)-1;
        bool has_index = vaddr->sibd.scale && (vaddr->sibd.index_reg != (u32)-1);
        assert(has_base || has_index);

        sibd_addr->kind = X64__SIBD_ADDR_LOCAL;
        sibd_addr->local.disp = vaddr->sibd.disp;
        sibd_addr->local.scale = vaddr->sibd.scale;

        if (has_base) {
            X64_LRegLoc base_loc = X64__lreg_loc(proc_state, vaddr->sibd.base_reg);
            assert(IS_LREG_IN_REG(base_loc.kind));

            sibd_addr->local.base_reg = base_loc.reg;
            u32_set_bit(&used_regs, base_loc.reg);

            if (has_index) {
                X64_LRegLoc index_loc = X64__lreg_loc(proc_state, vaddr->sibd.index_reg);
                assert(IS_LREG_IN_REG(index_loc.kind));

                sibd_addr->local.index_reg = index_loc.reg;
                u32_set_bit(&used_regs, index_loc.reg);
            }
            else {
                sibd_addr->local.index_reg = X64_REG_COUNT;
            }
        }
        else {
            X64_LRegLoc index_loc = X64__lreg_loc(proc_state, vaddr->sibd.index_reg);
            assert(IS_LREG_IN_REG(index_loc.kind));

            sibd_addr->local.base_reg = X64_REG_COUNT;
            sibd_addr->local.index_reg = index_loc.reg;
            u32_set_bit(&used_regs, index_loc.reg);
        }
    }

    return used_regs;
}

static void X64__patch_jmp_instrs(X64__Instr* instrs, size_t num_instrs, const HMap* bblock_instr_starts)
{
    for (size_t i = 0; i < num_instrs; ++i) {
        X64__Instr* instr = &instrs[i];
        X64_Instr_Kind kind = X64__get_instr_kind(instr);

        switch (kind) {
        case X64_Instr_Kind_JMP: {
            // jmp.target initially contains the target bblock ID.
            // Use the map to get the bblock's starting instruction index.
            size_t* instr_index = hmap_get(bblock_instr_starts, instr->jmp.target + 1); // The key is offset by 1 (can't have a 0 key)
            assert(instr_index != NULL);
            assert(*instr_index < num_instrs);
            instr->jmp.target = u64_to_u32(*instr_index);

            // Mark the jmp target instruction.
            X64__mark_instr_as_jmp_target(&instrs[*instr_index]);
        } break;
        case X64_Instr_Kind_JMPCC: {
            // jmpcc.target initially contains the target bblock ID.
            // Use the map to get the bblock's starting instruction index.
            size_t* instr_index = hmap_get(bblock_instr_starts, instr->jmpcc.target + 1); // The key is offset by 1 (can't have a 0 key)
            assert(instr_index != NULL);
            assert(*instr_index < num_instrs);
            instr->jmpcc.target = u64_to_u32(*instr_index);

            // Mark the jmp target instruction.
            X64__mark_instr_as_jmp_target(&instrs[*instr_index]);
        } break;
        case X64_Instr_Kind_JMP_TO_RET:
            // Jump after the last instruction (to post-amble).
            instr->jmp.target = u64_to_u32(num_instrs);
            break;
        default:
            break;
        }
    }
}

static void X64__emit_bin_int_rr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, u32 op_size,
                                       u32 op1_lreg, u32 op2_lreg)
{
    X64_LRegLoc op1_loc = X64__lreg_loc(proc_state, op1_lreg);
    X64_LRegLoc op2_loc = X64__lreg_loc(proc_state, op2_lreg);

    switch (op1_loc.kind) {
    case X64_LREG_LOC_REG: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG: {
            x64_bin_int_rr_emit_funcs(instr_kind)(&proc_state->instrs, op_size, op1_loc.reg, op2_loc.reg);
            break;
        }
        case X64_LREG_LOC_STACK: {
            x64_bin_int_rm_emit_funcs(instr_kind)(&proc_state->instrs, op_size, op1_loc.reg, X64__get_rbp_offset_addr(op2_loc.offset));
            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    case X64_LREG_LOC_STACK: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG: {
            x64_bin_int_mr_emit_funcs(instr_kind)(&proc_state->instrs, op_size, X64__get_rbp_offset_addr(op1_loc.offset), op2_loc.reg);
            break;
        }
        case X64_LREG_LOC_STACK: {
            const X64_SIBD_Addr op1_addr = X64__get_rbp_offset_addr(op1_loc.offset);
            const X64_SIBD_Addr op2_addr = X64__get_rbp_offset_addr(op2_loc.offset);

            const X64_Reg tmp_reg = X64_RAX;

            // Save the contents of a temporary register into the stack.
            X64__emit_instr_push(&proc_state->instrs, tmp_reg);

            // Load dst (currently spilled) into the temporary register,
            X64__emit_instr_mov_rm(&proc_state->instrs, op_size, tmp_reg, op1_addr);

            // Execute the instruction using the temporary register as the destination.
            x64_bin_int_rm_emit_funcs(instr_kind)(&proc_state->instrs, op_size, tmp_reg, op2_addr);

            // Store the result of the instruction (contents of temporary register) into dst.
            if (writes_op1) {
                X64__emit_instr_mov_mr(&proc_state->instrs, op_size, op1_addr, tmp_reg);
            }

            // Restore the contents of the temporary register.
            X64__emit_instr_pop(&proc_state->instrs, tmp_reg);
            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64__emit_bin_flt_rr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, FloatKind flt_kind,
                                       u32 op1_lreg, u32 op2_lreg)
{
    X64_LRegLoc op1_loc = X64__lreg_loc(proc_state, op1_lreg);
    X64_LRegLoc op2_loc = X64__lreg_loc(proc_state, op2_lreg);

    switch (op1_loc.kind) {
    case X64_LREG_LOC_REG: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG: {
            x64_bin_flt_rr_emit_funcs(instr_kind)(&proc_state->instrs, flt_kind, op1_loc.reg, op2_loc.reg);
            break;
        }
        case X64_LREG_LOC_STACK: {
            x64_bin_flt_rm_emit_funcs(instr_kind)(&proc_state->instrs, flt_kind, op1_loc.reg, X64__get_rbp_offset_addr(op2_loc.offset));
            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    case X64_LREG_LOC_STACK: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG: {
            x64_bin_flt_mr_emit_funcs(instr_kind)(&proc_state->instrs, flt_kind, X64__get_rbp_offset_addr(op1_loc.offset), op2_loc.reg);
            break;
        }
        case X64_LREG_LOC_STACK: {
            const X64_SIBD_Addr op1_addr = X64__get_rbp_offset_addr(op1_loc.offset);
            const X64_SIBD_Addr op2_addr = X64__get_rbp_offset_addr(op2_loc.offset);

            const X64_Reg tmp_reg = X64_XMM0;

            // Save the contents of a temporary register into the stack.
            X64__emit_instr_sub_ri(&proc_state->instrs, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Make room for 16 bytes on the stack.
            X64__emit_instr_movdqu_mr(&proc_state->instrs, X64__get_rsp_offset_addr(0), tmp_reg); // movdqu oword [rsp], tmp_reg

            // Load dst (currently spilled) into the temporary register,
            X64__emit_instr_mov_flt_rm(&proc_state->instrs, flt_kind, tmp_reg, op1_addr);

            // Execute the instruction using the temporary register as the destination.
            x64_bin_flt_rm_emit_funcs(instr_kind)(&proc_state->instrs, flt_kind, tmp_reg, op2_addr);

            // Store the result of the instruction (contents of temporary register) into dst.
            if (writes_op1) {
                X64__emit_instr_mov_flt_mr(&proc_state->instrs, flt_kind, op1_addr, tmp_reg);
            }

            // Restore the contents of the temporary register.
            X64__emit_instr_movdqu_rm(&proc_state->instrs, tmp_reg, X64__get_rsp_offset_addr(0)); // movdqu reg, oword [rsp]
            X64__emit_instr_add_ri(&proc_state->instrs, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Clean up 16 bytes from stack.
            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64__emit_mov_ext_rr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 dst_size,
                                       u32 dst_lreg, u32 src_size, u32 src_lreg)
{
    // NOTE: sign-extension instructions require the destination to be a register. So, if dst is spilled, we get a temporary
    // register.

    X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
    X64_Reg dst_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, dst_lreg, dst_size, true, 0); // Get actual reg or a tmp if spilled.

    X64_LRegLoc src_loc = X64__lreg_loc(proc_state, src_lreg);

    switch (src_loc.kind) {
    case X64_LREG_LOC_REG: {
        x64_mov_ext_rr_emit_funcs(instr_kind)(&proc_state->instrs, dst_size, dst_reg, src_size, src_loc.reg);
        break;
    }
    case X64_LREG_LOC_STACK: {
        x64_mov_ext_rm_emit_funcs(instr_kind)(&proc_state->instrs, dst_size, dst_reg,
                                              src_size, X64__get_rbp_offset_addr(src_loc.offset));
        break;
    }
    default:
        assert(0);
        break;
    }

    X64__end_reg_group(&tmp_group);
}

static void X64__emit_mov_ext_rm_instr(X64_Proc_State* proc_state, X64_Instr_Kind movext_kind, u32 dst_size,
                                       u32 dst_lreg, u32 src_size, const X64_MemAddr* src_vaddr)
{
        X64_SIBD_Addr src_addr = {0};
        u32 banned_tmp_regs = X64__get_sibd_addr(proc_state, &src_addr, src_vaddr);

        X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
        X64_Reg dst_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, dst_lreg,
                                       dst_size, true, banned_tmp_regs); // Get actual reg or a tmp if spilled.
        x64_mov_ext_rm_emit_funcs(movext_kind)(&proc_state->instrs, dst_size, dst_reg, src_size, src_addr);
        X64__end_reg_group(&tmp_group);
}

static void X64__emit_bin_int_rm_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, u32 op_size,
                                       u32 op1_lreg, const X64_MemAddr* op2_vaddr)
{
    X64_SIBD_Addr op2_addr = {0};
    u32 pinned_regs = X64__get_sibd_addr(proc_state, &op2_addr, op2_vaddr);

    X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
    X64_Reg op1_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, op1_lreg, op_size, writes_op1, pinned_regs);
    assert(op_size <= 8 && IS_POW2(op_size));

    x64_bin_int_rm_emit_funcs(instr_kind)(&proc_state->instrs, op_size, op1_reg, op2_addr);
    X64__end_reg_group(&tmp_group);
}

static void X64__emit_bin_int_mr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size,
                                       const X64_MemAddr* op1_vaddr, u32 op2_lreg)
{
    X64_SIBD_Addr op1_addr = {0};
    u32 pinned_regs = X64__get_sibd_addr(proc_state, &op1_addr, op1_vaddr);

    X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
    X64_Reg op2_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, op2_lreg, op_size, false, pinned_regs);
    assert(op_size <= 8 && IS_POW2(op_size));

    x64_bin_int_mr_emit_funcs(instr_kind)(&proc_state->instrs, op_size, op1_addr, op2_reg);
    X64__end_reg_group(&tmp_group);
}

static void X64__emit_bin_int_mi_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size,
                                       const X64_MemAddr* op1_vaddr, Scalar op2_imm)
{
    X64_SIBD_Addr op1_addr = {0};
    X64__get_sibd_addr(proc_state, &op1_addr, op1_vaddr);
    x64_bin_int_mi_emit_funcs(instr_kind)(&proc_state->instrs, op_size, op1_addr, op2_imm.as_int._u32);
}

static void X64__emit_bin_int_ri_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size, u32 op1_lreg, Scalar op2_imm)
{
    X64_LRegLoc op1_loc = X64__lreg_loc(proc_state, op1_lreg);

    switch (op1_loc.kind) {
    case X64_LREG_LOC_REG: {
        x64_bin_int_ri_emit_funcs(instr_kind)(&proc_state->instrs, op_size, op1_loc.reg, op2_imm.as_int._u32);
        break;
    }
    case X64_LREG_LOC_STACK: {
        const X64_SIBD_Addr op1_addr = X64__get_rbp_offset_addr(op1_loc.offset);
        x64_bin_int_mi_emit_funcs(instr_kind)(&proc_state->instrs, op_size, op1_addr, op2_imm.as_int._u32);
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64__place_args_in_regs(X64_Proc_State* proc_state, u32 num_args, const X64_InstrCallArg* args)
{
    for (u32 i = 0; i < num_args; i++) {
        const X64_InstrCallArg* arg = args + i;
        size_t arg_size = arg->type->size;

        if (type_is_obj_like(arg->type)) { // Argument is a struct/union/array object.
            const X64_ObjArgSlot* slot = &arg->slot.obj;

            if (!slot->num_regs) {
                continue;
            }

            // Move object address into the appropriate argument register.
            if (slot->as_ptr) {
                assert(slot->num_regs == 1);
                assert(x64_reg_classes[slot->pregs[0]] == X64_REG_CLASS_INT);

                X64__emit_instr_lea(&proc_state->instrs, slot->pregs[0], X64__get_rsp_offset_addr(slot->ptr_sp_offset));
            }
            // Copy 64-bit chunks of the struct object into the appropriate argument registers.
            else {
                X64_SIBD_Addr addr = {0};
                X64__get_sibd_addr(proc_state, &addr, &arg->val.addr);

                assert(addr.kind == X64__SIBD_ADDR_LOCAL);

                for (unsigned ii = 0; ii < slot->num_regs; ii++) {
                    X64_RegClass reg_class = x64_reg_classes[slot->pregs[ii]];

                    if (reg_class == X64_REG_CLASS_INT) {
                        X64__emit_instr_mov_rm(&proc_state->instrs, X64_MAX_INT_REG_SIZE, slot->pregs[ii], addr);
                    }
                    else {
                        assert(reg_class == X64_REG_CLASS_FLOAT);
                        X64__emit_instr_mov_flt_rm(&proc_state->instrs, FLOAT_F64, slot->pregs[ii], addr);
                    }

                    addr.local.disp += X64_MAX_INT_REG_SIZE;
                }
            }
        }
        else { // Argument is a primitive type
            const X64_PrimArgSlot* slot = &arg->slot.prim;

            if (!slot->in_reg) {
                continue;
            }

            X64_LRegLoc loc = X64__lreg_loc(proc_state, arg->val.reg);

            if (IS_LREG_IN_STACK(loc.kind)) {
                assert(slot->preg < X64_REG_COUNT);
                X64_RegClass reg_class = x64_reg_classes[slot->preg];

                if (reg_class == X64_REG_CLASS_INT) {
                    X64__emit_instr_mov_rm(&proc_state->instrs, arg_size, slot->preg, X64__get_rbp_offset_addr(loc.offset));
                }
                else {
                    assert(reg_class == X64_REG_CLASS_FLOAT);
                    X64_SIBD_Addr src_mem = X64__get_rbp_offset_addr(loc.offset);
                    X64__emit_instr_mov_flt_rm(&proc_state->instrs, arg_size == 8 ? FLOAT_F64 : FLOAT_F32, slot->preg, src_mem);
                }
            }
            else {
                assert(IS_LREG_IN_REG(loc.kind));
                assert(loc.reg == slot->preg); // Register allocator should have placed this in the correct register.
            }
        }
    }
}

static void X64__place_struct_args_in_stack(X64_Proc_State* proc_state, u32 num_args, const X64_InstrCallArg* args)
{
    bool pushed_cpy_state = false;

    for (u32 i = 0; i < num_args; i += 1) {
        const X64_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;

        if (type_is_obj_like(arg->type)) {
            // Argument is a struct/union/array object.
            const X64_ObjArgSlot* slot = &arg->slot.obj;

            assert(!slot->as_ptr);

            if (slot->num_regs) {
                continue;
            }

            // TODO: There's no need to push all (rdi, rsi, rcx) if not used.
            if (!pushed_cpy_state) {
                X64__emit_instr_push(&proc_state->instrs, X64_RDI);
                X64__emit_instr_push(&proc_state->instrs, X64_RSI);
                X64__emit_instr_push(&proc_state->instrs, X64_RCX);
                pushed_cpy_state = true;
            }

            const u32 sp_begin = X64_MAX_INT_REG_SIZE * 3;

            // Copy obj into its location in the stack with "rep movsb" instruction.
            X64_SIBD_Addr src_addr = {0};
            X64__get_sibd_addr(proc_state, &src_addr, &arg->val.addr);
            assert(src_addr.kind == X64__SIBD_ADDR_LOCAL);

            X64__emit_instr_lea(&proc_state->instrs, X64_RDI, X64__get_rsp_offset_addr(slot->sp_offset + sp_begin));
            X64__emit_instr_lea(&proc_state->instrs, X64_RSI, src_addr);
            X64__emit_instr_mov_ri(&proc_state->instrs, X64_MAX_INT_REG_SIZE, X64_RCX, arg_size);
            X64__emit_instr_rep_movsb(&proc_state->instrs);
        }
    }

    if (pushed_cpy_state) {
        X64__emit_instr_pop(&proc_state->instrs, X64_RCX);
        X64__emit_instr_pop(&proc_state->instrs, X64_RSI);
        X64__emit_instr_pop(&proc_state->instrs, X64_RDI);
    }
}

static void X64__place_args_in_stack(X64_Proc_State* proc_state, u32 num_args, const X64_InstrCallArg* args)
{
    // 1st pass: Copy struct arguments into the stack.
    X64__place_struct_args_in_stack(proc_state, num_args, args);

    // 2nd pass: Copy primitive args that are currently in registers into their stack slots.
    // This ensures that we can freely use RAX as a temporary register in the next pass.
    for (u32 i = 0; i < num_args; i += 1) {
        const X64_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;

        if (!type_is_obj_like(arg->type)) {
            const X64_PrimArgSlot* slot = &arg->slot.prim;

            if (slot->in_reg) {
                continue; // Skip register args
            }

            X64_LRegLoc loc = X64__lreg_loc(proc_state, arg->val.reg);

            // Move directly into stack slot.
            if (IS_LREG_IN_REG(loc.kind)) {
                X64_RegClass reg_class = x64_reg_classes[loc.reg];

                if (reg_class == X64_REG_CLASS_INT) {
                    X64__emit_instr_mov_mr(&proc_state->instrs, arg_size, X64__get_rsp_offset_addr(slot->sp_offset), loc.reg);
                }
                else {
                    assert(reg_class == X64_REG_CLASS_FLOAT);
                    X64__emit_instr_mov_flt_mr(&proc_state->instrs, arg_size == 8 ? FLOAT_F64 : FLOAT_F32,
                                               X64__get_rsp_offset_addr(slot->sp_offset), loc.reg);
                }
            }
        }
    }

    // 3rd pass: Copy primitive args that are currently spilled into the stack frame.
    for (u32 i = 0; i < num_args; i += 1) {
        const X64_InstrCallArg* arg = args + i;

        if (type_is_obj_like(arg->type)) {
            continue;
        }

        const X64_PrimArgSlot* slot = &arg->slot.prim;

        if (slot->in_reg) {
            continue; // Skip register args
        }

        u64 arg_size = arg->type->size;
        X64_LRegLoc loc = X64__lreg_loc(proc_state, arg->val.reg);

        if (IS_LREG_IN_STACK(loc.kind)) {
            // Move into RAX.
            X64__emit_instr_mov_rm(&proc_state->instrs, arg_size, X64_RAX, X64__get_rbp_offset_addr(loc.offset));

            // Move RAX into stack slot.
            X64__emit_instr_mov_mr(&proc_state->instrs, arg_size, X64__get_rsp_offset_addr(slot->sp_offset), X64_RAX);
        }
    }
}

static size_t X64__fill_mem_from_reg(X64_Proc_State* proc_state, X64_SIBD_Addr* dst, X64_Reg src, size_t size)
{
    static char pow2_sizes[8] = {
        [1] = 1, [2] = 2, [3] = 2, [4] = 4, [5] = 4, [6] = 4, [7] = 4,
    };

    size_t rem_amnt = size;
    const X64_RegClass src_reg_class = x64_reg_classes[src];

    // If need to copy 8 or more bytes, just copy entire register into memory, and then return.
    if (rem_amnt >= X64_MAX_INT_REG_SIZE) {
        if (src_reg_class == X64_REG_CLASS_FLOAT) {
            X64__emit_instr_mov_flt_mr(&proc_state->instrs, FLOAT_F64, *dst, src);
        } else {
            assert(src_reg_class == X64_REG_CLASS_INT);
            X64__emit_instr_mov_mr(&proc_state->instrs, X64_MAX_INT_REG_SIZE, *dst, src);
        }

        // Move dst addr forward.
        dst->local.disp += X64_MAX_INT_REG_SIZE;
        return rem_amnt - X64_MAX_INT_REG_SIZE;
    }

    // Have to copy less than 8 bytes. Copy in chunks of powers-of-two.
    assert(rem_amnt < X64_MAX_INT_REG_SIZE);

    if (src_reg_class == X64_REG_CLASS_INT) {
        while (rem_amnt) {
            // Calc the largest power of 2 that is less than or equal to min(8, rem_amnt).
            size_t n = pow2_sizes[rem_amnt];

            // Copy that amount into memory.
            X64__emit_instr_mov_mr(&proc_state->instrs, n, *dst, src);

            // Move dst addr forward.
            dst->local.disp += n;

            size_t new_rem_amnt = rem_amnt - n;

            // Shift src register right to discard copied bits.
            if (new_rem_amnt) {
                X64__emit_instr_sar_ri(&proc_state->instrs, X64_MAX_INT_REG_SIZE, src, (u8)(n << 3));
            }

            rem_amnt = new_rem_amnt;
        }
    }
    else {
        assert(src_reg_class == X64_REG_CLASS_FLOAT);

        if (rem_amnt == float_kind_sizes[FLOAT_F32]) {
            X64__emit_instr_mov_flt_mr(&proc_state->instrs, FLOAT_F32, *dst, src);
            rem_amnt = 0;
        }
        else {
            NIBBLE_FATAL_EXIT("X64_cpy_reg_to_mem(): Cannot copy %d bytes from XMM register.", rem_amnt);
        }
    }

    return rem_amnt;
}

static void X64__cpy_ret_small_obj(X64_Proc_State* proc_state, const Type* ret_type, const X64_CallValue* dst_val)
{
    X64_RegClass reg_class = X64_obj_reg_class(ret_type);
    X64_ScratchRegs ret_regs = (*x64_target.ret_regs)[reg_class];

    // Procedure returned a small struct/union/array object in registers.
    // Copy into appropriate memory location.
    if (!X64_is_obj_retarg_large(ret_type->size)) {
        X64_SIBD_Addr obj_addr = {0};
        X64__get_sibd_addr(proc_state, &obj_addr, &dst_val->addr);

        // Copy RAX into the first 8 bytes of struct memory.
        size_t rem_amnt = X64__fill_mem_from_reg(proc_state, &obj_addr, ret_regs.regs[0], ret_type->size);

        // Copy RDX into the second 8 bytes of struct memory.
        if (rem_amnt) {
            rem_amnt = X64__fill_mem_from_reg(proc_state, &obj_addr, ret_regs.regs[1], rem_amnt);
            assert(!rem_amnt);
        }
    }
}

static void X64__gen_instr(X64_Proc_State* proc_state, const X64_Instr* instr, bool is_last_instr, long bblock_id)
{
    AllocatorState mem_state = allocator_get_state(proc_state->tmp_mem);

    switch (instr->kind) {
    case X64_InstrAdd_R_R_KIND: {
        const X64_InstrAdd_R_R* act_instr = (const X64_InstrAdd_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_ADD_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrAdd_R_M_KIND: {
        const X64_InstrAdd_R_M* act_instr = (const X64_InstrAdd_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_ADD_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrAdd_R_I_KIND: {
        const X64_InstrAdd_R_I* act_instr = (const X64_InstrAdd_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_ADD_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrSub_R_R_KIND: {
        const X64_InstrSub_R_R* act_instr = (const X64_InstrSub_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_SUB_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrSub_R_M_KIND: {
        const X64_InstrSub_R_M* act_instr = (const X64_InstrSub_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_SUB_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrSub_R_I_KIND: {
        const X64_InstrSub_R_I* act_instr = (const X64_InstrSub_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_SUB_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrIMul_R_R_KIND: {
        const X64_InstrIMul_R_R* act_instr = (const X64_InstrIMul_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_IMUL_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrIMul_R_M_KIND: {
        const X64_InstrIMul_R_M* act_instr = (const X64_InstrIMul_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_IMUL_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrIMul_R_I_KIND: {
        const X64_InstrIMul_R_I* act_instr = (const X64_InstrIMul_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_IMUL_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    // AND
    case X64_InstrAnd_R_R_KIND: {
        const X64_InstrAnd_R_R* act_instr = (const X64_InstrAnd_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_AND_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrAnd_R_M_KIND: {
        const X64_InstrAnd_R_M* act_instr = (const X64_InstrAnd_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_AND_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrAnd_R_I_KIND: {
        const X64_InstrAnd_R_I* act_instr = (const X64_InstrAnd_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_AND_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    // OR
    case X64_InstrOr_R_R_KIND: {
        const X64_InstrOr_R_R* act_instr = (const X64_InstrOr_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_OR_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrOr_R_M_KIND: {
        const X64_InstrOr_R_M* act_instr = (const X64_InstrOr_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_OR_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrOr_R_I_KIND: {
        const X64_InstrOr_R_I* act_instr = (const X64_InstrOr_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_OR_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    // XOR
    case X64_InstrXor_R_R_KIND: {
        const X64_InstrXor_R_R* act_instr = (const X64_InstrXor_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_XOR_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrXor_R_M_KIND: {
        const X64_InstrXor_R_M* act_instr = (const X64_InstrXor_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_XOR_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrXor_R_I_KIND: {
        const X64_InstrXor_R_I* act_instr = (const X64_InstrXor_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_XOR_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    // NEG
    case X64_InstrNeg_KIND: {
        const X64_InstrNeg* act_instr = (const X64_InstrNeg*)instr;
        const u8 size = act_instr->size;
        X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64__emit_instr_neg_r(&proc_state->instrs, size, dst_loc.reg);
        } else {
            X64__emit_instr_neg_m(&proc_state->instrs, size, X64__get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    // NOT
    case X64_InstrNot_KIND: {
        const X64_InstrNot* act_instr = (const X64_InstrNot*)instr;
        const u8 size = act_instr->size;
        X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64__emit_instr_not_r(&proc_state->instrs, size, dst_loc.reg);
        } else {
            X64__emit_instr_not_m(&proc_state->instrs, size, X64__get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    // DIV
    case X64_InstrDiv_R_KIND: {
        const X64_InstrDiv_R* act_instr = (const X64_InstrDiv_R*)instr;
        const u8 size = act_instr->size;

        X64_LRegLoc src_loc = X64__lreg_loc(proc_state, act_instr->src);
        if (IS_LREG_IN_REG(src_loc.kind)) {
            X64__emit_instr_div_r(&proc_state->instrs, size, src_loc.reg);
        } else {
            X64__emit_instr_div_m(&proc_state->instrs, size, X64__get_rbp_offset_addr(src_loc.offset));
        }
        break;
    }
    case X64_InstrDiv_M_KIND: {
        const X64_InstrDiv_M* act_instr = (const X64_InstrDiv_M*)instr;
        const u8 size = act_instr->size;
        X64_SIBD_Addr op_addr = {0};

        X64__get_sibd_addr(proc_state, &op_addr, &act_instr->src);
        X64__emit_instr_div_m(&proc_state->instrs, size, op_addr);
        break;
    }
    // IDIV
    case X64_InstrIDiv_R_KIND: {
        const X64_InstrIDiv_R* act_instr = (const X64_InstrIDiv_R*)instr;
        const u8 size = act_instr->size;

        X64_LRegLoc src_loc = X64__lreg_loc(proc_state, act_instr->src);
        if (IS_LREG_IN_REG(src_loc.kind)) {
            X64__emit_instr_idiv_r(&proc_state->instrs, size, src_loc.reg);
        } else {
            X64__emit_instr_idiv_m(&proc_state->instrs, size, X64__get_rbp_offset_addr(src_loc.offset));
        }
        break;
    }
    case X64_InstrIDiv_M_KIND: {
        const X64_InstrIDiv_M* act_instr = (const X64_InstrIDiv_M*)instr;
        const u8 size = act_instr->size;
        X64_SIBD_Addr op_addr = {0};

        X64__get_sibd_addr(proc_state, &op_addr, &act_instr->src);
        X64__emit_instr_idiv_m(&proc_state->instrs, size, op_addr);
        break;
    }
    // Sign-extend _ax into _dx
    case X64_InstrSExtAxToDx_KIND: {
        const X64_InstrSExtAxToDx* act_instr = (const X64_InstrSExtAxToDx*)instr;
        X64__emit_instr_sext_ax_into_dx(&proc_state->instrs, act_instr->size);
        break;
    }
    // SAR
    case X64_InstrSar_R_R_KIND: {
        const X64_InstrSar_R_R* act_instr = (const X64_InstrSar_R_R*)instr;
        const u8 dst_size = act_instr->size;
        const X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);
        const X64_LRegLoc src_loc = X64__lreg_loc(proc_state, act_instr->src);

        assert(IS_LREG_IN_REG(src_loc.kind) && src_loc.reg == X64_RCX);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64__emit_instr_sar_rr(&proc_state->instrs, dst_size, dst_loc.reg);
        } else {
            X64__emit_instr_sar_mr(&proc_state->instrs, dst_size, X64__get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    case X64_InstrSar_R_I_KIND: {
        const X64_InstrSar_R_I* act_instr = (const X64_InstrSar_R_I*)instr;
        const u8 dst_size = act_instr->size;
        const X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64__emit_instr_sar_ri(&proc_state->instrs, dst_size, dst_loc.reg, act_instr->src.as_int._u8);
        } else {
            X64__emit_instr_sar_mi(&proc_state->instrs, dst_size, X64__get_rbp_offset_addr(dst_loc.offset),
                                   act_instr->src.as_int._u8);
        }
        break;
    }
    // SHL
    case X64_InstrShl_R_R_KIND: {
        const X64_InstrShl_R_R* act_instr = (const X64_InstrShl_R_R*)instr;
        const u8 dst_size = act_instr->size;
        const X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);
        const X64_LRegLoc src_loc = X64__lreg_loc(proc_state, act_instr->src);

        assert(IS_LREG_IN_REG(src_loc.kind) && src_loc.reg == X64_RCX);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64__emit_instr_shl_rr(&proc_state->instrs, dst_size, dst_loc.reg);
        } else {
            X64__emit_instr_shl_mr(&proc_state->instrs, dst_size, X64__get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    case X64_InstrShl_R_I_KIND: {
        const X64_InstrShl_R_I* act_instr = (const X64_InstrShl_R_I*)instr;
        const u8 dst_size = act_instr->size;
        const X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64__emit_instr_shl_ri(&proc_state->instrs, dst_size, dst_loc.reg, act_instr->src.as_int._u8);
        } else {
            X64__emit_instr_shl_mi(&proc_state->instrs, dst_size, X64__get_rbp_offset_addr(dst_loc.offset),
                                   act_instr->src.as_int._u8);
        }
        break;
    }
    // MOV
    case X64_InstrMov_R_R_KIND: {
        const X64_InstrMov_R_R* act_instr = (const X64_InstrMov_R_R*)instr;
        const u8 size = act_instr->size;

        X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);
        X64_LRegLoc src_loc = X64__lreg_loc(proc_state, act_instr->src);

        bool same_ops = (dst_loc.kind == src_loc.kind) && ((IS_LREG_IN_REG(dst_loc.kind) && (dst_loc.reg == src_loc.reg)) ||
                                                           (IS_LREG_IN_STACK(dst_loc.kind) && (dst_loc.offset == src_loc.offset)));

        if (!same_ops) {
            X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_MOV_RR, true, size, act_instr->dst, act_instr->src);
        }
        break;
    }
    case X64_InstrMov_R_RH_KIND: {
        X64_InstrMov_R_RH* act_instr = (X64_InstrMov_R_RH*)instr;
        X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);
        X64_LRegLoc src_loc = X64__lreg_loc(proc_state, act_instr->src);

        assert(IS_LREG_IN_REG(src_loc.kind));

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64__emit_instr_mov_rrh(&proc_state->instrs, dst_loc.reg, src_loc.reg);
        } else {
            assert(dst_loc.kind == X64_LREG_LOC_STACK);
            X64__emit_instr_mov_mrh(&proc_state->instrs, X64__get_rbp_offset_addr(dst_loc.offset), src_loc.reg);
        }

        break;
    }
    case X64_InstrMov_R_M_KIND: {
        const X64_InstrMov_R_M* act_instr = (const X64_InstrMov_R_M*)instr;
        const u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_MOV_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrMov_M_R_KIND: {
        const X64_InstrMov_M_R* act_instr = (const X64_InstrMov_M_R*)instr;
        const u8 size = act_instr->size;

        X64__emit_bin_int_mr_instr(proc_state, X64_Instr_Kind_MOV_MR, size, &act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMov_R_I_KIND: {
        const X64_InstrMov_R_I* act_instr = (const X64_InstrMov_R_I*)instr;
        const u8 size = act_instr->size;

        // NOTE: We don't use X64__emit_bin_int_ri_instr here because MOV is the only instruction
        // capable of loading a 64-bit immediate (others can only load u32). Additionally, if the destination
        // was spilled during register allocation, we have to load the immediate (potentially a 64-bit) into a temporary
        // register, which is subsequently stored into the spilled location.
        X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
        X64_Reg dst_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, size, true, 0); // Get actual reg or a tmp if spilled.
        X64__emit_instr_mov_ri(&proc_state->instrs, size, dst_reg, act_instr->src.as_int._u64);
        X64__end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrMov_M_I_KIND: {
        const X64_InstrMov_M_I* act_instr = (const X64_InstrMov_M_I*)instr;
        const u8 size = act_instr->size;

        X64__emit_bin_int_mi_instr(proc_state, X64_Instr_Kind_MOV_MI, size, &act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMovSX_R_R_KIND: {
        const X64_InstrMovSX_R_R* act_instr = (const X64_InstrMovSX_R_R*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;
        X64_Instr_Kind movsx_kind = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? X64_Instr_Kind_MOVSXD_RR :
                                                                                             X64_Instr_Kind_MOVSX_RR;

        X64__emit_mov_ext_rr_instr(proc_state, movsx_kind, dst_size, act_instr->dst, src_size, act_instr->src);
        break;
    }
    case X64_InstrMovSX_R_M_KIND: {
        const X64_InstrMovSX_R_M* act_instr = (const X64_InstrMovSX_R_M*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;
        X64_Instr_Kind movsx_kind = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? X64_Instr_Kind_MOVSXD_RR :
                                                                                             X64_Instr_Kind_MOVSX_RR;

        X64__emit_mov_ext_rm_instr(proc_state, movsx_kind, dst_size, act_instr->dst, src_size, &act_instr->src);
        break;
    }
    case X64_InstrMovZX_R_R_KIND: {
        const X64_InstrMovZX_R_R* act_instr = (const X64_InstrMovZX_R_R*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;

        // There is no encoding for an instruction that zero-extends a 4-byte source to an 8-byte destination! Also, note that
        // an instruction like mov eax, __ clears the upper 4 bytes of eax.
        // See: https://stackoverflow.com/a/51394642
        if (src_size != 4) {
            X64__emit_mov_ext_rr_instr(proc_state, X64_Instr_Kind_MOVZX_RR, dst_size, act_instr->dst, src_size, act_instr->src);
        }
        // EX: Instead of movzx rax, edi (invalid), use mov eax, edi to zero-extend edi into rax.
        else {
            assert(dst_size == X64_MAX_INT_REG_SIZE);

            // NOTE: Not necessary if a previous instruction already cleared the upper 4-bytes of the dest reg with a mov instruction.
            // We would need to track the "zxt" state of all registers: if mov rx, _ => rx is "zxt", otherwise if <not_mov> rx, _ =>
            // rx is NOT "zxt".
            X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_MOV_RR, true, src_size, act_instr->dst, act_instr->src);
        }
        break;
    }
    case X64_InstrMovZX_R_M_KIND: {
        const X64_InstrMovZX_R_M* act_instr = (const X64_InstrMovZX_R_M*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;

        // There is no encoding for an instruction that zero-extends a 4-byte source to an 8-byte destination! Also, note that
        // an instruction like mov eax, __ clears the upper 4 bytes of eax.
        // See: https://stackoverflow.com/a/51394642
        if (src_size != 4) {
            X64__emit_mov_ext_rm_instr(proc_state, X64_Instr_Kind_MOVZX_RM, dst_size, act_instr->dst, src_size, &act_instr->src);
        }
        // EX: Instead of movzx rax, edi (invalid), use mov eax, edi to zero-extend edi into rax.
        else {
            assert(dst_size == X64_MAX_INT_REG_SIZE);

            // NOTE: Not necessary if a previous instruction already cleared the upper 4-bytes of the dest reg with a mov instruction.
            // We would need to track the "zxt" state of all registers: if mov rx, _ => rx is "zxt", otherwise if <not_mov> rx, _ =>
            // rx is NOT "zxt".
            X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_MOV_RM, true, src_size, act_instr->dst, &act_instr->src);
        }
        break;
    }
    // CMP
    case X64_InstrCmp_R_R_KIND: {
        const X64_InstrCmp_R_R* act_instr = (const X64_InstrCmp_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_CMP_RR, true, size, act_instr->op1, act_instr->op2);
        break;
    }
    case X64_InstrCmp_R_I_KIND: {
        const X64_InstrCmp_R_I* act_instr = (const X64_InstrCmp_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_CMP_RI, size, act_instr->op1, act_instr->op2);
        break;
    }
    case X64_InstrCmp_R_M_KIND: {
        const X64_InstrCmp_R_M* act_instr = (const X64_InstrCmp_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_CMP_RM, true, size, act_instr->op1, &act_instr->op2);
        break;
    }
    case X64_InstrCmp_M_R_KIND: {
        const X64_InstrCmp_M_R* act_instr = (const X64_InstrCmp_M_R*)instr;
        const u8 size = act_instr->size;

        X64__emit_bin_int_mr_instr(proc_state, X64_Instr_Kind_CMP_MR, size, &act_instr->op1, act_instr->op2);
        break;
    }
    case X64_InstrCmp_M_I_KIND: {
        const X64_InstrCmp_M_I* act_instr = (const X64_InstrCmp_M_I*)instr;
        const u8 size = act_instr->size;

        X64__emit_bin_int_mi_instr(proc_state, X64_Instr_Kind_CMP_MI, size, &act_instr->op1, act_instr->op2);
        break;
    }
    // LEA
    case X64_InstrLEA_KIND: {
        const X64_InstrLEA* act_instr = (const X64_InstrLEA*)instr;
        const u8 size = X64_MAX_INT_REG_SIZE;

        X64_SIBD_Addr src_addr = {0};
        u32 pinned_regs = X64__get_sibd_addr(proc_state, &src_addr, &act_instr->mem);

        X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
        X64_Reg dst_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, size, true, pinned_regs);
        assert(size <= 8 && IS_POW2(size));

        X64__emit_instr_lea(&proc_state->instrs, dst_reg, src_addr);
        X64__end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrRepMovsb_KIND: {
        X64__emit_instr_rep_movsb(&proc_state->instrs);
        break;
    }
    case X64_InstrRepStosb_KIND: {
        X64__emit_instr_rep_stosb(&proc_state->instrs);
        break;
    }
    case X64_InstrSyscall_KIND: {
        X64__emit_instr_syscall(&proc_state->instrs);
        break;
    }
    case X64_InstrJmp_KIND: {
        const X64_InstrJmp* act_instr = (const X64_InstrJmp*)instr;
        long target_id = act_instr->target->id;

        if (target_id != bblock_id + 1) {
            assert(target_id >= 0);
            X64__emit_instr_jmp(&proc_state->instrs, s64_to_u32(target_id));
        }
        break;
    }
    case X64_InstrJmpCC_KIND: {
        const X64_InstrJmpCC* act_instr = (const X64_InstrJmpCC*)instr;
        X64__emit_instr_jmpcc(&proc_state->instrs, act_instr->cond, s64_to_u32(act_instr->true_bb->id));
        break;
    }
    case X64_InstrSetCC_KIND: {
        const X64_InstrSetCC* act_instr = (const X64_InstrSetCC*)instr;
        X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64__emit_instr_setcc_r(&proc_state->instrs, act_instr->cond, dst_loc.reg);
        } else {
            assert(IS_LREG_IN_STACK(dst_loc.kind));
            X64__emit_instr_setcc_m(&proc_state->instrs, act_instr->cond, X64__get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    case X64_InstrRet_KIND: {
        if (!is_last_instr) {
            X64__emit_instr_jmp_to_ret(&proc_state->instrs);
        }

        break;
    }
    case X64_InstrCall_KIND:
    case X64_InstrCall_R_KIND: {
        const Type* proc_type;
        X64_CallValue dst_val;
        u32 num_args;
        const X64_InstrCallArg* args;
        X64_StackArgsInfo stack_args_info;
        unsigned save_reg_mask;

        if (instr->kind == X64_InstrCall_KIND) {
            const X64_InstrCall* instr_call = (const X64_InstrCall*)instr;

            proc_type = instr_call->sym->type;
            dst_val = instr_call->dst;
            num_args = instr_call->num_args;
            args = instr_call->args;
            stack_args_info = instr_call->stack_info;
            save_reg_mask = instr_call->save_reg_mask;
        }
        else {
            assert(instr->kind == X64_InstrCall_R_KIND);
            const X64_InstrCall_R* instr_call_r = (const X64_InstrCall_R*)instr;

            proc_type = instr_call_r->proc_type;
            dst_val = instr_call_r->dst;
            num_args = instr_call_r->num_args;
            args = instr_call_r->args;
            stack_args_info = instr_call_r->stack_info;
            save_reg_mask = instr_call_r->save_reg_mask;
        }

        // NOTE: Stack frame must be 16-byte aligned before procedure call.
        // If the number of stack args + caller-saved regs is not even (16-byte aligned),
        // we MUST subtract 8 from stack BEFORE pushing anything into stack
        // See: https://godbolt.org/z/cM9Encdsc
        //
        // Placeholder instruction for sub rsp, <alignment>
        const size_t rsp_align_instr_idx = X64__emit_instr_placeholder(&proc_state->instrs, X64_Instr_Kind_NOOP);

        // NOTE: No need to save caller-saved registers before call because the register allocator currently
        // spills any values needed across procedure calls.
        X64_Reg_Group group = X64__begin_reg_group(proc_state);

        // Save caller-saved registers needed across the call.
        u32 r = 0;

        while (save_reg_mask) {
            if (save_reg_mask & 0x1) {
                X64__save_reg_to_group(&group, (X64_Reg)r);
            }

            save_reg_mask >>= 1;
            r++;
        }

        // If the called procedure returns a "large" object by value, provide the address to the destination
        // memory location as the first argument.
        Type* ret_type = proc_type->as_proc.ret;

        if (type_is_obj_like(ret_type) && X64_is_obj_retarg_large(ret_type->size)) {
            X64_Reg dst_reg = (*x64_target.arg_regs)[X64_REG_CLASS_INT].regs[0];
            X64_SIBD_Addr obj_addr = {0};

            X64__get_sibd_addr(proc_state, &obj_addr, &dst_val.addr);
            X64__emit_instr_lea(&proc_state->instrs, dst_reg, obj_addr);
        }

        //
        // Place arguments in the appropriate locations.
        //

        // Make room in the stack for arguments passed via the stack.
        if (stack_args_info.size) {
            X64__emit_instr_sub_ri(&proc_state->instrs, X64_MAX_INT_REG_SIZE, X64_RSP, stack_args_info.size);
        }

        // Place register args. It is expected that the register allocator either placed the arg in the correct register or spilled it.
        X64__place_args_in_regs(proc_state, num_args, args);

        // Place stack args. It is expected that the register allocator either placed the arg in a non-argument register or spilled it.
        if (stack_args_info.size) {
            X64__place_args_in_stack(proc_state, num_args, args);
        }

        //
        // Align stack before call.
        //

        u64 total_stack_size = stack_args_info.size + group.num_tmp_regs * X64_MAX_INT_REG_SIZE;
        u64 align_stack_size = 0;

        if (total_stack_size & (X64_STACK_ALIGN - 1)) {
            align_stack_size = X64_STACK_WORD_SIZE;

            X64__Instr* rsp_align_instr = &proc_state->instrs[rsp_align_instr_idx];
            X64__set_instr_kind(rsp_align_instr, X64_Instr_Kind_SUB_RI);
            rsp_align_instr->sub_ri.size = X64_MAX_INT_REG_SIZE;
            rsp_align_instr->sub_ri.dst = X64_RSP;
            rsp_align_instr->sub_ri.imm = align_stack_size;

            total_stack_size += align_stack_size;
        }

        // Stack should now be aligned properly for procedure call.
        assert((total_stack_size & (X64_STACK_ALIGN - 1)) == 0);

        //
        // Emit call instruction.
        //

        if (instr->kind == X64_InstrCall_KIND) {
            const X64_InstrCall* instr_call = (const X64_InstrCall*)instr;

            X64__emit_instr_call(&proc_state->instrs, instr_call->sym);
        }
        else {
            const X64_InstrCall_R* instr_call_r = (const X64_InstrCall_R*)instr;
            X64_LRegLoc proc_reg_loc = X64__lreg_loc(proc_state, instr_call_r->proc_loc);

            if (IS_LREG_IN_REG(proc_reg_loc.kind)) {
                X64__emit_instr_call_r(&proc_state->instrs, proc_reg_loc.reg);
            } else {
                X64__emit_instr_call_m(&proc_state->instrs, X64__get_rbp_offset_addr(proc_reg_loc.offset));
            }
        }

        //
        // Move return value (if any) to appropriate register.
        //

        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            if (!type_is_obj_like(ret_type)) { // If returns a primitive type.
                X64_LRegLoc dst_loc = X64__lreg_loc(proc_state, dst_val.reg);

                if (ret_type->kind == TYPE_FLOAT) {
                    X64_Reg ret_reg = (*x64_target.ret_regs)[X64_REG_CLASS_FLOAT].regs[0];

                    if (IS_LREG_IN_STACK(dst_loc.kind)) {
                        // Move result (in XMM0) to stack offset.
                        // Ex: movsd [rbp + x], xmm0
                        X64__emit_instr_mov_flt_mr(&proc_state->instrs, ret_type->as_float.kind,
                                                   X64__get_rbp_offset_addr(dst_loc.offset), ret_reg);
                    } else if (dst_loc.reg != ret_reg) {
                        // Move result (in XMM0) to allocated result register.
                        assert(IS_LREG_IN_REG(dst_loc.kind));
                        X64__emit_instr_mov_flt_rr(&proc_state->instrs, ret_type->as_float.kind, dst_loc.reg, ret_reg);
                    }
                }
                else {
                    X64_Reg ret_reg = (*x64_target.ret_regs)[X64_REG_CLASS_INT].regs[0];

                    if (IS_LREG_IN_STACK(dst_loc.kind)) {
                        // Move result (in RAX) to stack offset.
                        // Ex: mov qword [rbp + x], rax
                        X64__emit_instr_mov_mr(&proc_state->instrs, ret_type->size, X64__get_rbp_offset_addr(dst_loc.offset), ret_reg);
                    } else if (dst_loc.reg != ret_reg) {
                        // Move result (in RAX) to allocated result register.
                        assert(IS_LREG_IN_REG(dst_loc.kind));
                        X64__emit_instr_mov_rr(&proc_state->instrs, ret_type->size, dst_loc.reg, ret_reg);
                    }
                }
            } else { // Else returns a small object.
                X64__cpy_ret_small_obj(proc_state, ret_type, &dst_val);
            }
        }

        if (group.num_tmp_regs) {
            if (stack_args_info.size) { // Clean up stack args.
                X64__emit_instr_add_ri(&proc_state->instrs, X64_MAX_INT_REG_SIZE, X64_RSP, stack_args_info.size);
            }

            // Restore saved registers.
            X64__end_reg_group(&group);

            // Clean up any initial stack alignment
            if (align_stack_size) {
                X64__emit_instr_add_ri(&proc_state->instrs, X64_MAX_INT_REG_SIZE, X64_RSP, align_stack_size);
            }
        }
        else {
            size_t cleanup_amount = stack_args_info.size + align_stack_size;

            if (cleanup_amount) {
                // Clean up stack args + alignment
                X64__emit_instr_add_ri(&proc_state->instrs, X64_MAX_INT_REG_SIZE, X64_RSP, cleanup_amount);
            }
        }

        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unhandled X64 LIR instruction \"%s\" at IP %u\n", LIR_print_instr(proc_state->tmp_mem, instr), instr->ino);
        break;
    }

    allocator_restore_state(mem_state);
}

Array(X64__Instr) X64__gen_proc_instrs(Allocator* gen_mem, Allocator* tmp_mem, Symbol* proc_sym)
{
    const bool is_nonleaf = proc_sym->as_proc.is_nonleaf;

    X64_Proc_State state = {
        .gen_mem = gen_mem,
        .tmp_mem = tmp_mem,
        .sym = proc_sym,
        .instrs = array_create(gen_mem, X64__Instr, 64),
        .scratch_regs = is_nonleaf ? x64_target.nonleaf_scratch_regs : x64_target.leaf_scratch_regs,
    };

    AllocatorState tmp_mem_state = allocator_get_state(state.tmp_mem);
    //////////////////////////////////////////////////////////////////////////////////////////

    X64__emit_instr_push(&state.instrs, X64_RBP);
    X64__emit_instr_mov_rr(&state.instrs, X64_MAX_INT_REG_SIZE, X64_RBP, X64_RSP);
    const size_t sub_rsp_idx = X64__emit_instr_placeholder(&state.instrs, X64_Instr_Kind_NOOP);  // Placeholder sub rsp, <stack_size>

    // Calculate stack size from procedure arguments (spills) and local variables.
    u32 stack_size = X64__assign_proc_stack_offsets(&state);

    // Register allocation.
    BBlock** ir_bblocks = proc_sym->as_proc.bblocks;
    size_t num_ir_bblocks = array_len(ir_bblocks);
    X64_LIRBuilder builder = {.arena = tmp_mem};

    X64_emit_lir_instrs(&builder, proc_sym->as_proc.num_regs, num_ir_bblocks, ir_bblocks); // Generate LIR instructions.
    X64_compute_live_intervals(&builder); // Compute LIR register intervals.
    X64_RegAllocResult reg_alloc = X64_linear_scan_reg_alloc(&builder, state.scratch_regs, stack_size);  // May spill and increase stack_size.

    if (!reg_alloc.success) {
        NIBBLE_FATAL_EXIT("Register allocation for procedure `%s` failed.", proc_sym->name->str);
        return NULL;
    }

    stack_size = reg_alloc.stack_offset;
    state.builder = &builder;

    // Fill in sub rsp, <stack_size>
    if (stack_size) {
        X64__Instr* sub_rsp_instr = &state.instrs[sub_rsp_idx];
        X64__set_instr_kind(sub_rsp_instr, X64_Instr_Kind_SUB_RI);
        sub_rsp_instr->sub_ri.size = X64_MAX_INT_REG_SIZE;
        sub_rsp_instr->sub_ri.dst = X64_RSP;
        sub_rsp_instr->sub_ri.imm = stack_size;
    }

    // Emit instructions to save callee-saved registers.
    // NOTE: The procedure instructions may need to dynamically access callee-saved registers,
    // but they will always be saved and restored.
    for (uint32_t r = 0; r < X64_REG_COUNT; r += 1) {
        X64_Reg reg = (X64_Reg)r;

        if (reg == X64_RBP || reg == X64_RSP)
            continue;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg)) {
            X64__push_reg_to_stack(&state.instrs, reg);
        }
    }

    // Process procedure instructions.
    HMap bblock_instr_starts = hmap(clp2(num_ir_bblocks), tmp_mem);

    for (size_t ii = 0; ii < builder.num_bblocks; ii++) {
        X64_BBlock* bb = builder.bblocks[ii];
        bool last_bb = ii == builder.num_bblocks - 1;

        hmap_put(&bblock_instr_starts, bb->id + 1, array_len(state.instrs)); // The key is offset by 1 (can't have 0 key)

        for (X64_Instr* instr = bb->first; instr; instr = instr->next) {
            bool last_instr = last_bb && !instr->next;

            X64__gen_instr(&state, instr, last_instr, bb->id);
        }
    }

    // Patch jmp (to block, to ret label) instructions.
    const size_t num_instrs_before_postamble = array_len(state.instrs);
    X64__patch_jmp_instrs(state.instrs, num_instrs_before_postamble, &bblock_instr_starts);

    //
    // Postamble.
    //

    // Restore callee-saved registers.
    // NOTE: Iterating in the reverse order as the corresponding pushes.
    for (uint32_t r = X64_REG_COUNT; r-- > 0;) {
        X64_Reg reg = (X64_Reg)r;

        if (reg == X64_RBP || reg == X64_RSP)
            continue;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg)) {
            X64__pop_reg_from_stack(&state.instrs, reg);
        }
    }

    // Clean up stack and return to caller.
    X64__emit_instr_mov_rr(&state.instrs, X64_MAX_INT_REG_SIZE, X64_RSP, X64_RBP);
    X64__emit_instr_pop(&state.instrs, X64_RBP);
    X64__emit_instr_ret(&state.instrs);

    // Mark the first instruction of the postamble as a jump target (for 'return' instructions).
    X64__mark_instr_as_jmp_target(&state.instrs[num_instrs_before_postamble]);

    //////////////////////////////////////////////////////////////////////////////////////////
    allocator_restore_state(tmp_mem_state);

    return state.instrs;
}
