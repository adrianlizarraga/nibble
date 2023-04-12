#include "x64_gen/lir_to_x64.h"
#include "allocator.h"
#include "array.h"
#include "ast/module.h"
#include "hash_map.h"
#include "nibble.h"
#include "stream.h"
#include "x64_gen/regs.h"
#include "x64_gen/lir.h"
#include "x64_gen/reg_alloc.h"
#include "x64_gen/print_lir.h"

static X64_SIBD_Addr X64__get_stack_offset_addr(s32 offset)
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

static void X64__emit_instr_ret(Array(X64__Instr) * instrs)
{
    X64__Instr ret_instr = {.kind = X64_Instr_Kind_RET};
    array_push(*instrs, ret_instr);
}

static void X64__emit_instr_jmp(Array(X64__Instr)* instrs, size_t target)
{
    X64__Instr instr = {.kind = X64_Instr_Kind_JMP, .jmp.target = target};
    array_push(*instrs, instr);
}

static void X64__emit_instr_jmp_to_ret(Array(X64__Instr)* instrs)
{
    X64__Instr instr = {.kind = X64_Instr_Kind_JMP_TO_RET, .jmp.target = X64_REG_COUNT};
    array_push(*instrs, instr);
}

static void X64__emit_instr_push(Array(X64__Instr) * instrs, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64__Instr push_instr = {
        .kind = X64_Instr_Kind_PUSH,
        .push.reg = reg,
    };

    array_push(*instrs, push_instr);
}

static void X64__emit_instr_pop(Array(X64__Instr) * instrs, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64__Instr pop_instr = {
        .kind = X64_Instr_Kind_POP,
        .pop.reg = reg,
    };

    array_push(*instrs, pop_instr);
}

static void X64__emit_instr_add_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr add_rr_instr = {
        .kind = X64_Instr_Kind_ADD_RR,
        .add_rr.size = size,
        .add_rr.dst = dst,
        .add_rr.src = src,
    };

    array_push(*instrs, add_rr_instr);
}

static void X64__emit_instr_add_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr add_rm_instr = {
        .kind = X64_Instr_Kind_ADD_RM,
        .add_rm.size = size,
        .add_rm.dst = dst,
        .add_rm.src = src,
    };

    array_push(*instrs, add_rm_instr);
}

static void X64__emit_instr_add_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr add_mr_instr = {
        .kind = X64_Instr_Kind_ADD_MR,
        .add_mr.size = size,
        .add_mr.dst = dst,
        .add_mr.src = src,
    };

    array_push(*instrs, add_mr_instr);
}

static void X64__emit_instr_add_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr add_ri_instr = {
        .kind = X64_Instr_Kind_ADD_RI,
        .add_ri.size = size,
        .add_ri.dst = dst,
        .add_ri.imm = imm,
    };

    array_push(*instrs, add_ri_instr);
}

static void X64__emit_instr_sub_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr sub_rr_instr = {
        .kind = X64_Instr_Kind_SUB_RR,
        .sub_rr.size = size,
        .sub_rr.dst = dst,
        .sub_rr.src = src,
    };

    array_push(*instrs, sub_rr_instr);
}

static void X64__emit_instr_sub_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr sub_rm_instr = {
        .kind = X64_Instr_Kind_SUB_RM,
        .sub_rm.size = size,
        .sub_rm.dst = dst,
        .sub_rm.src = src,
    };

    array_push(*instrs, sub_rm_instr);
}

static void X64__emit_instr_sub_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr sub_mr_instr = {
        .kind = X64_Instr_Kind_SUB_MR,
        .sub_mr.size = size,
        .sub_mr.dst = dst,
        .sub_mr.src = src,
    };

    array_push(*instrs, sub_mr_instr);
}

static void X64__emit_instr_sub_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr sub_ri_instr = {
        .kind = X64_Instr_Kind_SUB_RI,
        .sub_ri.size = size,
        .sub_ri.dst = dst,
        .sub_ri.imm = imm,
    };

    array_push(*instrs, sub_ri_instr);
}

static void X64__emit_instr_imul_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr imul_rr_instr = {
        .kind = X64_Instr_Kind_IMUL_RR,
        .imul_rr.size = size,
        .imul_rr.dst = dst,
        .imul_rr.src = src,
    };

    array_push(*instrs, imul_rr_instr);
}

static void X64__emit_instr_imul_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr imul_rm_instr = {
        .kind = X64_Instr_Kind_IMUL_RM,
        .imul_rm.size = size,
        .imul_rm.dst = dst,
        .imul_rm.src = src,
    };

    array_push(*instrs, imul_rm_instr);
}

static void X64__emit_instr_imul_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr imul_mr_instr = {
        .kind = X64_Instr_Kind_IMUL_MR,
        .imul_mr.size = size,
        .imul_mr.dst = dst,
        .imul_mr.src = src,
    };

    array_push(*instrs, imul_mr_instr);
}

static void X64__emit_instr_imul_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u32 imm)
{
    X64__Instr imul_ri_instr = {
        .kind = X64_Instr_Kind_IMUL_RI,
        .imul_ri.size = size,
        .imul_ri.dst = dst,
        .imul_ri.imm = imm,
    };

    array_push(*instrs, imul_ri_instr);
}

static void X64__emit_instr_mov_rr(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_Reg src)
{
    X64__Instr mov_rr_instr = {
        .kind = X64_Instr_Kind_MOV_RR,
        .mov_rr.size = size,
        .mov_rr.dst = dst,
        .mov_rr.src = src,
    };

    array_push(*instrs, mov_rr_instr);
}

static void X64__emit_instr_mov_rm(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr mov_rm_instr = {
        .kind = X64_Instr_Kind_MOV_RM,
        .mov_rm.size = size,
        .mov_rm.dst = dst,
        .mov_rm.src = src,
    };

    array_push(*instrs, mov_rm_instr);
}

static void X64__emit_instr_mov_mr(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr mov_mr_instr = {
        .kind = X64_Instr_Kind_MOV_MR,
        .mov_mr.size = size,
        .mov_mr.dst = dst,
        .mov_mr.src = src,
    };

    array_push(*instrs, mov_mr_instr);
}

static void X64__emit_instr_mov_ri(Array(X64__Instr) * instrs, u8 size, X64_Reg dst, u64 imm)
{
    X64__Instr mov_ri_instr = {
        .kind = X64_Instr_Kind_MOV_RI,
        .mov_ri.size = size,
        .mov_ri.dst = dst,
        .mov_ri.imm = imm,
    };

    array_push(*instrs, mov_ri_instr);
}

static void X64__emit_instr_mov_mi(Array(X64__Instr) * instrs, u8 size, X64_SIBD_Addr dst, u32 imm)
{
    X64__Instr mov_mi_instr = {
        .kind = X64_Instr_Kind_MOV_MI,
        .mov_mi.size = size,
        .mov_mi.dst = dst,
        .mov_mi.imm = imm,
    };

    array_push(*instrs, mov_mi_instr);
}

static void X64__emit_instr_movsx_rr(Array(X64__Instr) * instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src)
{
    X64__Instr movsx_rr_instr = {
        .kind = X64_Instr_Kind_MOVSX_RR,
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
        .kind = X64_Instr_Kind_MOVSX_RM,
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
        .kind = X64_Instr_Kind_MOVSXD_RR,
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
        .kind = X64_Instr_Kind_MOVSXD_RM,
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
        .kind = X64_Instr_Kind_MOVZX_RR,
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
        .kind = X64_Instr_Kind_MOVZX_RM,
        .movzx_rm.dst_size = dst_size,
        .movzx_rm.src_size = src_size,
        .movzx_rm.dst = dst,
        .movzx_rm.src = src,
    };

    array_push(*instrs, movzx_rm_instr);
}

static void X64__emit_instr_movss_mr(Array(X64__Instr) * instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr movss_mr_instr = {
        .kind = X64_Instr_Kind_MOVSS_MR,
        .movss_mr.dst = dst,
        .movss_mr.src = src,
    };

    array_push(*instrs, movss_mr_instr);
}

static void X64__emit_instr_movss_rm(Array(X64__Instr) * instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr movss_rm_instr = {
        .kind = X64_Instr_Kind_MOVSS_RM,
        .movss_rm.dst = dst,
        .movss_rm.src = src,
    };

    array_push(*instrs, movss_rm_instr);
}

static void X64__emit_instr_movsd_mr(Array(X64__Instr) * instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    X64__Instr movsd_mr_instr = {
        .kind = X64_Instr_Kind_MOVSD_MR,
        .movsd_mr.dst = dst,
        .movsd_mr.src = src,
    };

    array_push(*instrs, movsd_mr_instr);
}

static void X64__emit_instr_movsd_rm(Array(X64__Instr) * instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr movsd_rm_instr = {
        .kind = X64_Instr_Kind_MOVSD_RM,
        .movsd_rm.dst = dst,
        .movsd_rm.src = src,
    };

    array_push(*instrs, movsd_rm_instr);
}

static void X64__emit_instr_movdqu_mr(Array(X64__Instr) * instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    assert(x64_reg_classes[src] == X64_REG_CLASS_FLOAT);
    X64__Instr movdqu_mr_instr = {
        .kind = X64_Instr_Kind_MOVDQU_MR,
        .movdqu_mr.dst = dst,
        .movdqu_mr.src = src,
    };

    array_push(*instrs, movdqu_mr_instr);
}

static void X64__emit_instr_movdqu_rm(Array(X64__Instr) * instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    assert(x64_reg_classes[dst] == X64_REG_CLASS_FLOAT);
    X64__Instr movdqu_rm_instr = {
        .kind = X64_Instr_Kind_MOVDQU_RM,
        .movdqu_rm.dst = dst,
        .movdqu_rm.src = src,
    };

    array_push(*instrs, movdqu_rm_instr);
}

static void X64__emit_instr_lea(Array(X64__Instr) * instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    X64__Instr lea_instr = {
        .kind = X64_Instr_Kind_LEA,
        .lea.dst = dst,
        .lea.src = src,
    };

    array_push(*instrs, lea_instr);
}

static size_t X64__emit_instr_placeholder(Array(X64__Instr) * instrs, X64_Instr_Kind kind)
{
    X64__Instr instr = {.kind = kind};
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

        X64_SIBD_Addr dst = {.kind = X64__SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.index_reg = X64_REG_COUNT};
        X64__emit_instr_movdqu_mr(instrs, dst, reg); // movdqu oword [rsp], reg
    }
}

static void X64__pop_reg_from_stack(Array(X64__Instr)* instrs, X64_Reg reg)
{
    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        X64__emit_instr_pop(instrs, reg);
    }
    else {
        X64_SIBD_Addr src = {.kind = X64__SIBD_ADDR_LOCAL, .local.base_reg = X64_RSP, .local.index_reg = X64_REG_COUNT};
        X64__emit_instr_movdqu_rm(instrs, reg, src); // movdqu reg, oword [rsp]

        X64__emit_instr_add_ri(instrs, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Clean up 16 bytes from stack.
    }
}

static void X64__load_prim_from_mem(Array(X64__Instr)* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    const X64_RegClass reg_class = x64_reg_classes[dst];

    if (reg_class == X64_REG_CLASS_INT) {
        X64__emit_instr_mov_rm(instrs, size, dst, src);
    }
    else if ((reg_class == X64_REG_CLASS_FLOAT) && (size == float_kind_sizes[FLOAT_F64])) {
        X64__emit_instr_movsd_rm(instrs, dst, src);
    }
    else {
        assert(reg_class == X64_REG_CLASS_FLOAT && (size == float_kind_sizes[FLOAT_F32]));
        X64__emit_instr_movss_rm(instrs, dst, src);
    }
}

static void X64__save_prim_to_mem(Array(X64__Instr)* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    const X64_RegClass reg_class = x64_reg_classes[src];
    if (reg_class == X64_REG_CLASS_INT) {
        X64__emit_instr_mov_mr(instrs, size, dst, src);
    }
    else if ((reg_class == X64_REG_CLASS_FLOAT) && (size == float_kind_sizes[FLOAT_F64])) {
        X64__emit_instr_movsd_mr(instrs, dst, src);
    }
    else {
        assert(reg_class == X64_REG_CLASS_FLOAT && (size == float_kind_sizes[FLOAT_F32]));
        X64__emit_instr_movss_mr(instrs, dst, src);
    }
}

typedef void X64_Emit_Bin_Int_RR_Func (Array(X64__Instr)* instrs, u8 size, X64_Reg dst, X64_Reg src);
typedef void X64_Emit_Bin_Int_RM_Func (Array(X64__Instr)* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src);
typedef void X64_Emit_Bin_Int_RI_Func (Array(X64__Instr)* instrs, u8 size, X64_Reg dst, u32 imm);
typedef void X64_Emit_Bin_Int_MR_Func (Array(X64__Instr)* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src);
typedef void X64_Emit_Bin_Int_MI_Func (Array(X64__Instr)* instrs, u8 size, X64_SIBD_Addr dst, u32 imm);
typedef void X64_Emit_Mov_Ext_RR_Func (Array(X64__Instr)* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src);
typedef void X64_Emit_Mov_Ext_RM_Func (Array(X64__Instr)* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src);

static X64_Emit_Bin_Int_RR_Func* x64_bin_int_rr_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_ADD_RR] = X64__emit_instr_add_rr,
    [X64_Instr_Kind_SUB_RR] = X64__emit_instr_sub_rr,
    [X64_Instr_Kind_IMUL_RR] = X64__emit_instr_imul_rr,
    [X64_Instr_Kind_MOV_RR] = X64__emit_instr_mov_rr,
};

static X64_Emit_Bin_Int_RM_Func* x64_bin_int_rm_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_ADD_RM] = X64__emit_instr_add_rm,
    [X64_Instr_Kind_SUB_RM] = X64__emit_instr_sub_rm,
    [X64_Instr_Kind_IMUL_RM] = X64__emit_instr_imul_rm,
    [X64_Instr_Kind_MOV_RM] = X64__emit_instr_mov_rm,
};

static X64_Emit_Bin_Int_RI_Func* x64_bin_int_ri_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_ADD_RI] = X64__emit_instr_add_ri,
    [X64_Instr_Kind_SUB_RI] = X64__emit_instr_sub_ri,
    [X64_Instr_Kind_IMUL_RI] = X64__emit_instr_imul_ri,
};

static X64_Emit_Bin_Int_MR_Func* x64_bin_int_mr_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_ADD_MR] = X64__emit_instr_add_mr,
    [X64_Instr_Kind_SUB_MR] = X64__emit_instr_sub_mr,
    [X64_Instr_Kind_IMUL_MR] = X64__emit_instr_imul_mr,
    [X64_Instr_Kind_MOV_MR] = X64__emit_instr_mov_mr,
};

static X64_Emit_Bin_Int_MI_Func* x64_bin_int_mi_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_MOV_MI] = X64__emit_instr_mov_mi,
};

static X64_Emit_Mov_Ext_RR_Func* x64_mov_ext_rr_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_MOVSX_RR] = X64__emit_instr_movsx_rr,
    [X64_Instr_Kind_MOVSXD_RR] = X64__emit_instr_movsxd_rr,
    [X64_Instr_Kind_MOVZX_RR] = X64__emit_instr_movzx_rr,
};

static X64_Emit_Mov_Ext_RM_Func* x64_mov_ext_rm_emit_funcs[X64_Instr_Kind_COUNT] = {
    [X64_Instr_Kind_MOVSX_RM] = X64__emit_instr_movsx_rm,
    [X64_Instr_Kind_MOVSXD_RM] = X64__emit_instr_movsxd_rm,
    [X64_Instr_Kind_MOVZX_RM] = X64__emit_instr_movzx_rm,
};

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
    X64__load_prim_from_mem(&proc_state->instrs, size, tmp_reg->reg, X64__get_stack_offset_addr(lreg_loc.offset));

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
            X64__save_prim_to_mem(&proc_state->instrs, it->size, X64__get_stack_offset_addr(it->offset), it->reg);
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

    X64_SIBD_Addr dst_addr = {.kind = X64__SIBD_ADDR_LOCAL, .local.base_reg = X64_RBP, .local.index_reg = X64_REG_COUNT, .local.disp = offset};

    X64__save_prim_to_mem(instrs, size, dst_addr, preg);

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

static u32 X64__get_sibd_addr(X64_Proc_State* proc_state, X64_SIBD_Addr* sibd_addr, X64_MemAddr* vaddr)
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

        if (instr->kind == X64_Instr_Kind_JMP) {
            // jmp.target initially contains the target bblock ID.
            // Use the map to get the bblock's starting instruction index.
            size_t* instr_index = hmap_get(bblock_instr_starts, instr->jmp.target + 1); // The key is offset by 1 (can't have a 0 key)
            assert(instr_index != NULL);
            assert(*instr_index < num_instrs);
            instr->jmp.target = *instr_index;
        }
        else if (instr->kind == X64_Instr_Kind_JMP_TO_RET) {
            // Jump after the last instruction (to post-amble).
            instr->jmp.target = num_instrs;
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
            x64_bin_int_rr_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_loc.reg, op2_loc.reg);
            break;
        }
        case X64_LREG_LOC_STACK: {
            x64_bin_int_rm_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_loc.reg, X64__get_stack_offset_addr(op2_loc.offset));
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
            x64_bin_int_mr_emit_funcs[instr_kind](&proc_state->instrs, op_size, X64__get_stack_offset_addr(op1_loc.offset), op2_loc.reg);
            break;
        }
        case X64_LREG_LOC_STACK: {
            const X64_SIBD_Addr op1_addr = X64__get_stack_offset_addr(op1_loc.offset);
            const X64_SIBD_Addr op2_addr = X64__get_stack_offset_addr(op2_loc.offset);

            const X64_Reg tmp_reg = X64_RAX;

            // Save the contents of a temporary register into the stack.
            X64__emit_instr_push(&proc_state->instrs, tmp_reg);

            // Load dst (currently spilled) into the temporary register,
            X64__emit_instr_mov_rm(&proc_state->instrs, op_size, tmp_reg, op1_addr);

            // Execute the instruction using the temporary register as the destination.
            x64_bin_int_rm_emit_funcs[instr_kind](&proc_state->instrs, op_size, tmp_reg, op2_addr);

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
        x64_mov_ext_rr_emit_funcs[instr_kind](&proc_state->instrs, dst_size, dst_reg, src_size, src_loc.reg);
        break;
    }
    case X64_LREG_LOC_STACK: {
        x64_mov_ext_rm_emit_funcs[instr_kind](&proc_state->instrs, dst_size, dst_reg,
                                              src_size, X64__get_stack_offset_addr(src_loc.offset));
        break;
    }
    default:
        assert(0);
        break;
    }

    X64__end_reg_group(&tmp_group);
}

static void X64__emit_mov_ext_rm_instr(X64_Proc_State* proc_state, X64_Instr_Kind movext_kind, u32 dst_size,
                                       u32 dst_lreg, u32 src_size, X64_MemAddr* src_vaddr)
{
        X64_SIBD_Addr src_addr = {0};
        u32 banned_tmp_regs = X64__get_sibd_addr(proc_state, &src_addr, src_vaddr);

        X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
        X64_Reg dst_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, dst_lreg,
                                       dst_size, true, banned_tmp_regs); // Get actual reg or a tmp if spilled.
        x64_mov_ext_rm_emit_funcs[movext_kind](&proc_state->instrs, dst_size, dst_reg, src_size, src_addr);
        X64__end_reg_group(&tmp_group);
}

static void X64__emit_bin_int_rm_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, u32 op_size,
                                       u32 op1_lreg, X64_MemAddr* op2_vaddr)
{
    X64_SIBD_Addr op2_addr = {0};
    u32 pinned_regs = X64__get_sibd_addr(proc_state, &op2_addr, op2_vaddr);

    X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
    X64_Reg op1_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, op1_lreg, op_size, writes_op1, pinned_regs);
    assert(op_size <= 8 && IS_POW2(op_size));

    x64_bin_int_rm_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_reg, op2_addr);
    X64__end_reg_group(&tmp_group);
}

static void X64__emit_bin_int_mr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size,
                                       X64_MemAddr* op1_vaddr, u32 op2_lreg)
{
    X64_SIBD_Addr op1_addr = {0};
    u32 pinned_regs = X64__get_sibd_addr(proc_state, &op1_addr, op1_vaddr);

    X64_Reg_Group tmp_group = X64__begin_reg_group(proc_state);
    X64_Reg op2_reg = X64__get_reg(&tmp_group, X64_REG_CLASS_INT, op2_lreg, op_size, false, pinned_regs);
    assert(op_size <= 8 && IS_POW2(op_size));

    x64_bin_int_mr_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_addr, op2_reg);
    X64__end_reg_group(&tmp_group);
}

static void X64__emit_bin_int_mi_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size,
                                       X64_MemAddr* op1_vaddr, Scalar op2_imm)
{
    X64_SIBD_Addr op1_addr = {0};
    X64__get_sibd_addr(proc_state, &op1_addr, op1_vaddr);
    x64_bin_int_mi_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_addr, op2_imm.as_int._u32);
}

static void X64__emit_bin_int_ri_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size, u32 op1_lreg, Scalar op2_imm)
{
    X64_LRegLoc op1_loc = X64__lreg_loc(proc_state, op1_lreg);

    switch (op1_loc.kind) {
    case X64_LREG_LOC_REG: {
        x64_bin_int_ri_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_loc.reg, op2_imm.as_int._u32);
        break;
    }
    case X64_LREG_LOC_STACK: {
        const X64_SIBD_Addr op1_addr = X64__get_stack_offset_addr(op1_loc.offset);
        x64_bin_int_mi_emit_funcs[instr_kind](&proc_state->instrs, op_size, op1_addr, op2_imm.as_int._u32);
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64__gen_instr(X64_Proc_State* proc_state, X64_Instr* instr, bool is_last_instr, long bblock_id)
{
    AllocatorState mem_state = allocator_get_state(proc_state->tmp_mem);

    switch (instr->kind) {
    case X64_InstrAdd_R_R_KIND: {
        X64_InstrAdd_R_R* act_instr = (X64_InstrAdd_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_ADD_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrAdd_R_M_KIND: {
        X64_InstrAdd_R_M* act_instr = (X64_InstrAdd_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_ADD_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrAdd_R_I_KIND: {
        X64_InstrAdd_R_I* act_instr = (X64_InstrAdd_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_ADD_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrSub_R_R_KIND: {
        X64_InstrSub_R_R* act_instr = (X64_InstrSub_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_SUB_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrSub_R_M_KIND: {
        X64_InstrSub_R_M* act_instr = (X64_InstrSub_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_SUB_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrSub_R_I_KIND: {
        X64_InstrSub_R_I* act_instr = (X64_InstrSub_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_SUB_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrIMul_R_R_KIND: {
        X64_InstrIMul_R_R* act_instr = (X64_InstrIMul_R_R*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_IMUL_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrIMul_R_M_KIND: {
        X64_InstrIMul_R_M* act_instr = (X64_InstrIMul_R_M*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_IMUL_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrIMul_R_I_KIND: {
        X64_InstrIMul_R_I* act_instr = (X64_InstrIMul_R_I*)instr;
        u8 size = act_instr->size;

        X64__emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_IMUL_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMov_R_R_KIND: {
        X64_InstrMov_R_R* act_instr = (X64_InstrMov_R_R*)instr;
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
    case X64_InstrMov_R_M_KIND: {
        X64_InstrMov_R_M* act_instr = (X64_InstrMov_R_M*)instr;
        const u8 size = act_instr->size;

        X64__emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_MOV_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case X64_InstrMov_M_R_KIND: {
        X64_InstrMov_M_R* act_instr = (X64_InstrMov_M_R*)instr;
        const u8 size = act_instr->size;

        X64__emit_bin_int_mr_instr(proc_state, X64_Instr_Kind_MOV_MR, size, &act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMov_R_I_KIND: {
        X64_InstrMov_R_I* act_instr = (X64_InstrMov_R_I*)instr;
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
        X64_InstrMov_M_I* act_instr = (X64_InstrMov_M_I*)instr;
        const u8 size = act_instr->size;

        X64__emit_bin_int_mi_instr(proc_state, X64_Instr_Kind_MOV_MI, size, &act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMovSX_R_R_KIND: {
        X64_InstrMovSX_R_R* act_instr = (X64_InstrMovSX_R_R*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;
        X64_Instr_Kind movsx_kind = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? X64_Instr_Kind_MOVSXD_RR :
                                                                                             X64_Instr_Kind_MOVSX_RR;

        X64__emit_mov_ext_rr_instr(proc_state, movsx_kind, dst_size, act_instr->dst, src_size, act_instr->src);
        break;
    }
    case X64_InstrMovSX_R_M_KIND: {
        X64_InstrMovSX_R_M* act_instr = (X64_InstrMovSX_R_M*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;
        X64_Instr_Kind movsx_kind = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? X64_Instr_Kind_MOVSXD_RR :
                                                                                             X64_Instr_Kind_MOVSX_RR;

        X64__emit_mov_ext_rm_instr(proc_state, movsx_kind, dst_size, act_instr->dst, src_size, &act_instr->src);
        break;
    }
    case X64_InstrMovZX_R_R_KIND: {
        X64_InstrMovZX_R_R* act_instr = (X64_InstrMovZX_R_R*)instr;
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
        X64_InstrMovZX_R_M* act_instr = (X64_InstrMovZX_R_M*)instr;
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
    case X64_InstrLEA_KIND: {
        X64_InstrLEA* act_instr = (X64_InstrLEA*)instr;
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
    case X64_InstrJmp_KIND: {
        X64_InstrJmp* act_instr = (X64_InstrJmp*)instr;
        long target_id = act_instr->target->id;

        if (target_id != bblock_id + 1) {
            assert(target_id >= 0);
            X64__emit_instr_jmp(&proc_state->instrs, (size_t)target_id);
        }
        break;
    }
    case X64_InstrRet_KIND: {
        if (!is_last_instr) {
            X64__emit_instr_jmp_to_ret(&proc_state->instrs);
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
        sub_rsp_instr->kind = X64_Instr_Kind_SUB_RI;
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
    X64__patch_jmp_instrs(state.instrs, array_len(state.instrs), &bblock_instr_starts);

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

    // Postamble.
    X64__emit_instr_mov_rr(&state.instrs, X64_MAX_INT_REG_SIZE, X64_RSP, X64_RBP);
    X64__emit_instr_pop(&state.instrs, X64_RBP);
    X64__emit_instr_ret(&state.instrs);

    //////////////////////////////////////////////////////////////////////////////////////////
    allocator_restore_state(tmp_mem_state);

    return state.instrs;
}
