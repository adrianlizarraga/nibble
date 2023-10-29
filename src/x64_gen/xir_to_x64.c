#include "x64_gen/xir_to_x64.h"
#include "allocator.h"
#include "array.h"
#include "ast/module.h"
#include "bytecode/module.h"
#include "hash_map.h"
#include "nibble.h"
#include "stream.h"
#include "x64_gen/regs.h"
#include "x64_gen/xir.h"
#include "x64_gen/reg_alloc.h"
#include "x64_gen/print_xir.h"

typedef struct X64_Proc_State {
    Allocator* gen_mem;
    Allocator* tmp_mem;
    Symbol* sym; // Procedure symbol.
    XIR_Builder* xir_builder;
    X64_ScratchRegs (*scratch_regs)[X64_REG_CLASS_COUNT];
    X64_Instrs instrs;
    u32 curr_bblock;
} X64_Proc_State;

static inline X64_SIBD_Addr X64_get_rbp_offset_addr(s32 offset)
{
    X64_SIBD_Addr addr = {.kind = X64_SIBD_ADDR_LOCAL, .local = {.base_reg = X64_RBP, .index_reg = X64_REG_COUNT, .disp = offset}};

    return addr;
}

static inline X64_SIBD_Addr X64_get_rsp_offset_addr(s32 offset)
{
    X64_SIBD_Addr addr = {.kind = X64_SIBD_ADDR_LOCAL, .local = {.base_reg = X64_RSP, .index_reg = X64_REG_COUNT, .disp = offset}};

    return addr;
}

static inline FloatKind X64_flt_kind_from_size(u8 size)
{
    assert(size == float_kind_sizes[FLOAT_F64] || size == float_kind_sizes[FLOAT_F32]);
    return size == float_kind_sizes[FLOAT_F64] ? FLOAT_F64 : FLOAT_F32;
}

static void X64_push_instr(X64_Proc_State* proc_state, X64_Instr* instr)
{
    const u32 bblock_idx = proc_state->curr_bblock;
    X64_Instrs* instrs = &proc_state->instrs;

    assert(bblock_idx < instrs->num_bblocks);
    X64_BBlock* bblock = &instrs->bblocks[bblock_idx];

    if (!bblock->tail) {
        bblock->head = instr;
    }
    else {
        bblock->tail->next = instr;
    }

    bblock->tail = instr;
    bblock->num_instrs += 1;
    instrs->num_instrs += 1;
}

static inline X64_Instr* X64_alloc_instr(Allocator* alloc, X64_Instr_Kind kind)
{
    X64_Instr* instr = alloc_type(alloc, X64_Instr, true);
    instr->flags = (u32)kind;
    return instr;
}

static void X64_emit_instr_ret(X64_Proc_State* proc_state)
{
    X64_push_instr(proc_state, X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_RET));
}

static void X64_emit_instr_call(X64_Proc_State* proc_state, const Symbol* proc_sym)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_CALL);
    instr->call.proc_sym = proc_sym;
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_call_r(X64_Proc_State* proc_state, u8 reg)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_CALL_R);
    instr->call_r.reg = reg;
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_call_m(X64_Proc_State* proc_state, X64_SIBD_Addr mem)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_CALL_M);
    instr->call_m.mem = mem;
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_jmp(X64_Proc_State* proc_state, u32 target)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_JMP);
    instr->jmp.target = target;
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_jmp_to_ret(X64_Proc_State* proc_state)
{
    X64_push_instr(proc_state, X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_JMP_TO_RET));
}

static void X64_emit_instr_jmpcc(X64_Proc_State* proc_state, ConditionKind cond_kind, u32 target)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_JMPCC);
    instr->jmpcc.target = target;
    instr->jmpcc.cond = cond_kind;
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_setcc_r(X64_Proc_State* proc_state, ConditionKind cond_kind, u8 dst)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_SETCC_R);
    instr->setcc_r.cond = cond_kind;
    instr->setcc_r.dst = dst;
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_setcc_m(X64_Proc_State* proc_state, ConditionKind cond_kind, X64_SIBD_Addr dst)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_SETCC_M);
    instr->setcc_m.cond = cond_kind;
    instr->setcc_m.dst = dst;
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_push(X64_Proc_State* proc_state, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_PUSH);
    instr->push.reg = reg;
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_pop(X64_Proc_State* proc_state, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_POP);
    instr->pop.reg = reg;
    X64_push_instr(proc_state, instr);
}

#define X64_DEF_EMIT_INSTR_BINARY_RR(field, kind)                                                     \
    static void X64_emit_instr_##field(X64_Proc_State* proc_state, u8 size, X64_Reg dst, X64_Reg src) \
    {                                                                                                 \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, kind);                                \
        instr->field.size = size;                                                                     \
        instr->field.dst = dst;                                                                       \
        instr->field.src = src;                                                                       \
                                                                                                      \
        X64_push_instr(proc_state, instr);                                                            \
    }

#define X64_DEF_EMIT_INSTR_BINARY_RM(field, kind)                                                           \
    static void X64_emit_instr_##field(X64_Proc_State* proc_state, u8 size, X64_Reg dst, X64_SIBD_Addr src) \
    {                                                                                                       \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, kind);                                      \
        instr->field.size = size;                                                                           \
        instr->field.dst = dst;                                                                             \
        instr->field.src = src;                                                                             \
                                                                                                            \
        X64_push_instr(proc_state, instr);                                                                  \
    }

#define X64_DEF_EMIT_INSTR_BINARY_MR(field, kind)                                                           \
    static void X64_emit_instr_##field(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr dst, X64_Reg src) \
    {                                                                                                       \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, kind);                                      \
        instr->field.size = size;                                                                           \
        instr->field.dst = dst;                                                                             \
        instr->field.src = src;                                                                             \
                                                                                                            \
        X64_push_instr(proc_state, instr);                                                                  \
    }

#define X64_DEF_EMIT_INSTR_BINARY_RI(field, kind)                                                 \
    static void X64_emit_instr_##field(X64_Proc_State* proc_state, u8 size, X64_Reg dst, u32 imm) \
    {                                                                                             \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, kind);                            \
        instr->field.size = size;                                                                 \
        instr->field.dst = dst;                                                                   \
        instr->field.imm = imm;                                                                   \
                                                                                                  \
        X64_push_instr(proc_state, instr);                                                        \
    }

#define X64_DEF_EMIT_INSTR_BINARY_MI(field, kind)                                                       \
    static void X64_emit_instr_##field(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr dst, u32 imm) \
    {                                                                                                   \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, kind);                                  \
        instr->field.size = size;                                                                       \
        instr->field.dst = dst;                                                                         \
        instr->field.imm = imm;                                                                         \
                                                                                                        \
        X64_push_instr(proc_state, instr);                                                              \
    }

X64_DEF_EMIT_INSTR_BINARY_RR(add_rr, X64_Instr_Kind_ADD_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(add_rm, X64_Instr_Kind_ADD_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(add_mr, X64_Instr_Kind_ADD_MR)
X64_DEF_EMIT_INSTR_BINARY_RI(add_ri, X64_Instr_Kind_ADD_RI)
X64_DEF_EMIT_INSTR_BINARY_MI(add_mi, X64_Instr_Kind_ADD_MI)

X64_DEF_EMIT_INSTR_BINARY_RR(sub_rr, X64_Instr_Kind_SUB_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(sub_rm, X64_Instr_Kind_SUB_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(sub_mr, X64_Instr_Kind_SUB_MR)
X64_DEF_EMIT_INSTR_BINARY_RI(sub_ri, X64_Instr_Kind_SUB_RI)
X64_DEF_EMIT_INSTR_BINARY_MI(sub_mi, X64_Instr_Kind_SUB_MI)

X64_DEF_EMIT_INSTR_BINARY_RR(imul_rr, X64_Instr_Kind_IMUL_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(imul_rm, X64_Instr_Kind_IMUL_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(imul_mr, X64_Instr_Kind_IMUL_MR)
X64_DEF_EMIT_INSTR_BINARY_RI(imul_ri, X64_Instr_Kind_IMUL_RI)
X64_DEF_EMIT_INSTR_BINARY_MI(imul_mi, X64_Instr_Kind_IMUL_MI)

X64_DEF_EMIT_INSTR_BINARY_RR(and_rr, X64_Instr_Kind_AND_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(and_rm, X64_Instr_Kind_AND_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(and_mr, X64_Instr_Kind_AND_MR)
X64_DEF_EMIT_INSTR_BINARY_RI(and_ri, X64_Instr_Kind_AND_RI)
X64_DEF_EMIT_INSTR_BINARY_MI(and_mi, X64_Instr_Kind_AND_MI)

X64_DEF_EMIT_INSTR_BINARY_RR(or_rr, X64_Instr_Kind_OR_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(or_rm, X64_Instr_Kind_OR_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(or_mr, X64_Instr_Kind_OR_MR)
X64_DEF_EMIT_INSTR_BINARY_RI(or_ri, X64_Instr_Kind_OR_RI)
X64_DEF_EMIT_INSTR_BINARY_MI(or_mi, X64_Instr_Kind_OR_MI)

X64_DEF_EMIT_INSTR_BINARY_RR(xor_rr, X64_Instr_Kind_XOR_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(xor_rm, X64_Instr_Kind_XOR_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(xor_mr, X64_Instr_Kind_XOR_MR)
X64_DEF_EMIT_INSTR_BINARY_RI(xor_ri, X64_Instr_Kind_XOR_RI)
X64_DEF_EMIT_INSTR_BINARY_MI(xor_mi, X64_Instr_Kind_XOR_MI)

#define X64_DEF_EMIT_INSTR_FLT_BINARY_RR(f_d, k_d)                                                         \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, FloatKind kind, X64_Reg dst, X64_Reg src) \
    {                                                                                                      \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                      \
        instr->f_d.kind = kind;                                                                            \
        instr->f_d.dst = dst;                                                                              \
        instr->f_d.src = src;                                                                              \
                                                                                                           \
        X64_push_instr(proc_state, instr);                                                                 \
    }

#define X64_DEF_EMIT_INSTR_FLT_BINARY_RM(f_d, k_d)                                                               \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src) \
    {                                                                                                            \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                            \
        instr->f_d.kind = kind;                                                                                  \
        instr->f_d.dst = dst;                                                                                    \
        instr->f_d.src = src;                                                                                    \
                                                                                                                 \
        X64_push_instr(proc_state, instr);                                                                       \
    }

#define X64_DEF_EMIT_INSTR_FLT_BINARY_MR(f_d, k_d)                                                               \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src) \
    {                                                                                                            \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                            \
        instr->f_d.kind = kind;                                                                                  \
        instr->f_d.dst = dst;                                                                                    \
        instr->f_d.src = src;                                                                                    \
                                                                                                                 \
        X64_push_instr(proc_state, instr);                                                                       \
    }

X64_DEF_EMIT_INSTR_FLT_BINARY_RR(add_flt_rr, X64_Instr_Kind_ADD_FLT_RR)
X64_DEF_EMIT_INSTR_FLT_BINARY_RM(add_flt_rm, X64_Instr_Kind_ADD_FLT_RM)
X64_DEF_EMIT_INSTR_FLT_BINARY_MR(add_flt_mr, X64_Instr_Kind_ADD_FLT_MR)

X64_DEF_EMIT_INSTR_FLT_BINARY_RR(sub_flt_rr, X64_Instr_Kind_SUB_FLT_RR)
X64_DEF_EMIT_INSTR_FLT_BINARY_RM(sub_flt_rm, X64_Instr_Kind_SUB_FLT_RM)
X64_DEF_EMIT_INSTR_FLT_BINARY_MR(sub_flt_mr, X64_Instr_Kind_SUB_FLT_MR)

X64_DEF_EMIT_INSTR_FLT_BINARY_RR(mul_flt_rr, X64_Instr_Kind_MUL_FLT_RR)
X64_DEF_EMIT_INSTR_FLT_BINARY_RM(mul_flt_rm, X64_Instr_Kind_MUL_FLT_RM)
X64_DEF_EMIT_INSTR_FLT_BINARY_MR(mul_flt_mr, X64_Instr_Kind_MUL_FLT_MR)

X64_DEF_EMIT_INSTR_FLT_BINARY_RR(div_flt_rr, X64_Instr_Kind_DIV_FLT_RR)
X64_DEF_EMIT_INSTR_FLT_BINARY_RM(div_flt_rm, X64_Instr_Kind_DIV_FLT_RM)
X64_DEF_EMIT_INSTR_FLT_BINARY_MR(div_flt_mr, X64_Instr_Kind_DIV_FLT_MR)

#define X64_DEF_EMIT_INSTR_UNARY_R(f_d, k_d)                                           \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 size, X64_Reg dst) \
    {                                                                                  \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                  \
        instr->f_d.size = size;                                                        \
        instr->f_d.dst = dst;                                                          \
                                                                                       \
        X64_push_instr(proc_state, instr);                                             \
    }

#define X64_DEF_EMIT_INSTR_UNARY_M(f_d, k_d)                                                 \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr dst) \
    {                                                                                        \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                        \
        instr->f_d.size = size;                                                              \
        instr->f_d.dst = dst;                                                                \
                                                                                             \
        X64_push_instr(proc_state, instr);                                                   \
    }

X64_DEF_EMIT_INSTR_UNARY_R(neg_r, X64_Instr_Kind_NEG_R)
X64_DEF_EMIT_INSTR_UNARY_M(neg_m, X64_Instr_Kind_NEG_M)

X64_DEF_EMIT_INSTR_UNARY_R(not_r, X64_Instr_Kind_NOT_R)
X64_DEF_EMIT_INSTR_UNARY_M(not_m, X64_Instr_Kind_NOT_M)

#define X64_DEF_EMIT_INSTR_SHIFT_RR(f_d, k_d)                                          \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 size, X64_Reg dst) \
    {                                                                                  \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                  \
        instr->f_d.size = size;                                                        \
        instr->f_d.dst = dst;                                                          \
                                                                                       \
        X64_push_instr(proc_state, instr);                                             \
    }

#define X64_DEF_EMIT_INSTR_SHIFT_MR(f_d, k_d)                                                \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr dst) \
    {                                                                                        \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                        \
        instr->f_d.size = size;                                                              \
        instr->f_d.dst = dst;                                                                \
                                                                                             \
        X64_push_instr(proc_state, instr);                                                   \
    }

#define X64_DEF_EMIT_INSTR_SHIFT_RI(f_d, k_d)                                                  \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 size, X64_Reg dst, u8 imm) \
    {                                                                                          \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                          \
        instr->f_d.size = size;                                                                \
        instr->f_d.dst = dst;                                                                  \
        instr->f_d.imm = imm;                                                                  \
                                                                                               \
        X64_push_instr(proc_state, instr);                                                     \
    }

#define X64_DEF_EMIT_INSTR_SHIFT_MI(f_d, k_d)                                                        \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr dst, u8 imm) \
    {                                                                                                \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                \
        instr->f_d.size = size;                                                                      \
        instr->f_d.dst = dst;                                                                        \
        instr->f_d.imm = imm;                                                                        \
                                                                                                     \
        X64_push_instr(proc_state, instr);                                                           \
    }

X64_DEF_EMIT_INSTR_SHIFT_RR(sar_rr, X64_Instr_Kind_SAR_RR)
X64_DEF_EMIT_INSTR_SHIFT_MR(sar_mr, X64_Instr_Kind_SAR_MR)
X64_DEF_EMIT_INSTR_SHIFT_RI(sar_ri, X64_Instr_Kind_SAR_RI)
X64_DEF_EMIT_INSTR_SHIFT_MI(sar_mi, X64_Instr_Kind_SAR_MI)

X64_DEF_EMIT_INSTR_SHIFT_RR(shl_rr, X64_Instr_Kind_SHL_RR)
X64_DEF_EMIT_INSTR_SHIFT_MR(shl_mr, X64_Instr_Kind_SHL_MR)
X64_DEF_EMIT_INSTR_SHIFT_RI(shl_ri, X64_Instr_Kind_SHL_RI)
X64_DEF_EMIT_INSTR_SHIFT_MI(shl_mi, X64_Instr_Kind_SHL_MI)

#define X64_DEF_EMIT_INSTR_DIV_R(f_d, k_d)                                             \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 size, X64_Reg src) \
    {                                                                                  \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                  \
        instr->f_d.size = size;                                                        \
        instr->f_d.src = src;                                                          \
                                                                                       \
        X64_push_instr(proc_state, instr);                                             \
    }

#define X64_DEF_EMIT_INSTR_DIV_M(f_d, k_d)                                                   \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr src) \
    {                                                                                        \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                        \
        instr->f_d.size = size;                                                              \
        instr->f_d.src = src;                                                                \
                                                                                             \
        X64_push_instr(proc_state, instr);                                                   \
    }

X64_DEF_EMIT_INSTR_DIV_R(div_r, X64_Instr_Kind_DIV_R)
X64_DEF_EMIT_INSTR_DIV_M(div_m, X64_Instr_Kind_DIV_M)

X64_DEF_EMIT_INSTR_DIV_R(idiv_r, X64_Instr_Kind_IDIV_R)
X64_DEF_EMIT_INSTR_DIV_M(idiv_m, X64_Instr_Kind_IDIV_M)

static void X64_emit_instr_sext_ax_into_dx(X64_Proc_State* proc_state, u8 size)
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
        NIBBLE_FATAL_EXIT("Unhandled size %d for X64_emit_instr_sext_ax_into_dx()", size);
        break;
    }

    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, kind);
    X64_push_instr(proc_state, instr);
}

X64_DEF_EMIT_INSTR_BINARY_RR(mov_rr, X64_Instr_Kind_MOV_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(mov_rm, X64_Instr_Kind_MOV_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(mov_mr, X64_Instr_Kind_MOV_MR)
X64_DEF_EMIT_INSTR_BINARY_MI(mov_mi, X64_Instr_Kind_MOV_MI)

// MOV_RI is the only instruction that can load an 8-byte immediate constant.
static void X64_emit_instr_mov_ri(X64_Proc_State* proc_state, u8 size, X64_Reg dst, u64 imm)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_MOV_RI);
    instr->mov_ri.size = size;
    instr->mov_ri.dst = dst;
    instr->mov_ri.imm = imm;

    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_mov_rrh(X64_Proc_State* proc_state, X64_Reg dst, X64_Reg src)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_MOV_RR);
    instr->flags |= X64_INSTR_MOV_SRC_RH_MASK;
    instr->mov_rr.size = 1;
    instr->mov_rr.dst = dst;
    instr->mov_rr.src = src;

    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_mov_mrh(X64_Proc_State* proc_state, X64_SIBD_Addr dst, X64_Reg src)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_MOV_MR);
    instr->flags |= X64_INSTR_MOV_SRC_RH_MASK;
    instr->mov_mr.size = 1;
    instr->mov_mr.dst = dst;
    instr->mov_mr.src = src;

    X64_push_instr(proc_state, instr);
}

#define X64_DEF_EMIT_INSTR_MOV_EXT_RR(f_d, k_d)                                                                      \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src) \
    {                                                                                                                \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                                \
        instr->f_d.dst_size = dst_size;                                                                              \
        instr->f_d.src_size = src_size;                                                                              \
        instr->f_d.dst = dst;                                                                                        \
        instr->f_d.src = src;                                                                                        \
                                                                                                                     \
        X64_push_instr(proc_state, instr);                                                                           \
    }

#define X64_DEF_EMIT_INSTR_MOV_EXT_RM(f_d, k_d)                                                                            \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src) \
    {                                                                                                                      \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                                      \
        instr->f_d.dst_size = dst_size;                                                                                    \
        instr->f_d.src_size = src_size;                                                                                    \
        instr->f_d.dst = dst;                                                                                              \
        instr->f_d.src = src;                                                                                              \
                                                                                                                           \
        X64_push_instr(proc_state, instr);                                                                                 \
    }

X64_DEF_EMIT_INSTR_MOV_EXT_RR(movsx_rr, X64_Instr_Kind_MOVSX_RR)
X64_DEF_EMIT_INSTR_MOV_EXT_RM(movsx_rm, X64_Instr_Kind_MOVSX_RM)

X64_DEF_EMIT_INSTR_MOV_EXT_RR(movsxd_rr, X64_Instr_Kind_MOVSXD_RR)
X64_DEF_EMIT_INSTR_MOV_EXT_RM(movsxd_rm, X64_Instr_Kind_MOVSXD_RM)

X64_DEF_EMIT_INSTR_MOV_EXT_RR(movzx_rr, X64_Instr_Kind_MOVZX_RR)
X64_DEF_EMIT_INSTR_MOV_EXT_RM(movzx_rm, X64_Instr_Kind_MOVZX_RM)

X64_DEF_EMIT_INSTR_FLT_BINARY_RR(mov_flt_rr, X64_Instr_Kind_MOV_FLT_RR)
X64_DEF_EMIT_INSTR_FLT_BINARY_RM(mov_flt_rm, X64_Instr_Kind_MOV_FLT_RM)
X64_DEF_EMIT_INSTR_FLT_BINARY_MR(mov_flt_mr, X64_Instr_Kind_MOV_FLT_MR)

#define X64_DEF_EMIT_INSTR_CVT_FLT_RR(f_d, k_d)                                            \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, X64_Reg dst, X64_Reg src) \
    {                                                                                      \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                      \
        instr->f_d.dst = dst;                                                              \
        instr->f_d.src = src;                                                              \
                                                                                           \
        X64_push_instr(proc_state, instr);                                                 \
    }

#define X64_DEF_EMIT_INSTR_CVT_FLT_RM(f_d, k_d)                                                  \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, X64_Reg dst, X64_SIBD_Addr src) \
    {                                                                                            \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                            \
        instr->f_d.dst = dst;                                                                    \
        instr->f_d.src = src;                                                                    \
                                                                                                 \
        X64_push_instr(proc_state, instr);                                                       \
    }

X64_DEF_EMIT_INSTR_CVT_FLT_RR(cvtss2sd_rr, X64_Instr_Kind_CVTSS2SD_RR)
X64_DEF_EMIT_INSTR_CVT_FLT_RM(cvtss2sd_rm, X64_Instr_Kind_CVTSS2SD_RM)

X64_DEF_EMIT_INSTR_CVT_FLT_RR(cvtsd2ss_rr, X64_Instr_Kind_CVTSD2SS_RR)
X64_DEF_EMIT_INSTR_CVT_FLT_RM(cvtsd2ss_rm, X64_Instr_Kind_CVTSD2SS_RM)

#define X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RR(f_d, k_d)                                                      \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, X64_Reg dst, bool dst_8bytes, X64_Reg src) \
    {                                                                                                       \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                       \
        instr->f_d.dst = dst;                                                                               \
        instr->f_d.src = src;                                                                               \
                                                                                                            \
        if (dst_8bytes) {                                                                                   \
            instr->flags |= X64_INSTR_CVT_FLT_SI_INT64_MASK;                                                \
        }                                                                                                   \
                                                                                                            \
        X64_push_instr(proc_state, instr);                                                                  \
    }

#define X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RM(f_d, k_d)                                                            \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, X64_Reg dst, bool dst_8bytes, X64_SIBD_Addr src) \
    {                                                                                                             \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                             \
        instr->f_d.dst = dst;                                                                                     \
        instr->f_d.src = src;                                                                                     \
                                                                                                                  \
        if (dst_8bytes) {                                                                                         \
            instr->flags |= X64_INSTR_CVT_FLT_SI_INT64_MASK;                                                      \
        }                                                                                                         \
                                                                                                                  \
        X64_push_instr(proc_state, instr);                                                                        \
    }

// f32 to int
X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RR(cvttss2si_rr, X64_Instr_Kind_CVTTSS2SI_RR)
X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RM(cvttss2si_rm, X64_Instr_Kind_CVTTSS2SI_RM)

// f64 to int
X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RR(cvttsd2si_rr, X64_Instr_Kind_CVTTSD2SI_RR)
X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RM(cvttsd2si_rm, X64_Instr_Kind_CVTTSD2SI_RM)

#define X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RR(f_d, k_d)                                                      \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, X64_Reg dst, X64_Reg src, bool src_8bytes) \
    {                                                                                                       \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                       \
        instr->f_d.dst = dst;                                                                               \
        instr->f_d.src = src;                                                                               \
                                                                                                            \
        if (src_8bytes) {                                                                                   \
            instr->flags |= X64_INSTR_CVT_FLT_SI_INT64_MASK;                                                \
        }                                                                                                   \
                                                                                                            \
        X64_push_instr(proc_state, instr);                                                                  \
    }

#define X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RM(f_d, k_d)                                                            \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, X64_Reg dst, X64_SIBD_Addr src, bool src_8bytes) \
    {                                                                                                             \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                                             \
        instr->f_d.dst = dst;                                                                                     \
        instr->f_d.src = src;                                                                                     \
                                                                                                                  \
        if (src_8bytes) {                                                                                         \
            instr->flags |= X64_INSTR_CVT_FLT_SI_INT64_MASK;                                                      \
        }                                                                                                         \
                                                                                                                  \
        X64_push_instr(proc_state, instr);                                                                        \
    }

// int to f32
X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RR(cvtsi2ss_rr, X64_Instr_Kind_CVTSI2SS_RR)
X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RM(cvtsi2ss_rm, X64_Instr_Kind_CVTSI2SS_RM)

// int to f64
X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RR(cvtsi2sd_rr, X64_Instr_Kind_CVTSI2SD_RR)
X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RM(cvtsi2sd_rm, X64_Instr_Kind_CVTSI2SD_RM)

static void X64_emit_instr_movdqu_mr(X64_Proc_State* proc_state, X64_SIBD_Addr dst, X64_Reg src)
{
    assert(x64_reg_classes[src] == X64_REG_CLASS_FLOAT);
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_MOVDQU_MR);
    instr->movdqu_mr.dst = dst;
    instr->movdqu_mr.src = src;

    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_movdqu_rm(X64_Proc_State* proc_state, X64_Reg dst, X64_SIBD_Addr src)
{
    assert(x64_reg_classes[dst] == X64_REG_CLASS_FLOAT);
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_MOVDQU_RM);
    instr->movdqu_rm.dst = dst;
    instr->movdqu_rm.src = src;

    X64_push_instr(proc_state, instr);
}

X64_DEF_EMIT_INSTR_BINARY_RR(cmp_rr, X64_Instr_Kind_CMP_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(cmp_rm, X64_Instr_Kind_CMP_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(cmp_mr, X64_Instr_Kind_CMP_MR)
X64_DEF_EMIT_INSTR_BINARY_RI(cmp_ri, X64_Instr_Kind_CMP_RI)
X64_DEF_EMIT_INSTR_BINARY_MI(cmp_mi, X64_Instr_Kind_CMP_MI)

#define X64_DEF_EMIT_INSTR_CMP_FLT_RR(f_d, k_d)                                            \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, X64_Reg dst, X64_Reg src) \
    {                                                                                      \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                      \
        instr->f_d.dst = dst;                                                              \
        instr->f_d.src = src;                                                              \
                                                                                           \
        X64_push_instr(proc_state, instr);                                                 \
    }

#define X64_DEF_EMIT_INSTR_CMP_FLT_RM(f_d, k_d)                                                  \
    static void X64_emit_instr_##f_d(X64_Proc_State* proc_state, X64_Reg dst, X64_SIBD_Addr src) \
    {                                                                                            \
        X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, k_d);                            \
        instr->f_d.dst = dst;                                                                    \
        instr->f_d.src = src;                                                                    \
                                                                                                 \
        X64_push_instr(proc_state, instr);                                                       \
    }

// Compare f32s
X64_DEF_EMIT_INSTR_CMP_FLT_RR(ucomiss_rr, X64_Instr_Kind_UCOMISS_RR)
X64_DEF_EMIT_INSTR_CMP_FLT_RM(ucomiss_rm, X64_Instr_Kind_UCOMISS_RM)

// Compare f64s
X64_DEF_EMIT_INSTR_CMP_FLT_RR(ucomisd_rr, X64_Instr_Kind_UCOMISD_RR)
X64_DEF_EMIT_INSTR_CMP_FLT_RM(ucomisd_rm, X64_Instr_Kind_UCOMISD_RM)

static void X64_emit_instr_lea(X64_Proc_State* proc_state, X64_Reg dst, X64_SIBD_Addr src)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_LEA);
    instr->lea.dst = dst;
    instr->lea.src = src;

    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_rep_movsb(X64_Proc_State* proc_state)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_REP_MOVSB);
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_rep_stosb(X64_Proc_State* proc_state)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_REP_STOSB);
    X64_push_instr(proc_state, instr);
}

static void X64_emit_instr_syscall(X64_Proc_State* proc_state)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, X64_Instr_Kind_SYSCALL);
    X64_push_instr(proc_state, instr);
}

static X64_Instr* X64_emit_instr_placeholder(X64_Proc_State* proc_state, X64_Instr_Kind kind)
{
    X64_Instr* instr = X64_alloc_instr(proc_state->gen_mem, kind);
    X64_push_instr(proc_state, instr);
    return instr;
}

static void X64_push_reg_to_stack(X64_Proc_State* proc_state, X64_Reg reg)
{
    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        X64_emit_instr_push(proc_state, reg);
    }
    else {
        X64_emit_instr_sub_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Make room for 16 bytes on the stack.
        X64_emit_instr_movdqu_mr(proc_state, X64_get_rsp_offset_addr(0), reg); // movdqu oword [rsp], reg
    }
}

static void X64_pop_reg_from_stack(X64_Proc_State* proc_state, X64_Reg reg)
{
    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        X64_emit_instr_pop(proc_state, reg);
    }
    else {
        X64_emit_instr_movdqu_rm(proc_state, reg, X64_get_rsp_offset_addr(0)); // movdqu reg, oword [rsp]
        X64_emit_instr_add_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Clean up 16 bytes from stack.
    }
}

static void X64_load_prim_from_mem(X64_Proc_State* proc_state, u8 size, X64_Reg dst, X64_SIBD_Addr src)
{
    const X64_RegClass reg_class = x64_reg_classes[dst];

    if (reg_class == X64_REG_CLASS_INT) {
        X64_emit_instr_mov_rm(proc_state, size, dst, src);
    }
    else {
        assert(reg_class == X64_REG_CLASS_FLOAT);
        X64_emit_instr_mov_flt_rm(proc_state, X64_flt_kind_from_size(size), dst, src);
    }
}

static void X64_save_prim_to_mem(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr dst, X64_Reg src)
{
    const X64_RegClass reg_class = x64_reg_classes[src];
    if (reg_class == X64_REG_CLASS_INT) {
        X64_emit_instr_mov_mr(proc_state, size, dst, src);
    }
    else {
        assert(reg_class == X64_REG_CLASS_FLOAT);
        X64_emit_instr_mov_flt_mr(proc_state, X64_flt_kind_from_size(size), dst, src);
    }
}

typedef void X64_Emit_Bin_Int_RR_Func(X64_Proc_State* proc_state, u8 size, X64_Reg dst, X64_Reg src);
typedef void X64_Emit_Bin_Int_RM_Func(X64_Proc_State* proc_state, u8 size, X64_Reg dst, X64_SIBD_Addr src);
typedef void X64_Emit_Bin_Int_RI_Func(X64_Proc_State* proc_state, u8 size, X64_Reg dst, u32 imm);
typedef void X64_Emit_Bin_Int_MR_Func(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr dst, X64_Reg src);
typedef void X64_Emit_Bin_Int_MI_Func(X64_Proc_State* proc_state, u8 size, X64_SIBD_Addr dst, u32 imm);
typedef void X64_Emit_Mov_Ext_RR_Func(X64_Proc_State* proc_state, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src);
typedef void X64_Emit_Mov_Ext_RM_Func(X64_Proc_State* proc_state, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src);

static inline X64_Emit_Bin_Int_RR_Func* x64_bin_int_rr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_RR:
        return X64_emit_instr_add_rr;
    case X64_Instr_Kind_SUB_RR:
        return X64_emit_instr_sub_rr;
    case X64_Instr_Kind_IMUL_RR:
        return X64_emit_instr_imul_rr;
    case X64_Instr_Kind_AND_RR:
        return X64_emit_instr_and_rr;
    case X64_Instr_Kind_OR_RR:
        return X64_emit_instr_or_rr;
    case X64_Instr_Kind_XOR_RR:
        return X64_emit_instr_xor_rr;
    case X64_Instr_Kind_MOV_RR:
        return X64_emit_instr_mov_rr;
    case X64_Instr_Kind_CMP_RR:
        return X64_emit_instr_cmp_rr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_rr_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Int_RM_Func* x64_bin_int_rm_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_RM:
        return X64_emit_instr_add_rm;
    case X64_Instr_Kind_SUB_RM:
        return X64_emit_instr_sub_rm;
    case X64_Instr_Kind_IMUL_RM:
        return X64_emit_instr_imul_rm;
    case X64_Instr_Kind_AND_RM:
        return X64_emit_instr_and_rm;
    case X64_Instr_Kind_OR_RM:
        return X64_emit_instr_or_rm;
    case X64_Instr_Kind_XOR_RM:
        return X64_emit_instr_xor_rm;
    case X64_Instr_Kind_MOV_RM:
        return X64_emit_instr_mov_rm;
    case X64_Instr_Kind_CMP_RM:
        return X64_emit_instr_cmp_rm;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_rm_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Int_RI_Func* x64_bin_int_ri_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_RI:
        return X64_emit_instr_add_ri;
    case X64_Instr_Kind_SUB_RI:
        return X64_emit_instr_sub_ri;
    case X64_Instr_Kind_IMUL_RI:
        return X64_emit_instr_imul_ri;
    case X64_Instr_Kind_AND_RI:
        return X64_emit_instr_and_ri;
    case X64_Instr_Kind_OR_RI:
        return X64_emit_instr_or_ri;
    case X64_Instr_Kind_XOR_RI:
        return X64_emit_instr_xor_ri;
    case X64_Instr_Kind_CMP_RI:
        return X64_emit_instr_cmp_ri;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_ri_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Int_MR_Func* x64_bin_int_mr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_MR:
        return X64_emit_instr_add_mr;
    case X64_Instr_Kind_SUB_MR:
        return X64_emit_instr_sub_mr;
    case X64_Instr_Kind_IMUL_MR:
        return X64_emit_instr_imul_mr;
    case X64_Instr_Kind_AND_MR:
        return X64_emit_instr_and_mr;
    case X64_Instr_Kind_OR_MR:
        return X64_emit_instr_or_mr;
    case X64_Instr_Kind_XOR_MR:
        return X64_emit_instr_xor_mr;
    case X64_Instr_Kind_MOV_MR:
        return X64_emit_instr_mov_mr;
    case X64_Instr_Kind_CMP_MR:
        return X64_emit_instr_cmp_mr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_mr_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Int_MI_Func* x64_bin_int_mi_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_MI:
        return X64_emit_instr_add_mi;
    case X64_Instr_Kind_SUB_MI:
        return X64_emit_instr_sub_mi;
    case X64_Instr_Kind_IMUL_MI:
        return X64_emit_instr_imul_mi;
    case X64_Instr_Kind_AND_MI:
        return X64_emit_instr_and_mi;
    case X64_Instr_Kind_OR_MI:
        return X64_emit_instr_or_mi;
    case X64_Instr_Kind_XOR_MI:
        return X64_emit_instr_xor_mi;
    case X64_Instr_Kind_MOV_MI:
        return X64_emit_instr_mov_mi;
    case X64_Instr_Kind_CMP_MI:
        return X64_emit_instr_cmp_mi;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_int_mi_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Mov_Ext_RR_Func* x64_mov_ext_rr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_MOVSX_RR:
        return X64_emit_instr_movsx_rr;
    case X64_Instr_Kind_MOVSXD_RR:
        return X64_emit_instr_movsxd_rr;
    case X64_Instr_Kind_MOVZX_RR:
        return X64_emit_instr_movzx_rr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_mov_ext_rr_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Mov_Ext_RM_Func* x64_mov_ext_rm_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_MOVSX_RM:
        return X64_emit_instr_movsx_rm;
    case X64_Instr_Kind_MOVSXD_RM:
        return X64_emit_instr_movsxd_rm;
    case X64_Instr_Kind_MOVZX_RM:
        return X64_emit_instr_movzx_rm;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_mov_ext_rm_emit_funcs()", kind);
        return NULL;
    }
}

typedef void X64_Emit_Bin_Flt_RR_Func(X64_Proc_State* proc_state, FloatKind kind, X64_Reg dst, X64_Reg src);
typedef void X64_Emit_Bin_Flt_RM_Func(X64_Proc_State* proc_state, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src);
typedef void X64_Emit_Bin_Flt_MR_Func(X64_Proc_State* proc_state, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src);

static inline X64_Emit_Bin_Flt_RR_Func* x64_bin_flt_rr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_FLT_RR:
        return X64_emit_instr_add_flt_rr;
    case X64_Instr_Kind_SUB_FLT_RR:
        return X64_emit_instr_sub_flt_rr;
    case X64_Instr_Kind_MUL_FLT_RR:
        return X64_emit_instr_mul_flt_rr;
    case X64_Instr_Kind_DIV_FLT_RR:
        return X64_emit_instr_div_flt_rr;
    case X64_Instr_Kind_MOV_FLT_RR:
        return X64_emit_instr_mov_flt_rr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_flt_rr_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Flt_RM_Func* x64_bin_flt_rm_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_FLT_RM:
        return X64_emit_instr_add_flt_rm;
    case X64_Instr_Kind_SUB_FLT_RM:
        return X64_emit_instr_sub_flt_rm;
    case X64_Instr_Kind_MUL_FLT_RM:
        return X64_emit_instr_mul_flt_rm;
    case X64_Instr_Kind_DIV_FLT_RM:
        return X64_emit_instr_div_flt_rm;
    case X64_Instr_Kind_MOV_FLT_RM:
        return X64_emit_instr_mov_flt_rm;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_flt_rm_emit_funcs()", kind);
        return NULL;
    }
}

static inline X64_Emit_Bin_Flt_MR_Func* x64_bin_flt_mr_emit_funcs(X64_Instr_Kind kind)
{
    switch (kind) {
    case X64_Instr_Kind_ADD_FLT_MR:
        return X64_emit_instr_add_flt_mr;
    case X64_Instr_Kind_SUB_FLT_MR:
        return X64_emit_instr_sub_flt_mr;
    case X64_Instr_Kind_MUL_FLT_MR:
        return X64_emit_instr_mul_flt_mr;
    case X64_Instr_Kind_DIV_FLT_MR:
        return X64_emit_instr_div_flt_mr;
    case X64_Instr_Kind_MOV_FLT_MR:
        return X64_emit_instr_mov_flt_mr;
    default:
        NIBBLE_FATAL_EXIT("Unexpected X64_Instr_Kind %d in x64_bin_flt_mr_emit_funcs()", kind);
        return NULL;
    }
}

#define IS_LREG_IN_REG(k) ((k) == XIR_LREG_LOC_REG)
#define IS_LREG_IN_STACK(k) ((k) == XIR_LREG_LOC_STACK)
static XIR_RegLoc X64_lreg_loc(X64_Proc_State* proc_state, u32 lreg)
{
    u32 rng_idx = XIR_find_alias_reg(proc_state->xir_builder, lreg);
    XIR_RegLoc reg_loc = proc_state->xir_builder->lreg_ranges[rng_idx].loc;

    assert(reg_loc.kind != XIR_LREG_LOC_UNASSIGNED);

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

static X64_Reg_Group X64_begin_reg_group(X64_Proc_State* proc_state)
{
    X64_Reg_Group group = {
        .proc_state = proc_state,
    };

    return group;
}

static X64_Reg X64_get_reg(X64_Reg_Group* group, X64_RegClass reg_class, u32 lreg, u32 size, bool store, u32 banned_regs)
{
    XIR_RegLoc lreg_loc = X64_lreg_loc(group->proc_state, lreg);

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

    X64_push_reg_to_stack(proc_state, tmp_reg->reg);
    X64_load_prim_from_mem(proc_state, size, tmp_reg->reg, X64_get_rbp_offset_addr(lreg_loc.offset));

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    return tmp_reg->reg;
}

static void X64_save_reg_to_group(X64_Reg_Group* group, X64_Reg reg)
{
    assert(reg != X64_REG_COUNT);
    assert(!u32_is_bit_set(group->used_tmp_reg_mask, reg));

    Allocator* tmp_mem = group->proc_state->tmp_mem;

    X64_Tmp_Reg* tmp_reg = alloc_type(tmp_mem, X64_Tmp_Reg, true);
    tmp_reg->reg = reg;

    X64_push_reg_to_stack(group->proc_state, tmp_reg->reg);

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    // Record register in group
    u32_set_bit(&group->used_tmp_reg_mask, tmp_reg->reg);

    group->num_tmp_regs += 1;
}

static void X64_end_reg_group(X64_Reg_Group* group)
{
    if (!group->num_tmp_regs)
        return;

    X64_Proc_State* proc_state = group->proc_state;

    X64_Tmp_Reg* it = group->first_tmp_reg;

    while (it) {
        if (it->store) {
            // Save to memory if the temporary register was holding a value for a variable that lives on the stack.
            X64_save_prim_to_mem(proc_state, it->size, X64_get_rbp_offset_addr(it->offset), it->reg);
        }

        // Restore temporary register's original value.
        X64_pop_reg_from_stack(proc_state, it->reg);
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

static s32 X64_consume_stack_arg(u64* stack_arg_offset, u64 arg_size, u64 arg_align)
{
    s32 offset = (s32)*stack_arg_offset;

    *stack_arg_offset += arg_size;
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, arg_align);
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, X64_STACK_WORD_SIZE);

    return offset;
}

static s32 X64_spill_reg(X64_Proc_State* proc_state, X64_Stack_Spill_State* state, u64 size, u64 align, X64_Reg preg)
{
    state->stack_spill_size += size;
    state->stack_spill_size = ALIGN_UP(state->stack_spill_size, align);
    s32 offset = -state->stack_spill_size;

    X64_save_prim_to_mem(proc_state, size, X64_get_rbp_offset_addr(offset), preg);

    return offset;
}

static void X64_assign_proc_param_offsets(X64_Proc_State* proc_state, const Symbol* sproc, X64_Stack_Params_Info* stack_params_info)
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

        X64_spill_reg(proc_state, &state, X64_MAX_INT_REG_SIZE, X64_MAX_INT_REG_SIZE,
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

                sym->as_var.offset = X64_spill_reg(proc_state, &state, X64_MAX_INT_REG_SIZE, arg_align, arg_reg);
            }
            else if ((arg_size <= (X64_MAX_INT_REG_SIZE << 1)) && (rem_regs >= 2)) {
                X64_Reg low_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;
                X64_Reg high_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                X64_spill_reg(proc_state, &state, X64_MAX_INT_REG_SIZE, arg_align, high_reg);
                sym->as_var.offset = X64_spill_reg(proc_state, &state, X64_MAX_INT_REG_SIZE, arg_align, low_reg);
            }
            else {
                sym->as_var.offset = X64_consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
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

                sym->as_var.offset = X64_spill_reg(proc_state, &state, arg_size, arg_align, arg_reg);
            }
            else {
                sym->as_var.offset = X64_consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }

        index += 1;
        it = it->next;
    }

    stack_params_info->stack_spill_size = state.stack_spill_size;
    stack_params_info->local_var_iter = it;
}

static u64 X64_assign_scope_stack_offsets(Scope* scope, u64 offset)
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
            u64 child_size = X64_assign_scope_stack_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static u64 X64_assign_proc_stack_offsets(X64_Proc_State* proc_state)
{
    Symbol* sproc = proc_state->sym;
    DeclProc* dproc = (DeclProc*)sproc->decl;
    Scope* scope = dproc->scope;

    //
    // Spill procedure params into the stack (assign stack offsets to params).
    //

    X64_Stack_Params_Info stack_params_info = {0};
    X64_assign_proc_param_offsets(proc_state, sproc, &stack_params_info);

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
            u32 child_size = X64_assign_scope_stack_offsets(child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static u32 X64_get_sibd_addr(X64_Proc_State* proc_state, X64_SIBD_Addr* sibd_addr, const XIR_MemAddr* vaddr)
{
    u32 used_regs = 0;

    if (vaddr->kind == XIR_ADDR_GLOBAL_SYM) {
        sibd_addr->kind = X64_SIBD_ADDR_GLOBAL;
        sibd_addr->global = vaddr->global;
    }
    else if (vaddr->kind == XIR_ADDR_STR_LIT) {
        sibd_addr->kind = X64_SIBD_ADDR_STR_LIT;
        sibd_addr->str_lit = vaddr->str_lit;
    }
    else if (vaddr->kind == XIR_ADDR_FLOAT_LIT) {
        sibd_addr->kind = X64_SIBD_ADDR_FLOAT_LIT;
        sibd_addr->float_lit = vaddr->float_lit;
    }
    else {
        assert(vaddr->kind == XIR_ADDR_SIBD);
        bool has_base = vaddr->sibd.base_reg != (u32)-1;
        bool has_index = vaddr->sibd.scale && (vaddr->sibd.index_reg != (u32)-1);
        assert(has_base || has_index);

        sibd_addr->kind = X64_SIBD_ADDR_LOCAL;
        sibd_addr->local.disp = vaddr->sibd.disp;
        sibd_addr->local.scale = vaddr->sibd.scale;

        if (has_base) {
            XIR_RegLoc base_loc = X64_lreg_loc(proc_state, vaddr->sibd.base_reg);
            assert(IS_LREG_IN_REG(base_loc.kind));

            sibd_addr->local.base_reg = base_loc.reg;
            u32_set_bit(&used_regs, base_loc.reg);

            if (has_index) {
                XIR_RegLoc index_loc = X64_lreg_loc(proc_state, vaddr->sibd.index_reg);
                assert(IS_LREG_IN_REG(index_loc.kind));

                sibd_addr->local.index_reg = index_loc.reg;
                u32_set_bit(&used_regs, index_loc.reg);
            }
            else {
                sibd_addr->local.index_reg = X64_REG_COUNT;
            }
        }
        else {
            XIR_RegLoc index_loc = X64_lreg_loc(proc_state, vaddr->sibd.index_reg);
            assert(IS_LREG_IN_REG(index_loc.kind));

            sibd_addr->local.base_reg = X64_REG_COUNT;
            sibd_addr->local.index_reg = index_loc.reg;
            u32_set_bit(&used_regs, index_loc.reg);
        }
    }

    return used_regs;
}

static void X64_patch_jmp_instrs(X64_Instrs* instrs, u32 ret_bblock_idx)
{
    for (u32 bb = 0; bb < instrs->num_bblocks; ++bb) {
        X64_BBlock* bblock = &instrs->bblocks[bb];
        X64_Instr* instr = bblock->head;

        while (instr) {
            X64_Instr_Kind kind = X64_get_instr_kind(instr);

            if (kind == X64_Instr_Kind_JMP_TO_RET) {
                instr->jmp.target = ret_bblock_idx;
            }

            instr = instr->next;
        }
    }
}

static void X64_emit_bin_int_rr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, u32 op_size,
                                      u32 op1_lreg, u32 op2_lreg)
{
    XIR_RegLoc op1_loc = X64_lreg_loc(proc_state, op1_lreg);
    XIR_RegLoc op2_loc = X64_lreg_loc(proc_state, op2_lreg);

    switch (op1_loc.kind) {
    case XIR_LREG_LOC_REG: {
        switch (op2_loc.kind) {
        case XIR_LREG_LOC_REG: {
            x64_bin_int_rr_emit_funcs(instr_kind)(proc_state, op_size, op1_loc.reg, op2_loc.reg);
            break;
        }
        case XIR_LREG_LOC_STACK: {
            x64_bin_int_rm_emit_funcs(instr_kind)(proc_state, op_size, op1_loc.reg, X64_get_rbp_offset_addr(op2_loc.offset));
            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    case XIR_LREG_LOC_STACK: {
        switch (op2_loc.kind) {
        case XIR_LREG_LOC_REG: {
            x64_bin_int_mr_emit_funcs(instr_kind)(proc_state, op_size, X64_get_rbp_offset_addr(op1_loc.offset), op2_loc.reg);
            break;
        }
        case XIR_LREG_LOC_STACK: {
            const X64_SIBD_Addr op1_addr = X64_get_rbp_offset_addr(op1_loc.offset);
            const X64_SIBD_Addr op2_addr = X64_get_rbp_offset_addr(op2_loc.offset);

            const X64_Reg tmp_reg = X64_RAX;

            // Save the contents of a temporary register into the stack.
            X64_emit_instr_push(proc_state, tmp_reg);

            // Load dst (currently spilled) into the temporary register,
            X64_emit_instr_mov_rm(proc_state, op_size, tmp_reg, op1_addr);

            // Execute the instruction using the temporary register as the destination.
            x64_bin_int_rm_emit_funcs(instr_kind)(proc_state, op_size, tmp_reg, op2_addr);

            // Store the result of the instruction (contents of temporary register) into dst.
            if (writes_op1) {
                X64_emit_instr_mov_mr(proc_state, op_size, op1_addr, tmp_reg);
            }

            // Restore the contents of the temporary register.
            X64_emit_instr_pop(proc_state, tmp_reg);
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

static void X64_emit_bin_flt_rr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, FloatKind flt_kind,
                                      u32 op1_lreg, u32 op2_lreg)
{
    XIR_RegLoc op1_loc = X64_lreg_loc(proc_state, op1_lreg);
    XIR_RegLoc op2_loc = X64_lreg_loc(proc_state, op2_lreg);

    switch (op1_loc.kind) {
    case XIR_LREG_LOC_REG: {
        switch (op2_loc.kind) {
        case XIR_LREG_LOC_REG: {
            x64_bin_flt_rr_emit_funcs(instr_kind)(proc_state, flt_kind, op1_loc.reg, op2_loc.reg);
            break;
        }
        case XIR_LREG_LOC_STACK: {
            x64_bin_flt_rm_emit_funcs(instr_kind)(proc_state, flt_kind, op1_loc.reg, X64_get_rbp_offset_addr(op2_loc.offset));
            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    case XIR_LREG_LOC_STACK: {
        switch (op2_loc.kind) {
        case XIR_LREG_LOC_REG: {
            x64_bin_flt_mr_emit_funcs(instr_kind)(proc_state, flt_kind, X64_get_rbp_offset_addr(op1_loc.offset), op2_loc.reg);
            break;
        }
        case XIR_LREG_LOC_STACK: {
            const X64_SIBD_Addr op1_addr = X64_get_rbp_offset_addr(op1_loc.offset);
            const X64_SIBD_Addr op2_addr = X64_get_rbp_offset_addr(op2_loc.offset);

            const X64_Reg tmp_reg = X64_XMM0;

            // Save the contents of a temporary register into the stack.
            X64_emit_instr_sub_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Make room for 16 bytes on the stack.
            X64_emit_instr_movdqu_mr(proc_state, X64_get_rsp_offset_addr(0), tmp_reg); // movdqu oword [rsp], tmp_reg

            // Load dst (currently spilled) into the temporary register,
            X64_emit_instr_mov_flt_rm(proc_state, flt_kind, tmp_reg, op1_addr);

            // Execute the instruction using the temporary register as the destination.
            x64_bin_flt_rm_emit_funcs(instr_kind)(proc_state, flt_kind, tmp_reg, op2_addr);

            // Store the result of the instruction (contents of temporary register) into dst.
            if (writes_op1) {
                X64_emit_instr_mov_flt_mr(proc_state, flt_kind, op1_addr, tmp_reg);
            }

            // Restore the contents of the temporary register.
            X64_emit_instr_movdqu_rm(proc_state, tmp_reg, X64_get_rsp_offset_addr(0)); // movdqu reg, oword [rsp]
            X64_emit_instr_add_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, 16); // Clean up 16 bytes from stack.
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

static void X64_emit_mov_ext_rr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 dst_size, u32 dst_lreg, u32 src_size,
                                      u32 src_lreg)
{
    // NOTE: sign-extension instructions require the destination to be a register. So, if dst is spilled, we get a temporary
    // register.

    X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
    X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, dst_lreg, dst_size, true, 0); // Get actual reg or a tmp if spilled.

    XIR_RegLoc src_loc = X64_lreg_loc(proc_state, src_lreg);

    switch (src_loc.kind) {
    case XIR_LREG_LOC_REG: {
        x64_mov_ext_rr_emit_funcs(instr_kind)(proc_state, dst_size, dst_reg, src_size, src_loc.reg);
        break;
    }
    case XIR_LREG_LOC_STACK: {
        x64_mov_ext_rm_emit_funcs(instr_kind)(proc_state, dst_size, dst_reg, src_size, X64_get_rbp_offset_addr(src_loc.offset));
        break;
    }
    default:
        assert(0);
        break;
    }

    X64_end_reg_group(&tmp_group);
}

static void X64_emit_mov_ext_rm_instr(X64_Proc_State* proc_state, X64_Instr_Kind movext_kind, u32 dst_size, u32 dst_lreg, u32 src_size,
                                      const XIR_MemAddr* src_vaddr)
{
    X64_SIBD_Addr src_addr = {0};
    u32 banned_tmp_regs = X64_get_sibd_addr(proc_state, &src_addr, src_vaddr);

    X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
    X64_Reg dst_reg =
        X64_get_reg(&tmp_group, X64_REG_CLASS_INT, dst_lreg, dst_size, true, banned_tmp_regs); // Get actual reg or a tmp if spilled.
    x64_mov_ext_rm_emit_funcs(movext_kind)(proc_state, dst_size, dst_reg, src_size, src_addr);
    X64_end_reg_group(&tmp_group);
}

static void X64_emit_bin_int_rm_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, u32 op_size,
                                      u32 op1_lreg, const XIR_MemAddr* op2_vaddr)
{
    X64_SIBD_Addr op2_addr = {0};
    u32 pinned_regs = X64_get_sibd_addr(proc_state, &op2_addr, op2_vaddr);

    X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
    X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, op1_lreg, op_size, writes_op1, pinned_regs);
    assert(op_size <= 8 && IS_POW2(op_size));

    x64_bin_int_rm_emit_funcs(instr_kind)(proc_state, op_size, op1_reg, op2_addr);
    X64_end_reg_group(&tmp_group);
}

static void X64_emit_bin_flt_rm_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, bool writes_op1, FloatKind flt_kind,
                                      u32 op1_lreg, const XIR_MemAddr* op2_vaddr)
{
    X64_SIBD_Addr op2_addr = {0};
    u32 pinned_regs = X64_get_sibd_addr(proc_state, &op2_addr, op2_vaddr);

    X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
    X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, op1_lreg, float_kind_sizes[flt_kind], writes_op1, pinned_regs);

    x64_bin_flt_rm_emit_funcs(instr_kind)(proc_state, flt_kind, op1_reg, op2_addr);
    X64_end_reg_group(&tmp_group);
}

static void X64_emit_bin_int_mr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size, const XIR_MemAddr* op1_vaddr,
                                      u32 op2_lreg)
{
    X64_SIBD_Addr op1_addr = {0};
    u32 pinned_regs = X64_get_sibd_addr(proc_state, &op1_addr, op1_vaddr);

    X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
    X64_Reg op2_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, op2_lreg, op_size, false, pinned_regs);
    assert(op_size <= 8 && IS_POW2(op_size));

    x64_bin_int_mr_emit_funcs(instr_kind)(proc_state, op_size, op1_addr, op2_reg);
    X64_end_reg_group(&tmp_group);
}

static void X64_emit_bin_flt_mr_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, FloatKind flt_kind,
                                      const XIR_MemAddr* op1_vaddr, u32 op2_lreg)
{
    X64_SIBD_Addr op1_addr = {0};
    u32 pinned_regs = X64_get_sibd_addr(proc_state, &op1_addr, op1_vaddr);

    X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
    X64_Reg op2_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, op2_lreg, float_kind_sizes[flt_kind], false, pinned_regs);

    x64_bin_flt_mr_emit_funcs(instr_kind)(proc_state, flt_kind, op1_addr, op2_reg);
    X64_end_reg_group(&tmp_group);
}

static void X64_emit_bin_int_mi_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size, const XIR_MemAddr* op1_vaddr,
                                      Scalar op2_imm)
{
    X64_SIBD_Addr op1_addr = {0};
    X64_get_sibd_addr(proc_state, &op1_addr, op1_vaddr);
    x64_bin_int_mi_emit_funcs(instr_kind)(proc_state, op_size, op1_addr, op2_imm.as_int._u32);
}

static void X64_emit_bin_int_ri_instr(X64_Proc_State* proc_state, X64_Instr_Kind instr_kind, u32 op_size, u32 op1_lreg, Scalar op2_imm)
{
    XIR_RegLoc op1_loc = X64_lreg_loc(proc_state, op1_lreg);

    switch (op1_loc.kind) {
    case XIR_LREG_LOC_REG: {
        x64_bin_int_ri_emit_funcs(instr_kind)(proc_state, op_size, op1_loc.reg, op2_imm.as_int._u32);
        break;
    }
    case XIR_LREG_LOC_STACK: {
        const X64_SIBD_Addr op1_addr = X64_get_rbp_offset_addr(op1_loc.offset);
        x64_bin_int_mi_emit_funcs(instr_kind)(proc_state, op_size, op1_addr, op2_imm.as_int._u32);
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64_place_args_in_regs(X64_Proc_State* proc_state, u32 num_args, const XIR_InstrCallArg* args)
{
    for (u32 i = 0; i < num_args; i++) {
        const XIR_InstrCallArg* arg = args + i;
        size_t arg_size = arg->type->size;

        if (type_is_obj_like(arg->type)) { // Argument is a struct/union/array object.
            const XIR_ObjArgSlot* slot = &arg->slot.obj;

            if (!slot->num_regs) {
                continue;
            }

            // Move object address into the appropriate argument register.
            if (slot->as_ptr) {
                assert(slot->num_regs == 1);
                assert(x64_reg_classes[slot->pregs[0]] == X64_REG_CLASS_INT);

                X64_emit_instr_lea(proc_state, slot->pregs[0], X64_get_rsp_offset_addr(slot->ptr_sp_offset));
            }
            // Copy 64-bit chunks of the struct object into the appropriate argument registers.
            else {
                X64_SIBD_Addr addr = {0};
                X64_get_sibd_addr(proc_state, &addr, &arg->val.addr);

                assert(addr.kind == X64_SIBD_ADDR_LOCAL);

                for (unsigned ii = 0; ii < slot->num_regs; ii++) {
                    X64_RegClass reg_class = x64_reg_classes[slot->pregs[ii]];

                    if (reg_class == X64_REG_CLASS_INT) {
                        X64_emit_instr_mov_rm(proc_state, X64_MAX_INT_REG_SIZE, slot->pregs[ii], addr);
                    }
                    else {
                        assert(reg_class == X64_REG_CLASS_FLOAT);
                        X64_emit_instr_mov_flt_rm(proc_state, FLOAT_F64, slot->pregs[ii], addr);
                    }

                    addr.local.disp += X64_MAX_INT_REG_SIZE;
                }
            }
        }
        else { // Argument is a primitive type
            const XIR_PrimArgSlot* slot = &arg->slot.prim;

            if (!slot->in_reg) {
                continue;
            }

            XIR_RegLoc loc = X64_lreg_loc(proc_state, arg->val.reg);

            if (IS_LREG_IN_STACK(loc.kind)) {
                assert(slot->preg < X64_REG_COUNT);
                X64_RegClass reg_class = x64_reg_classes[slot->preg];

                if (reg_class == X64_REG_CLASS_INT) {
                    X64_emit_instr_mov_rm(proc_state, arg_size, slot->preg, X64_get_rbp_offset_addr(loc.offset));
                }
                else {
                    assert(reg_class == X64_REG_CLASS_FLOAT);
                    X64_SIBD_Addr src_mem = X64_get_rbp_offset_addr(loc.offset);
                    X64_emit_instr_mov_flt_rm(proc_state, X64_flt_kind_from_size(arg_size), slot->preg, src_mem);
                }
            }
            else {
                assert(IS_LREG_IN_REG(loc.kind));
                assert(loc.reg == slot->preg); // Register allocator should have placed this in the correct register.
            }
        }
    }
}

static void X64_place_struct_args_in_stack(X64_Proc_State* proc_state, u32 num_args, const XIR_InstrCallArg* args)
{
    bool pushed_cpy_state = false;

    for (u32 i = 0; i < num_args; i += 1) {
        const XIR_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;

        if (type_is_obj_like(arg->type)) {
            // Argument is a struct/union/array object.
            const XIR_ObjArgSlot* slot = &arg->slot.obj;

            assert(!slot->as_ptr);

            if (slot->num_regs) {
                continue;
            }

            // TODO: There's no need to push all (rdi, rsi, rcx) if not used.
            if (!pushed_cpy_state) {
                X64_emit_instr_push(proc_state, X64_RDI);
                X64_emit_instr_push(proc_state, X64_RSI);
                X64_emit_instr_push(proc_state, X64_RCX);
                pushed_cpy_state = true;
            }

            const u32 sp_begin = X64_MAX_INT_REG_SIZE * 3;

            // Copy obj into its location in the stack with "rep movsb" instruction.
            X64_SIBD_Addr src_addr = {0};
            X64_get_sibd_addr(proc_state, &src_addr, &arg->val.addr);
            assert(src_addr.kind == X64_SIBD_ADDR_LOCAL);

            X64_emit_instr_lea(proc_state, X64_RDI, X64_get_rsp_offset_addr(slot->sp_offset + sp_begin));
            X64_emit_instr_lea(proc_state, X64_RSI, src_addr);
            X64_emit_instr_mov_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RCX, arg_size);
            X64_emit_instr_rep_movsb(proc_state);
        }
    }

    if (pushed_cpy_state) {
        X64_emit_instr_pop(proc_state, X64_RCX);
        X64_emit_instr_pop(proc_state, X64_RSI);
        X64_emit_instr_pop(proc_state, X64_RDI);
    }
}

static void X64_place_args_in_stack(X64_Proc_State* proc_state, u32 num_args, const XIR_InstrCallArg* args)
{
    // 1st pass: Copy struct arguments into the stack.
    X64_place_struct_args_in_stack(proc_state, num_args, args);

    // 2nd pass: Copy primitive args that are currently in registers into their stack slots.
    // This ensures that we can freely use RAX as a temporary register in the next pass.
    for (u32 i = 0; i < num_args; i += 1) {
        const XIR_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;

        if (!type_is_obj_like(arg->type)) {
            const XIR_PrimArgSlot* slot = &arg->slot.prim;

            if (slot->in_reg) {
                continue; // Skip register args
            }

            XIR_RegLoc loc = X64_lreg_loc(proc_state, arg->val.reg);

            // Move directly into stack slot.
            if (IS_LREG_IN_REG(loc.kind)) {
                X64_RegClass reg_class = x64_reg_classes[loc.reg];

                if (reg_class == X64_REG_CLASS_INT) {
                    X64_emit_instr_mov_mr(proc_state, arg_size, X64_get_rsp_offset_addr(slot->sp_offset), loc.reg);
                }
                else {
                    assert(reg_class == X64_REG_CLASS_FLOAT);
                    X64_emit_instr_mov_flt_mr(proc_state, X64_flt_kind_from_size(arg_size), X64_get_rsp_offset_addr(slot->sp_offset),
                                              loc.reg);
                }
            }
        }
    }

    // 3rd pass: Copy primitive args that are currently spilled into the stack frame.
    for (u32 i = 0; i < num_args; i += 1) {
        const XIR_InstrCallArg* arg = args + i;

        if (type_is_obj_like(arg->type)) {
            continue;
        }

        const XIR_PrimArgSlot* slot = &arg->slot.prim;

        if (slot->in_reg) {
            continue; // Skip register args
        }

        u64 arg_size = arg->type->size;
        XIR_RegLoc loc = X64_lreg_loc(proc_state, arg->val.reg);

        if (IS_LREG_IN_STACK(loc.kind)) {
            // Move into RAX.
            X64_emit_instr_mov_rm(proc_state, arg_size, X64_RAX, X64_get_rbp_offset_addr(loc.offset));

            // Move RAX into stack slot.
            X64_emit_instr_mov_mr(proc_state, arg_size, X64_get_rsp_offset_addr(slot->sp_offset), X64_RAX);
        }
    }
}

static size_t X64_fill_mem_from_reg(X64_Proc_State* proc_state, X64_SIBD_Addr* dst, X64_Reg src, size_t size)
{
    static char pow2_sizes[8] = {
        [1] = 1, [2] = 2, [3] = 2, [4] = 4, [5] = 4, [6] = 4, [7] = 4,
    };

    size_t rem_amnt = size;
    const X64_RegClass src_reg_class = x64_reg_classes[src];

    // If need to copy 8 or more bytes, just copy entire register into memory, and then return.
    if (rem_amnt >= X64_MAX_INT_REG_SIZE) {
        if (src_reg_class == X64_REG_CLASS_FLOAT) {
            X64_emit_instr_mov_flt_mr(proc_state, FLOAT_F64, *dst, src);
        }
        else {
            assert(src_reg_class == X64_REG_CLASS_INT);
            X64_emit_instr_mov_mr(proc_state, X64_MAX_INT_REG_SIZE, *dst, src);
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
            X64_emit_instr_mov_mr(proc_state, n, *dst, src);

            // Move dst addr forward.
            dst->local.disp += n;

            size_t new_rem_amnt = rem_amnt - n;

            // Shift src register right to discard copied bits.
            if (new_rem_amnt) {
                X64_emit_instr_sar_ri(proc_state, X64_MAX_INT_REG_SIZE, src, (u8)(n << 3));
            }

            rem_amnt = new_rem_amnt;
        }
    }
    else {
        assert(src_reg_class == X64_REG_CLASS_FLOAT);

        if (rem_amnt == float_kind_sizes[FLOAT_F32]) {
            X64_emit_instr_mov_flt_mr(proc_state, FLOAT_F32, *dst, src);
            rem_amnt = 0;
        }
        else {
            NIBBLE_FATAL_EXIT("X64_cpy_reg_to_mem(): Cannot copy %d bytes from XMM register.", rem_amnt);
        }
    }

    return rem_amnt;
}

static void X64_cpy_ret_small_obj(X64_Proc_State* proc_state, const Type* ret_type, const XIR_CallValue* dst_val)
{
    X64_RegClass reg_class = X64_obj_reg_class(ret_type);
    X64_ScratchRegs ret_regs = (*x64_target.ret_regs)[reg_class];

    // Procedure returned a small struct/union/array object in registers.
    // Copy into appropriate memory location.
    if (!X64_is_obj_retarg_large(ret_type->size)) {
        X64_SIBD_Addr obj_addr = {0};
        X64_get_sibd_addr(proc_state, &obj_addr, &dst_val->addr);

        // Copy RAX into the first 8 bytes of struct memory.
        size_t rem_amnt = X64_fill_mem_from_reg(proc_state, &obj_addr, ret_regs.regs[0], ret_type->size);

        // Copy RDX into the second 8 bytes of struct memory.
        if (rem_amnt) {
            rem_amnt = X64_fill_mem_from_reg(proc_state, &obj_addr, ret_regs.regs[1], rem_amnt);
            assert(!rem_amnt);
        }
    }
}

static void X64_gen_instr(X64_Proc_State* proc_state, const XIR_Instr* instr, bool is_last_instr, long bblock_id)
{
    AllocatorState mem_state = allocator_get_state(proc_state->tmp_mem);

    switch (instr->kind) {
    case XIR_InstrAdd_R_R_KIND: {
        const XIR_InstrAdd_R_R* act_instr = (const XIR_InstrAdd_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_ADD_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrAdd_R_M_KIND: {
        const XIR_InstrAdd_R_M* act_instr = (const XIR_InstrAdd_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_ADD_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrAdd_R_I_KIND: {
        const XIR_InstrAdd_R_I* act_instr = (const XIR_InstrAdd_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_ADD_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrSub_R_R_KIND: {
        const XIR_InstrSub_R_R* act_instr = (const XIR_InstrSub_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_SUB_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrSub_R_M_KIND: {
        const XIR_InstrSub_R_M* act_instr = (const XIR_InstrSub_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_SUB_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrSub_R_I_KIND: {
        const XIR_InstrSub_R_I* act_instr = (const XIR_InstrSub_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_SUB_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrIMul_R_R_KIND: {
        const XIR_InstrIMul_R_R* act_instr = (const XIR_InstrIMul_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_IMUL_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrIMul_R_M_KIND: {
        const XIR_InstrIMul_R_M* act_instr = (const XIR_InstrIMul_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_IMUL_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrIMul_R_I_KIND: {
        const XIR_InstrIMul_R_I* act_instr = (const XIR_InstrIMul_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_IMUL_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    // AND
    case XIR_InstrAnd_R_R_KIND: {
        const XIR_InstrAnd_R_R* act_instr = (const XIR_InstrAnd_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_AND_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrAnd_R_M_KIND: {
        const XIR_InstrAnd_R_M* act_instr = (const XIR_InstrAnd_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_AND_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrAnd_R_I_KIND: {
        const XIR_InstrAnd_R_I* act_instr = (const XIR_InstrAnd_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_AND_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    // OR
    case XIR_InstrOr_R_R_KIND: {
        const XIR_InstrOr_R_R* act_instr = (const XIR_InstrOr_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_OR_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrOr_R_M_KIND: {
        const XIR_InstrOr_R_M* act_instr = (const XIR_InstrOr_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_OR_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrOr_R_I_KIND: {
        const XIR_InstrOr_R_I* act_instr = (const XIR_InstrOr_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_OR_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    // XOR
    case XIR_InstrXor_R_R_KIND: {
        const XIR_InstrXor_R_R* act_instr = (const XIR_InstrXor_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_XOR_RR, true, size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrXor_R_M_KIND: {
        const XIR_InstrXor_R_M* act_instr = (const XIR_InstrXor_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_XOR_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrXor_R_I_KIND: {
        const XIR_InstrXor_R_I* act_instr = (const XIR_InstrXor_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_XOR_RI, size, act_instr->dst, act_instr->src);
        break;
    }
    // ADDSS
    case XIR_InstrAddSS_R_R_KIND: {
        const XIR_InstrAddSS_R_R* act_instr = (const XIR_InstrAddSS_R_R*)instr;
        X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_ADD_FLT_RR, true, FLOAT_F32, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrAddSS_R_M_KIND: {
        const XIR_InstrAddSS_R_M* act_instr = (const XIR_InstrAddSS_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_ADD_FLT_RM, true, FLOAT_F32, act_instr->dst, &act_instr->src);
        break;
    }
    // ADDSD
    case XIR_InstrAddSD_R_R_KIND: {
        const XIR_InstrAddSD_R_R* act_instr = (const XIR_InstrAddSD_R_R*)instr;
        X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_ADD_FLT_RR, true, FLOAT_F64, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrAddSD_R_M_KIND: {
        const XIR_InstrAddSD_R_M* act_instr = (const XIR_InstrAddSD_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_ADD_FLT_RM, true, FLOAT_F64, act_instr->dst, &act_instr->src);
        break;
    }
    // SUBSS
    case XIR_InstrSubSS_R_R_KIND: {
        const XIR_InstrSubSS_R_R* act_instr = (const XIR_InstrSubSS_R_R*)instr;
        X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_SUB_FLT_RR, true, FLOAT_F32, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrSubSS_R_M_KIND: {
        const XIR_InstrSubSS_R_M* act_instr = (const XIR_InstrSubSS_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_SUB_FLT_RM, true, FLOAT_F32, act_instr->dst, &act_instr->src);
        break;
    }
    // SUBSD
    case XIR_InstrSubSD_R_R_KIND: {
        const XIR_InstrSubSD_R_R* act_instr = (const XIR_InstrSubSD_R_R*)instr;
        X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_SUB_FLT_RR, true, FLOAT_F64, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrSubSD_R_M_KIND: {
        const XIR_InstrSubSD_R_M* act_instr = (const XIR_InstrSubSD_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_SUB_FLT_RM, true, FLOAT_F64, act_instr->dst, &act_instr->src);
        break;
    }
    // MULSS
    case XIR_InstrMulSS_R_R_KIND: {
        const XIR_InstrMulSS_R_R* act_instr = (const XIR_InstrMulSS_R_R*)instr;
        X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_MUL_FLT_RR, true, FLOAT_F32, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMulSS_R_M_KIND: {
        const XIR_InstrMulSS_R_M* act_instr = (const XIR_InstrMulSS_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_MUL_FLT_RM, true, FLOAT_F32, act_instr->dst, &act_instr->src);
        break;
    }
    // MULSD
    case XIR_InstrMulSD_R_R_KIND: {
        const XIR_InstrMulSD_R_R* act_instr = (const XIR_InstrMulSD_R_R*)instr;
        X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_MUL_FLT_RR, true, FLOAT_F64, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMulSD_R_M_KIND: {
        const XIR_InstrMulSD_R_M* act_instr = (const XIR_InstrMulSD_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_MUL_FLT_RM, true, FLOAT_F64, act_instr->dst, &act_instr->src);
        break;
    }
    // DIVSS
    case XIR_InstrDivSS_R_R_KIND: {
        const XIR_InstrDivSS_R_R* act_instr = (const XIR_InstrDivSS_R_R*)instr;
        X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_DIV_FLT_RR, true, FLOAT_F32, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrDivSS_R_M_KIND: {
        const XIR_InstrDivSS_R_M* act_instr = (const XIR_InstrDivSS_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_DIV_FLT_RM, true, FLOAT_F32, act_instr->dst, &act_instr->src);
        break;
    }
    // DIVSD
    case XIR_InstrDivSD_R_R_KIND: {
        const XIR_InstrDivSD_R_R* act_instr = (const XIR_InstrDivSD_R_R*)instr;
        X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_DIV_FLT_RR, true, FLOAT_F64, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrDivSD_R_M_KIND: {
        const XIR_InstrDivSD_R_M* act_instr = (const XIR_InstrDivSD_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_DIV_FLT_RM, true, FLOAT_F64, act_instr->dst, &act_instr->src);
        break;
    }
    // NEG
    case XIR_InstrNeg_KIND: {
        const XIR_InstrNeg* act_instr = (const XIR_InstrNeg*)instr;
        const u8 size = act_instr->size;
        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_instr_neg_r(proc_state, size, dst_loc.reg);
        }
        else {
            X64_emit_instr_neg_m(proc_state, size, X64_get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    // NOT
    case XIR_InstrNot_KIND: {
        const XIR_InstrNot* act_instr = (const XIR_InstrNot*)instr;
        const u8 size = act_instr->size;
        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_instr_not_r(proc_state, size, dst_loc.reg);
        }
        else {
            X64_emit_instr_not_m(proc_state, size, X64_get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    // DIV
    case XIR_InstrDiv_R_KIND: {
        const XIR_InstrDiv_R* act_instr = (const XIR_InstrDiv_R*)instr;
        const u8 size = act_instr->size;

        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);
        if (IS_LREG_IN_REG(src_loc.kind)) {
            X64_emit_instr_div_r(proc_state, size, src_loc.reg);
        }
        else {
            X64_emit_instr_div_m(proc_state, size, X64_get_rbp_offset_addr(src_loc.offset));
        }
        break;
    }
    case XIR_InstrDiv_M_KIND: {
        const XIR_InstrDiv_M* act_instr = (const XIR_InstrDiv_M*)instr;
        const u8 size = act_instr->size;
        X64_SIBD_Addr op_addr = {0};

        X64_get_sibd_addr(proc_state, &op_addr, &act_instr->src);
        X64_emit_instr_div_m(proc_state, size, op_addr);
        break;
    }
    // IDIV
    case XIR_InstrIDiv_R_KIND: {
        const XIR_InstrIDiv_R* act_instr = (const XIR_InstrIDiv_R*)instr;
        const u8 size = act_instr->size;

        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);
        if (IS_LREG_IN_REG(src_loc.kind)) {
            X64_emit_instr_idiv_r(proc_state, size, src_loc.reg);
        }
        else {
            X64_emit_instr_idiv_m(proc_state, size, X64_get_rbp_offset_addr(src_loc.offset));
        }
        break;
    }
    case XIR_InstrIDiv_M_KIND: {
        const XIR_InstrIDiv_M* act_instr = (const XIR_InstrIDiv_M*)instr;
        const u8 size = act_instr->size;
        X64_SIBD_Addr op_addr = {0};

        X64_get_sibd_addr(proc_state, &op_addr, &act_instr->src);
        X64_emit_instr_idiv_m(proc_state, size, op_addr);
        break;
    }
    // Sign-extend _ax into _dx
    case XIR_InstrSExtAxToDx_KIND: {
        const XIR_InstrSExtAxToDx* act_instr = (const XIR_InstrSExtAxToDx*)instr;
        X64_emit_instr_sext_ax_into_dx(proc_state, act_instr->size);
        break;
    }
    // SAR
    case XIR_InstrSar_R_R_KIND: {
        const XIR_InstrSar_R_R* act_instr = (const XIR_InstrSar_R_R*)instr;
        const u8 dst_size = act_instr->size;
        const XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        const XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        if (!(IS_LREG_IN_REG(src_loc.kind) && src_loc.reg == X64_RCX)) {
            NIBBLE_FATAL_EXIT("Source operator for x64 sar instruction must be RCX");
        }

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_instr_sar_rr(proc_state, dst_size, dst_loc.reg);
        }
        else {
            X64_emit_instr_sar_mr(proc_state, dst_size, X64_get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    case XIR_InstrSar_R_I_KIND: {
        const XIR_InstrSar_R_I* act_instr = (const XIR_InstrSar_R_I*)instr;
        const u8 dst_size = act_instr->size;
        const XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_instr_sar_ri(proc_state, dst_size, dst_loc.reg, act_instr->src.as_int._u8);
        }
        else {
            X64_emit_instr_sar_mi(proc_state, dst_size, X64_get_rbp_offset_addr(dst_loc.offset), act_instr->src.as_int._u8);
        }
        break;
    }
    // SHL
    case XIR_InstrShl_R_R_KIND: {
        const XIR_InstrShl_R_R* act_instr = (const XIR_InstrShl_R_R*)instr;
        const u8 dst_size = act_instr->size;
        const XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        const XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        if (!(IS_LREG_IN_REG(src_loc.kind) && src_loc.reg == X64_RCX)) {
            NIBBLE_FATAL_EXIT("Source operator for x64 shl instruction must be RCX");
        }

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_instr_shl_rr(proc_state, dst_size, dst_loc.reg);
        }
        else {
            X64_emit_instr_shl_mr(proc_state, dst_size, X64_get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    case XIR_InstrShl_R_I_KIND: {
        const XIR_InstrShl_R_I* act_instr = (const XIR_InstrShl_R_I*)instr;
        const u8 dst_size = act_instr->size;
        const XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_instr_shl_ri(proc_state, dst_size, dst_loc.reg, act_instr->src.as_int._u8);
        }
        else {
            X64_emit_instr_shl_mi(proc_state, dst_size, X64_get_rbp_offset_addr(dst_loc.offset), act_instr->src.as_int._u8);
        }
        break;
    }
    // MOV
    case XIR_InstrMov_R_R_KIND: {
        const XIR_InstrMov_R_R* act_instr = (const XIR_InstrMov_R_R*)instr;
        const u8 size = act_instr->size;

        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        bool same_ops = (dst_loc.kind == src_loc.kind) && ((IS_LREG_IN_REG(dst_loc.kind) && (dst_loc.reg == src_loc.reg)) ||
                                                           (IS_LREG_IN_STACK(dst_loc.kind) && (dst_loc.offset == src_loc.offset)));

        if (!same_ops) {
            X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_MOV_RR, true, size, act_instr->dst, act_instr->src);
        }
        break;
    }
    case XIR_InstrMov_R_RH_KIND: {
        const XIR_InstrMov_R_RH* act_instr = (const XIR_InstrMov_R_RH*)instr;
        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        assert(IS_LREG_IN_REG(src_loc.kind));

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_instr_mov_rrh(proc_state, dst_loc.reg, src_loc.reg);
        }
        else {
            assert(dst_loc.kind == XIR_LREG_LOC_STACK);
            X64_emit_instr_mov_mrh(proc_state, X64_get_rbp_offset_addr(dst_loc.offset), src_loc.reg);
        }

        break;
    }
    case XIR_InstrMov_R_M_KIND: {
        const XIR_InstrMov_R_M* act_instr = (const XIR_InstrMov_R_M*)instr;
        const u8 size = act_instr->size;

        X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_MOV_RM, true, size, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrMov_M_R_KIND: {
        const XIR_InstrMov_M_R* act_instr = (const XIR_InstrMov_M_R*)instr;
        const u8 size = act_instr->size;

        X64_emit_bin_int_mr_instr(proc_state, X64_Instr_Kind_MOV_MR, size, &act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMov_R_I_KIND: {
        const XIR_InstrMov_R_I* act_instr = (const XIR_InstrMov_R_I*)instr;
        const u8 size = act_instr->size;

        // NOTE: We don't use X64_emit_bin_int_ri_instr here because MOV is the only instruction
        // capable of loading a 64-bit immediate (others can only load u32). Additionally, if the destination
        // was spilled during register allocation, we have to load the immediate (potentially a 64-bit) into a temporary
        // register, which is subsequently stored into the spilled location.
        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg dst_reg =
            X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, size, true, 0); // Get actual reg or a tmp if spilled.
        X64_emit_instr_mov_ri(proc_state, size, dst_reg, act_instr->src.as_int._u64);
        X64_end_reg_group(&tmp_group);
        break;
    }
    case XIR_InstrMov_M_I_KIND: {
        const XIR_InstrMov_M_I* act_instr = (const XIR_InstrMov_M_I*)instr;
        const u8 size = act_instr->size;

        X64_emit_bin_int_mi_instr(proc_state, X64_Instr_Kind_MOV_MI, size, &act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMovSX_R_R_KIND: {
        const XIR_InstrMovSX_R_R* act_instr = (const XIR_InstrMovSX_R_R*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;
        X64_Instr_Kind movsx_kind =
            src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? X64_Instr_Kind_MOVSXD_RR : X64_Instr_Kind_MOVSX_RR;

        X64_emit_mov_ext_rr_instr(proc_state, movsx_kind, dst_size, act_instr->dst, src_size, act_instr->src);
        break;
    }
    case XIR_InstrMovSX_R_M_KIND: {
        const XIR_InstrMovSX_R_M* act_instr = (const XIR_InstrMovSX_R_M*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;
        X64_Instr_Kind movsx_kind =
            src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? X64_Instr_Kind_MOVSXD_RR : X64_Instr_Kind_MOVSX_RR;

        X64_emit_mov_ext_rm_instr(proc_state, movsx_kind, dst_size, act_instr->dst, src_size, &act_instr->src);
        break;
    }
    case XIR_InstrMovZX_R_R_KIND: {
        const XIR_InstrMovZX_R_R* act_instr = (const XIR_InstrMovZX_R_R*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;

        // There is no encoding for an instruction that zero-extends a 4-byte source to an 8-byte destination! Also, note that
        // an instruction like mov eax, __ clears the upper 4 bytes of eax.
        // See: https://stackoverflow.com/a/51394642
        if (src_size != 4) {
            X64_emit_mov_ext_rr_instr(proc_state, X64_Instr_Kind_MOVZX_RR, dst_size, act_instr->dst, src_size, act_instr->src);
        }
        // EX: Instead of movzx rax, edi (invalid), use mov eax, edi to zero-extend edi into rax.
        else {
            assert(dst_size == X64_MAX_INT_REG_SIZE);

            // NOTE: Not necessary if a previous instruction already cleared the upper 4-bytes of the dest reg with a mov instruction.
            // We would need to track the "zxt" state of all registers: if mov rx, _ => rx is "zxt", otherwise if <not_mov> rx, _ =>
            // rx is NOT "zxt".
            X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_MOV_RR, true, src_size, act_instr->dst, act_instr->src);
        }
        break;
    }
    case XIR_InstrMovZX_R_M_KIND: {
        const XIR_InstrMovZX_R_M* act_instr = (const XIR_InstrMovZX_R_M*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;

        // There is no encoding for an instruction that zero-extends a 4-byte source to an 8-byte destination! Also, note that
        // an instruction like mov eax, __ clears the upper 4 bytes of eax.
        // See: https://stackoverflow.com/a/51394642
        if (src_size != 4) {
            X64_emit_mov_ext_rm_instr(proc_state, X64_Instr_Kind_MOVZX_RM, dst_size, act_instr->dst, src_size, &act_instr->src);
        }
        // EX: Instead of movzx rax, edi (invalid), use mov eax, edi to zero-extend edi into rax.
        else {
            assert(dst_size == X64_MAX_INT_REG_SIZE);

            // NOTE: Not necessary if a previous instruction already cleared the upper 4-bytes of the dest reg with a mov instruction.
            // We would need to track the "zxt" state of all registers: if mov rx, _ => rx is "zxt", otherwise if <not_mov> rx, _ =>
            // rx is NOT "zxt".
            X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_MOV_RM, true, src_size, act_instr->dst, &act_instr->src);
        }
        break;
    }
    // MOVSS
    case XIR_InstrMovSS_R_R_KIND: {
        const XIR_InstrMovSS_R_R* act_instr = (const XIR_InstrMovSS_R_R*)instr;
        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        bool same_ops = (dst_loc.kind == src_loc.kind) && ((IS_LREG_IN_REG(dst_loc.kind) && (dst_loc.reg == src_loc.reg)) ||
                                                           (IS_LREG_IN_STACK(dst_loc.kind) && (dst_loc.offset == src_loc.offset)));

        if (!same_ops) {
            X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_MOV_FLT_RR, true, FLOAT_F32, act_instr->dst, act_instr->src);
        }
        break;
    }
    case XIR_InstrMovSS_R_M_KIND: {
        const XIR_InstrMovSS_R_M* act_instr = (const XIR_InstrMovSS_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_MOV_FLT_RM, true, FLOAT_F32, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrMovSS_M_R_KIND: {
        const XIR_InstrMovSS_M_R* act_instr = (const XIR_InstrMovSS_M_R*)instr;
        X64_emit_bin_flt_mr_instr(proc_state, X64_Instr_Kind_MOV_FLT_MR, FLOAT_F32, &act_instr->dst, act_instr->src);
        break;
    }
    // MOVSD
    case XIR_InstrMovSD_R_R_KIND: {
        const XIR_InstrMovSD_R_R* act_instr = (const XIR_InstrMovSD_R_R*)instr;
        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        bool same_ops = (dst_loc.kind == src_loc.kind) && ((IS_LREG_IN_REG(dst_loc.kind) && (dst_loc.reg == src_loc.reg)) ||
                                                           (IS_LREG_IN_STACK(dst_loc.kind) && (dst_loc.offset == src_loc.offset)));

        if (!same_ops) {
            X64_emit_bin_flt_rr_instr(proc_state, X64_Instr_Kind_MOV_FLT_RR, true, FLOAT_F64, act_instr->dst, act_instr->src);
        }
        break;
    }
    case XIR_InstrMovSD_R_M_KIND: {
        const XIR_InstrMovSD_R_M* act_instr = (const XIR_InstrMovSD_R_M*)instr;
        X64_emit_bin_flt_rm_instr(proc_state, X64_Instr_Kind_MOV_FLT_RM, true, FLOAT_F64, act_instr->dst, &act_instr->src);
        break;
    }
    case XIR_InstrMovSD_M_R_KIND: {
        const XIR_InstrMovSD_M_R* act_instr = (const XIR_InstrMovSD_M_R*)instr;
        X64_emit_bin_flt_mr_instr(proc_state, X64_Instr_Kind_MOV_FLT_MR, FLOAT_F64, &act_instr->dst, act_instr->src);
        break;
    }
    // CVTSS2SD
    case XIR_InstrCvtSS2SD_R_R_KIND: { // f32 to f64
        const XIR_InstrCvtSS2SD_R_R* act_instr = (const XIR_InstrCvtSS2SD_R_R*)instr;

        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_emit_instr_cvtss2sd_rr(proc_state, dst_loc.reg, src_loc.reg);
        }
        else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
            X64_emit_instr_cvtss2sd_rm(proc_state, dst_loc.reg, X64_get_rbp_offset_addr(src_loc.offset));
        }
        else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg =
                X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, float_kind_sizes[FLOAT_F64], true, (1 << src_loc.reg));
            X64_emit_instr_cvtss2sd_rr(proc_state, dst_reg, src_loc.reg);
            X64_end_reg_group(&tmp_group);
        }
        else {
            assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, float_kind_sizes[FLOAT_F64], true, 0);
            X64_emit_instr_cvtss2sd_rm(proc_state, dst_reg, X64_get_rbp_offset_addr(src_loc.offset));
            X64_end_reg_group(&tmp_group);
        }
        break;
    }

    case XIR_InstrCvtSS2SD_R_M_KIND: { // f32 (in memory) to f64
        const XIR_InstrCvtSS2SD_R_M* act_instr = (const XIR_InstrCvtSS2SD_R_M*)instr;
        X64_SIBD_Addr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(proc_state, &src_addr, &act_instr->src);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, float_kind_sizes[FLOAT_F64], true, used_regs);
        X64_emit_instr_cvtss2sd_rm(proc_state, dst_reg, src_addr);
        X64_end_reg_group(&tmp_group);
        break;
    }
    // CVTSD2SS
    case XIR_InstrCvtSD2SS_R_R_KIND: { // f64 to f32
        const XIR_InstrCvtSD2SS_R_R* act_instr = (const XIR_InstrCvtSD2SS_R_R*)instr;

        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_emit_instr_cvtsd2ss_rr(proc_state, dst_loc.reg, src_loc.reg);
        }
        else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
            X64_emit_instr_cvtsd2ss_rm(proc_state, dst_loc.reg, X64_get_rbp_offset_addr(src_loc.offset));
        }
        else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg =
                X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, float_kind_sizes[FLOAT_F32], true, (1 << src_loc.reg));
            X64_emit_instr_cvtsd2ss_rr(proc_state, dst_reg, src_loc.reg);
            X64_end_reg_group(&tmp_group);
        }
        else {
            assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, float_kind_sizes[FLOAT_F32], true, 0);
            X64_emit_instr_cvtsd2ss_rm(proc_state, dst_reg, X64_get_rbp_offset_addr(src_loc.offset));
            X64_end_reg_group(&tmp_group);
        }
        break;
    }
    case XIR_InstrCvtSD2SS_R_M_KIND: { // f64 (in memory) to f32
        const XIR_InstrCvtSD2SS_R_M* act_instr = (const XIR_InstrCvtSD2SS_R_M*)instr;
        X64_SIBD_Addr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(proc_state, &src_addr, &act_instr->src);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, float_kind_sizes[FLOAT_F32], true, used_regs);

        X64_emit_instr_cvtsd2ss_rm(proc_state, dst_reg, src_addr);
        X64_end_reg_group(&tmp_group);
        break;
    }
    // CVTTSS2SI
    case XIR_InstrCvtSS2SI_R_R_KIND: { // f32 to integer
        const XIR_InstrCvtSS2SI_R_R* act_instr = (const XIR_InstrCvtSS2SI_R_R*)instr;

        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);
        const bool dst_is_8byte = act_instr->dst_size == 8;

        if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_emit_instr_cvttss2si_rr(proc_state, dst_loc.reg, dst_is_8byte, src_loc.reg);
        }
        else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
            X64_emit_instr_cvttss2si_rm(proc_state, dst_loc.reg, dst_is_8byte, X64_get_rbp_offset_addr(src_loc.offset));
        }
        else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg =
                X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, dst_is_8byte ? 8 : 4, true, (1 << src_loc.reg));
            X64_emit_instr_cvttss2si_rr(proc_state, dst_reg, dst_is_8byte, src_loc.reg);
            X64_end_reg_group(&tmp_group);
        }
        else {
            assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, dst_is_8byte ? 8 : 4, true, 0);
            X64_emit_instr_cvttss2si_rm(proc_state, dst_reg, dst_is_8byte, X64_get_rbp_offset_addr(src_loc.offset));
            X64_end_reg_group(&tmp_group);
        }
        break;
    }
    case XIR_InstrCvtSS2SI_R_M_KIND: { // f32 (in memory) to integer
        const XIR_InstrCvtSS2SI_R_M* act_instr = (const XIR_InstrCvtSS2SI_R_M*)instr;

        X64_SIBD_Addr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(proc_state, &src_addr, &act_instr->src);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, act_instr->dst_size, true, used_regs);

        X64_emit_instr_cvttss2si_rm(proc_state, dst_reg, act_instr->dst_size == 8, src_addr);
        X64_end_reg_group(&tmp_group);
        break;
    }
    // CVTTSD2SI
    case XIR_InstrCvtSD2SI_R_R_KIND: { // f64 to integer
        const XIR_InstrCvtSD2SI_R_R* act_instr = (const XIR_InstrCvtSD2SI_R_R*)instr;

        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);
        const bool dst_is_8byte = act_instr->dst_size == 8;

        if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_emit_instr_cvttsd2si_rr(proc_state, dst_loc.reg, dst_is_8byte, src_loc.reg);
        }
        else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
            X64_emit_instr_cvttsd2si_rm(proc_state, dst_loc.reg, dst_is_8byte, X64_get_rbp_offset_addr(src_loc.offset));
        }
        else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg =
                X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, dst_is_8byte ? 8 : 4, true, (1 << src_loc.reg));
            X64_emit_instr_cvttsd2si_rr(proc_state, dst_reg, dst_is_8byte, src_loc.reg);
            X64_end_reg_group(&tmp_group);
        }
        else {
            assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, dst_is_8byte ? 8 : 4, true, 0);
            X64_emit_instr_cvttsd2si_rm(proc_state, dst_reg, dst_is_8byte, X64_get_rbp_offset_addr(src_loc.offset));
            X64_end_reg_group(&tmp_group);
        }
        break;
    }
    case XIR_InstrCvtSD2SI_R_M_KIND: { // f64 (in memory) to integer
        const XIR_InstrCvtSD2SI_R_M* act_instr = (const XIR_InstrCvtSD2SI_R_M*)instr;

        X64_SIBD_Addr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(proc_state, &src_addr, &act_instr->src);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, act_instr->dst_size, true, used_regs);

        X64_emit_instr_cvttsd2si_rm(proc_state, dst_reg, act_instr->dst_size == 8, src_addr);
        X64_end_reg_group(&tmp_group);
        break;
    }
    // CVTSI2SS
    case XIR_InstrCvtSI2SS_R_R_KIND: { // integer -> f32
        const XIR_InstrCvtSI2SS_R_R* act_instr = (const XIR_InstrCvtSI2SS_R_R*)instr;

        assert(act_instr->src_size >= 4);
        const bool src_is_8bytes = act_instr->src_size == 8;
        const u8 dst_size = float_kind_sizes[FLOAT_F32];

        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_emit_instr_cvtsi2ss_rr(proc_state, dst_loc.reg, src_loc.reg, src_is_8bytes);
        }
        else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
            X64_emit_instr_cvtsi2ss_rm(proc_state, dst_loc.reg, X64_get_rbp_offset_addr(src_loc.offset), src_is_8bytes);
        }
        else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, dst_size, true, (1 << src_loc.reg));
            X64_emit_instr_cvtsi2ss_rr(proc_state, dst_reg, src_loc.reg, src_is_8bytes);
            X64_end_reg_group(&tmp_group);
        }
        else {
            assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, dst_size, true, 0);
            X64_emit_instr_cvtsi2ss_rm(proc_state, dst_reg, X64_get_rbp_offset_addr(src_loc.offset), src_is_8bytes);
            X64_end_reg_group(&tmp_group);
        }
        break;
    }
    case XIR_InstrCvtSI2SS_R_M_KIND: { // integer (in memory) -> f32
        const XIR_InstrCvtSI2SS_R_M* act_instr = (const XIR_InstrCvtSI2SS_R_M*)instr;

        assert(act_instr->src_size >= 4);
        const u8 dst_size = float_kind_sizes[FLOAT_F32];
        const bool src_is_8bytes = act_instr->src_size == 8;

        X64_SIBD_Addr src_addr = {0};
        const u32 used_regs = X64_get_sibd_addr(proc_state, &src_addr, &act_instr->src);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, dst_size, true, used_regs);

        X64_emit_instr_cvtsi2ss_rm(proc_state, dst_reg, src_addr, src_is_8bytes);
        X64_end_reg_group(&tmp_group);
        break;
    }
    // CVTSI2SD
    case XIR_InstrCvtSI2SD_R_R_KIND: { // integer -> f64
        const XIR_InstrCvtSI2SD_R_R* act_instr = (const XIR_InstrCvtSI2SD_R_R*)instr;

        assert(act_instr->src_size >= 4);
        const bool src_is_8bytes = act_instr->src_size == 8;
        const u8 dst_size = float_kind_sizes[FLOAT_F64];

        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);
        XIR_RegLoc src_loc = X64_lreg_loc(proc_state, act_instr->src);

        if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_emit_instr_cvtsi2sd_rr(proc_state, dst_loc.reg, src_loc.reg, src_is_8bytes);
        }
        else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
            X64_emit_instr_cvtsi2sd_rm(proc_state, dst_loc.reg, X64_get_rbp_offset_addr(src_loc.offset), src_is_8bytes);
        }
        else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, dst_size, true, (1 << src_loc.reg));
            X64_emit_instr_cvtsi2sd_rr(proc_state, dst_reg, src_loc.reg, src_is_8bytes);
            X64_end_reg_group(&tmp_group);
        }
        else {
            assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
            X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
            X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, dst_size, true, 0);
            X64_emit_instr_cvtsi2sd_rm(proc_state, dst_reg, X64_get_rbp_offset_addr(src_loc.offset), src_is_8bytes);
            X64_end_reg_group(&tmp_group);
        }
        break;
    }
    case XIR_InstrCvtSI2SD_R_M_KIND: { // integer (in memory) -> f64
        const XIR_InstrCvtSI2SD_R_M* act_instr = (const XIR_InstrCvtSI2SD_R_M*)instr;

        assert(act_instr->src_size >= 4);
        const u8 dst_size = float_kind_sizes[FLOAT_F64];
        const bool src_is_8bytes = act_instr->src_size == 8;

        X64_SIBD_Addr src_addr = {0};
        const u32 used_regs = X64_get_sibd_addr(proc_state, &src_addr, &act_instr->src);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, dst_size, true, used_regs);

        X64_emit_instr_cvtsi2sd_rm(proc_state, dst_reg, src_addr, src_is_8bytes);
        X64_end_reg_group(&tmp_group);
        break;
    }
    // CMP
    case XIR_InstrCmp_R_R_KIND: {
        const XIR_InstrCmp_R_R* act_instr = (const XIR_InstrCmp_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rr_instr(proc_state, X64_Instr_Kind_CMP_RR, true, size, act_instr->op1, act_instr->op2);
        break;
    }
    case XIR_InstrCmp_R_I_KIND: {
        const XIR_InstrCmp_R_I* act_instr = (const XIR_InstrCmp_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_ri_instr(proc_state, X64_Instr_Kind_CMP_RI, size, act_instr->op1, act_instr->op2);
        break;
    }
    case XIR_InstrCmp_R_M_KIND: {
        const XIR_InstrCmp_R_M* act_instr = (const XIR_InstrCmp_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_bin_int_rm_instr(proc_state, X64_Instr_Kind_CMP_RM, true, size, act_instr->op1, &act_instr->op2);
        break;
    }
    case XIR_InstrCmp_M_R_KIND: {
        const XIR_InstrCmp_M_R* act_instr = (const XIR_InstrCmp_M_R*)instr;
        const u8 size = act_instr->size;

        X64_emit_bin_int_mr_instr(proc_state, X64_Instr_Kind_CMP_MR, size, &act_instr->op1, act_instr->op2);
        break;
    }
    case XIR_InstrCmp_M_I_KIND: {
        const XIR_InstrCmp_M_I* act_instr = (const XIR_InstrCmp_M_I*)instr;
        const u8 size = act_instr->size;

        X64_emit_bin_int_mi_instr(proc_state, X64_Instr_Kind_CMP_MI, size, &act_instr->op1, act_instr->op2);
        break;
    }
    // UCOMISS
    case XIR_InstrUComiSS_R_R_KIND: {
        const XIR_InstrUComiSS_R_R* act_instr = (const XIR_InstrUComiSS_R_R*)instr;
        const XIR_RegLoc op2_loc = X64_lreg_loc(proc_state, act_instr->op2);
        const u8 op_size = float_kind_sizes[FLOAT_F32];

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);

        if (IS_LREG_IN_REG(op2_loc.kind)) {
            X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->op1, op_size, false, (1 << op2_loc.reg));
            X64_emit_instr_ucomiss_rr(proc_state, op1_reg, op2_loc.reg);
        }
        else {
            assert(IS_LREG_IN_STACK(op2_loc.kind));
            X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->op1, op_size, false, 0);
            X64_emit_instr_ucomiss_rm(proc_state, op1_reg, X64_get_rbp_offset_addr(op2_loc.offset));
        }
        X64_end_reg_group(&tmp_group);
        break;
    }
    case XIR_InstrUComiSS_R_M_KIND: {
        const XIR_InstrUComiSS_R_M* act_instr = (const XIR_InstrUComiSS_R_M*)instr;
        const u8 op_size = float_kind_sizes[FLOAT_F32];
        X64_SIBD_Addr op2_addr = {0};
        u32 used_regs = X64_get_sibd_addr(proc_state, &op2_addr, &act_instr->op2);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->op1, op_size, false, used_regs);

        X64_emit_instr_ucomiss_rm(proc_state, op1_reg, op2_addr);
        X64_end_reg_group(&tmp_group);
        break;
    }
    // UCOMISD
    case XIR_InstrUComiSD_R_R_KIND: {
        const XIR_InstrUComiSD_R_R* act_instr = (const XIR_InstrUComiSD_R_R*)instr;
        const XIR_RegLoc op2_loc = X64_lreg_loc(proc_state, act_instr->op2);
        const u8 op_size = float_kind_sizes[FLOAT_F64];

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);

        if (IS_LREG_IN_REG(op2_loc.kind)) {
            X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->op1, op_size, false, (1 << op2_loc.reg));
            X64_emit_instr_ucomisd_rr(proc_state, op1_reg, op2_loc.reg);
        }
        else {
            assert(IS_LREG_IN_STACK(op2_loc.kind));
            X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->op1, op_size, false, 0);
            X64_emit_instr_ucomisd_rm(proc_state, op1_reg, X64_get_rbp_offset_addr(op2_loc.offset));
        }
        X64_end_reg_group(&tmp_group);
        break;
    }
    case XIR_InstrUComiSD_R_M_KIND: {
        const XIR_InstrUComiSD_R_M* act_instr = (const XIR_InstrUComiSD_R_M*)instr;
        const u8 op_size = float_kind_sizes[FLOAT_F64];
        X64_SIBD_Addr op2_addr = {0};
        u32 used_regs = X64_get_sibd_addr(proc_state, &op2_addr, &act_instr->op2);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->op1, op_size, false, used_regs);

        X64_emit_instr_ucomisd_rm(proc_state, op1_reg, op2_addr);
        X64_end_reg_group(&tmp_group);
        break;
    }
    // LEA
    case XIR_InstrLEA_KIND: {
        const XIR_InstrLEA* act_instr = (const XIR_InstrLEA*)instr;
        const u8 size = X64_MAX_INT_REG_SIZE;

        X64_SIBD_Addr src_addr = {0};
        u32 pinned_regs = X64_get_sibd_addr(proc_state, &src_addr, &act_instr->mem);

        X64_Reg_Group tmp_group = X64_begin_reg_group(proc_state);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, size, true, pinned_regs);
        assert(size <= 8 && IS_POW2(size));

        X64_emit_instr_lea(proc_state, dst_reg, src_addr);
        X64_end_reg_group(&tmp_group);
        break;
    }
    case XIR_InstrRepMovsb_KIND: {
        X64_emit_instr_rep_movsb(proc_state);
        break;
    }
    case XIR_InstrRepStosb_KIND: {
        X64_emit_instr_rep_stosb(proc_state);
        break;
    }
    case XIR_InstrSyscall_KIND: {
        X64_emit_instr_syscall(proc_state);
        break;
    }
    case XIR_InstrJmp_KIND: {
        const XIR_InstrJmp* act_instr = (const XIR_InstrJmp*)instr;
        long target_id = act_instr->target->id;

        if (target_id != bblock_id + 1) {
            assert(target_id >= 0);
            X64_emit_instr_jmp(proc_state, s64_to_u32(target_id));
        }
        break;
    }
    case XIR_InstrJmpCC_KIND: {
        const XIR_InstrJmpCC* act_instr = (const XIR_InstrJmpCC*)instr;
        X64_emit_instr_jmpcc(proc_state, act_instr->cond, s64_to_u32(act_instr->true_bb->id));
        break;
    }
    case XIR_InstrSetCC_KIND: {
        const XIR_InstrSetCC* act_instr = (const XIR_InstrSetCC*)instr;
        XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_instr_setcc_r(proc_state, act_instr->cond, dst_loc.reg);
        }
        else {
            assert(IS_LREG_IN_STACK(dst_loc.kind));
            X64_emit_instr_setcc_m(proc_state, act_instr->cond, X64_get_rbp_offset_addr(dst_loc.offset));
        }
        break;
    }
    case XIR_InstrRet_KIND: {
        if (!is_last_instr) {
            X64_emit_instr_jmp_to_ret(proc_state);
        }

        break;
    }
    case XIR_InstrCall_KIND:
    case XIR_InstrCall_R_KIND: {
        const Type* proc_type;
        XIR_CallValue dst_val;
        u32 num_args;
        const XIR_InstrCallArg* args;
        X64_StackArgsInfo stack_args_info;
        unsigned save_reg_mask;

        if (instr->kind == XIR_InstrCall_KIND) {
            const XIR_InstrCall* instr_call = (const XIR_InstrCall*)instr;

            proc_type = instr_call->sym->type;
            dst_val = instr_call->dst;
            num_args = instr_call->num_args;
            args = instr_call->args;
            stack_args_info = instr_call->stack_info;
            save_reg_mask = instr_call->save_reg_mask;
        }
        else {
            assert(instr->kind == XIR_InstrCall_R_KIND);
            const XIR_InstrCall_R* instr_call_r = (const XIR_InstrCall_R*)instr;

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
        X64_Instr* rsp_align_instr = X64_emit_instr_placeholder(proc_state, X64_Instr_Kind_NOOP);

        // NOTE: No need to save caller-saved registers before call because the register allocator currently
        // spills any values needed across procedure calls.
        X64_Reg_Group group = X64_begin_reg_group(proc_state);

        // Save caller-saved registers needed across the call.
        u32 r = 0;

        while (save_reg_mask) {
            if (save_reg_mask & 0x1) {
                X64_save_reg_to_group(&group, (X64_Reg)r);
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

            X64_get_sibd_addr(proc_state, &obj_addr, &dst_val.addr);
            X64_emit_instr_lea(proc_state, dst_reg, obj_addr);
        }

        //
        // Place arguments in the appropriate locations.
        //

        // Make room in the stack for arguments passed via the stack.
        if (stack_args_info.size) {
            X64_emit_instr_sub_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, stack_args_info.size);
        }

        // Place register args. It is expected that the register allocator either placed the arg in the correct register or spilled it.
        X64_place_args_in_regs(proc_state, num_args, args);

        // Place stack args. It is expected that the register allocator either placed the arg in a non-argument register or spilled it.
        if (stack_args_info.size) {
            X64_place_args_in_stack(proc_state, num_args, args);
        }

        //
        // Align stack before call.
        //

        u64 total_stack_size = stack_args_info.size + group.num_tmp_regs * X64_MAX_INT_REG_SIZE;
        u64 align_stack_size = 0;

        if (total_stack_size & (X64_STACK_ALIGN - 1)) {
            align_stack_size = X64_STACK_WORD_SIZE;

            X64_set_instr_kind(rsp_align_instr, X64_Instr_Kind_SUB_RI);
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

        if (instr->kind == XIR_InstrCall_KIND) {
            const XIR_InstrCall* instr_call = (const XIR_InstrCall*)instr;

            X64_emit_instr_call(proc_state, instr_call->sym);
        }
        else {
            const XIR_InstrCall_R* instr_call_r = (const XIR_InstrCall_R*)instr;
            XIR_RegLoc proc_reg_loc = X64_lreg_loc(proc_state, instr_call_r->proc_loc);

            if (IS_LREG_IN_REG(proc_reg_loc.kind)) {
                X64_emit_instr_call_r(proc_state, proc_reg_loc.reg);
            }
            else {
                X64_emit_instr_call_m(proc_state, X64_get_rbp_offset_addr(proc_reg_loc.offset));
            }
        }

        //
        // Move return value (if any) to appropriate register.
        //

        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            if (!type_is_obj_like(ret_type)) { // If returns a primitive type.
                XIR_RegLoc dst_loc = X64_lreg_loc(proc_state, dst_val.reg);

                if (ret_type->kind == TYPE_FLOAT) {
                    X64_Reg ret_reg = (*x64_target.ret_regs)[X64_REG_CLASS_FLOAT].regs[0];

                    if (IS_LREG_IN_STACK(dst_loc.kind)) {
                        // Move result (in XMM0) to stack offset.
                        // Ex: movsd [rbp + x], xmm0
                        X64_emit_instr_mov_flt_mr(proc_state, ret_type->as_float.kind, X64_get_rbp_offset_addr(dst_loc.offset),
                                                  ret_reg);
                    }
                    else if (dst_loc.reg != ret_reg) {
                        // Move result (in XMM0) to allocated result register.
                        assert(IS_LREG_IN_REG(dst_loc.kind));
                        X64_emit_instr_mov_flt_rr(proc_state, ret_type->as_float.kind, dst_loc.reg, ret_reg);
                    }
                }
                else {
                    X64_Reg ret_reg = (*x64_target.ret_regs)[X64_REG_CLASS_INT].regs[0];

                    if (IS_LREG_IN_STACK(dst_loc.kind)) {
                        // Move result (in RAX) to stack offset.
                        // Ex: mov qword [rbp + x], rax
                        X64_emit_instr_mov_mr(proc_state, ret_type->size, X64_get_rbp_offset_addr(dst_loc.offset), ret_reg);
                    }
                    else if (dst_loc.reg != ret_reg) {
                        // Move result (in RAX) to allocated result register.
                        assert(IS_LREG_IN_REG(dst_loc.kind));
                        X64_emit_instr_mov_rr(proc_state, ret_type->size, dst_loc.reg, ret_reg);
                    }
                }
            }
            else { // Else returns a small object.
                X64_cpy_ret_small_obj(proc_state, ret_type, &dst_val);
            }
        }

        if (group.num_tmp_regs) {
            if (stack_args_info.size) { // Clean up stack args.
                X64_emit_instr_add_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, stack_args_info.size);
            }

            // Restore saved registers.
            X64_end_reg_group(&group);

            // Clean up any initial stack alignment
            if (align_stack_size) {
                X64_emit_instr_add_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, align_stack_size);
            }
        }
        else {
            size_t cleanup_amount = stack_args_info.size + align_stack_size;

            if (cleanup_amount) {
                // Clean up stack args + alignment
                X64_emit_instr_add_ri(proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, cleanup_amount);
            }
        }

        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unhandled X64 LIR instruction \"%s\" at IP %u\n", XIR_print_instr(proc_state->tmp_mem, instr), instr->ino);
        break;
    }

    allocator_restore_state(mem_state);
}

X64_Instrs X64_gen_proc_instrs(Allocator* gen_mem, Allocator* tmp_mem, Symbol* proc_sym)
{
    const bool is_nonleaf = proc_sym->as_proc.is_nonleaf;
    BBlock** ir_bblocks = proc_sym->as_proc.bblocks;
    size_t num_ir_bblocks = array_len(ir_bblocks);
    const u32 num_x64_bblocks = num_ir_bblocks + 1;
    XIR_Builder xir_builder = {.arena = tmp_mem};

    X64_Proc_State proc_state = {
        .gen_mem = gen_mem,
        .tmp_mem = tmp_mem,
        .sym = proc_sym,
        .instrs = {.num_bblocks = num_x64_bblocks, .bblocks = alloc_array(gen_mem, X64_BBlock, num_x64_bblocks, true)},
        .curr_bblock = 0,
        .scratch_regs = is_nonleaf ? x64_target.nonleaf_scratch_regs : x64_target.leaf_scratch_regs,
    };

    AllocatorState tmp_mem_state = allocator_get_state(proc_state.tmp_mem);
    //////////////////////////////////////////////////////////////////////////////////////////
    X64_emit_instr_push(&proc_state, X64_RBP);
    X64_emit_instr_mov_rr(&proc_state, X64_MAX_INT_REG_SIZE, X64_RBP, X64_RSP);
    X64_Instr* sub_rsp_instr = X64_emit_instr_placeholder(&proc_state, X64_Instr_Kind_NOOP); // Placeholder sub rsp, <stack_size>

    // Calculate stack size from procedure arguments (spills) and local variables.
    u32 stack_size = X64_assign_proc_stack_offsets(&proc_state);

    // Register allocation.
    XIR_emit_instrs(&xir_builder, proc_sym->as_proc.num_regs, num_ir_bblocks, ir_bblocks); // Generate LIR instructions.
    XIR_compute_live_intervals(&xir_builder); // Compute LIR register intervals.
    assert(num_ir_bblocks == xir_builder.num_bblocks);

    X64_RegAllocResult reg_alloc =
        X64_linear_scan_reg_alloc(&xir_builder, proc_state.scratch_regs, stack_size); // May spill and increase stack_size.

    if (!reg_alloc.success) {
        NIBBLE_FATAL_EXIT("Register allocation for procedure `%s` failed.", proc_sym->name->str);
        return proc_state.instrs;
    }

    stack_size = reg_alloc.stack_offset;
    proc_state.xir_builder = &xir_builder;

    // Fill in sub rsp, <stack_size>
    if (stack_size) {
        X64_set_instr_kind(sub_rsp_instr, X64_Instr_Kind_SUB_RI);
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
            X64_push_reg_to_stack(&proc_state, reg);
        }
    }

    // Process procedure instructions.
    for (size_t ii = 0; ii < xir_builder.num_bblocks; ii++) {
        XIR_BBlock* bb = xir_builder.bblocks[ii];
        bool last_bb = ii == xir_builder.num_bblocks - 1;

        proc_state.curr_bblock = ii;

        for (XIR_Instr* instr = bb->first; instr; instr = instr->next) {
            bool last_instr = last_bb && !instr->next;

            X64_gen_instr(&proc_state, instr, last_instr, bb->id);
        }
    }

    const u32 postamble_bblock_idx = proc_state.instrs.num_bblocks - 1;

    // Patch jmp "to ret label" instructions.
    X64_patch_jmp_instrs(&proc_state.instrs, postamble_bblock_idx);

    //
    // Postamble.
    //

    proc_state.curr_bblock = postamble_bblock_idx; // The last bblock is for postamble.

    // Restore callee-saved registers.
    // NOTE: Iterating in the reverse order as the corresponding pushes.
    for (uint32_t r = X64_REG_COUNT; r-- > 0;) {
        X64_Reg reg = (X64_Reg)r;

        if (reg == X64_RBP || reg == X64_RSP)
            continue;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg)) {
            X64_pop_reg_from_stack(&proc_state, reg);
        }
    }

    // Clean up stack and return to caller.
    X64_emit_instr_mov_rr(&proc_state, X64_MAX_INT_REG_SIZE, X64_RSP, X64_RBP);
    X64_emit_instr_pop(&proc_state, X64_RBP);
    X64_emit_instr_ret(&proc_state);

    //////////////////////////////////////////////////////////////////////////////////////////
    allocator_restore_state(tmp_mem_state);

    return proc_state.instrs;
}
