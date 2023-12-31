#include "x64_gen/x64_instrs.h"

#define X_MACRO(name) [X64_Instr_Kind_ ## name] = string_view_lit("X64_Instr_Kind_" #name),
StringView x64_instr_kind_names[X64_Instr_Kind_COUNT] = {
    LIST_OF_X64_INSTR_KINDS
};
#undef X_MACRO

static void X64_push_instr(X64_Instrs* instrs, X64_Instr* instr)
{
    assert(array_len(instrs->bblocks) > 0);
    X64_BBlock* bblock = &array_back(instrs->bblocks);

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

void X64_emit_instr_ret(X64_Instrs* instrs)
{
    X64_push_instr(instrs, X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_RET));
}

void X64_emit_instr_call(X64_Instrs* instrs, const Symbol* proc_sym)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_CALL);
    instr->call.proc_sym = proc_sym;
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_call_r(X64_Instrs* instrs, u8 reg)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_CALL_R);
    instr->call_r.reg = reg;
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_call_m(X64_Instrs* instrs, X64_SIBD_Addr mem)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_CALL_M);
    instr->call_m.mem = mem;
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_jmp(X64_Instrs* instrs, u32 target)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_JMP);
    instr->jmp.target = target;
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_jmp_to_ret(X64_Instrs* instrs)
{
    X64_push_instr(instrs, X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_JMP_TO_RET));
}

void X64_emit_instr_jmpcc(X64_Instrs* instrs, ConditionKind cond_kind, u32 target)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_JMPCC);
    instr->jmpcc.target = target;
    instr->jmpcc.cond = cond_kind;
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_setcc_r(X64_Instrs* instrs, ConditionKind cond_kind, u8 dst)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_SETCC_R);
    instr->setcc_r.cond = cond_kind;
    instr->setcc_r.dst = dst;
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_setcc_m(X64_Instrs* instrs, ConditionKind cond_kind, X64_SIBD_Addr dst)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_SETCC_M);
    instr->setcc_m.cond = cond_kind;
    instr->setcc_m.dst = dst;
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_push(X64_Instrs* instrs, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_PUSH);
    instr->push.reg = reg;
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_pop(X64_Instrs* instrs, X64_Reg reg)
{
    assert(x64_reg_classes[reg] == X64_REG_CLASS_INT);
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_POP);
    instr->pop.reg = reg;
    X64_push_instr(instrs, instr);
}

#define X64_DEF_EMIT_INSTR_BINARY_RR(field, kind)                                      \
    void X64_emit_instr_##field(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_Reg src) \
    {                                                                                  \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), kind);      \
        instr->field.size = size;                                                      \
        instr->field.dst = dst;                                                        \
        instr->field.src = src;                                                        \
                                                                                       \
        X64_push_instr(instrs, instr);                                                 \
    }

#define X64_DEF_EMIT_INSTR_BINARY_RM(field, kind)                                            \
    void X64_emit_instr_##field(X64_Instrs* instrs, u8 size, X64_Reg dst, X64_SIBD_Addr src) \
    {                                                                                        \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), kind);            \
        instr->field.size = size;                                                            \
        instr->field.dst = dst;                                                              \
        instr->field.src = src;                                                              \
                                                                                             \
        X64_push_instr(instrs, instr);                                                       \
    }

#define X64_DEF_EMIT_INSTR_BINARY_MR(field, kind)                                            \
    void X64_emit_instr_##field(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, X64_Reg src) \
    {                                                                                        \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), kind);            \
        instr->field.size = size;                                                            \
        instr->field.dst = dst;                                                              \
        instr->field.src = src;                                                              \
                                                                                             \
        X64_push_instr(instrs, instr);                                                       \
    }

#define X64_DEF_EMIT_INSTR_BINARY_RI(field, kind)                                  \
    void X64_emit_instr_##field(X64_Instrs* instrs, u8 size, X64_Reg dst, u32 imm) \
    {                                                                              \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), kind);  \
        instr->field.size = size;                                                  \
        instr->field.dst = dst;                                                    \
        instr->field.imm = imm;                                                    \
                                                                                   \
        X64_push_instr(instrs, instr);                                             \
    }

#define X64_DEF_EMIT_INSTR_BINARY_MI(field, kind)                                        \
    void X64_emit_instr_##field(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u32 imm) \
    {                                                                                    \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), kind);        \
        instr->field.size = size;                                                        \
        instr->field.dst = dst;                                                          \
        instr->field.imm = imm;                                                          \
                                                                                         \
        X64_push_instr(instrs, instr);                                                   \
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

#define X64_DEF_EMIT_INSTR_FLT_BINARY_RR(f_d, k_d)                                                 \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_Reg src) \
    {                                                                                              \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                   \
        instr->f_d.kind = kind;                                                                    \
        instr->f_d.dst = dst;                                                                      \
        instr->f_d.src = src;                                                                      \
                                                                                                   \
        X64_push_instr(instrs, instr);                                                             \
    }

#define X64_DEF_EMIT_INSTR_FLT_BINARY_RM(f_d, k_d)                                                       \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, FloatKind kind, X64_Reg dst, X64_SIBD_Addr src) \
    {                                                                                                    \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                         \
        instr->f_d.kind = kind;                                                                          \
        instr->f_d.dst = dst;                                                                            \
        instr->f_d.src = src;                                                                            \
                                                                                                         \
        X64_push_instr(instrs, instr);                                                                   \
    }

#define X64_DEF_EMIT_INSTR_FLT_BINARY_MR(f_d, k_d)                                                       \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, FloatKind kind, X64_SIBD_Addr dst, X64_Reg src) \
    {                                                                                                    \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                         \
        instr->f_d.kind = kind;                                                                          \
        instr->f_d.dst = dst;                                                                            \
        instr->f_d.src = src;                                                                            \
                                                                                                         \
        X64_push_instr(instrs, instr);                                                                   \
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

#define X64_DEF_EMIT_INSTR_UNARY_R(f_d, k_d)                                     \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 size, X64_Reg dst)   \
    {                                                                            \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d); \
        instr->f_d.size = size;                                                  \
        instr->f_d.dst = dst;                                                    \
                                                                                 \
        X64_push_instr(instrs, instr);                                           \
    }

#define X64_DEF_EMIT_INSTR_UNARY_M(f_d, k_d)                                         \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst) \
    {                                                                                \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);     \
        instr->f_d.size = size;                                                      \
        instr->f_d.dst = dst;                                                        \
                                                                                     \
        X64_push_instr(instrs, instr);                                               \
    }

X64_DEF_EMIT_INSTR_UNARY_R(neg_r, X64_Instr_Kind_NEG_R)
X64_DEF_EMIT_INSTR_UNARY_M(neg_m, X64_Instr_Kind_NEG_M)

X64_DEF_EMIT_INSTR_UNARY_R(not_r, X64_Instr_Kind_NOT_R)
X64_DEF_EMIT_INSTR_UNARY_M(not_m, X64_Instr_Kind_NOT_M)

#define X64_DEF_EMIT_INSTR_SHIFT_RR(f_d, k_d)                                    \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 size, X64_Reg dst)   \
    {                                                                            \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d); \
        instr->f_d.size = size;                                                  \
        instr->f_d.dst = dst;                                                    \
                                                                                 \
        X64_push_instr(instrs, instr);                                           \
    }

#define X64_DEF_EMIT_INSTR_SHIFT_MR(f_d, k_d)                                        \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst) \
    {                                                                                \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);     \
        instr->f_d.size = size;                                                      \
        instr->f_d.dst = dst;                                                        \
                                                                                     \
        X64_push_instr(instrs, instr);                                               \
    }

#define X64_DEF_EMIT_INSTR_SHIFT_RI(f_d, k_d)                                          \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 size, X64_Reg dst, u8 imm) \
    {                                                                                  \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);       \
        instr->f_d.size = size;                                                        \
        instr->f_d.dst = dst;                                                          \
        instr->f_d.imm = imm;                                                          \
                                                                                       \
        X64_push_instr(instrs, instr);                                                 \
    }

#define X64_DEF_EMIT_INSTR_SHIFT_MI(f_d, k_d)                                                \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 size, X64_SIBD_Addr dst, u8 imm) \
    {                                                                                        \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);             \
        instr->f_d.size = size;                                                              \
        instr->f_d.dst = dst;                                                                \
        instr->f_d.imm = imm;                                                                \
                                                                                             \
        X64_push_instr(instrs, instr);                                                       \
    }

X64_DEF_EMIT_INSTR_SHIFT_RR(sar_rr, X64_Instr_Kind_SAR_RR)
X64_DEF_EMIT_INSTR_SHIFT_MR(sar_mr, X64_Instr_Kind_SAR_MR)
X64_DEF_EMIT_INSTR_SHIFT_RI(sar_ri, X64_Instr_Kind_SAR_RI)
X64_DEF_EMIT_INSTR_SHIFT_MI(sar_mi, X64_Instr_Kind_SAR_MI)

X64_DEF_EMIT_INSTR_SHIFT_RR(shl_rr, X64_Instr_Kind_SHL_RR)
X64_DEF_EMIT_INSTR_SHIFT_MR(shl_mr, X64_Instr_Kind_SHL_MR)
X64_DEF_EMIT_INSTR_SHIFT_RI(shl_ri, X64_Instr_Kind_SHL_RI)
X64_DEF_EMIT_INSTR_SHIFT_MI(shl_mi, X64_Instr_Kind_SHL_MI)

#define X64_DEF_EMIT_INSTR_DIV_MUL_R(f_d, k_d)                                   \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 size, X64_Reg src)          \
    {                                                                            \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d); \
        instr->f_d.size = size;                                                  \
        instr->f_d.src = src;                                                    \
                                                                                 \
        X64_push_instr(instrs, instr);                                           \
    }

#define X64_DEF_EMIT_INSTR_DIV_MUL_M(f_d, k_d)                                       \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 size, X64_SIBD_Addr src)        \
    {                                                                                \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);     \
        instr->f_d.size = size;                                                      \
        instr->f_d.src = src;                                                        \
                                                                                     \
        X64_push_instr(instrs, instr);                                               \
    }

X64_DEF_EMIT_INSTR_DIV_MUL_R(div_r, X64_Instr_Kind_DIV_R)
X64_DEF_EMIT_INSTR_DIV_MUL_M(div_m, X64_Instr_Kind_DIV_M)

X64_DEF_EMIT_INSTR_DIV_MUL_R(idiv_r, X64_Instr_Kind_IDIV_R)
X64_DEF_EMIT_INSTR_DIV_MUL_M(idiv_m, X64_Instr_Kind_IDIV_M)

X64_DEF_EMIT_INSTR_DIV_MUL_R(mul_r, X64_Instr_Kind_MUL_R)
X64_DEF_EMIT_INSTR_DIV_MUL_M(mul_m, X64_Instr_Kind_MUL_M)

void X64_emit_instr_sext_ax_into_dx(X64_Instrs* instrs, u8 size)
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

    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), kind);
    X64_push_instr(instrs, instr);
}

X64_DEF_EMIT_INSTR_BINARY_RR(mov_rr, X64_Instr_Kind_MOV_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(mov_rm, X64_Instr_Kind_MOV_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(mov_mr, X64_Instr_Kind_MOV_MR)
X64_DEF_EMIT_INSTR_BINARY_MI(mov_mi, X64_Instr_Kind_MOV_MI)

// MOV_RI is the only instruction that can load an 8-byte immediate constant.
void X64_emit_instr_mov_ri(X64_Instrs* instrs, u8 size, X64_Reg dst, u64 imm)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_MOV_RI);
    instr->mov_ri.size = size;
    instr->mov_ri.dst = dst;
    instr->mov_ri.imm = imm;

    X64_push_instr(instrs, instr);
}

void X64_emit_instr_mov_rrh(X64_Instrs* instrs, X64_Reg dst, X64_Reg src)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_MOV_RR);
    instr->flags |= X64_INSTR_MOV_SRC_RH_MASK;
    instr->mov_rr.size = 1;
    instr->mov_rr.dst = dst;
    instr->mov_rr.src = src;

    X64_push_instr(instrs, instr);
}

void X64_emit_instr_mov_mrh(X64_Instrs* instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_MOV_MR);
    instr->flags |= X64_INSTR_MOV_SRC_RH_MASK;
    instr->mov_mr.size = 1;
    instr->mov_mr.dst = dst;
    instr->mov_mr.src = src;

    X64_push_instr(instrs, instr);
}

#define X64_DEF_EMIT_INSTR_MOV_EXT_RR(f_d, k_d)                                                              \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_Reg src) \
    {                                                                                                        \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                             \
        instr->f_d.dst_size = dst_size;                                                                      \
        instr->f_d.src_size = src_size;                                                                      \
        instr->f_d.dst = dst;                                                                                \
        instr->f_d.src = src;                                                                                \
                                                                                                             \
        X64_push_instr(instrs, instr);                                                                       \
    }

#define X64_DEF_EMIT_INSTR_MOV_EXT_RM(f_d, k_d)                                                                    \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, u8 dst_size, X64_Reg dst, u8 src_size, X64_SIBD_Addr src) \
    {                                                                                                              \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                                   \
        instr->f_d.dst_size = dst_size;                                                                            \
        instr->f_d.src_size = src_size;                                                                            \
        instr->f_d.dst = dst;                                                                                      \
        instr->f_d.src = src;                                                                                      \
                                                                                                                   \
        X64_push_instr(instrs, instr);                                                                             \
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

#define X64_DEF_EMIT_INSTR_CVT_FLT_RR(f_d, k_d)                                    \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, X64_Reg dst, X64_Reg src) \
    {                                                                              \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);   \
        instr->f_d.dst = dst;                                                      \
        instr->f_d.src = src;                                                      \
                                                                                   \
        X64_push_instr(instrs, instr);                                             \
    }

#define X64_DEF_EMIT_INSTR_CVT_FLT_RM(f_d, k_d)                                          \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src) \
    {                                                                                    \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);         \
        instr->f_d.dst = dst;                                                            \
        instr->f_d.src = src;                                                            \
                                                                                         \
        X64_push_instr(instrs, instr);                                                   \
    }

X64_DEF_EMIT_INSTR_CVT_FLT_RR(cvtss2sd_rr, X64_Instr_Kind_CVTSS2SD_RR)
X64_DEF_EMIT_INSTR_CVT_FLT_RM(cvtss2sd_rm, X64_Instr_Kind_CVTSS2SD_RM)

X64_DEF_EMIT_INSTR_CVT_FLT_RR(cvtsd2ss_rr, X64_Instr_Kind_CVTSD2SS_RR)
X64_DEF_EMIT_INSTR_CVT_FLT_RM(cvtsd2ss_rm, X64_Instr_Kind_CVTSD2SS_RM)

#define X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RR(f_d, k_d)                                              \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, X64_Reg dst, bool dst_8bytes, X64_Reg src) \
    {                                                                                               \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                    \
        instr->f_d.dst = dst;                                                                       \
        instr->f_d.src = src;                                                                       \
                                                                                                    \
        if (dst_8bytes) {                                                                           \
            instr->flags |= X64_INSTR_CVT_FLT_SI_INT64_MASK;                                        \
        }                                                                                           \
                                                                                                    \
        X64_push_instr(instrs, instr);                                                              \
    }

#define X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RM(f_d, k_d)                                                    \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, X64_Reg dst, bool dst_8bytes, X64_SIBD_Addr src) \
    {                                                                                                     \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                          \
        instr->f_d.dst = dst;                                                                             \
        instr->f_d.src = src;                                                                             \
                                                                                                          \
        if (dst_8bytes) {                                                                                 \
            instr->flags |= X64_INSTR_CVT_FLT_SI_INT64_MASK;                                              \
        }                                                                                                 \
                                                                                                          \
        X64_push_instr(instrs, instr);                                                                    \
    }

// f32 to int
X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RR(cvttss2si_rr, X64_Instr_Kind_CVTTSS2SI_RR)
X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RM(cvttss2si_rm, X64_Instr_Kind_CVTTSS2SI_RM)

// f64 to int
X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RR(cvttsd2si_rr, X64_Instr_Kind_CVTTSD2SI_RR)
X64_DEF_EMIT_INSTR_CVT_FLT_TO_INT_RM(cvttsd2si_rm, X64_Instr_Kind_CVTTSD2SI_RM)

#define X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RR(f_d, k_d)                                              \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, X64_Reg dst, X64_Reg src, bool src_8bytes) \
    {                                                                                               \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                    \
        instr->f_d.dst = dst;                                                                       \
        instr->f_d.src = src;                                                                       \
                                                                                                    \
        if (src_8bytes) {                                                                           \
            instr->flags |= X64_INSTR_CVT_FLT_SI_INT64_MASK;                                        \
        }                                                                                           \
                                                                                                    \
        X64_push_instr(instrs, instr);                                                              \
    }

#define X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RM(f_d, k_d)                                                    \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src, bool src_8bytes) \
    {                                                                                                     \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);                          \
        instr->f_d.dst = dst;                                                                             \
        instr->f_d.src = src;                                                                             \
                                                                                                          \
        if (src_8bytes) {                                                                                 \
            instr->flags |= X64_INSTR_CVT_FLT_SI_INT64_MASK;                                              \
        }                                                                                                 \
                                                                                                          \
        X64_push_instr(instrs, instr);                                                                    \
    }

// int to f32
X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RR(cvtsi2ss_rr, X64_Instr_Kind_CVTSI2SS_RR)
X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RM(cvtsi2ss_rm, X64_Instr_Kind_CVTSI2SS_RM)

// int to f64
X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RR(cvtsi2sd_rr, X64_Instr_Kind_CVTSI2SD_RR)
X64_DEF_EMIT_INSTR_CVT_INT_TO_FLT_RM(cvtsi2sd_rm, X64_Instr_Kind_CVTSI2SD_RM)

void X64_emit_instr_movdqu_mr(X64_Instrs* instrs, X64_SIBD_Addr dst, X64_Reg src)
{
    assert(x64_reg_classes[src] == X64_REG_CLASS_FLOAT);
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_MOVDQU_MR);
    instr->movdqu_mr.dst = dst;
    instr->movdqu_mr.src = src;

    X64_push_instr(instrs, instr);
}

void X64_emit_instr_movdqu_rm(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    assert(x64_reg_classes[dst] == X64_REG_CLASS_FLOAT);
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_MOVDQU_RM);
    instr->movdqu_rm.dst = dst;
    instr->movdqu_rm.src = src;

    X64_push_instr(instrs, instr);
}

X64_DEF_EMIT_INSTR_BINARY_RR(cmp_rr, X64_Instr_Kind_CMP_RR)
X64_DEF_EMIT_INSTR_BINARY_RM(cmp_rm, X64_Instr_Kind_CMP_RM)
X64_DEF_EMIT_INSTR_BINARY_MR(cmp_mr, X64_Instr_Kind_CMP_MR)
X64_DEF_EMIT_INSTR_BINARY_RI(cmp_ri, X64_Instr_Kind_CMP_RI)
X64_DEF_EMIT_INSTR_BINARY_MI(cmp_mi, X64_Instr_Kind_CMP_MI)

#define X64_DEF_EMIT_INSTR_CMP_FLT_RR(f_d, k_d)                                    \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, X64_Reg dst, X64_Reg src) \
    {                                                                              \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);   \
        instr->f_d.dst = dst;                                                      \
        instr->f_d.src = src;                                                      \
                                                                                   \
        X64_push_instr(instrs, instr);                                             \
    }

#define X64_DEF_EMIT_INSTR_CMP_FLT_RM(f_d, k_d)                                          \
    void X64_emit_instr_##f_d(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src) \
    {                                                                                    \
        X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), k_d);         \
        instr->f_d.dst = dst;                                                            \
        instr->f_d.src = src;                                                            \
                                                                                         \
        X64_push_instr(instrs, instr);                                                   \
    }

// Compare f32s
X64_DEF_EMIT_INSTR_CMP_FLT_RR(ucomiss_rr, X64_Instr_Kind_UCOMISS_RR)
X64_DEF_EMIT_INSTR_CMP_FLT_RM(ucomiss_rm, X64_Instr_Kind_UCOMISS_RM)

// Compare f64s
X64_DEF_EMIT_INSTR_CMP_FLT_RR(ucomisd_rr, X64_Instr_Kind_UCOMISD_RR)
X64_DEF_EMIT_INSTR_CMP_FLT_RM(ucomisd_rm, X64_Instr_Kind_UCOMISD_RM)

void X64_emit_instr_lea(X64_Instrs* instrs, X64_Reg dst, X64_SIBD_Addr src)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_LEA);
    instr->lea.dst = dst;
    instr->lea.src = src;

    X64_push_instr(instrs, instr);
}

void X64_emit_instr_rep_movsb(X64_Instrs* instrs)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_REP_MOVSB);
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_rep_stosb(X64_Instrs* instrs)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_REP_STOSB);
    X64_push_instr(instrs, instr);
}

void X64_emit_instr_syscall(X64_Instrs* instrs)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), X64_Instr_Kind_SYSCALL);
    X64_push_instr(instrs, instr);
}

X64_Instr* X64_emit_instr_placeholder(X64_Instrs* instrs, X64_Instr_Kind kind)
{
    X64_Instr* instr = X64_alloc_instr(_array_allctr(instrs->bblocks), kind);
    X64_push_instr(instrs, instr);
    return instr;
}

