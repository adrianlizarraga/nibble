#include "x64_gen/lir.h"

static void X64_bblock_add_instr(X64_BBlock* bblock, X64_Instr* instr)
{
    if (!bblock->first) {
        bblock->first = instr;
    }
    else {
        bblock->last->next = instr;
    }

    instr->prev = bblock->last;
    bblock->last = instr;

    bblock->num_instrs += 1;
}

static void X64_add_lir_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_Instr* instr)
{
    instr->ino = builder->num_instrs;
    builder->num_instrs += 1;

    X64_bblock_add_instr(xbblock, instr);
}

#define X64_new_instr(a, k) (k*)X64_new_instr_((a), k##_KIND, sizeof(k), alignof(k))
static X64_Instr* X64_new_instr_(Allocator* arena, X64_InstrKind kind, size_t size, size_t align)
{
    X64_Instr* instr = mem_allocate(arena, size, align, true);
    instr->kind = kind;

    return instr;
}

void X64_emit_instr_add_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrAdd_R_R* instr = X64_new_instr(builder->arena, X64_InstrAdd_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_sub_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrSub_R_R* instr = X64_new_instr(builder->arena, X64_InstrSub_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_imul_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrIMul_R_R* instr = X64_new_instr(builder->arena, X64_InstrIMul_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_and_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrAnd_R_R* instr = X64_new_instr(builder->arena, X64_InstrAnd_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_or_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrOr_R_R* instr = X64_new_instr(builder->arena, X64_InstrOr_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_xor_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrXor_R_R* instr = X64_new_instr(builder->arena, X64_InstrXor_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_add_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrAdd_R_I* instr = X64_new_instr(builder->arena, X64_InstrAdd_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_sub_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrSub_R_I* instr = X64_new_instr(builder->arena, X64_InstrSub_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_imul_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrIMul_R_I* instr = X64_new_instr(builder->arena, X64_InstrIMul_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_and_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrAnd_R_I* instr = X64_new_instr(builder->arena, X64_InstrAnd_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_or_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrOr_R_I* instr = X64_new_instr(builder->arena, X64_InstrOr_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_xor_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrXor_R_I* instr = X64_new_instr(builder->arena, X64_InstrXor_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_add_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src)
{
    X64_InstrAdd_R_M* instr = X64_new_instr(builder->arena, X64_InstrAdd_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_sub_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src)
{
    X64_InstrSub_R_M* instr = X64_new_instr(builder->arena, X64_InstrSub_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_imul_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src)
{
    X64_InstrIMul_R_M* instr = X64_new_instr(builder->arena, X64_InstrIMul_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_and_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src)
{
    X64_InstrAnd_R_M* instr = X64_new_instr(builder->arena, X64_InstrAnd_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_or_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src)
{
    X64_InstrOr_R_M* instr = X64_new_instr(builder->arena, X64_InstrOr_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_xor_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src)
{
    X64_InstrXor_R_M* instr = X64_new_instr(builder->arena, X64_InstrXor_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_addss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrAddSS_R_R* instr = X64_new_instr(builder->arena, X64_InstrAddSS_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_addsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrAddSD_R_R* instr = X64_new_instr(builder->arena, X64_InstrAddSD_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

// TODO: Left off here
void X64_emit_instr_flt_add_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, X64_MemAddr src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrAddSD_R_M_KIND, [FLOAT_F32] = X64_InstrAddSS_R_M_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->binary_flt_r_m.dst = dst;
    instr->binary_flt_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt_sub_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, u32 src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrSubSD_R_R_KIND, [FLOAT_F32] = X64_InstrSubSS_R_R_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->binary_flt_r_r.dst = dst;
    instr->binary_flt_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt_sub_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, X64_MemAddr src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrSubSD_R_M_KIND, [FLOAT_F32] = X64_InstrSubSS_R_M_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->binary_flt_r_m.dst = dst;
    instr->binary_flt_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt_mul_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, u32 src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrMulSD_R_R_KIND, [FLOAT_F32] = X64_InstrMulSS_R_R_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->binary_flt_r_r.dst = dst;
    instr->binary_flt_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt_mul_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, X64_MemAddr src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrMulSD_R_M_KIND, [FLOAT_F32] = X64_InstrMulSS_R_M_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->binary_flt_r_m.dst = dst;
    instr->binary_flt_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt_div_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, u32 src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrDivSD_R_R_KIND, [FLOAT_F32] = X64_InstrDivSS_R_R_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->binary_flt_r_r.dst = dst;
    instr->binary_flt_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt_div_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, X64_MemAddr src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrDivSD_R_M_KIND, [FLOAT_F32] = X64_InstrDivSS_R_M_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->binary_flt_r_m.dst = dst;
    instr->binary_flt_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_shift_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, u8 size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->shift_r_r.size = size;
    instr->shift_r_r.dst = dst;
    instr->shift_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_shift_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, u8 size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->shift_r_i.size = size;
    instr->shift_r_i.dst = dst;
    instr->shift_r_i.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_div_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, u8 size, u32 rdx, u32 rax, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->div_r.size = size;
    instr->div_r.rdx = rdx;
    instr->div_r.rax = rax;
    instr->div_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_div_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, u8 size, u32 rdx, u32 rax, X64_MemAddr src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->div_m.size = size;
    instr->div_m.rdx = rdx;
    instr->div_m.rax = rax;
    instr->div_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_unary(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, u8 size, u32 dst)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->unary.size = size;
    instr->unary.dst = dst;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrMov_R_R_KIND);
    instr->mov_r_r.size = size;
    instr->mov_r_r.dst = dst;
    instr->mov_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_r_rh(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrMov_R_RH_KIND);
    instr->mov_r_rh.dst = dst;
    instr->mov_r_rh.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrMov_R_M_KIND);
    instr->mov_r_m.size = size;
    instr->mov_r_m.dst = dst;
    instr->mov_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrMov_R_I_KIND);
    instr->mov_r_i.size = size;
    instr->mov_r_i.dst = dst;
    instr->mov_r_i.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrMov_M_R_KIND);
    instr->mov_m_r.size = size;
    instr->mov_m_r.dst = dst;
    instr->mov_m_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrMov_M_I_KIND);
    instr->mov_m_i.size = size;
    instr->mov_m_i.dst = dst;
    instr->mov_m_i.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_flt_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, u32 src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrMovSD_R_R_KIND, [FLOAT_F32] = X64_InstrMovSS_R_R_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->mov_flt_r_r.dst = dst;
    instr->mov_flt_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_flt_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 dst, X64_MemAddr src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrMovSD_R_M_KIND, [FLOAT_F32] = X64_InstrMovSS_R_M_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->mov_flt_r_m.dst = dst;
    instr->mov_flt_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_flt_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, X64_MemAddr dst, u32 src)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrMovSD_M_R_KIND, [FLOAT_F32] = X64_InstrMovSS_M_R_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->mov_flt_m_r.dst = dst;
    instr->mov_flt_m_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt2flt_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind dst_kind, u32 dst, FloatKind src_kind, u32 src)
{
    assert(src_kind != dst_kind);
    X64_InstrKind kind = src_kind == FLOAT_F64 ? X64_InstrCvtSD2SS_R_R_KIND : X64_InstrCvtSS2SD_R_R_KIND;

    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->flt2flt_r_r.dst = dst;
    instr->flt2flt_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt2flt_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind dst_kind, u32 dst, FloatKind src_kind,
                                X64_MemAddr src)
{
    assert(src_kind != dst_kind);
    X64_InstrKind kind = src_kind == FLOAT_F64 ? X64_InstrCvtSD2SS_R_M_KIND : X64_InstrCvtSS2SD_R_M_KIND;

    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->flt2flt_r_m.dst = dst;
    instr->flt2flt_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt2int_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, FloatKind src_kind, u32 src)
{
    X64_InstrKind kind = src_kind == FLOAT_F64 ? X64_InstrCvtSD2SI_R_R_KIND : X64_InstrCvtSS2SI_R_R_KIND;

    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->flt2int_r_r.dst_size = dst_size;
    instr->flt2int_r_r.dst = dst;
    instr->flt2int_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt2int_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, FloatKind src_kind,
                                X64_MemAddr src)
{
    X64_InstrKind kind = src_kind == FLOAT_F64 ? X64_InstrCvtSD2SI_R_M_KIND : X64_InstrCvtSS2SI_R_M_KIND;

    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->flt2int_r_m.dst_size = dst_size;
    instr->flt2int_r_m.dst = dst;
    instr->flt2int_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_int2flt_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind dst_kind, u32 dst, u8 src_size, u32 src)
{
    X64_InstrKind kind = dst_kind == FLOAT_F64 ? X64_InstrCvtSI2SD_R_R_KIND : X64_InstrCvtSI2SS_R_R_KIND;

    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->int2flt_r_r.src_size = src_size;
    instr->int2flt_r_r.dst = dst;
    instr->int2flt_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_int2flt_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind dst_kind, u32 dst, u8 src_size,
                                X64_MemAddr src)
{
    X64_InstrKind kind = dst_kind == FLOAT_F64 ? X64_InstrCvtSI2SD_R_M_KIND : X64_InstrCvtSI2SS_R_M_KIND;

    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->int2flt_r_m.src_size = src_size;
    instr->int2flt_r_m.dst = dst;
    instr->int2flt_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_convert_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, u8 dst_size, u32 dst, u8 src_size,
                                u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->convert_r_r.dst_size = dst_size;
    instr->convert_r_r.src_size = src_size;
    instr->convert_r_r.dst = dst;
    instr->convert_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_convert_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, u8 dst_size, u32 dst, u8 src_size,
                                X64_MemAddr src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->convert_r_m.dst_size = dst_size;
    instr->convert_r_m.src_size = src_size;
    instr->convert_r_m.dst = dst;
    instr->convert_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_sext_ax_to_dx(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrSExtAxToDx_KIND);
    instr->sext_ax_to_dx.size = size;
    instr->sext_ax_to_dx.rdx = rdx;
    instr->sext_ax_to_dx.rax = rax;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_lea(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr mem)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrLEA_KIND);
    instr->lea.dst = dst;
    instr->lea.mem = mem;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_cmp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, u32 op2)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrCmp_R_R_KIND);
    instr->cmp_r_r.size = size;
    instr->cmp_r_r.op1 = op1;
    instr->cmp_r_r.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_cmp_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, Scalar op2)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrCmp_R_I_KIND);
    instr->cmp_r_i.size = size;
    instr->cmp_r_i.op1 = op1;
    instr->cmp_r_i.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_cmp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, X64_MemAddr op2)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrCmp_R_M_KIND);
    instr->cmp_r_m.size = size;
    instr->cmp_r_m.op1 = op1;
    instr->cmp_r_m.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_cmp_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr op1, u32 op2)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrCmp_M_R_KIND);
    instr->cmp_m_r.size = size;
    instr->cmp_m_r.op1 = op1;
    instr->cmp_m_r.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_cmp_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr op1, Scalar op2)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrCmp_M_I_KIND);
    instr->cmp_m_i.size = size;
    instr->cmp_m_i.op1 = op1;
    instr->cmp_m_i.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt_cmp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 op1, u32 op2)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrUComiSD_R_R_KIND, [FLOAT_F32] = X64_InstrUComiSS_R_R_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->cmp_flt_r_r.op1 = op1;
    instr->cmp_flt_r_r.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_flt_cmp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, u32 op1, X64_MemAddr op2)
{
    static const X64_InstrKind fkind_to_instr_kind[FLOAT_KIND_COUNT] =
        {[FLOAT_F64] = X64_InstrUComiSD_R_M_KIND, [FLOAT_F32] = X64_InstrUComiSS_R_M_KIND};
    X64_Instr* instr = X64_new_instr(builder->arena, fkind_to_instr_kind[fkind]);
    instr->cmp_flt_r_m.op1 = op1;
    instr->cmp_flt_r_m.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_jmp(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_BBlock* target)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrJmp_KIND);
    instr->jmp.from = xbblock;
    instr->jmp.target = target;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_jmpcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, X64_BBlock* true_bb, X64_BBlock* false_bb)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrJmpCC_KIND);
    instr->jmpcc.cond = cond;
    instr->jmpcc.from = xbblock;
    instr->jmpcc.true_bb = true_bb;
    instr->jmpcc.false_bb = false_bb;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_setcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, u32 dst)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrSetCC_KIND);
    instr->setcc.cond = cond;
    instr->setcc.dst = dst;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_rep_movsb(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rdi, u32 rsi, u32 rcx)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrRepMovsb_KIND);
    instr->rep_movsb.rdi = rdi;
    instr->rep_movsb.rsi = rsi;
    instr->rep_movsb.rcx = rcx;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_rep_stosb(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rdi, u32 rax, u32 rcx)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrRepStosb_KIND);
    instr->rep_stosb.rdi = rdi;
    instr->rep_stosb.rax = rax;
    instr->rep_stosb.rcx = rcx;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_syscall(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rax, u8 num_args, u32* args, u32 rcx, u32 r11)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrSyscall_KIND);
    instr->syscall.rax = rax;
    instr->syscall.rcx = rcx;
    instr->syscall.r11 = r11;
    instr->syscall.num_args = num_args;
    instr->syscall.args = mem_dup_array(builder->arena, u32, args, num_args);

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_ret(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rax, u32 rdx)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrRet_KIND);
    instr->ret.rax = rax;
    instr->ret.rdx = rdx;

    X64_add_lir_instr(builder, xbblock, instr);
}

X64_Instr* X64_emit_instr_call(X64_LIRBuilder* builder, X64_BBlock* xbblock, Symbol* sym, X64_CallValue dst, u32 num_args,
                               X64_InstrCallArg* args, X64_StackArgsInfo stack_info)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrCall_KIND);
    instr->call.sym = sym;
    instr->call.dst = dst;
    instr->call.num_args = num_args;
    instr->call.args = args;
    instr->call.stack_info = stack_info;

    X64_add_lir_instr(builder, xbblock, instr);

    return instr;
}

X64_Instr* X64_emit_instr_call_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, Type* proc_type, u32 proc_loc, X64_CallValue dst,
                                 u32 num_args, X64_InstrCallArg* args, X64_StackArgsInfo stack_info)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_InstrCall_R_KIND);
    instr->call_r.proc_type = proc_type;
    instr->call_r.proc_loc = proc_loc;
    instr->call_r.dst = dst;
    instr->call_r.num_args = num_args;
    instr->call_r.args = args;
    instr->call_r.stack_info = stack_info;

    X64_add_lir_instr(builder, xbblock, instr);

    return instr;
}
