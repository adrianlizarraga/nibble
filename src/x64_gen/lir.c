#include "x64_gen/lir.h"

static void X64_bblock_add_instr(X64_BBlock* bblock, X64_Instr* instr)
{
    if (!bblock->first) {
        bblock->first = instr;
    }
    else {
        bblock->last->next = instr;
    }

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

void X64_emit_instr_addss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrAddSS_R_M* instr = X64_new_instr(builder->arena, X64_InstrAddSS_R_M);
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

void X64_emit_instr_addsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrAddSD_R_M* instr = X64_new_instr(builder->arena, X64_InstrAddSD_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_subss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrSubSS_R_R* instr = X64_new_instr(builder->arena, X64_InstrSubSS_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_subss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrSubSS_R_M* instr = X64_new_instr(builder->arena, X64_InstrSubSS_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_subsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrSubSD_R_R* instr = X64_new_instr(builder->arena, X64_InstrSubSD_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_subsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrSubSD_R_M* instr = X64_new_instr(builder->arena, X64_InstrSubSD_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mulss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrMulSS_R_R* instr = X64_new_instr(builder->arena, X64_InstrMulSS_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mulss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrMulSS_R_M* instr = X64_new_instr(builder->arena, X64_InstrMulSS_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mulsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrMulSD_R_R* instr = X64_new_instr(builder->arena, X64_InstrMulSD_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mulsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrMulSD_R_M* instr = X64_new_instr(builder->arena, X64_InstrMulSD_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_divss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrDivSS_R_R* instr = X64_new_instr(builder->arena, X64_InstrDivSS_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_divss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrDivSS_R_M* instr = X64_new_instr(builder->arena, X64_InstrDivSS_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_divsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrDivSD_R_R* instr = X64_new_instr(builder->arena, X64_InstrDivSD_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_divsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrDivSD_R_M* instr = X64_new_instr(builder->arena, X64_InstrDivSD_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_sar_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrSar_R_R* instr = X64_new_instr(builder->arena, X64_InstrSar_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_sar_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrSar_R_I* instr = X64_new_instr(builder->arena, X64_InstrSar_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_shl_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrShl_R_R* instr = X64_new_instr(builder->arena, X64_InstrShl_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_shl_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrShl_R_I* instr = X64_new_instr(builder->arena, X64_InstrShl_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_div_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src)
{
    X64_InstrDiv_R* instr = X64_new_instr(builder->arena, X64_InstrDiv_R);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_div_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, X64_MemAddr src)
{
    X64_InstrDiv_M* instr = X64_new_instr(builder->arena, X64_InstrDiv_M);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_idiv_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src)
{
    X64_InstrIDiv_R* instr = X64_new_instr(builder->arena, X64_InstrIDiv_R);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_idiv_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax, X64_MemAddr src)
{
    X64_InstrIDiv_M* instr = X64_new_instr(builder->arena, X64_InstrIDiv_M);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_neg(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst)
{
    X64_InstrNeg* instr = X64_new_instr(builder->arena, X64_InstrNeg);
    instr->size = size;
    instr->dst = dst;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_not(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst)
{
    X64_InstrNot* instr = X64_new_instr(builder->arena, X64_InstrNot);
    instr->size = size;
    instr->dst = dst;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movzx_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src)
{
    X64_InstrMovZX_R_R* instr = X64_new_instr(builder->arena, X64_InstrMovZX_R_R);
    instr->dst_size = dst_size;
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movzx_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, X64_MemAddr src)
{
    X64_InstrMovZX_R_M* instr = X64_new_instr(builder->arena, X64_InstrMovZX_R_M);
    instr->dst_size = dst_size;
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movsx_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src)
{
    X64_InstrMovSX_R_R* instr = X64_new_instr(builder->arena, X64_InstrMovSX_R_R);
    instr->dst_size = dst_size;
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movsx_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, X64_MemAddr src)
{
    X64_InstrMovSX_R_M* instr = X64_new_instr(builder->arena, X64_InstrMovSX_R_M);
    instr->dst_size = dst_size;
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtss2sd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrCvtSS2SD_R_R* instr = X64_new_instr(builder->arena, X64_InstrCvtSS2SD_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtss2sd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrCvtSS2SD_R_M* instr = X64_new_instr(builder->arena, X64_InstrCvtSS2SD_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtsd2ss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrCvtSD2SS_R_R* instr = X64_new_instr(builder->arena, X64_InstrCvtSD2SS_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtsd2ss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrCvtSD2SS_R_M* instr = X64_new_instr(builder->arena, X64_InstrCvtSD2SS_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtss2si_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u32 src)
{
    X64_InstrCvtSS2SI_R_R* instr = X64_new_instr(builder->arena, X64_InstrCvtSS2SI_R_R);
    instr->dst_size = dst_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtss2si_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, X64_MemAddr src)
{
    X64_InstrCvtSS2SI_R_M* instr = X64_new_instr(builder->arena, X64_InstrCvtSS2SI_R_M);
    instr->dst_size = dst_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtsd2si_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, u32 src)
{
    X64_InstrCvtSD2SI_R_R* instr = X64_new_instr(builder->arena, X64_InstrCvtSD2SI_R_R);
    instr->dst_size = dst_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtsd2si_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 dst_size, u32 dst, X64_MemAddr src)
{
    X64_InstrCvtSD2SI_R_M* instr = X64_new_instr(builder->arena, X64_InstrCvtSD2SI_R_M);
    instr->dst_size = dst_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtsi2ss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, u32 src)
{
    X64_InstrCvtSI2SS_R_R* instr = X64_new_instr(builder->arena, X64_InstrCvtSI2SS_R_R);
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtsi2ss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, X64_MemAddr src)
{
    X64_InstrCvtSI2SS_R_M* instr = X64_new_instr(builder->arena, X64_InstrCvtSI2SS_R_M);
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtsi2sd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, u32 src)
{
    X64_InstrCvtSI2SD_R_R* instr = X64_new_instr(builder->arena, X64_InstrCvtSI2SD_R_R);
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cvtsi2sd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u8 src_size, X64_MemAddr src)
{
    X64_InstrCvtSI2SD_R_M* instr = X64_new_instr(builder->arena, X64_InstrCvtSI2SD_R_M);
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mov_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    X64_InstrMov_R_R* instr = X64_new_instr(builder->arena, X64_InstrMov_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mov_r_rh(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrMov_R_RH* instr = X64_new_instr(builder->arena, X64_InstrMov_R_RH);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mov_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, X64_MemAddr src)
{
    X64_InstrMov_R_M* instr = X64_new_instr(builder->arena, X64_InstrMov_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mov_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    X64_InstrMov_R_I* instr = X64_new_instr(builder->arena, X64_InstrMov_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mov_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr dst, u32 src)
{
    X64_InstrMov_M_R* instr = X64_new_instr(builder->arena, X64_InstrMov_M_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_mov_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr dst, Scalar src)
{
    X64_InstrMov_M_I* instr = X64_new_instr(builder->arena, X64_InstrMov_M_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrMovSS_R_R* instr = X64_new_instr(builder->arena, X64_InstrMovSS_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrMovSS_R_M* instr = X64_new_instr(builder->arena, X64_InstrMovSS_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movss_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_MemAddr dst, u32 src)
{
    X64_InstrMovSS_M_R* instr = X64_new_instr(builder->arena, X64_InstrMovSS_M_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movsd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, u32 src)
{
    X64_InstrMovSD_R_R* instr = X64_new_instr(builder->arena, X64_InstrMovSD_R_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movsd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr src)
{
    X64_InstrMovSD_R_M* instr = X64_new_instr(builder->arena, X64_InstrMovSD_R_M);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_movsd_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_MemAddr dst, u32 src)
{
    X64_InstrMovSD_M_R* instr = X64_new_instr(builder->arena, X64_InstrMovSD_M_R);
    instr->dst = dst;
    instr->src = src;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_sext_ax_to_dx(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 rdx, u32 rax)
{
    X64_InstrSExtAxToDx* instr = X64_new_instr(builder->arena, X64_InstrSExtAxToDx);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_lea(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr mem)
{
    X64_InstrLEA* instr = X64_new_instr(builder->arena, X64_InstrLEA);
    instr->dst = dst;
    instr->mem = mem;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cmp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, u32 op2)
{
    X64_InstrCmp_R_R* instr = X64_new_instr(builder->arena, X64_InstrCmp_R_R);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cmp_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, Scalar op2)
{
    X64_InstrCmp_R_I* instr = X64_new_instr(builder->arena, X64_InstrCmp_R_I);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cmp_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, u32 op1, X64_MemAddr op2)
{
    X64_InstrCmp_R_M* instr = X64_new_instr(builder->arena, X64_InstrCmp_R_M);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cmp_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr op1, u32 op2)
{
    X64_InstrCmp_M_R* instr = X64_new_instr(builder->arena, X64_InstrCmp_M_R);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_cmp_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, u8 size, X64_MemAddr op1, Scalar op2)
{
    X64_InstrCmp_M_I* instr = X64_new_instr(builder->arena, X64_InstrCmp_M_I);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_ucomiss_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, u32 op2)
{
    X64_InstrUComiSS_R_R* instr = X64_new_instr(builder->arena, X64_InstrUComiSS_R_R);
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_ucomiss_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, X64_MemAddr op2)
{
    X64_InstrUComiSS_R_M* instr = X64_new_instr(builder->arena, X64_InstrUComiSS_R_M);
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_ucomisd_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, u32 op2)
{
    X64_InstrUComiSD_R_R* instr = X64_new_instr(builder->arena, X64_InstrUComiSD_R_R);
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_ucomisd_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 op1, X64_MemAddr op2)
{
    X64_InstrUComiSD_R_M* instr = X64_new_instr(builder->arena, X64_InstrUComiSD_R_M);
    instr->op1 = op1;
    instr->op2 = op2;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_jmp(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_BBlock* target)
{
    X64_InstrJmp* instr = X64_new_instr(builder->arena, X64_InstrJmp);
    instr->from = xbblock;
    instr->target = target;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_jmpcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, X64_BBlock* true_bb, X64_BBlock* false_bb)
{
    X64_InstrJmpCC* instr = X64_new_instr(builder->arena, X64_InstrJmpCC);
    instr->cond = cond;
    instr->from = xbblock;
    instr->true_bb = true_bb;
    instr->false_bb = false_bb;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_setcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, u32 dst)
{
    X64_InstrSetCC* instr = X64_new_instr(builder->arena, X64_InstrSetCC);
    instr->cond = cond;
    instr->dst = dst;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_rep_movsb(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rdi, u32 rsi, u32 rcx)
{
    X64_InstrRepMovsb* instr = X64_new_instr(builder->arena, X64_InstrRepMovsb);
    instr->rdi = rdi;
    instr->rsi = rsi;
    instr->rcx = rcx;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_rep_stosb(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rdi, u32 rax, u32 rcx)
{
    X64_InstrRepStosb* instr = X64_new_instr(builder->arena, X64_InstrRepStosb);
    instr->rdi = rdi;
    instr->rax = rax;
    instr->rcx = rcx;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_syscall(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rax, u8 num_args, u32* args, u32 rcx, u32 r11)
{
    X64_InstrSyscall* instr = X64_new_instr(builder->arena, X64_InstrSyscall);
    instr->rax = rax;
    instr->rcx = rcx;
    instr->r11 = r11;
    instr->num_args = num_args;
    instr->args = mem_dup_array(builder->arena, u32, args, num_args);

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

void X64_emit_instr_ret(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rax, u32 rdx)
{
    X64_InstrRet* instr = X64_new_instr(builder->arena, X64_InstrRet);
    instr->rax = rax;
    instr->rdx = rdx;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);
}

X64_Instr* X64_emit_instr_call(X64_LIRBuilder* builder, X64_BBlock* xbblock, Symbol* sym, X64_CallValue dst, u32 num_args,
                               X64_InstrCallArg* args, X64_StackArgsInfo stack_info)
{
    X64_InstrCall* instr = X64_new_instr(builder->arena, X64_InstrCall);
    instr->sym = sym;
    instr->dst = dst;
    instr->num_args = num_args;
    instr->args = args;
    instr->stack_info = stack_info;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);

    return (X64_Instr*)instr;
}

X64_Instr* X64_emit_instr_call_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, Type* proc_type, u32 proc_loc, X64_CallValue dst,
                                 u32 num_args, X64_InstrCallArg* args, X64_StackArgsInfo stack_info)
{
    X64_InstrCall_R* instr = X64_new_instr(builder->arena, X64_InstrCall_R);
    instr->proc_type = proc_type;
    instr->proc_loc = proc_loc;
    instr->dst = dst;
    instr->num_args = num_args;
    instr->args = args;
    instr->stack_info = stack_info;

    X64_add_lir_instr(builder, xbblock, (X64_Instr*)instr);

    return (X64_Instr*)instr;
}
