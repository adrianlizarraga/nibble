#include "x64_gen/xir.h"

static void XIR_bblock_add_instr(XIR_BBlock* bblock, XIR_Instr* instr)
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

static void XIR_add_lir_instr(XIR_Builder* builder, XIR_BBlock* xbblock, XIR_Instr* instr)
{
    instr->ino = builder->num_instrs;
    builder->num_instrs += 1;

    XIR_bblock_add_instr(xbblock, instr);
}

#define XIR_new_instr(a, k) (k*)XIR_new_instr_((a), k##_KIND, sizeof(k), alignof(k))
static XIR_Instr* XIR_new_instr_(Allocator* arena, XIR_InstrKind kind, size_t size, size_t align)
{
    XIR_Instr* instr = mem_allocate(arena, size, align, true);
    instr->kind = kind;

    return instr;
}

void XIR_emit_instr_add_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrAdd_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrAdd_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_sub_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrSub_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrSub_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_imul_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrIMul_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrIMul_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_and_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrAnd_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrAnd_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_or_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrOr_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrOr_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_xor_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrXor_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrXor_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_add_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrAdd_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrAdd_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_sub_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrSub_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrSub_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_imul_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrIMul_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrIMul_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_and_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrAnd_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrAnd_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_or_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrOr_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrOr_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_xor_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrXor_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrXor_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_add_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrAdd_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrAdd_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_sub_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrSub_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrSub_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_imul_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrIMul_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrIMul_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_and_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrAnd_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrAnd_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_or_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrOr_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrOr_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_xor_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrXor_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrXor_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_addss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrAddSS_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrAddSS_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_addss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrAddSS_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrAddSS_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_addsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrAddSD_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrAddSD_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_addsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrAddSD_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrAddSD_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_subss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrSubSS_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrSubSS_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_subss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrSubSS_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrSubSS_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_subsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrSubSD_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrSubSD_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_subsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrSubSD_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrSubSD_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mulss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrMulSS_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrMulSS_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mulss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrMulSS_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrMulSS_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mulsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrMulSD_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrMulSD_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mulsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrMulSD_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrMulSD_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_divss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrDivSS_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrDivSS_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_divss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrDivSS_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrDivSS_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_divsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrDivSD_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrDivSD_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_divsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrDivSD_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrDivSD_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_sar_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrSar_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrSar_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_sar_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrSar_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrSar_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_shl_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrShl_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrShl_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_shl_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrShl_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrShl_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_div_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src)
{
    XIR_InstrDiv_R* instr = XIR_new_instr(builder->arena, XIR_InstrDiv_R);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_div_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, XIR_MemAddr src)
{
    XIR_InstrDiv_M* instr = XIR_new_instr(builder->arena, XIR_InstrDiv_M);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_idiv_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src)
{
    XIR_InstrIDiv_R* instr = XIR_new_instr(builder->arena, XIR_InstrIDiv_R);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_idiv_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, XIR_MemAddr src)
{
    XIR_InstrIDiv_M* instr = XIR_new_instr(builder->arena, XIR_InstrIDiv_M);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mul_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, u32 src)
{
    XIR_InstrMul_R* instr = XIR_new_instr(builder->arena, XIR_InstrMul_R);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mul_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax, XIR_MemAddr src)
{
    XIR_InstrMul_M* instr = XIR_new_instr(builder->arena, XIR_InstrMul_M);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_neg(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst)
{
    XIR_InstrNeg* instr = XIR_new_instr(builder->arena, XIR_InstrNeg);
    instr->size = size;
    instr->dst = dst;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_not(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst)
{
    XIR_InstrNot* instr = XIR_new_instr(builder->arena, XIR_InstrNot);
    instr->size = size;
    instr->dst = dst;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movzx_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src)
{
    XIR_InstrMovZX_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrMovZX_R_R);
    instr->dst_size = dst_size;
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movzx_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, XIR_MemAddr src)
{
    XIR_InstrMovZX_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrMovZX_R_M);
    instr->dst_size = dst_size;
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movsx_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, u32 src)
{
    XIR_InstrMovSX_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrMovSX_R_R);
    instr->dst_size = dst_size;
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movsx_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u8 src_size, XIR_MemAddr src)
{
    XIR_InstrMovSX_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrMovSX_R_M);
    instr->dst_size = dst_size;
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtss2sd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrCvtSS2SD_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSS2SD_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtss2sd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrCvtSS2SD_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSS2SD_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtsd2ss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrCvtSD2SS_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSD2SS_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtsd2ss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrCvtSD2SS_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSD2SS_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtss2si_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u32 src)
{
    XIR_InstrCvtSS2SI_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSS2SI_R_R);
    instr->dst_size = dst_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtss2si_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrCvtSS2SI_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSS2SI_R_M);
    instr->dst_size = dst_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtsd2si_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, u32 src)
{
    XIR_InstrCvtSD2SI_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSD2SI_R_R);
    instr->dst_size = dst_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtsd2si_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 dst_size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrCvtSD2SI_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSD2SI_R_M);
    instr->dst_size = dst_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtsi2ss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, u32 src)
{
    XIR_InstrCvtSI2SS_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSI2SS_R_R);
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtsi2ss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, XIR_MemAddr src)
{
    XIR_InstrCvtSI2SS_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSI2SS_R_M);
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtsi2sd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, u32 src)
{
    XIR_InstrCvtSI2SD_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSI2SD_R_R);
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cvtsi2sd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u8 src_size, XIR_MemAddr src)
{
    XIR_InstrCvtSI2SD_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrCvtSI2SD_R_M);
    instr->src_size = src_size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mov_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, u32 src)
{
    XIR_InstrMov_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrMov_R_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mov_r_rh(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrMov_R_RH* instr = XIR_new_instr(builder->arena, XIR_InstrMov_R_RH);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mov_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, XIR_MemAddr src)
{
    XIR_InstrMov_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrMov_R_M);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mov_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 dst, Scalar src)
{
    XIR_InstrMov_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrMov_R_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mov_m_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, XIR_MemAddr dst, u32 src)
{
    XIR_InstrMov_M_R* instr = XIR_new_instr(builder->arena, XIR_InstrMov_M_R);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_mov_m_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, XIR_MemAddr dst, Scalar src)
{
    XIR_InstrMov_M_I* instr = XIR_new_instr(builder->arena, XIR_InstrMov_M_I);
    instr->size = size;
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrMovSS_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrMovSS_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrMovSS_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrMovSS_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movss_m_r(XIR_Builder* builder, XIR_BBlock* xbblock, XIR_MemAddr dst, u32 src)
{
    XIR_InstrMovSS_M_R* instr = XIR_new_instr(builder->arena, XIR_InstrMovSS_M_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movsd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, u32 src)
{
    XIR_InstrMovSD_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrMovSD_R_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movsd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr src)
{
    XIR_InstrMovSD_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrMovSD_R_M);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_movsd_m_r(XIR_Builder* builder, XIR_BBlock* xbblock, XIR_MemAddr dst, u32 src)
{
    XIR_InstrMovSD_M_R* instr = XIR_new_instr(builder->arena, XIR_InstrMovSD_M_R);
    instr->dst = dst;
    instr->src = src;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_sext_ax_to_dx(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 rdx, u32 rax)
{
    XIR_InstrSExtAxToDx* instr = XIR_new_instr(builder->arena, XIR_InstrSExtAxToDx);
    instr->size = size;
    instr->rdx = rdx;
    instr->rax = rax;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_lea(XIR_Builder* builder, XIR_BBlock* xbblock, u32 dst, XIR_MemAddr mem)
{
    XIR_InstrLEA* instr = XIR_new_instr(builder->arena, XIR_InstrLEA);
    instr->dst = dst;
    instr->mem = mem;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cmp_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 op1, u32 op2)
{
    XIR_InstrCmp_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrCmp_R_R);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cmp_r_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 op1, Scalar op2)
{
    XIR_InstrCmp_R_I* instr = XIR_new_instr(builder->arena, XIR_InstrCmp_R_I);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cmp_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, u32 op1, XIR_MemAddr op2)
{
    XIR_InstrCmp_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrCmp_R_M);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cmp_m_r(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, XIR_MemAddr op1, u32 op2)
{
    XIR_InstrCmp_M_R* instr = XIR_new_instr(builder->arena, XIR_InstrCmp_M_R);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_cmp_m_i(XIR_Builder* builder, XIR_BBlock* xbblock, u8 size, XIR_MemAddr op1, Scalar op2)
{
    XIR_InstrCmp_M_I* instr = XIR_new_instr(builder->arena, XIR_InstrCmp_M_I);
    instr->size = size;
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_ucomiss_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, u32 op2)
{
    XIR_InstrUComiSS_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrUComiSS_R_R);
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_ucomiss_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, XIR_MemAddr op2)
{
    XIR_InstrUComiSS_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrUComiSS_R_M);
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_ucomisd_r_r(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, u32 op2)
{
    XIR_InstrUComiSD_R_R* instr = XIR_new_instr(builder->arena, XIR_InstrUComiSD_R_R);
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_ucomisd_r_m(XIR_Builder* builder, XIR_BBlock* xbblock, u32 op1, XIR_MemAddr op2)
{
    XIR_InstrUComiSD_R_M* instr = XIR_new_instr(builder->arena, XIR_InstrUComiSD_R_M);
    instr->op1 = op1;
    instr->op2 = op2;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_jmp(XIR_Builder* builder, XIR_BBlock* xbblock, XIR_BBlock* target)
{
    XIR_InstrJmp* instr = XIR_new_instr(builder->arena, XIR_InstrJmp);
    instr->from = xbblock;
    instr->target = target;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_jmpcc(XIR_Builder* builder, XIR_BBlock* xbblock, ConditionKind cond, XIR_BBlock* true_bb, XIR_BBlock* false_bb)
{
    XIR_InstrJmpCC* instr = XIR_new_instr(builder->arena, XIR_InstrJmpCC);
    instr->cond = cond;
    instr->from = xbblock;
    instr->true_bb = true_bb;
    instr->false_bb = false_bb;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_setcc(XIR_Builder* builder, XIR_BBlock* xbblock, ConditionKind cond, u32 dst)
{
    XIR_InstrSetCC* instr = XIR_new_instr(builder->arena, XIR_InstrSetCC);
    instr->cond = cond;
    instr->dst = dst;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_rep_movsb(XIR_Builder* builder, XIR_BBlock* xbblock, u32 rdi, u32 rsi, u32 rcx)
{
    XIR_InstrRepMovsb* instr = XIR_new_instr(builder->arena, XIR_InstrRepMovsb);
    instr->rdi = rdi;
    instr->rsi = rsi;
    instr->rcx = rcx;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_rep_stosb(XIR_Builder* builder, XIR_BBlock* xbblock, u32 rdi, u32 rax, u32 rcx)
{
    XIR_InstrRepStosb* instr = XIR_new_instr(builder->arena, XIR_InstrRepStosb);
    instr->rdi = rdi;
    instr->rax = rax;
    instr->rcx = rcx;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_syscall(XIR_Builder* builder, XIR_BBlock* xbblock, u32 rax, u8 num_args, u32* args, u32 rcx, u32 r11)
{
    XIR_InstrSyscall* instr = XIR_new_instr(builder->arena, XIR_InstrSyscall);
    instr->rax = rax;
    instr->rcx = rcx;
    instr->r11 = r11;
    instr->num_args = num_args;
    instr->args = mem_dup_array(builder->arena, u32, args, num_args);

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

void XIR_emit_instr_ret(XIR_Builder* builder, XIR_BBlock* xbblock, u32 rax, u32 rdx)
{
    XIR_InstrRet* instr = XIR_new_instr(builder->arena, XIR_InstrRet);
    instr->rax = rax;
    instr->rdx = rdx;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);
}

XIR_Instr* XIR_emit_instr_call(XIR_Builder* builder, XIR_BBlock* xbblock, Symbol* sym, XIR_CallValue dst, u32 num_args,
                               XIR_InstrCallArg* args, X64_StackArgsInfo stack_info)
{
    XIR_InstrCall* instr = XIR_new_instr(builder->arena, XIR_InstrCall);
    instr->sym = sym;
    instr->dst = dst;
    instr->num_args = num_args;
    instr->args = args;
    instr->stack_info = stack_info;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);

    return (XIR_Instr*)instr;
}

XIR_Instr* XIR_emit_instr_call_r(XIR_Builder* builder, XIR_BBlock* xbblock, Type* proc_type, u32 proc_loc, XIR_CallValue dst,
                                 u32 num_args, XIR_InstrCallArg* args, X64_StackArgsInfo stack_info)
{
    XIR_InstrCall_R* instr = XIR_new_instr(builder->arena, XIR_InstrCall_R);
    instr->proc_type = proc_type;
    instr->proc_loc = proc_loc;
    instr->dst = dst;
    instr->num_args = num_args;
    instr->args = args;
    instr->stack_info = stack_info;

    XIR_add_lir_instr(builder, xbblock, (XIR_Instr*)instr);

    return (XIR_Instr*)instr;
}
