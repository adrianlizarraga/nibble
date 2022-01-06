#include "x64_gen/lir.h"

void X64_add_lir_instr(X64_LIRBuilder* builder, X64_Instr* instr)
{
    if (builder->next_instr_is_jmp_target) {
        instr->is_jmp_target = true;
        builder->next_instr_is_jmp_target = false;
    }

    array_push(builder->instrs, instr);
}

X64_Instr* X64_new_instr(Allocator* arena, X64_InstrKind kind)
{
    X64_Instr* instr = alloc_type(arena, X64_Instr, true);
    instr->kind = kind;

    return instr;
}

void X64_emit_instr_binary_r_r(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->binary_r_r.size = size;
    instr->binary_r_r.dst = dst;
    instr->binary_r_r.src = src;

    return instr;
}

void X64_emit_instr_binary_r_i(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->binary_r_i.size = size;
    instr->binary_r_i.dst = dst;
    instr->binary_r_i.src = src;

    return instr;
}

void X64_emit_instr_shift_r_r(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->binary_r_r.size = size;
    instr->binary_r_r.dst = dst;
    instr->binary_r_r.src = src;

    return instr;
}

void X64_emit_instr_shift_r_i(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->binary_r_i.size = size;
    instr->binary_r_i.dst = dst;
    instr->binary_r_i.src = src;

    return instr;
}

void X64_emit_instr_div_r(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->div_r.size = size;
    instr->div_r.src = src;

    return instr;
}

void X64_emit_instr_unary(X64_LIRBuilder* builder, X64_InstrKind kind, size_t size, u32 dst)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->unary.size = size;
    instr->unary.dst = dst;

    return instr;
}

void X64_emit_instr_mov_r_r(X64_LIRBuilder* builder, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_MOV_R_R);
    instr->mov_r_r.size = size;
    instr->mov_r_r.dst = dst;
    instr->mov_r_r.src = src;

    return instr;
}
