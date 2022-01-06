#include "x64_gen/lir.h"

X64_new_instr(Allocator* arena, X64_InstrKind kind)
{
    X64_Instr* instr = alloc_type(arena, X64_Instr, true);
    instr->kind = kind;

    return instr;
}

X64_Instr* X64_new_instr_binary_r_r(Allocator* arena, X64_InstrKind kind, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(arena, kind);
    instr->binary_r_r.size = size;
    instr->binary_r_r.dst = dst;
    instr->binary_r_r.src = src;

    return instr;
}

X64_Instr* X64_new_instr_binary_r_i(Allocator* arena, X64_InstrKind kind, size_t size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(arena, kind);
    instr->binary_r_i.size = size;
    instr->binary_r_i.dst = dst;
    instr->binary_r_i.src = src;

    return instr;
}

X64_Instr* X64_new_instr_shift_r_r(Allocator* arena, X64_InstrKind kind, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(arena, kind);
    instr->binary_r_r.size = size;
    instr->binary_r_r.dst = dst;
    instr->binary_r_r.src = src;

    return instr;
}

X64_Instr* X64_new_instr_shift_r_i(Allocator* arena, X64_InstrKind kind, size_t size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(arena, kind);
    instr->binary_r_i.size = size;
    instr->binary_r_i.dst = dst;
    instr->binary_r_i.src = src;

    return instr;
}

X64_Instr* X64_new_instr_div_r(Allocator* arena, X64_InstrKind kind, size_t size, u32 src)
{
    X64_Instr* instr = X64_new_instr(arena, kind);
    instr->div_r.size = size;
    instr->div_r.src = src;

    return instr;
}

X64_Instr* X64_new_instr_unary(Allocator* arena, X64_InstrKind kind, size_t size, u32 dst)
{
    X64_Instr* instr = X64_new_instr(arena, kind);
    instr->unary.size = size;
    instr->unary.dst = dst;

    return instr;
}

X64_Instr* X64_new_instr_mov_r_r(Allocator* arena, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(arena, X64_INSTR_MOV_R_R);
    instr->mov_r_r.size = size;
    instr->mov_r_r.dst = dst;
    instr->mov_r_r.src = src;

    return instr;
}
