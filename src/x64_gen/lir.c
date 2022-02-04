#include "x64_gen/lir.h"

static void X64_bblock_add_instr(X64_BBlock* bblock, X64_Instr* instr)
{
    if (!bblock->first) {
        bblock->first = instr;
        instr->is_leader = true;
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

static X64_Instr* X64_new_instr(Allocator* arena, X64_InstrKind kind)
{
    X64_Instr* instr = alloc_type(arena, X64_Instr, true);
    instr->kind = kind;

    return instr;
}

void X64_emit_instr_binary_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->binary_r_r.size = size;
    instr->binary_r_r.dst = dst;
    instr->binary_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_binary_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->binary_r_i.size = size;
    instr->binary_r_i.dst = dst;
    instr->binary_r_i.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_shift_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->shift_r_r.size = size;
    instr->shift_r_r.dst = dst;
    instr->shift_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_shift_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->shift_r_i.size = size;
    instr->shift_r_i.dst = dst;
    instr->shift_r_i.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_div(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 rdx, u32 rax, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->div.size = size;
    instr->div.rdx = rdx;
    instr->div.rax = rax;
    instr->div.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_unary(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t size, u32 dst)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->unary.size = size;
    instr->unary.dst = dst;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_MOV_R_R);
    instr->mov_r_r.size = size;
    instr->mov_r_r.dst = dst;
    instr->mov_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_r_m(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 dst, X64_MemAddr src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_MOV_R_M);
    instr->mov_r_m.size = size;
    instr->mov_r_m.dst = dst;
    instr->mov_r_m.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_MOV_R_I);
    instr->mov_r_i.size = size;
    instr->mov_r_i.dst = dst;
    instr->mov_r_i.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_m_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, X64_MemAddr dst, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_MOV_M_R);
    instr->mov_m_r.size = size;
    instr->mov_m_r.dst = dst;
    instr->mov_m_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_mov_m_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, X64_MemAddr dst, Scalar src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_MOV_M_I);
    instr->mov_m_i.size = size;
    instr->mov_m_i.dst = dst;
    instr->mov_m_i.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_convert_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrKind kind, size_t dst_size, u32 dst,
                                size_t src_size, u32 src)
{
    X64_Instr* instr = X64_new_instr(builder->arena, kind);
    instr->convert_r_r.dst_size = dst_size;
    instr->convert_r_r.src_size = src_size;
    instr->convert_r_r.dst = dst;
    instr->convert_r_r.src = src;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_sext_ax_to_dx(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 rdx, u32 rax)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_SEXT_AX_TO_DX);
    instr->sext_ax_to_dx.size = size;
    instr->sext_ax_to_dx.rdx = rdx;
    instr->sext_ax_to_dx.rax = rax;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_lea(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 dst, X64_MemAddr mem)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_LEA);
    instr->lea.dst = dst;
    instr->lea.mem = mem;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_cmp_r_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 op1, u32 op2)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_CMP_R_R);
    instr->cmp_r_r.size = size;
    instr->cmp_r_r.op1 = op1;
    instr->cmp_r_r.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_cmp_r_i(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 op1, Scalar op2)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_CMP_R_I);
    instr->cmp_r_i.size = size;
    instr->cmp_r_i.op1 = op1;
    instr->cmp_r_i.op2 = op2;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_jmp(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_BBlock* target)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_JMP);
    instr->jmp.from = xbblock;
    instr->jmp.target = target;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_jmpcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, X64_BBlock* true_bb, X64_BBlock* false_bb)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_JMPCC);
    instr->jmpcc.cond = cond;
    instr->jmpcc.from = xbblock;
    instr->jmpcc.true_bb = true_bb;
    instr->jmpcc.false_bb = false_bb;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_setcc(X64_LIRBuilder* builder, X64_BBlock* xbblock, ConditionKind cond, u32 dst)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_SETCC);
    instr->setcc.cond = cond;
    instr->setcc.dst = dst;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_rep_movsb(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rdi, u32 rsi, u32 rcx)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_REP_MOVSB);
    instr->rep_movsb.rdi = rdi;
    instr->rep_movsb.rsi = rsi;
    instr->rep_movsb.rcx = rcx;

    X64_add_lir_instr(builder, xbblock, instr);
}

void X64_emit_instr_ret(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 rax, u32 rdx)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_RET);
    instr->ret.rax = rax;
    instr->ret.rdx = rdx;

    X64_add_lir_instr(builder, xbblock, instr);
}

X64_Instr* X64_emit_instr_call(X64_LIRBuilder* builder, X64_BBlock* xbblock, Symbol* sym, u32 dst, u32 num_args, X64_InstrCallArg* args,
                               X64_StackArgsInfo stack_info)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_CALL);
    instr->call.sym = sym;
    instr->call.dst = dst;
    instr->call.num_args = num_args;
    instr->call.args = args;
    instr->call.stack_info = stack_info;

    X64_add_lir_instr(builder, xbblock, instr);

    return instr;
}

X64_Instr* X64_emit_instr_call_r(X64_LIRBuilder* builder, X64_BBlock* xbblock, Type* proc_type, u32 proc_loc, u32 dst, u32 num_args,
                                 X64_InstrCallArg* args, X64_StackArgsInfo stack_info)
{
    X64_Instr* instr = X64_new_instr(builder->arena, X64_INSTR_CALL_R);
    instr->call_r.proc_type = proc_type;
    instr->call_r.proc_loc = proc_loc;
    instr->call_r.dst = dst;
    instr->call_r.num_args = num_args;
    instr->call_r.args = args;
    instr->call_r.stack_info = stack_info;

    X64_add_lir_instr(builder, xbblock, instr);

    return instr;
}

