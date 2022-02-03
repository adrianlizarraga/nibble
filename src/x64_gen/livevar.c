#include "x64_gen/lir.h"

static void X64_touch_lreg(X64_LIRBuilder* builder, u32 lreg, long ino)
{
    assert(lreg < builder->num_regs);

    X64_LRegRange* range = &builder->lreg_ranges[X64_find_alias_reg(builder, lreg)];

    if (range->start == -1) {
        range->start = ino;
        range->end = ino;
    }
    else {
        assert(range->end == -1 || range->end < ino);
        range->end = ino;
    }
}

static void X64_touch_mem_lregs(X64_LIRBuilder* builder, X64_MemAddr* addr, long ino)
{
    if (addr->kind == X64_ADDR_LOCAL) {
        if (addr->local.base_reg != X64_LIR_REG_COUNT) {
            X64_touch_lreg(builder, addr->local.base_reg, ino);
        }

        if (addr->local.scale && addr->local.index_reg != X64_LIR_REG_COUNT) {
            X64_touch_lreg(builder, addr->local.index_reg, ino);
        }
    }
}

static long X64_compute_bblock_live_intervals(X64_LIRBuilder* builder, X64_BBlock* bblock)
{
    long ino = 0;

    for (X64_Instr* instr = bblock->first; instr; instr = instr->next) {
        ino = instr->ino;

        switch (instr->kind) {
        case X64_INSTR_ADD_R_R:
        case X64_INSTR_SUB_R_R:
        case X64_INSTR_IMUL_R_R:
        case X64_INSTR_AND_R_R:
        case X64_INSTR_OR_R_R:
        case X64_INSTR_XOR_R_R: {
            X64_touch_lreg(builder, instr->binary_r_r.dst, ino);
            X64_touch_lreg(builder, instr->binary_r_r.src, ino);
            break; 
        }
        case X64_INSTR_ADD_R_I:
        case X64_INSTR_SUB_R_I:
        case X64_INSTR_IMUL_R_I:
        case X64_INSTR_AND_R_I:
        case X64_INSTR_OR_R_I:
        case X64_INSTR_XOR_R_I: {
            X64_touch_lreg(builder, instr->binary_r_i.dst, ino);
            break; 
        }
        case X64_INSTR_DIV:
        case X64_INSTR_IDIV: {
            X64_touch_lreg(builder, instr->div.rdx, ino);
            X64_touch_lreg(builder, instr->div.rax, ino);
            X64_touch_lreg(builder, instr->div.src, ino);
            break;
        }
        case X64_INSTR_SEXT_AX_TO_DX: {
            X64_touch_lreg(builder, instr->sext_ax_to_dx.rax, ino);
            X64_touch_lreg(builder, instr->sext_ax_to_dx.rdx, ino);
            break;
        }
        case X64_INSTR_SAR_R_R:
        case X64_INSTR_SHL_R_R: {
            X64_touch_lreg(builder, instr->shift_r_r.dst, ino);
            X64_touch_lreg(builder, instr->shift_r_r.src, ino);
            break;
        }
        case X64_INSTR_SAR_R_I:
        case X64_INSTR_SHL_R_I: {
            X64_touch_lreg(builder, instr->shift_r_i.dst, ino);
            break;
        }
        case X64_INSTR_NEG:
        case X64_INSTR_NOT: {
            X64_touch_lreg(builder, instr->unary.dst, ino);
            break;
        }
        case X64_INSTR_REP_MOVSB: {
            X64_touch_lreg(builder, instr->rep_movsb.rdi, ino);
            X64_touch_lreg(builder, instr->rep_movsb.rsi, ino);
            X64_touch_lreg(builder, instr->rep_movsb.rcx, ino);
            break;
        }
        case X64_INSTR_MOV_R_R: {
            X64_touch_lreg(builder, instr->mov_r_r.src, ino);
            X64_touch_lreg(builder, instr->mov_r_r.dst, ino);
            break;
        }
        case X64_INSTR_MOV_R_I: {
            X64_touch_lreg(builder, instr->mov_r_i.dst, ino);
            break;
        }
        case X64_INSTR_MOV_R_M: {
            X64_touch_mem_lregs(builder, &instr->mov_r_m.src, ino);
            X64_touch_lreg(builder, instr->mov_r_m.dst, ino);
            break;
        }
        case X64_INSTR_MOV_M_R: {
            X64_touch_mem_lregs(builder, &instr->mov_m_r.dst, ino);
            X64_touch_lreg(builder, instr->mov_m_r.src, ino);
            break;
        }
        case X64_INSTR_MOV_M_I: {
            X64_touch_mem_lregs(builder, &instr->mov_m_i.dst, ino);
            break;
        }
        case X64_INSTR_MOVSX_R_R:
        case X64_INSTR_MOVZX_R_R: {
            X64_touch_lreg(builder, instr->convert_r_r.src, ino);
            X64_touch_lreg(builder, instr->convert_r_r.dst, ino);
            break;
        }
        case X64_INSTR_LEA: {
            X64_touch_mem_lregs(builder, &instr->lea.mem, ino);
            X64_touch_lreg(builder, instr->lea.dst, ino);
            break;
        }
        case X64_INSTR_CMP_R_R: {
            X64_touch_lreg(builder, instr->cmp_r_r.op1, ino);
            X64_touch_lreg(builder, instr->cmp_r_r.op2, ino);
            break;
        }
        case X64_INSTR_CMP_R_I: {
            X64_touch_lreg(builder, instr->cmp_r_i.op1, ino);
            break;
        }
        case X64_INSTR_JMP:
        case X64_INSTR_JMPCC: {
            // Do nothing.
            break;
        }
        case X64_INSTR_SETCC: {
            X64_touch_lreg(builder, instr->setcc.dst, ino);
            break;
        }
        case X64_INSTR_RET: {
            if (instr->ret.rax != X64_LIR_REG_COUNT) {
                X64_touch_lreg(builder, instr->ret.rax, ino);
            }

            if (instr->ret.rdx != X64_LIR_REG_COUNT) {
                X64_touch_lreg(builder, instr->ret.rdx, ino);
            }
            break;
        }
        case X64_INSTR_CALL:
        case X64_INSTR_CALL_R: {
            u32 num_args;
            X64_InstrCallArg* args;
            u32 dst;

            if (instr->kind == X64_INSTR_CALL) {
                num_args = instr->call.num_args;
                args = instr->call.args;
                dst = instr->call.dst;
            }
            else {
                assert(instr->kind == X64_INSTR_CALL_R);
                num_args = instr->call_r.num_args;
                args = instr->call_r.args;
                dst = instr->call_r.dst;
            }

            for (u32 i = 0; i < num_args; i++) {
                X64_touch_lreg(builder, args[i].lreg, ino);
            }

            if (dst != X64_LIR_REG_COUNT) {
                X64_touch_lreg(builder, dst, ino);
            }

            break;
        }
        default:
            assert(0);
            break;
        }
    }

    return ino;
}

void X64_compute_live_intervals(X64_LIRBuilder* builder)
{
    long ino = 0;

    for (size_t i = 0; i < builder->num_bblocks; i++) {
        ino = X64_compute_bblock_live_intervals(builder, builder->bblocks[i]);
    }

    X64_touch_lreg(builder, builder->lreg_rbp, ino);
}