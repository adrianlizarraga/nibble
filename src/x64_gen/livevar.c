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
        assert(range->end <= ino);
        range->end = ino;
    }
}

static void X64_touch_mem_lregs(X64_LIRBuilder* builder, X64_MemAddr* addr, long ino)
{
    if (addr->kind == X64_ADDR_SIBD) {
        if (addr->sibd.base_reg != X64_LIR_REG_COUNT) {
            X64_touch_lreg(builder, addr->sibd.base_reg, ino);
        }

        if (addr->sibd.scale && addr->sibd.index_reg != X64_LIR_REG_COUNT) {
            X64_touch_lreg(builder, addr->sibd.index_reg, ino);
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
        case X64_INSTR_ADD_R_M:
        case X64_INSTR_SUB_R_M:
        case X64_INSTR_IMUL_R_M:
        case X64_INSTR_AND_R_M:
        case X64_INSTR_OR_R_M:
        case X64_INSTR_XOR_R_M: {
            X64_touch_lreg(builder, instr->binary_r_m.dst, ino);
            X64_touch_mem_lregs(builder, &instr->binary_r_m.src, ino);
            break;
        }
        case X64_INSTR_ADDSS_R_R:
        case X64_INSTR_ADDSD_R_R: {
            X64_touch_lreg(builder, instr->binary_fp_r_r.dst, ino);
            X64_touch_lreg(builder, instr->binary_fp_r_r.src, ino);
            break;
        }
        case X64_INSTR_ADDSS_R_M:
        case X64_INSTR_ADDSD_R_M: {
            X64_touch_lreg(builder, instr->binary_fp_r_m.dst, ino);
            X64_touch_mem_lregs(builder, &instr->binary_fp_r_m.src, ino);
            break;
        }
        case X64_INSTR_DIV_R:
        case X64_INSTR_IDIV_R: {
            X64_touch_lreg(builder, instr->div_r.rdx, ino);
            X64_touch_lreg(builder, instr->div_r.rax, ino);
            X64_touch_lreg(builder, instr->div_r.src, ino);
            break;
        }
        case X64_INSTR_DIV_M:
        case X64_INSTR_IDIV_M: {
            X64_touch_lreg(builder, instr->div_m.rdx, ino);
            X64_touch_lreg(builder, instr->div_m.rax, ino);
            X64_touch_mem_lregs(builder, &instr->div_m.src, ino);
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
        case X64_INSTR_REP_STOSB: {
            X64_touch_lreg(builder, instr->rep_stosb.rdi, ino);
            X64_touch_lreg(builder, instr->rep_stosb.rax, ino);
            X64_touch_lreg(builder, instr->rep_stosb.rcx, ino);
            break;
        }
        case X64_INSTR_MOV_R_RH: {
            X64_touch_lreg(builder, instr->mov_r_rh.src, ino);
            X64_touch_lreg(builder, instr->mov_r_rh.dst, ino);
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
        case X64_INSTR_MOVZX_R_M:
        case X64_INSTR_MOVSX_R_M: {
            X64_touch_mem_lregs(builder, &instr->convert_r_m.src, ino);
            X64_touch_lreg(builder, instr->convert_r_m.dst, ino);
            break;
        }
        case X64_INSTR_MOVSS_R_R:
        case X64_INSTR_MOVSD_R_R: {
            X64_touch_lreg(builder, instr->movfp_r_r.src, ino);
            X64_touch_lreg(builder, instr->movfp_r_r.dst, ino);
            break;
        }
        case X64_INSTR_MOVSS_R_M:
        case X64_INSTR_MOVSD_R_M: {
            X64_touch_mem_lregs(builder, &instr->movfp_r_m.src, ino);
            X64_touch_lreg(builder, instr->movfp_r_m.dst, ino);
            break;
        }
        case X64_INSTR_MOVSS_M_R:
        case X64_INSTR_MOVSD_M_R: {
            X64_touch_lreg(builder, instr->movfp_m_r.src, ino);
            X64_touch_mem_lregs(builder, &instr->movfp_m_r.dst, ino);
            break;
        }
        case X64_INSTR_CVTTSS2SI_R_R:
        case X64_INSTR_CVTTSD2SI_R_R: {
            X64_touch_lreg(builder, instr->fp2int_r_r.src, ino);
            X64_touch_lreg(builder, instr->fp2int_r_r.dst, ino);
            break;
        }
        case X64_INSTR_CVTTSS2SI_R_M:
        case X64_INSTR_CVTTSD2SI_R_M: {
            X64_touch_mem_lregs(builder, &instr->fp2int_r_m.src, ino);
            X64_touch_lreg(builder, instr->fp2int_r_m.dst, ino);
            break;
        }
        case X64_INSTR_CVTSI2SS_R_R:
        case X64_INSTR_CVTSI2SD_R_R: {
            X64_touch_lreg(builder, instr->int2fp_r_r.src, ino);
            X64_touch_lreg(builder, instr->int2fp_r_r.dst, ino);
            break;
        }
        case X64_INSTR_CVTSI2SS_R_M:
        case X64_INSTR_CVTSI2SD_R_M: {
            X64_touch_mem_lregs(builder, &instr->int2fp_r_m.src, ino);
            X64_touch_lreg(builder, instr->int2fp_r_m.dst, ino);
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
        case X64_INSTR_CMP_R_M: {
            X64_touch_lreg(builder, instr->cmp_r_m.op1, ino);
            X64_touch_mem_lregs(builder, &instr->cmp_r_m.op2, ino);
            break;
        }
        case X64_INSTR_CMP_M_R: {
            X64_touch_mem_lregs(builder, &instr->cmp_m_r.op1, ino);
            X64_touch_lreg(builder, instr->cmp_m_r.op2, ino);
            break;
        }
        case X64_INSTR_CMP_M_I: {
            X64_touch_mem_lregs(builder, &instr->cmp_m_i.op1, ino);
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
            X64_CallValue dst;
            Type* proc_type;

            if (instr->kind == X64_INSTR_CALL) {
                num_args = instr->call.num_args;
                args = instr->call.args;
                dst = instr->call.dst;
                proc_type = instr->call.sym->type;
            }
            else {
                assert(instr->kind == X64_INSTR_CALL_R);
                num_args = instr->call_r.num_args;
                args = instr->call_r.args;
                dst = instr->call_r.dst;
                proc_type = instr->call_r.proc_type;

                X64_touch_lreg(builder, instr->call_r.proc_loc, ino);
            }

            for (u32 i = 0; i < num_args; i++) {
                X64_InstrCallArg* arg = args + i;

                if (type_is_obj_like(arg->type)) {
                    X64_touch_mem_lregs(builder, &arg->val.addr, ino);
                }
                else {
                    X64_touch_lreg(builder, arg->val.reg, ino);
                }
            }

            Type* ret_type = proc_type->as_proc.ret;

            if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
                if (type_is_obj_like(ret_type)) {
                    X64_touch_mem_lregs(builder, &dst.addr, ino);
                }
                else {
                    X64_touch_lreg(builder, dst.reg, ino);
                }
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
