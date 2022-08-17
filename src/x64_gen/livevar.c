#include "x64_gen/lir.h"

static void X64_touch_lreg(X64_LIRBuilder* builder, u32 lreg, long ino)
{
    if (lreg == X64_LIR_REG_COUNT) {
        return;
    }

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
        case X64_InstrAdd_R_R_KIND:
        case X64_InstrSub_R_R_KIND:
        case X64_InstrIMul_R_R_KIND:
        case X64_InstrAnd_R_R_KIND:
        case X64_InstrOr_R_R_KIND:
        case X64_InstrXor_R_R_KIND: {
            X64_InstrBinary_R_R* act_instr = (X64_InstrBinary_R_R*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            X64_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case X64_InstrAdd_R_I_KIND:
        case X64_InstrSub_R_I_KIND:
        case X64_InstrIMul_R_I_KIND:
        case X64_InstrAnd_R_I_KIND:
        case X64_InstrOr_R_I_KIND:
        case X64_InstrXor_R_I_KIND: {
            X64_InstrBinary_R_I* act_instr = (X64_InstrBinary_R_I*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrAdd_R_M_KIND:
        case X64_InstrSub_R_M_KIND:
        case X64_InstrIMul_R_M_KIND:
        case X64_InstrAnd_R_M_KIND:
        case X64_InstrOr_R_M_KIND:
        case X64_InstrXor_R_M_KIND: {
            X64_InstrBinary_R_M* act_instr = (X64_InstrBinary_R_M*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            break;
        }
        case X64_InstrAddSS_R_R_KIND:
        case X64_InstrAddSD_R_R_KIND:
        case X64_InstrSubSS_R_R_KIND:
        case X64_InstrSubSD_R_R_KIND:
        case X64_InstrMulSS_R_R_KIND:
        case X64_InstrMulSD_R_R_KIND:
        case X64_InstrDivSS_R_R_KIND:
        case X64_InstrDivSD_R_R_KIND: {
            X64_InstrBinaryFlt_R_R* act_instr = (X64_InstrBinaryFlt_R_R*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            X64_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case X64_InstrAddSS_R_M_KIND:
        case X64_InstrAddSD_R_M_KIND:
        case X64_InstrSubSS_R_M_KIND:
        case X64_InstrSubSD_R_M_KIND:
        case X64_InstrMulSS_R_M_KIND:
        case X64_InstrMulSD_R_M_KIND:
        case X64_InstrDivSS_R_M_KIND:
        case X64_InstrDivSD_R_M_KIND: {
            X64_InstrBinaryFlt_R_M* act_instr = (X64_InstrBinaryFlt_R_M*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            break;
        }
        case X64_InstrDiv_R_KIND:
        case X64_InstrIDiv_R_KIND: {
            X64_InstrBaseDiv_R* act_instr = (X64_InstrBaseDiv_R*)instr;

            X64_touch_lreg(builder, act_instr->rdx, ino);
            X64_touch_lreg(builder, act_instr->rax, ino);
            X64_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case X64_InstrDiv_M_KIND:
        case X64_InstrIDiv_M_KIND: {
            X64_InstrBaseDiv_M* act_instr = (X64_InstrBaseDiv_M*)instr;

            X64_touch_lreg(builder, act_instr->rdx, ino);
            X64_touch_lreg(builder, act_instr->rax, ino);
            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            break;
        }
        case X64_InstrSExtAxToDx_KIND: {
            X64_InstrSExtAxToDx* act_instr = (X64_InstrSExtAxToDx*)instr;

            X64_touch_lreg(builder, act_instr->rax, ino);
            X64_touch_lreg(builder, act_instr->rdx, ino);
            break;
        }
        case X64_InstrSar_R_R_KIND:
        case X64_InstrShl_R_R_KIND: {
            X64_InstrShift_R_R* act_instr = (X64_InstrShift_R_R*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            X64_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case X64_InstrSar_R_I_KIND:
        case X64_InstrShl_R_I_KIND: {
            X64_InstrShift_R_I* act_instr = (X64_InstrShift_R_I*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrNeg_KIND:
        case X64_InstrNot_KIND: {
            X64_InstrUnary* act_instr = (X64_InstrUnary*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrRepMovsb_KIND: {
            X64_InstrRepMovsb* act_instr = (X64_InstrRepMovsb*)instr;

            X64_touch_lreg(builder, act_instr->rdi, ino);
            X64_touch_lreg(builder, act_instr->rsi, ino);
            X64_touch_lreg(builder, act_instr->rcx, ino);
            break;
        }
        case X64_InstrRepStosb_KIND: {
            X64_InstrRepStosb* act_instr = (X64_InstrRepStosb*)instr;

            X64_touch_lreg(builder, act_instr->rdi, ino);
            X64_touch_lreg(builder, act_instr->rax, ino);
            X64_touch_lreg(builder, act_instr->rcx, ino);
            break;
        }
        case X64_InstrSyscall_KIND: {
            X64_InstrSyscall* act_instr = (X64_InstrSyscall*)instr;

            X64_touch_lreg(builder, act_instr->rax, ino); // Return reg
            X64_touch_lreg(builder, act_instr->rcx, ino); // Clobbered reg
            X64_touch_lreg(builder, act_instr->r11, ino); // Clobbered reg

            for (u8 i = 0; i < act_instr->num_args; i += 1) {
                X64_touch_lreg(builder, act_instr->args[i], ino);
            }
            break;
        }
        case X64_InstrMov_R_RH_KIND: {
            X64_InstrMov_R_RH* act_instr = (X64_InstrMov_R_RH*)instr;

            X64_touch_lreg(builder, act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrMov_R_R_KIND: {
            X64_InstrMov_R_R* act_instr = (X64_InstrMov_R_R*)instr;

            X64_touch_lreg(builder, act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrMov_R_I_KIND: {
            X64_InstrMov_R_I* act_instr = (X64_InstrMov_R_I*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrMov_R_M_KIND: {
            X64_InstrMov_R_M* act_instr = (X64_InstrMov_R_M*)instr;

            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrMov_M_R_KIND: {
            X64_InstrMov_M_R* act_instr = (X64_InstrMov_M_R*)instr;

            X64_touch_mem_lregs(builder, &act_instr->dst, ino);
            X64_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case X64_InstrMov_M_I_KIND: {
            X64_InstrMov_M_I* act_instr = (X64_InstrMov_M_I*)instr;

            X64_touch_mem_lregs(builder, &act_instr->dst, ino);
            break;
        }
        case X64_InstrMovSX_R_R_KIND:
        case X64_InstrMovZX_R_R_KIND: {
            X64_InstrConvert_R_R* act_instr = (X64_InstrConvert_R_R*)instr;

            X64_touch_lreg(builder, act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrMovZX_R_M_KIND:
        case X64_InstrMovSX_R_M_KIND: {
            X64_InstrConvert_R_M* act_instr = (X64_InstrConvert_R_M*)instr;

            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrMovSS_R_R_KIND:
        case X64_InstrMovSD_R_R_KIND: {
            X64_InstrMovFlt_R_R* act_instr = (X64_InstrMovFlt_R_R*)instr;

            X64_touch_lreg(builder, act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrMovSS_R_M_KIND:
        case X64_InstrMovSD_R_M_KIND: {
            X64_InstrMovFlt_R_M* act_instr = (X64_InstrMovFlt_R_M*)instr;

            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrMovSS_M_R_KIND:
        case X64_InstrMovSD_M_R_KIND: {
            X64_InstrMovFlt_M_R* act_instr = (X64_InstrMovFlt_M_R*)instr;

            X64_touch_lreg(builder, act_instr->src, ino);
            X64_touch_mem_lregs(builder, &act_instr->dst, ino);
            break;
        }
        case X64_InstrCvtSS2SD_R_R_KIND:
        case X64_InstrCvtSD2SS_R_R_KIND: {
            X64_InstrFlt2Flt_R_R* act_instr = (X64_InstrFlt2Flt_R_R*)instr;

            X64_touch_lreg(builder, act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrCvtSS2SD_R_M_KIND:
        case X64_InstrCvtSD2SS_R_M_KIND: {
            X64_InstrFlt2Flt_R_M* act_instr = (X64_InstrFlt2Flt_R_M*)instr;

            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrCvtSS2SI_R_R_KIND:
        case X64_InstrCvtSD2SI_R_R_KIND: {
            X64_InstrFlt2Int_R_R* act_instr = (X64_InstrFlt2Int_R_R*)instr;

            X64_touch_lreg(builder, act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrCvtSS2SI_R_M_KIND:
        case X64_InstrCvtSD2SI_R_M_KIND: {
            X64_InstrFlt2Int_R_M* act_instr = (X64_InstrFlt2Int_R_M*)instr;

            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrCvtSI2SS_R_R_KIND:
        case X64_InstrCvtSI2SD_R_R_KIND: {
            X64_InstrInt2Flt_R_R* act_instr = (X64_InstrInt2Flt_R_R*)instr;

            X64_touch_lreg(builder, act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrCvtSI2SS_R_M_KIND:
        case X64_InstrCvtSI2SD_R_M_KIND: {
            X64_InstrInt2Flt_R_M* act_instr = (X64_InstrInt2Flt_R_M*)instr;

            X64_touch_mem_lregs(builder, &act_instr->src, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrLEA_KIND: {
            X64_InstrLEA* act_instr = (X64_InstrLEA*)instr;

            X64_touch_mem_lregs(builder, &act_instr->mem, ino);
            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrCmp_R_R_KIND: {
            X64_InstrCmp_R_R* act_instr = (X64_InstrCmp_R_R*)instr;

            X64_touch_lreg(builder, act_instr->op1, ino);
            X64_touch_lreg(builder, act_instr->op2, ino);
            break;
        }
        case X64_InstrCmp_R_I_KIND: {
            X64_InstrCmp_R_I* act_instr = (X64_InstrCmp_R_I*)instr;

            X64_touch_lreg(builder, act_instr->op1, ino);
            break;
        }
        case X64_InstrCmp_R_M_KIND: {
            X64_InstrCmp_R_M* act_instr = (X64_InstrCmp_R_M*)instr;

            X64_touch_lreg(builder, act_instr->op1, ino);
            X64_touch_mem_lregs(builder, &act_instr->op2, ino);
            break;
        }
        case X64_InstrCmp_M_R_KIND: {
            X64_InstrCmp_M_R* act_instr = (X64_InstrCmp_M_R*)instr;

            X64_touch_mem_lregs(builder, &act_instr->op1, ino);
            X64_touch_lreg(builder, act_instr->op2, ino);
            break;
        }
        case X64_InstrCmp_M_I_KIND: {
            X64_InstrCmp_M_I* act_instr = (X64_InstrCmp_M_I*)instr;

            X64_touch_mem_lregs(builder, &act_instr->op1, ino);
            break;
        }
        case X64_InstrUComiSS_R_R_KIND:
        case X64_InstrUComiSD_R_R_KIND: {
            X64_InstrCmpFlt_R_R* act_instr = (X64_InstrCmpFlt_R_R*)instr;

            X64_touch_lreg(builder, act_instr->op1, ino);
            X64_touch_lreg(builder, act_instr->op2, ino);
            break;
        }
        case X64_InstrUComiSS_R_M_KIND:
        case X64_InstrUComiSD_R_M_KIND: {
            X64_InstrCmpFlt_R_M* act_instr = (X64_InstrCmpFlt_R_M*)instr;

            X64_touch_lreg(builder, act_instr->op1, ino);
            X64_touch_mem_lregs(builder, &act_instr->op2, ino);
            break;
        }
        case X64_InstrJmp_KIND:
        case X64_InstrJmpCC_KIND: {
            // Do nothing.
            break;
        }
        case X64_InstrSetCC_KIND: {
            X64_InstrSetCC* act_instr = (X64_InstrSetCC*)instr;

            X64_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case X64_InstrRet_KIND: {
            X64_InstrRet* act_instr = (X64_InstrRet*)instr;

            if (act_instr->rax != X64_LIR_REG_COUNT) {
                X64_touch_lreg(builder, act_instr->rax, ino);
            }

            if (act_instr->rdx != X64_LIR_REG_COUNT) {
                X64_touch_lreg(builder, act_instr->rdx, ino);
            }
            break;
        }
        case X64_InstrCall_KIND:
        case X64_InstrCall_R_KIND: {
            u32 num_args;
            X64_InstrCallArg* args;
            X64_CallValue dst;
            Type* proc_type;

            if (instr->kind == X64_InstrCall_KIND) {
                X64_InstrCall* instr_call = (X64_InstrCall*)instr;

                num_args = instr_call->num_args;
                args = instr_call->args;
                dst = instr_call->dst;
                proc_type = instr_call->sym->type;
            }
            else {
                assert(instr->kind == X64_InstrCall_R_KIND);
                X64_InstrCall_R* instr_call_r = (X64_InstrCall_R*)instr;

                num_args = instr_call_r->num_args;
                args = instr_call_r->args;
                dst = instr_call_r->dst;
                proc_type = instr_call_r->proc_type;

                X64_touch_lreg(builder, instr_call_r->proc_loc, ino);
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
            NIBBLE_FATAL_EXIT("livevar.c: Unhandled X64 instr kind '%d'", instr->kind);
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
