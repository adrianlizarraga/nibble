#include "x64_gen/xir.h"

static void XIR_touch_lreg(XIR_Builder* builder, u32 lreg, long ino)
{
    if (lreg == XIR_REG_COUNT) {
        return;
    }

    assert(lreg < builder->num_regs);

    XIR_RegRange* range = &builder->lreg_ranges[XIR_find_alias_reg(builder, lreg)];

    if (range->start == -1) {
        range->start = ino;
        range->end = ino;
    }
    else {
        assert(range->end <= ino);
        range->end = ino;
    }
}

static void XIR_touch_mem_lregs(XIR_Builder* builder, XIR_MemAddr* addr, long ino)
{
    if (addr->kind == XIR_ADDR_SIBD) {
        if (addr->sibd.base_reg != XIR_REG_COUNT) {
            XIR_touch_lreg(builder, addr->sibd.base_reg, ino);
        }

        if (addr->sibd.scale && addr->sibd.index_reg != XIR_REG_COUNT) {
            XIR_touch_lreg(builder, addr->sibd.index_reg, ino);
        }
    }
}

static long XIR_compute_bblock_live_intervals(XIR_Builder* builder, XIR_BBlock* bblock)
{
    long ino = 0;

    for (XIR_Instr* instr = bblock->first; instr; instr = instr->next) {
        ino = instr->ino;

        switch (instr->kind) {
        case XIR_InstrAdd_R_R_KIND:
        case XIR_InstrSub_R_R_KIND:
        case XIR_InstrIMul_R_R_KIND:
        case XIR_InstrAnd_R_R_KIND:
        case XIR_InstrOr_R_R_KIND:
        case XIR_InstrXor_R_R_KIND: {
            XIR_InstrBinary_R_R* act_instr = (XIR_InstrBinary_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            XIR_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case XIR_InstrAdd_R_I_KIND:
        case XIR_InstrSub_R_I_KIND:
        case XIR_InstrIMul_R_I_KIND:
        case XIR_InstrAnd_R_I_KIND:
        case XIR_InstrOr_R_I_KIND:
        case XIR_InstrXor_R_I_KIND: {
            XIR_InstrBinary_R_I* act_instr = (XIR_InstrBinary_R_I*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrAdd_R_M_KIND:
        case XIR_InstrSub_R_M_KIND:
        case XIR_InstrIMul_R_M_KIND:
        case XIR_InstrAnd_R_M_KIND:
        case XIR_InstrOr_R_M_KIND:
        case XIR_InstrXor_R_M_KIND: {
            XIR_InstrBinary_R_M* act_instr = (XIR_InstrBinary_R_M*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            break;
        }
        case XIR_InstrAddSS_R_R_KIND:
        case XIR_InstrAddSD_R_R_KIND:
        case XIR_InstrSubSS_R_R_KIND:
        case XIR_InstrSubSD_R_R_KIND:
        case XIR_InstrMulSS_R_R_KIND:
        case XIR_InstrMulSD_R_R_KIND:
        case XIR_InstrDivSS_R_R_KIND:
        case XIR_InstrDivSD_R_R_KIND: {
            XIR_InstrBinaryFlt_R_R* act_instr = (XIR_InstrBinaryFlt_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            XIR_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case XIR_InstrAddSS_R_M_KIND:
        case XIR_InstrAddSD_R_M_KIND:
        case XIR_InstrSubSS_R_M_KIND:
        case XIR_InstrSubSD_R_M_KIND:
        case XIR_InstrMulSS_R_M_KIND:
        case XIR_InstrMulSD_R_M_KIND:
        case XIR_InstrDivSS_R_M_KIND:
        case XIR_InstrDivSD_R_M_KIND: {
            XIR_InstrBinaryFlt_R_M* act_instr = (XIR_InstrBinaryFlt_R_M*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            break;
        }
        case XIR_InstrDiv_R_KIND:
        case XIR_InstrIDiv_R_KIND: {
            XIR_InstrBaseDiv_R* act_instr = (XIR_InstrBaseDiv_R*)instr;

            XIR_touch_lreg(builder, act_instr->rdx, ino);
            XIR_touch_lreg(builder, act_instr->rax, ino);
            XIR_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case XIR_InstrDiv_M_KIND:
        case XIR_InstrIDiv_M_KIND: {
            XIR_InstrBaseDiv_M* act_instr = (XIR_InstrBaseDiv_M*)instr;

            XIR_touch_lreg(builder, act_instr->rdx, ino);
            XIR_touch_lreg(builder, act_instr->rax, ino);
            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            break;
        }
        case XIR_InstrSExtAxToDx_KIND: {
            XIR_InstrSExtAxToDx* act_instr = (XIR_InstrSExtAxToDx*)instr;

            XIR_touch_lreg(builder, act_instr->rax, ino);
            XIR_touch_lreg(builder, act_instr->rdx, ino);
            break;
        }
        case XIR_InstrSar_R_R_KIND:
        case XIR_InstrShl_R_R_KIND: {
            XIR_InstrShift_R_R* act_instr = (XIR_InstrShift_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            XIR_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case XIR_InstrSar_R_I_KIND:
        case XIR_InstrShl_R_I_KIND: {
            XIR_InstrShift_R_I* act_instr = (XIR_InstrShift_R_I*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrNeg_KIND:
        case XIR_InstrNot_KIND: {
            XIR_InstrUnary* act_instr = (XIR_InstrUnary*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrRepMovsb_KIND: {
            XIR_InstrRepMovsb* act_instr = (XIR_InstrRepMovsb*)instr;

            XIR_touch_lreg(builder, act_instr->rdi, ino);
            XIR_touch_lreg(builder, act_instr->rsi, ino);
            XIR_touch_lreg(builder, act_instr->rcx, ino);
            break;
        }
        case XIR_InstrRepStosb_KIND: {
            XIR_InstrRepStosb* act_instr = (XIR_InstrRepStosb*)instr;

            XIR_touch_lreg(builder, act_instr->rdi, ino);
            XIR_touch_lreg(builder, act_instr->rax, ino);
            XIR_touch_lreg(builder, act_instr->rcx, ino);
            break;
        }
        case XIR_InstrSyscall_KIND: {
            XIR_InstrSyscall* act_instr = (XIR_InstrSyscall*)instr;

            XIR_touch_lreg(builder, act_instr->rax, ino); // Return reg
            XIR_touch_lreg(builder, act_instr->rcx, ino); // Clobbered reg
            XIR_touch_lreg(builder, act_instr->r11, ino); // Clobbered reg

            for (u8 i = 0; i < act_instr->num_args; i += 1) {
                XIR_touch_lreg(builder, act_instr->args[i], ino);
            }
            break;
        }
        case XIR_InstrMov_R_RH_KIND: {
            XIR_InstrMov_R_RH* act_instr = (XIR_InstrMov_R_RH*)instr;

            XIR_touch_lreg(builder, act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrMov_R_R_KIND: {
            XIR_InstrMov_R_R* act_instr = (XIR_InstrMov_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrMov_R_I_KIND: {
            XIR_InstrMov_R_I* act_instr = (XIR_InstrMov_R_I*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrMov_R_M_KIND: {
            XIR_InstrMov_R_M* act_instr = (XIR_InstrMov_R_M*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrMov_M_R_KIND: {
            XIR_InstrMov_M_R* act_instr = (XIR_InstrMov_M_R*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->dst, ino);
            XIR_touch_lreg(builder, act_instr->src, ino);
            break;
        }
        case XIR_InstrMov_M_I_KIND: {
            XIR_InstrMov_M_I* act_instr = (XIR_InstrMov_M_I*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->dst, ino);
            break;
        }
        case XIR_InstrMovSX_R_R_KIND:
        case XIR_InstrMovZX_R_R_KIND: {
            XIR_InstrConvert_R_R* act_instr = (XIR_InstrConvert_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrMovZX_R_M_KIND:
        case XIR_InstrMovSX_R_M_KIND: {
            XIR_InstrConvert_R_M* act_instr = (XIR_InstrConvert_R_M*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrMovSS_R_R_KIND:
        case XIR_InstrMovSD_R_R_KIND: {
            XIR_InstrMovFlt_R_R* act_instr = (XIR_InstrMovFlt_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrMovSS_R_M_KIND:
        case XIR_InstrMovSD_R_M_KIND: {
            XIR_InstrMovFlt_R_M* act_instr = (XIR_InstrMovFlt_R_M*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrMovSS_M_R_KIND:
        case XIR_InstrMovSD_M_R_KIND: {
            XIR_InstrMovFlt_M_R* act_instr = (XIR_InstrMovFlt_M_R*)instr;

            XIR_touch_lreg(builder, act_instr->src, ino);
            XIR_touch_mem_lregs(builder, &act_instr->dst, ino);
            break;
        }
        case XIR_InstrCvtSS2SD_R_R_KIND:
        case XIR_InstrCvtSD2SS_R_R_KIND: {
            XIR_InstrFlt2Flt_R_R* act_instr = (XIR_InstrFlt2Flt_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrCvtSS2SD_R_M_KIND:
        case XIR_InstrCvtSD2SS_R_M_KIND: {
            XIR_InstrFlt2Flt_R_M* act_instr = (XIR_InstrFlt2Flt_R_M*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrCvtSS2SI_R_R_KIND:
        case XIR_InstrCvtSD2SI_R_R_KIND: {
            XIR_InstrFlt2Int_R_R* act_instr = (XIR_InstrFlt2Int_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrCvtSS2SI_R_M_KIND:
        case XIR_InstrCvtSD2SI_R_M_KIND: {
            XIR_InstrFlt2Int_R_M* act_instr = (XIR_InstrFlt2Int_R_M*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrCvtSI2SS_R_R_KIND:
        case XIR_InstrCvtSI2SD_R_R_KIND: {
            XIR_InstrInt2Flt_R_R* act_instr = (XIR_InstrInt2Flt_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrCvtSI2SS_R_M_KIND:
        case XIR_InstrCvtSI2SD_R_M_KIND: {
            XIR_InstrInt2Flt_R_M* act_instr = (XIR_InstrInt2Flt_R_M*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->src, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrLEA_KIND: {
            XIR_InstrLEA* act_instr = (XIR_InstrLEA*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->mem, ino);
            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrCmp_R_R_KIND: {
            XIR_InstrCmp_R_R* act_instr = (XIR_InstrCmp_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->op1, ino);
            XIR_touch_lreg(builder, act_instr->op2, ino);
            break;
        }
        case XIR_InstrCmp_R_I_KIND: {
            XIR_InstrCmp_R_I* act_instr = (XIR_InstrCmp_R_I*)instr;

            XIR_touch_lreg(builder, act_instr->op1, ino);
            break;
        }
        case XIR_InstrCmp_R_M_KIND: {
            XIR_InstrCmp_R_M* act_instr = (XIR_InstrCmp_R_M*)instr;

            XIR_touch_lreg(builder, act_instr->op1, ino);
            XIR_touch_mem_lregs(builder, &act_instr->op2, ino);
            break;
        }
        case XIR_InstrCmp_M_R_KIND: {
            XIR_InstrCmp_M_R* act_instr = (XIR_InstrCmp_M_R*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->op1, ino);
            XIR_touch_lreg(builder, act_instr->op2, ino);
            break;
        }
        case XIR_InstrCmp_M_I_KIND: {
            XIR_InstrCmp_M_I* act_instr = (XIR_InstrCmp_M_I*)instr;

            XIR_touch_mem_lregs(builder, &act_instr->op1, ino);
            break;
        }
        case XIR_InstrUComiSS_R_R_KIND:
        case XIR_InstrUComiSD_R_R_KIND: {
            XIR_InstrCmpFlt_R_R* act_instr = (XIR_InstrCmpFlt_R_R*)instr;

            XIR_touch_lreg(builder, act_instr->op1, ino);
            XIR_touch_lreg(builder, act_instr->op2, ino);
            break;
        }
        case XIR_InstrUComiSS_R_M_KIND:
        case XIR_InstrUComiSD_R_M_KIND: {
            XIR_InstrCmpFlt_R_M* act_instr = (XIR_InstrCmpFlt_R_M*)instr;

            XIR_touch_lreg(builder, act_instr->op1, ino);
            XIR_touch_mem_lregs(builder, &act_instr->op2, ino);
            break;
        }
        case XIR_InstrJmp_KIND:
        case XIR_InstrJmpCC_KIND: {
            // Do nothing.
            break;
        }
        case XIR_InstrSetCC_KIND: {
            XIR_InstrSetCC* act_instr = (XIR_InstrSetCC*)instr;

            XIR_touch_lreg(builder, act_instr->dst, ino);
            break;
        }
        case XIR_InstrRet_KIND: {
            XIR_InstrRet* act_instr = (XIR_InstrRet*)instr;

            if (act_instr->rax != XIR_REG_COUNT) {
                XIR_touch_lreg(builder, act_instr->rax, ino);
            }

            if (act_instr->rdx != XIR_REG_COUNT) {
                XIR_touch_lreg(builder, act_instr->rdx, ino);
            }
            break;
        }
        case XIR_InstrCall_KIND:
        case XIR_InstrCall_R_KIND: {
            u32 num_args;
            XIR_InstrCallArg* args;
            XIR_CallValue dst;
            Type* proc_type;

            if (instr->kind == XIR_InstrCall_KIND) {
                XIR_InstrCall* instr_call = (XIR_InstrCall*)instr;

                num_args = instr_call->num_args;
                args = instr_call->args;
                dst = instr_call->dst;
                proc_type = instr_call->sym->type;
            }
            else {
                assert(instr->kind == XIR_InstrCall_R_KIND);
                XIR_InstrCall_R* instr_call_r = (XIR_InstrCall_R*)instr;

                num_args = instr_call_r->num_args;
                args = instr_call_r->args;
                dst = instr_call_r->dst;
                proc_type = instr_call_r->proc_type;

                XIR_touch_lreg(builder, instr_call_r->proc_loc, ino);
            }

            for (u32 i = 0; i < num_args; i++) {
                XIR_InstrCallArg* arg = args + i;

                if (type_is_obj_like(arg->type)) {
                    XIR_touch_mem_lregs(builder, &arg->val.addr, ino);
                }
                else {
                    XIR_touch_lreg(builder, arg->val.reg, ino);
                }
            }

            Type* ret_type = proc_type->as_proc.ret;

            if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
                if (type_is_obj_like(ret_type)) {
                    XIR_touch_mem_lregs(builder, &dst.addr, ino);
                }
                else {
                    XIR_touch_lreg(builder, dst.reg, ino);
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

void XIR_compute_live_intervals(XIR_Builder* builder)
{
    long ino = 0;

    for (size_t i = 0; i < builder->num_bblocks; i++) {
        ino = XIR_compute_bblock_live_intervals(builder, builder->bblocks[i]);
    }

    XIR_touch_lreg(builder, builder->lreg_rbp, ino);
}
