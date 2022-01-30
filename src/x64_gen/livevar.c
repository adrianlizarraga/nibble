

typedef struct X64_BBlockLiveInfo {
    BitArray livein;
    BitArray liveout;
    BitArray use;
    BitArray def;
} X64_BBlockLiveInfo;

static void X64_try_add_use(X64_BBlockLiveInfo* info, u32 lreg)
{
   if (!bit_arr_get(&info->def, lreg)) {
       bit_arr_set(&info->use, lreg, true);
   }
}

static void X64_try_add_mem_use(X64_BBlockLiveInfo* info, X64_MemAddr* addr)
{
    if (addr->kind == X64_ADDR_LOCAL) {
        if (addr->local.base_reg != X64_LIR_REG_COUNT) {
            X64_try_add_use(info, addr->local.base_reg);
        }

        if (addr->local.scale && addr->local.index_reg != X64_LIR_REG_COUNT) {
            X64_try_add_use(info, addr->local.index_reg);
        }
    }
}

static inline void X64_add_def(X64_BBlockLiveInfo* info, u32 lreg)
{
    bit_arr_set(&info->def, lreg, true);
}

static void X64_init_bblock_usedef(X64_BBlock* bblock, X64_BBlockLiveInfo* info, u32* lreg_phys)
{
    for (X64_Instr* instr = bblock->first; instr; instr = instr->next) {
        switch (instr->kind) {
        case X64_INSTR_ADD_R_R:
        case X64_INSTR_SUB_R_R:
        case X64_INSTR_IMUL_R_R:
        case X64_INSTR_AND_R_R:
        case X64_INSTR_OR_R_R:
        case X64_INSTR_XOR_R_R: {
            X64_try_add_use(info, instr->binary_r_r.dst);
            X64_try_add_use(info, instr->binary_r_r.src);
            X64_add_def(info, instr->binary_r_r.dst);
            break; 
        }
        case X64_INSTR_ADD_R_I:
        case X64_INSTR_SUB_R_I:
        case X64_INSTR_IMUL_R_I:
        case X64_INSTR_AND_R_I:
        case X64_INSTR_OR_R_I:
        case X64_INSTR_XOR_R_I: {
            X64_try_add_use(info, instr->binary_r_i.dst);
            X64_add_def(info, instr->binary_r_i.dst);
            break; 
        }
        case X64_INSTR_DIV:
        case X64_INSTR_IDIV: {
            X64_try_add_use(info, lreg_phys[X64_RDX]);
            X64_try_add_use(info, lreg_phys[X64_RAX]);
            X64_try_add_use(info, instr->div.src);

            X64_add_def(info, lreg_phys[X64_RDX]);
            X64_add_def(info, lreg_phys[X64_RAX]);
            break;
        }
        case X64_INSTR_SEXT_AX_TO_DX: {
            X64_try_add_use(info, lreg_phys[X64_RAX]);
            X64_add_def(info, lreg_phys[X64_RDX]);
            break;
        }
        case X64_INSTR_SAR_R_R:
        case X64_INSTR_SHL_R_R: {
            X64_try_add_use(info, instr->shift_r_r.dst);
            X64_try_add_use(info, instr->shift_r_r.src);
            X64_add_def(info, instr->shift_r_r.dst);
            break;
        }
        case X64_INSTR_SAR_R_I:
        case X64_INSTR_SHL_R_I: {
            X64_try_add_use(info, instr->shift_r_i.dst);
            X64_add_def(info, instr->shift_r_i.dst);
            break;
        }
        case X64_INSTR_NEG:
        case X64_INSTR_NOT: {
            X64_try_add_use(info, instr->unary.dst);
            X64_add_def(info, instr->unary.dst);
            break;
        }
        case X64_INSTR_REP_MOVSB: {
            X64_try_add_use(info, lreg_phys[X64_RDI]);
            X64_try_add_use(info, lreg_phys[X64_RSI]);
            X64_try_add_use(info, lreg_phys[X64_RCX]);
            break;
        }
        case X64_INSTR_MOV_R_R: {
            X64_try_add_use(info, instr->mov_r_r.src);
            X64_add_def(info, instr->mov_r_r.dst);
            break;
        }
        case X64_INSTR_MOV_R_I: {
            X64_add_def(info, instr->mov_r_i.dst);
            break;
        }
        case X64_INSTR_MOV_R_M: {
            X64_try_add_mem_use(info, &instr->mov_r_m.src);
            X64_add_def(info, instr->mov_r_m.dst);
            break;
        }
        case X64_INSTR_MOV_M_R: {
            X64_try_add_mem_use(info, &instr->mov_m_r.dst);
            X64_try_add_use(info, instr->mov_m_r.src);
            break;
        }
        case X64_INSTR_MOV_M_I: {
            X64_try_add_mem_use(info, &instr->mov_m_i.dst);
            break;
        }
        case X64_INSTR_MOVSX_R_R:
        case X64_INSTR_MOVZX_R_R: {
            X64_try_add_use(info, instr->convert_r_r.src);
            X64_add_def(info, instr->convert_r_r.dst);
            break;
        }
        case X64_INSTR_LEA: {
            X64_try_add_mem_use(info, &instr->lea.mem);
            X64_add_def(info, instr->lea.dst);
            break;
        }
        case X64_INSTR_CMP_R_R: {
            X64_try_add_use(info, instr->cmp_r_r.op1);
            X64_try_add_use(info, instr->cmp_r_r.op2);
            break;
        }
        case X64_INSTR_CMP_R_I: {
            X64_try_add_use(info, instr->cmp_r_i.op1);
            break;
        }
        case X64_INSTR_JMP:
        case X64_INSTR_JMPCC: {
            // Do nothing.
            break;
        }
        case X64_INSTR_SETCC: {
            X64_add_def(info, instr->setcc.dst);
            break;
        }
        case X64_INSTR_RET: {
            if (instr->ret.regs & X64_RET_REG_RAX) {
                X64_try_add_use(info, lreg_phys[X64_RAX]);
            }

            if (instr->ret.regs & X64_RET_REG_RDX) {
                X64_try_add_use(info, lreg_phys[X64_RDX]);
            }
            break;
        }
        default:
            break;
        }
    }
}

void X64_live_var_analysis(Allocator* arena, size_t num_bblocks, X64_BBlock** bblocks, u32 num_regs, u32* lreg_phys)
{
    X64_BBlockLiveInfo* liveinfos = alloc_array(arena, X64_BBlockLiveInfo, num_bblocks, false);

    // Initialize bit arrays for each basic block
    for (size_t i = 0; i < num_bblocks; i++) {
        X64_BBlockLiveInfo* info = liveinfos + i;

        bit_arr_init(&info->livein, arena, num_regs);
        bit_arr_init(&info->liveout, arena, num_regs);
        bit_arr_init(&info->use, arena, num_regs);
        bit_arr_init(&info->def, arena, num_regs);

        // Init def and use
        X64_init_bblock_usedef(bblocks[i], info, lreg_phys);
    }
}
