#include "bytecode/module.h"
#include "x64_gen/lir.h"

static X64_InstrKind binary_kind[] = {[IR_InstrIntAdd_KIND] = X64_InstrAdd_R_R_KIND,  [IR_InstrIntSub_KIND] = X64_InstrSub_R_R_KIND,
                                      [IR_InstrIntMul_KIND] = X64_InstrIMul_R_R_KIND, [IR_InstrAnd_KIND] = X64_InstrAnd_R_R_KIND,
                                      [IR_InstrOr_KIND] = X64_InstrOr_R_R_KIND,       [IR_InstrXor_KIND] = X64_InstrXor_R_R_KIND};

static X64_InstrKind binary_r_i_kind[] = {
    [IR_InstrIntAdd_KIND] = X64_InstrAdd_R_I_KIND,  [IR_InstrIntSub_KIND] = X64_InstrSub_R_I_KIND,
    [IR_InstrIntMul_KIND] = X64_InstrIMul_R_I_KIND, [IR_InstrAnd_KIND] = X64_InstrAnd_R_I_KIND,
    [IR_InstrOr_KIND] = X64_InstrOr_R_I_KIND,       [IR_InstrXor_KIND] = X64_InstrXor_R_I_KIND};

static X64_InstrKind binary_r_m_kind[] = {
    [IR_InstrIntAdd_KIND] = X64_InstrAdd_R_M_KIND,  [IR_InstrIntSub_KIND] = X64_InstrSub_R_M_KIND,
    [IR_InstrIntMul_KIND] = X64_InstrIMul_R_M_KIND, [IR_InstrAnd_KIND] = X64_InstrAnd_R_M_KIND,
    [IR_InstrOr_KIND] = X64_InstrOr_R_M_KIND,       [IR_InstrXor_KIND] = X64_InstrXor_R_M_KIND};

static X64_InstrKind shift_kind[] = {[IR_InstrShl_KIND] = X64_InstrShl_R_R_KIND, [IR_InstrSar_KIND] = X64_InstrSar_R_R_KIND};

static X64_InstrKind shift_r_i_kind[] = {[IR_InstrShl_KIND] = X64_InstrShl_R_I_KIND, [IR_InstrSar_KIND] = X64_InstrSar_R_I_KIND};

// Indexed on the source kind!!
static const X64_EmitInstrFlt2Flt_R_R_Func x64_flt2flt_r_r_funcs[] = {
    [FLOAT_F32] = X64_emit_instr_cvtss2sd_r_r,
    [FLOAT_F64] = X64_emit_instr_cvtsd2ss_r_r
};

// Indexed on the source kind!!
static const X64_EmitInstrFlt2Flt_R_M_Func x64_flt2flt_r_m_funcs[] = {
    [FLOAT_F32] = X64_emit_instr_cvtss2sd_r_m,
    [FLOAT_F64] = X64_emit_instr_cvtsd2ss_r_m
};

static const X64_EmitInstrFlt2Int_R_R_Func x64_flt2int_r_r_funcs[] = {
    [FLOAT_F32] = X64_emit_instr_cvtss2si_r_r,
    [FLOAT_F64] = X64_emit_instr_cvtsd2si_r_r
};

static const X64_EmitInstrFlt2Int_R_M_Func x64_flt2int_r_m_funcs[] = {
    [FLOAT_F32] = X64_emit_instr_cvtss2si_r_m,
    [FLOAT_F64] = X64_emit_instr_cvtsd2si_r_m
};

static const X64_EmitInstrInt2Flt_R_R_Func x64_int2flt_r_r_funcs[] = {
    [FLOAT_F64] = X64_emit_instr_cvtsi2sd_r_r,
    [FLOAT_F32] = X64_emit_instr_cvtsi2ss_r_r
};

static const X64_EmitInstrInt2Flt_R_M_Func x64_int2flt_r_m_funcs[] = {
    [FLOAT_F64] = X64_emit_instr_cvtsi2sd_r_m,
    [FLOAT_F32] = X64_emit_instr_cvtsi2ss_r_m
};

// Indexed on `is_signed` boolean.
static const X64_EmitInstrMovXX_R_R_Func x64_movxx_r_r_funcs[] = {
    [0] = X64_emit_instr_movzx_r_r,
    [1] = X64_emit_instr_movsx_r_r,
};

// Indexed on `is_signed` boolean.
static const X64_EmitInstrMovXX_R_M_Func x64_movxx_r_m_funcs[] = {
    [0] = X64_emit_instr_movzx_r_m,
    [1] = X64_emit_instr_movsx_r_m,
};

// The floating-point comparison instructions in X86_64 use the unsigned condition variants.
static const ConditionKind flt_cond_map[] = {
    [COND_U_LT] = COND_U_LT, [COND_S_LT] = COND_U_LT, [COND_U_LTEQ] = COND_U_LTEQ, [COND_S_LTEQ] = COND_U_LTEQ,
    [COND_U_GT] = COND_U_GT, [COND_S_GT] = COND_U_GT, [COND_U_GTEQ] = COND_U_GTEQ, [COND_S_GTEQ] = COND_U_GTEQ,
    [COND_EQ] = COND_EQ,     [COND_NEQ] = COND_NEQ,
};

static void X64_merge_ranges(X64_LRegRange* dst_range, X64_LRegRange* src_range)
{
    if (src_range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_NONE) {
        assert((dst_range->ra_ctrl_kind == X64_REG_ALLOC_CTRL_NONE) || (dst_range->ra_ctrl_kind == X64_REG_ALLOC_CTRL_HINT_LIR_REG) ||
               (dst_range->ra_ctrl_kind == X64_REG_ALLOC_CTRL_HINT_PHYS_REG));
        dst_range->ra_ctrl_kind = src_range->ra_ctrl_kind;
        dst_range->ra_ctrl = src_range->ra_ctrl;
    }
}

u32 X64_find_alias_reg(X64_LIRBuilder* builder, u32 r)
{
    u32* roots = builder->lreg_aliases;

    while (roots[r] != r) {
        u32 next_r = roots[r];
        roots[r] = roots[next_r];
        r = next_r;
    }

    return r;
}

static void X64_alias_lir_regs(X64_LIRBuilder* builder, u32 u, u32 v)
{
    u32 root_u = X64_find_alias_reg(builder, u);
    u32 root_v = X64_find_alias_reg(builder, v);

    if (root_u == root_v) {
        return;
    }

    X64_LRegRange* ranges = builder->lreg_ranges;
    u32* roots = builder->lreg_aliases;
    u32* sizes = builder->lreg_sizes;

    if (sizes[root_u] > sizes[root_v]) {
        roots[root_v] = root_u;
        sizes[root_u] += sizes[root_v];

        X64_merge_ranges(ranges + root_u, ranges + root_v);
    }
    else {
        roots[root_u] = root_v;
        sizes[root_v] += sizes[root_u];

        X64_merge_ranges(ranges + root_v, ranges + root_u);
    }
}

static u32 X64_next_lir_reg(X64_LIRBuilder* builder, X64_RegClass reg_class)
{
    u32 next_reg = builder->num_regs++;

    assert(next_reg != X64_LIR_REG_COUNT);

    array_push(builder->lreg_ranges, (X64_LRegRange){.lreg = next_reg, .start = -1, .end = -1, .reg_class = reg_class});
    array_push(builder->lreg_aliases, next_reg);
    array_push(builder->lreg_sizes, 1);

    return next_reg;
}

static u32 X64_get_lir_reg(X64_LIRBuilder* builder, u32 ireg, X64_RegClass reg_class)
{
    u32 result = builder->reg_map[ireg];

    if (result == (u32)-1) {
        result = X64_next_lir_reg(builder, reg_class);
        builder->reg_map[ireg] = result;
    }
    else {
        result = X64_find_alias_reg(builder, result);
    }

    return result;
}

static u32 X64_def_phys_reg(X64_LIRBuilder* builder, X64_Reg phys_reg)
{
    u32 result = X64_next_lir_reg(builder, x64_reg_classes[phys_reg]);
    X64_LRegRange* range = &builder->lreg_ranges[result];
    assert(result == array_len(builder->lreg_ranges) - 1);

    range->ra_ctrl_kind = X64_REG_ALLOC_CTRL_FORCE_REG;
    range->ra_ctrl.preg = phys_reg;

    return result;
}

static void X64_force_arg_reg(X64_LIRBuilder* builder, u32 lreg, X64_Reg phys_reg)
{
    lreg = X64_find_alias_reg(builder, lreg);

    assert(lreg < builder->num_regs);
    X64_LRegRange* range = &builder->lreg_ranges[lreg];

    assert(range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_FORCE_REG);
    assert(range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_FORCE_ANY_REG);
    assert(range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_FORCE_REG_OR_SPILL);
    range->ra_ctrl_kind = X64_REG_ALLOC_CTRL_FORCE_REG_OR_SPILL;
    range->ra_ctrl.preg_mask = (1 << phys_reg);
}

static void X64_force_stack_arg_reg(X64_LIRBuilder* builder, u32 lreg)
{
    lreg = X64_find_alias_reg(builder, lreg);

    assert(lreg < builder->num_regs);
    X64_LRegRange* range = &builder->lreg_ranges[lreg];

    assert(range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_FORCE_REG);
    assert(range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_FORCE_ANY_REG);
    assert(range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_FORCE_REG_OR_SPILL);
    range->ra_ctrl_kind = X64_REG_ALLOC_CTRL_FORCE_REG_OR_SPILL;
    range->ra_ctrl.preg_mask = builder->stack_reg_mask;
}

static void X64_force_any_reg(X64_LIRBuilder* builder, u32 lreg, u32 banned_regs)
{
    lreg = X64_find_alias_reg(builder, lreg);

    assert(lreg < builder->num_regs);
    X64_LRegRange* range = &builder->lreg_ranges[lreg];

    assert(range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_FORCE_REG);
    assert(range->ra_ctrl_kind != X64_REG_ALLOC_CTRL_FORCE_REG_OR_SPILL);

    if (range->ra_ctrl_kind == X64_REG_ALLOC_CTRL_FORCE_ANY_REG) {
        range->ra_ctrl.preg_mask &= (~banned_regs);
    }
    else {
        range->ra_ctrl_kind = X64_REG_ALLOC_CTRL_FORCE_ANY_REG;
        range->ra_ctrl.preg_mask = x64_target.scratch_reg_mask & (~banned_regs);
    }
}

static void X64_hint_same_reg(X64_LIRBuilder* builder, u32 copier_lreg, u32 copied_lreg)
{
    u32 lreg = X64_find_alias_reg(builder, copier_lreg);

    assert(lreg < builder->num_regs);
    X64_LRegRange* range = &builder->lreg_ranges[lreg];

    if (range->ra_ctrl_kind == X64_REG_ALLOC_CTRL_NONE) {
        range->ra_ctrl_kind = X64_REG_ALLOC_CTRL_HINT_LIR_REG;
        range->ra_ctrl.lreg = copied_lreg;
    }
}

static void X64_hint_phys_reg(X64_LIRBuilder* builder, u32 lreg, X64_Reg phys_reg)
{
    lreg = X64_find_alias_reg(builder, lreg);

    assert(lreg < builder->num_regs);
    X64_LRegRange* range = &builder->lreg_ranges[lreg];

    // Follow chain of LIR hints and set the root's hint to phys_reg instead if:
    // - The root has no other hints.
    // - The physical register is not caller-saved (to prevent unnecessary pushes/pops across procs).
    if (!X64_is_caller_saved_reg(phys_reg)) {
        while (range->ra_ctrl_kind == X64_REG_ALLOC_CTRL_HINT_LIR_REG) {
            range = &builder->lreg_ranges[range->ra_ctrl.lreg];
        }
    }

    if (range->ra_ctrl_kind == X64_REG_ALLOC_CTRL_NONE) {
        range->ra_ctrl_kind = X64_REG_ALLOC_CTRL_HINT_PHYS_REG;
        range->ra_ctrl.preg = phys_reg;
    }
}

static u32 X64_lea_sib(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 index_reg, s8 scale, u32 base_reg)
{
    bool has_index = scale != 0 && (index_reg != (u32)-1);

    assert(has_index);

    // mul index_reg, <scale>
    Scalar scale_imm = {.as_int._u64 = scale};
    X64_emit_instr_binary_r_i(builder, xbblock, binary_r_i_kind[IR_InstrIntMul_KIND], X64_MAX_INT_REG_SIZE, index_reg, scale_imm);

    // add index_reg, base_reg
    if (base_reg != (u32)-1) {
        X64_emit_instr_binary_r_r(builder, xbblock, binary_kind[IR_InstrIntAdd_KIND], X64_MAX_INT_REG_SIZE, index_reg, base_reg);
    }

    return index_reg;
}

static void X64_get_lir_addr(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_MemAddr* dst, MemAddr* src, u32 banned_regs)
{
    if (src->base_kind == MEM_BASE_STR_LIT) {
        dst->kind = X64_ADDR_STR_LIT;
        dst->str_lit = src->base.str_lit;
        return;
    }

    if (src->base_kind == MEM_BASE_FLOAT_LIT) {
        dst->kind = X64_ADDR_FLOAT_LIT;
        dst->float_lit = src->base.float_lit;
        return;
    }

    bool has_base = src->base_kind != MEM_BASE_NONE;
    bool has_index = src->scale && (src->index_reg < IR_REG_COUNT);
    assert(has_base || has_index);

    if (has_base) {
        u32 base_reg = (u32)-1;
        u32 index_reg = (u32)-1;
        s8 scale = src->scale;
        s32 disp = src->disp;

        if (has_index) {
            index_reg = X64_get_lir_reg(builder, src->index_reg, X64_REG_CLASS_INT);
            X64_force_any_reg(builder, index_reg, banned_regs);
        }

        if (src->base_kind == MEM_BASE_MEM_OBJ) {
            MemObj* mem_obj = src->base.obj;

            while (mem_obj->kind == MEM_OBJ_ALIAS) {
                mem_obj = mem_obj->alias;
            }

            if (mem_obj->kind == MEM_OBJ_SYM) {
                Symbol* sym = mem_obj->sym;

                // Early exit for global variable addresses.
                if (!sym->is_local) {
                    dst->kind = X64_ADDR_GLOBAL_SYM;
                    dst->global = sym;

                    return;
                }

                base_reg = builder->lreg_rbp;
                disp += sym->as_var.offset;
            }
            else if (mem_obj->kind == MEM_OBJ_ADDR) {
                X64_MemAddr base_addr = {0};
                X64_get_lir_addr(builder, xbblock, &base_addr, &mem_obj->addr, banned_regs);

                assert(base_addr.kind == X64_ADDR_SIBD);
                base_reg = base_addr.sibd.base_reg;

                // Merge scales and indices into a single index register with a scale of 1.
                if (has_index && base_addr.sibd.scale && (base_addr.sibd.index_reg != (u32)-1)) {
                    index_reg = X64_lea_sib(builder, xbblock, index_reg, scale, (u32)-1);
                    index_reg = X64_lea_sib(builder, xbblock, base_addr.sibd.index_reg, base_addr.sibd.scale, index_reg);
                    scale = 1;
                }
                else {
                    index_reg = base_addr.sibd.index_reg;
                    scale = base_addr.sibd.scale;
                }

                disp += base_addr.sibd.disp;
            }
            else {
                assert(mem_obj->kind == MEM_OBJ_ANON_OBJ);
                base_reg = builder->lreg_rbp;
                disp += mem_obj->anon_obj->offset;
            }
        }
        else {
            base_reg = X64_get_lir_reg(builder, src->base.reg, X64_REG_CLASS_INT);
            X64_force_any_reg(builder, base_reg, banned_regs);
        }

        dst->kind = X64_ADDR_SIBD;
        dst->sibd.base_reg = base_reg;
        dst->sibd.disp = disp;
        dst->sibd.scale = scale;
        dst->sibd.index_reg = index_reg;
    }
    else {
        u32 index_reg = X64_get_lir_reg(builder, src->index_reg, X64_REG_CLASS_INT);
        X64_force_any_reg(builder, index_reg, banned_regs);

        dst->kind = X64_ADDR_SIBD;
        dst->sibd.base_reg = (u32)-1;
        dst->sibd.disp = src->disp;
        dst->sibd.scale = src->scale;
        dst->sibd.index_reg = index_reg;
    }

    // Fix invalid scale by consolidating the base reg, index reg, and scale.
    if (has_index && (!IS_POW2(dst->sibd.scale) || (dst->sibd.scale > X64_MAX_SIBD_SCALE))) {
        assert(dst->kind == X64_ADDR_SIBD);

        dst->sibd.base_reg = X64_lea_sib(builder, xbblock, dst->sibd.index_reg, dst->sibd.scale, dst->sibd.base_reg);
        dst->sibd.index_reg = (u32)-1;
        dst->sibd.scale = 0;
    }
}

static void X64_load_op_ri(X64_LIRBuilder* builder, X64_BBlock* xbblock, u32 lreg, unsigned size, OpRI op_ri)
{
    if (op_ri.is_imm) {
        X64_emit_instr_mov_r_i(builder, xbblock, size, lreg, op_ri.imm);
    }
    else {
        u32 s = X64_get_lir_reg(builder, op_ri.reg, X64_REG_CLASS_INT);
        X64_hint_same_reg(builder, s, lreg);
        X64_emit_instr_mov_r_r(builder, xbblock, size, lreg, s);
    }
}

static void X64_load_op_ria(X64_LIRBuilder* builder, X64_BBlock* xbblock, size_t size, u32 dst, OpRIA src)
{
    // mov dst, src
    if (src.kind == OP_RIA_IMM) {
        X64_emit_instr_mov_r_i(builder, xbblock, size, dst, src.imm);
    }
    else if (src.kind == OP_RIA_REG) {
        u32 a = X64_get_lir_reg(builder, src.reg, X64_REG_CLASS_INT);
        X64_hint_same_reg(builder, dst, a);
        X64_emit_instr_mov_r_r(builder, xbblock, size, dst, a);
    }
    else {
        assert(src.kind == OP_RIA_ADDR);
        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &src.addr, 0);
        X64_emit_instr_mov_r_m(builder, xbblock, size, dst, addr);
    }
}

static void X64_emit_memcpy(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_MemAddr dst, X64_MemAddr src, OpRI size)
{
    // lea rdi, [..dst]
    // lea rsi, [..src]
    // mov rcx, size
    // rep movsb

    u32 rdi = X64_def_phys_reg(builder, X64_RDI);
    X64_emit_instr_lea(builder, xbblock, rdi, dst);

    u32 rsi = X64_def_phys_reg(builder, X64_RSI);
    X64_emit_instr_lea(builder, xbblock, rsi, src);

    u32 rcx = X64_def_phys_reg(builder, X64_RCX);
    X64_load_op_ri(builder, xbblock, rcx, PTR_SIZE, size);

    X64_emit_instr_rep_movsb(builder, xbblock, rdi, rsi, rcx);
}

static void X64_emit_memset(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_MemAddr dst, OpRI value, OpRI size)
{
    // lea rdi, [..dst]
    // mov al, value
    // mov rcx, size
    // rep stosb

    u32 rdi = X64_def_phys_reg(builder, X64_RDI);
    X64_emit_instr_lea(builder, xbblock, rdi, dst);

    u32 al = X64_def_phys_reg(builder, X64_RAX);
    X64_load_op_ri(builder, xbblock, al, 1, value);

    u32 rcx = X64_def_phys_reg(builder, X64_RCX);
    X64_load_op_ri(builder, xbblock, rcx, PTR_SIZE, size);

    X64_emit_instr_rep_stosb(builder, xbblock, rdi, al, rcx);
}

static void X64_place_prim_arg(X64_LIRBuilder* builder, X64_InstrCallArg* dst, IR_Value* src,
                               u32 (*arg_reg_indices)[X64_REG_CLASS_COUNT], X64_StackArgsInfo* stack_info)
{
    Type* type = src->type;
    size_t size = type->size;
    X64_RegClass reg_class = type->kind == TYPE_FLOAT ? X64_REG_CLASS_FLOAT : X64_REG_CLASS_INT;
    u32* arg_reg_index = &(*arg_reg_indices)[reg_class];
    X64_ScratchRegs arg_regs = (*x64_target.arg_regs)[reg_class];

    assert(size <= X64_MAX_INT_REG_SIZE);

    dst->type = type;
    dst->val.reg = X64_get_lir_reg(builder, src->reg, reg_class);

    if (*arg_reg_index >= arg_regs.num_regs) {
        dst->slot.prim.in_reg = false;
        dst->slot.prim.sp_offset = stack_info->size;

        X64_force_stack_arg_reg(builder, dst->val.reg);

        stack_info->size += ALIGN_UP(size, X64_STACK_WORD_SIZE);
    }
    else {
        dst->slot.prim.in_reg = true;
        dst->slot.prim.preg = arg_regs.regs[*arg_reg_index];

        *arg_reg_index += 1;
        X64_force_arg_reg(builder, dst->val.reg, dst->slot.prim.preg);
    }
}

static void X64_place_obj_arg(X64_LIRBuilder* builder, X64_BBlock* xbblock, X64_InstrCallArg* dst, IR_Value* src,
                              u32 (*arg_reg_indices)[X64_REG_CLASS_COUNT], X64_StackArgsInfo* stack_info)
{
    Type* type = src->type;
    size_t size = type->size;

    assert(type_is_obj_like(type));

    X64_RegClass reg_class = X64_obj_reg_class(type);
    u32* arg_reg_index = &(*arg_reg_indices)[reg_class];
    X64_ScratchRegs arg_regs = (*x64_target.arg_regs)[reg_class];

    dst->type = type;
    X64_get_lir_addr(builder, xbblock, &dst->val.addr, &src->addr, (1 << X64_RDI));

    u32 rem_regs = arg_regs.num_regs - *arg_reg_index;

    if ((type->size <= X64_MAX_INT_REG_SIZE) && (rem_regs >= 1)) {
        dst->slot.obj.num_regs = 1;
        dst->slot.obj.pregs[0] = arg_regs.regs[*arg_reg_index];
        *arg_reg_index += 1;
    }
    else if ((type->size <= (X64_MAX_INT_REG_SIZE << 1)) && (rem_regs >= 2)) {
        dst->slot.obj.num_regs = 2;
        dst->slot.obj.pregs[0] = arg_regs.regs[*arg_reg_index];
        *arg_reg_index += 1;
        dst->slot.obj.pregs[1] = arg_regs.regs[*arg_reg_index];
        *arg_reg_index += 1;
    }
    else {
        dst->slot.obj.num_regs = 0;
        dst->slot.obj.sp_offset = stack_info->size;

        stack_info->size += ALIGN_UP(size, X64_STACK_WORD_SIZE);
    }
}

static X64_StackArgsInfo X64_convert_call_args(X64_LIRBuilder* builder, X64_BBlock* xbblock, Type* ret_type, u32 num_args,
                                               IR_Value* args, X64_InstrCallArg* x64_args)
{
    X64_StackArgsInfo stack_info = {0};
    u32 arg_int_reg_offset = type_is_obj_like(ret_type) && X64_is_obj_retarg_large(ret_type->size);
    u32 arg_reg_indices[X64_REG_CLASS_COUNT] = {[X64_REG_CLASS_INT] = arg_int_reg_offset};

    for (u32 i = 0; i < num_args; i++) {
        IR_Value* ir_arg = args + i;
        X64_InstrCallArg* lir_arg = x64_args + i;

        if (type_is_obj_like(ir_arg->type)) {
            X64_place_obj_arg(builder, xbblock, lir_arg, ir_arg, &arg_reg_indices, &stack_info);
        }
        else {
            X64_place_prim_arg(builder, lir_arg, ir_arg, &arg_reg_indices, &stack_info);
        }
    }

    return stack_info;
}

static void X64_add_call_site(X64_LIRBuilder* builder, X64_Instr* instr)
{
    assert(instr->kind == X64_InstrCall_KIND || instr->kind == X64_InstrCall_R_KIND);
    array_push(builder->call_sites, instr);
}

typedef bool (*X64_LIRCreateFunc)(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr);

#define X64_DEF_CONVERT_INT_BINARY_FUNC(f_n, ir_t, f_r_i, f_r_r, f_r_m)                                                     \
    static bool f_n(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)              \
    {                                                                                                                       \
        (void)next_ir_instr;                                                                                                \
        ir_t* ir_v = (ir_t*)ir_instr;                                                                                       \
        size_t size = ir_v->type->size;                                                                                     \
        OpRIA ir_a = ir_v->a;                                                                                               \
        OpRIA ir_b = ir_v->b;                                                                                               \
                                                                                                                            \
        assert(ir_a.kind != OP_RIA_IMM || ir_b.kind != OP_RIA_IMM);                                                         \
        u32 r = X64_get_lir_reg(builder, ir_v->r, X64_REG_CLASS_INT);                                                       \
                                                                                                                            \
        X64_load_op_ria(builder, xbblock, size, r, ir_a);                                                                   \
                                                                                                                            \
        if (ir_b.kind == OP_RIA_IMM) {                                                                                      \
            bool fits_in_4bytes = (size < X64_MAX_INT_REG_SIZE) || (ir_b.imm.as_int._u64 < (u64)int_kind_max[INTEGER_U32]); \
                                                                                                                            \
            if (fits_in_4bytes) {                                                                                           \
                f_r_i(builder, xbblock, size, r, ir_b.imm);                                                                 \
            }                                                                                                               \
            else {                                                                                                          \
                u32 imm_reg = X64_next_lir_reg(builder, X64_REG_CLASS_INT);                                                 \
                X64_emit_instr_mov_r_i(builder, xbblock, size, imm_reg, ir_b.imm);                                          \
                f_r_r(builder, xbblock, size, r, imm_reg);                                                                  \
            }                                                                                                               \
        }                                                                                                                   \
        else if (ir_b.kind == OP_RIA_REG) {                                                                                 \
            u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_INT);                                                  \
            f_r_r(builder, xbblock, size, r, b);                                                                            \
        }                                                                                                                   \
        else {                                                                                                              \
            assert(ir_b.kind == OP_RIA_ADDR);                                                                               \
            X64_MemAddr addr = {0};                                                                                         \
            X64_get_lir_addr(builder, xbblock, &addr, &ir_b.addr, 0);                                                       \
            f_r_m(builder, xbblock, size, r, addr);                                                                         \
        }                                                                                                                   \
        return false;                                                                                                       \
    }

X64_DEF_CONVERT_INT_BINARY_FUNC(X64_convert_int_add_instr, IR_InstrIntAdd, X64_emit_instr_add_r_i, X64_emit_instr_add_r_r,
                                X64_emit_instr_add_r_m)
X64_DEF_CONVERT_INT_BINARY_FUNC(X64_convert_int_sub_instr, IR_InstrIntSub, X64_emit_instr_sub_r_i, X64_emit_instr_sub_r_r,
                                X64_emit_instr_sub_r_m)
X64_DEF_CONVERT_INT_BINARY_FUNC(X64_convert_int_mul_instr, IR_InstrIntMul, X64_emit_instr_mul_r_i, X64_emit_instr_mul_r_r,
                                X64_emit_instr_mul_r_m)
X64_DEF_CONVERT_INT_BINARY_FUNC(X64_convert_and_instr, IR_InstrAnd, X64_emit_instr_and_r_i, X64_emit_instr_and_r_r,
                                X64_emit_instr_and_r_m)
X64_DEF_CONVERT_INT_BINARY_FUNC(X64_convert_or_instr, IR_InstrOr, X64_emit_instr_or_r_i, X64_emit_instr_or_r_r, X64_emit_instr_or_r_m)
X64_DEF_CONVERT_INT_BINARY_FUNC(X64_convert_xor_instr, IR_InstrXor, X64_emit_instr_xor_r_i, X64_emit_instr_xor_r_r,
                                X64_emit_instr_xor_r_m)

static bool X64_convert_int_div_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_ir_instr;

    // EX: r = a / b
    //
    // mov _ax, a ; Only if `a` is not already in `_ax`
    // cqo        ; sign extend into `_dx` if size >= 2 bytes
    // div b      ; `b` must be in a register or memory
    // mov r, _ax

    IR_InstrIntDiv* ir_idiv = (IR_InstrIntDiv*)ir_instr;

    Type* type = ir_idiv->type;
    size_t size = type->size;
    bool uses_dx = size >= 2;

    OpRIA ir_a = ir_idiv->a;
    OpRIA ir_b = ir_idiv->b;

    // mov _ax, a
    u32 ax = X64_def_phys_reg(builder, X64_RAX);
    X64_load_op_ria(builder, xbblock, size, ax, ir_a);

    // cqo
    u32 dx = X64_LIR_REG_COUNT;
    if (uses_dx) { // Reserve rdx
        dx = X64_def_phys_reg(builder, X64_RDX);

        if (type_is_signed(type)) {
            X64_emit_instr_sext_ax_to_dx(builder, xbblock, size, dx, ax);
        }
        else {
            // Clear rdx by xor
            X64_emit_instr_xor_r_r(builder, xbblock, X64_MAX_INT_REG_SIZE, dx, dx);
        }
    }

    // div b
    if (ir_b.kind == OP_RIA_IMM) {
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_R_KIND : X64_InstrDiv_R_KIND;
        u32 b = X64_next_lir_reg(builder, X64_REG_CLASS_INT);
        X64_emit_instr_mov_r_i(builder, xbblock, size, b, ir_b.imm);
        X64_emit_instr_div_r(builder, xbblock, x64_div_kind, size, dx, ax, b);
    }
    else if (ir_b.kind == OP_RIA_REG) {
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_R_KIND : X64_InstrDiv_R_KIND;
        u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_INT);
        X64_emit_instr_div_r(builder, xbblock, x64_div_kind, size, dx, ax, b);
    }
    else {
        assert(ir_b.kind == OP_RIA_ADDR);
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_M_KIND : X64_InstrDiv_M_KIND;
        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &ir_b.addr, 0);
        X64_emit_instr_div_m(builder, xbblock, x64_div_kind, size, dx, ax, addr);
    }

    // mov r, _ax
    u32 r = X64_get_lir_reg(builder, ir_idiv->r, X64_REG_CLASS_INT);
    X64_emit_instr_mov_r_r(builder, xbblock, size, r, ax);
    X64_hint_same_reg(builder, r, ax);

    return false;
}

static bool X64_convert_mod_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_ir_instr;

    // EX: r = a % b
    //
    // mov _ax, a ; Only if `a` is not already in `_ax`
    // cqo        ; sign extend into `_dx` if size >= 2 bytes
    // div b      ; `b` must be in a register
    // mov r, _dx ; _ax[h] if size < 2 bytes

    IR_InstrMod* ir_mod = (IR_InstrMod*)ir_instr;

    Type* type = ir_mod->type;
    size_t size = type->size;
    bool uses_dx = size >= 2;

    OpRIA ir_a = ir_mod->a;
    OpRIA ir_b = ir_mod->b;

    // mov _ax, a
    u32 ax = X64_def_phys_reg(builder, X64_RAX);
    X64_load_op_ria(builder, xbblock, size, ax, ir_a);

    // cqo
    u32 dx = X64_LIR_REG_COUNT;
    if (uses_dx) { // Reserve rdx
        dx = X64_def_phys_reg(builder, X64_RDX);

        if (type_is_signed(type)) {
            X64_emit_instr_sext_ax_to_dx(builder, xbblock, size, dx, ax);
        }
        else {
            // Clear rdx by xor
            X64_emit_instr_xor_r_r(builder, xbblock, X64_MAX_INT_REG_SIZE, dx, dx);
        }
    }

    // div b
    if (ir_b.kind == OP_RIA_IMM) {
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_R_KIND : X64_InstrDiv_R_KIND;
        u32 b = X64_next_lir_reg(builder, X64_REG_CLASS_INT);
        X64_emit_instr_mov_r_i(builder, xbblock, size, b, ir_b.imm);
        X64_emit_instr_div_r(builder, xbblock, x64_div_kind, size, dx, ax, b);
    }
    else if (ir_b.kind == OP_RIA_REG) {
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_R_KIND : X64_InstrDiv_R_KIND;
        u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_INT);
        X64_emit_instr_div_r(builder, xbblock, x64_div_kind, size, dx, ax, b);
    }
    else {
        assert(ir_b.kind == OP_RIA_ADDR);
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_M_KIND : X64_InstrDiv_M_KIND;
        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &ir_b.addr, 0);
        X64_emit_instr_div_m(builder, xbblock, x64_div_kind, size, dx, ax, addr);
    }

    if (uses_dx) {
        // mov r, _dx
        u32 r = X64_get_lir_reg(builder, ir_mod->r, X64_REG_CLASS_INT);
        X64_emit_instr_mov_r_r(builder, xbblock, size, r, dx);
        X64_hint_same_reg(builder, r, dx);
    }
    else {
        // mov r, _ax[h]
        u32 r = X64_get_lir_reg(builder, ir_mod->r, X64_REG_CLASS_INT);
        X64_emit_instr_mov_r_rh(builder, xbblock, r, ax);
    }

    return false;
}

static bool X64_convert_divmod_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_ir_instr;

    // EX: q,r = a % b
    //
    // mov _ax, a ; Only if `a` is not already in `_ax`
    // cqo        ; sign extend into `_dx` if size >= 2 bytes
    // div b      ; `b` must be in a register
    // mov r, _dx ; _ax[h] if size < 2 bytes
    // mov q, _ax

    IR_InstrDivMod* ir_divmod = (IR_InstrDivMod*)ir_instr;

    Type* type = ir_divmod->type;
    size_t size = type->size;
    bool uses_dx = size >= 2;

    OpRIA ir_a = ir_divmod->a;
    OpRIA ir_b = ir_divmod->b;

    // mov _ax, a
    u32 ax = X64_def_phys_reg(builder, X64_RAX);
    X64_load_op_ria(builder, xbblock, size, ax, ir_a);

    // cqo
    u32 dx = X64_LIR_REG_COUNT;
    if (uses_dx) { // Reserve rdx
        dx = X64_def_phys_reg(builder, X64_RDX);

        if (type_is_signed(type)) {
            X64_emit_instr_sext_ax_to_dx(builder, xbblock, size, dx, ax);
        }
        else {
            // Clear rdx by xor
            X64_emit_instr_xor_r_r(builder, xbblock, X64_MAX_INT_REG_SIZE, dx, dx);
        }
    }

    // div b
    if (ir_b.kind == OP_RIA_IMM) {
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_R_KIND : X64_InstrDiv_R_KIND;
        u32 b = X64_next_lir_reg(builder, X64_REG_CLASS_INT);
        X64_emit_instr_mov_r_i(builder, xbblock, size, b, ir_b.imm);
        X64_emit_instr_div_r(builder, xbblock, x64_div_kind, size, dx, ax, b);
    }
    else if (ir_b.kind == OP_RIA_REG) {
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_R_KIND : X64_InstrDiv_R_KIND;
        u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_INT);
        X64_emit_instr_div_r(builder, xbblock, x64_div_kind, size, dx, ax, b);
    }
    else {
        assert(ir_b.kind == OP_RIA_ADDR);
        X64_InstrKind x64_div_kind = type_is_signed(type) ? X64_InstrIDiv_M_KIND : X64_InstrDiv_M_KIND;
        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &ir_b.addr, 0);
        X64_emit_instr_div_m(builder, xbblock, x64_div_kind, size, dx, ax, addr);
    }

    if (uses_dx) {
        // mov r, _dx
        u32 r = X64_get_lir_reg(builder, ir_divmod->r, X64_REG_CLASS_INT);
        X64_emit_instr_mov_r_r(builder, xbblock, size, r, dx);
        X64_hint_same_reg(builder, r, dx);
    }
    else {
        // mov r, _ax[h]
        u32 r = X64_get_lir_reg(builder, ir_divmod->r, X64_REG_CLASS_INT);
        X64_emit_instr_mov_r_rh(builder, xbblock, r, ax);
    }

    // mov q, _ax
    u32 q = X64_get_lir_reg(builder, ir_divmod->q, X64_REG_CLASS_INT);
    X64_emit_instr_mov_r_r(builder, xbblock, size, q, ax);
    X64_hint_same_reg(builder, q, ax);

    return false;
}

#define X64_DEF_CONVERT_FLT_BINARY_FUNC(f_n, ir_t, f32_r_r, f32_r_m, f64_r_r, f64_r_m)                         \
    static bool f_n(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr) \
    {                                                                                                          \
        (void)next_ir_instr;                                                                                   \
                                                                                                               \
        ir_t* ir_f = (ir_t*)ir_instr;                                                                          \
                                                                                                               \
        FloatKind fkind = ir_f->fkind;                                                                         \
        OpRA ir_a = ir_f->a;                                                                                   \
        OpRA ir_b = ir_f->b;                                                                                   \
                                                                                                               \
        u32 r = X64_mov_flt_op_ra_into_reg(builder, xbblock, fkind, ir_f->r, ir_a);                            \
                                                                                                               \
        if (ir_b.is_addr) {                                                                                    \
            X64_MemAddr addr = {0};                                                                            \
            X64_get_lir_addr(builder, xbblock, &addr, &ir_b.addr, 0);                                          \
            if (fkind == FLOAT_F32)                                                                            \
                f32_r_m(builder, xbblock, r, addr);                                                            \
            else                                                                                               \
                f64_r_m(builder, xbblock, r, addr);                                                            \
        }                                                                                                      \
        else {                                                                                                 \
            u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_FLOAT);                                   \
            if (fkind == FLOAT_F32)                                                                            \
                f32_r_r(builder, xbblock, r, b);                                                               \
            else                                                                                               \
                f64_r_r(builder, xbblock, r, b);                                                               \
        }                                                                                                      \
                                                                                                               \
        return false;                                                                                          \
    }

X64_DEF_CONVERT_FLT_BINARY_FUNC(X64_convert_flt_add_instr, IR_InstrFltAdd, X64_emit_instr_addss_r_r, X64_emit_instr_addss_r_m,
                                X64_emit_instr_addsd_r_r, X64_emit_instr_addsd_r_m)
X64_DEF_CONVERT_FLT_BINARY_FUNC(X64_convert_flt_sub_instr, IR_InstrFltSub, X64_emit_instr_subss_r_r, X64_emit_instr_subss_r_m,
                                X64_emit_instr_subsd_r_r, X64_emit_instr_subsd_r_m)
X64_DEF_CONVERT_FLT_BINARY_FUNC(X64_convert_flt_mul_instr, IR_InstrFltMul, X64_emit_instr_mulss_r_r, X64_emit_instr_mulss_r_m,
                                X64_emit_instr_mulsd_r_r, X64_emit_instr_mulsd_r_m)
X64_DEF_CONVERT_FLT_BINARY_FUNC(X64_convert_flt_div_instr, IR_InstrFltDiv, X64_emit_instr_divss_r_r, X64_emit_instr_divss_r_m,
                                X64_emit_instr_divsd_r_r, X64_emit_instr_divsd_r_m)

#define X64_DEF_CONVERT_SHIFT_FUNC(f_n, ir_t, f_r_r, f_r_i)                                                    \
    static bool f_n(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr) \
    {                                                                                                          \
        (void)next_ir_instr;                                                                                   \
        ir_t* ir_shift = (ir_t*)ir_instr;                                                                      \
                                                                                                               \
        size_t size = ir_shift->type->size;                                                                    \
        OpRIA ir_a = ir_shift->a;                                                                              \
        OpRIA ir_b = ir_shift->b;                                                                              \
                                                                                                               \
        u32 r = X64_get_lir_reg(builder, ir_shift->r, X64_REG_CLASS_INT);                                      \
        X64_load_op_ria(builder, xbblock, size, r, ir_a);                                                      \
                                                                                                               \
        if (ir_b.kind == OP_RIA_IMM) {                                                                         \
            f_r_i(builder, xbblock, size, r, ir_b.imm);                                                        \
        }                                                                                                      \
        else if (ir_b.kind == OP_RIA_REG) {                                                                    \
            u32 cx = X64_def_phys_reg(builder, X64_RCX);                                                       \
            u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_INT);                                     \
            X64_hint_phys_reg(builder, b, X64_RCX);                                                            \
            X64_emit_instr_mov_r_r(builder, xbblock, size, cx, b);                                             \
                                                                                                               \
            f_r_r(builder, xbblock, size, r, cx);                                                              \
        }                                                                                                      \
        else {                                                                                                 \
            assert(ir_b.kind == OP_RIA_ADDR);                                                                  \
            u32 cx = X64_def_phys_reg(builder, X64_RCX);                                                       \
            X64_MemAddr b = {0};                                                                               \
            X64_get_lir_addr(builder, xbblock, &b, &ir_b.addr, 0);                                             \
            X64_emit_instr_mov_r_m(builder, xbblock, size, cx, b);                                             \
                                                                                                               \
            f_r_r(builder, xbblock, size, r, cx);                                                              \
        }                                                                                                      \
        \ 
        return false;                                                                                          \
    }

X64_DEF_CONVERT_SHIFT_FUNC(X64_convert_sar_instr, IR_InstrSar, X64_emit_instr_sar_r_r, X64_emit_instr_sar_r_i)
X64_DEF_CONVERT_SHIFT_FUNC(X64_convert_shl_instr, IR_InstrShl, X64_emit_instr_shl_r_r, X64_emit_instr_shl_r_i)

// EX: r = -a
//
// mov r, a
// neg r
#define X64_DEF_CONVERT_UNARY_FUNC(f_n, ir_t, f_u)                                                             \
    static bool f_n(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr) \
    {                                                                                                          \
        (void)next_ir_instr;                                                                                   \
        ir_t* ir_unary = (ir_t*)ir_instr;                                                                      \
                                                                                                               \
        size_t size = ir_unary->type->size;                                                                    \
                                                                                                               \
        u32 r = X64_get_lir_reg(builder, ir_unary->r, X64_REG_CLASS_INT);                                      \
        u32 a = X64_get_lir_reg(builder, ir_unary->a, X64_REG_CLASS_INT);                                      \
        X64_hint_same_reg(builder, r, a);                                                                      \
                                                                                                               \
        X64_emit_instr_mov_r_r(builder, xbblock, size, r, a);                                                  \
        f_u(builder, xbblock, size, r);                                                                        \
                                                                                                               \
        return false;                                                                                          \
    }

X64_DEF_CONVERT_UNARY_FUNC(X64_convert_neg_instr, IR_InstrNeg, X64_emit_instr_neg)
X64_DEF_CONVERT_UNARY_FUNC(X64_convert_not_instr, IR_InstrNot, X64_emit_instr_not)

static bool X64_convert_trunc_instr(X64_LIRBuilder* builder, X64_BBlock xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_instr;
    IR_InstrTrunc* ir_trunc = (IR_InstrTrunc*)ir_instr;

    size_t dst_size = ir_trunc->dst_type->size;

    u32 r = X64_get_lir_reg(builder, ir_trunc->r, X64_REG_CLASS_INT);
    u32 a = X64_get_lir_reg(builder, ir_trunc->a, X64_REG_CLASS_INT);

    X64_emit_instr_mov_r_r(builder, xbblock, dst_size, r, a);
    return false;
}

static bool X64_convert_zext_instr(X64_LIRBuilder* builder, X64_BBlock xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_instr;

    // EX: r = zext(a)
    //
    // movzx r, a

    IR_InstrZExt* ir_zext = (IR_InstrZExt*)ir_instr;

    size_t dst_size = ir_zext->dst_type->size;
    size_t src_size = ir_zext->src_type->size;

    u32 r = X64_get_lir_reg(builder, ir_zext->r, X64_REG_CLASS_INT);
    u32 a = X64_get_lir_reg(builder, ir_zext->a, X64_REG_CLASS_INT);

    X64_emit_instr_movzx_r_r(builder, xbblock, dst_size, r, src_size, a);
    return false;
}

static bool X64_convert_sext_instr(X64_LIRBuilder* builder, X64_BBlock xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_ir_instr;

    // EX: r = sext(a)
    //
    // movsx r, a
    IR_InstrSExt* ir_sext = (IR_InstrSExt*)ir_instr;

    size_t dst_size = ir_sext->dst_type->size;
    size_t src_size = ir_sext->src_type->size;

    u32 r = X64_get_lir_reg(builder, ir_sext->r, X64_REG_CLASS_INT);
    u32 a = X64_get_lir_reg(builder, ir_sext->a, X64_REG_CLASS_INT);

    X64_emit_instr_movsx_r_r(builder, xbblock, dst_size, r, src_size, a);
    return false;
}

static bool X64_convert_flt2flt_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_ir_instr;

    // EX: r = flt2flt(a)
    //
    // cvtss2sd r, a

    IR_InstrFlt2Flt* ir_flt2flt = (IR_InstrFlt2Flt*)ir_instr;

    FloatKind src_kind = ir_flt2flt->src_kind;
    FloatKind dst_kind = ir_flt2flt->dst_kind;
    assert(src_kind != dst_kind);

    OpRA ir_a = ir_flt2flt->src;

    u32 r = X64_get_lir_reg(builder, ir_flt2flt->dst, X64_REG_CLASS_FLOAT);

    if (ir_a.is_addr) {
        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &ir_a.addr, 0);
        x64_flt2flt_r_m_funcs[src_kind](builder, xbblock, r, addr);
    }
    else {
        u32 a = X64_get_lir_reg(builder, ir_a.reg, X64_REG_CLASS_FLOAT);
        x64_flt2flt_r_r_funcs[src_kind](builder, xbblock, r, a);
    }

    return false;
}

static bool X64_convert_flt2int_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_ir_instr;

    // EX: r = flt2int(a_fp)
    //
    // cvttsd2si r, a_fp

    IR_InstrFlt2Int* ir_flt2int = (IR_InstrFlt2Int*)ir_instr;

    FloatKind src_kind = ir_flt2int->src_kind;
    IntegerKind dst_kind = ir_flt2int->dst_kind;
    u8 dst_size = int_kind_sizes[dst_kind];
    OpRA ir_a = ir_flt2int->src;

    u32 r = X64_get_lir_reg(builder, ir_flt2int->dst, X64_REG_CLASS_INT);

    if (ir_a.is_addr) {
        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &ir_a.addr, 0);

        x64_flt2int_r_m_funcs[src_kind](builder, xbblock, dst_size, r, addr);
    }
    else {
        u32 a = X64_get_lir_reg(builder, ir_a.reg, X64_REG_CLASS_FLOAT);
        x64_flt2int_r_r_funcs[src_kind](builder, xbblock, dst_size, r, a);
    }

    return false;
}

static bool X64_convert_int2flt_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_ir_instr;

    // EX: r_fp = int2flt(a)
    //
    // cvtsi2sd r_fp, a

    IR_InstrInt2Flt* ir_int2flt = (IR_InstrInt2Flt*)ir_instr;

    const size_t smallest_src_size = 4;
    FloatKind dst_kind = ir_int2flt->dst_kind;
    IntegerKind src_kind = ir_int2flt->src_kind;
    size_t src_size = int_kind_sizes[src_kind];
    OpRA ir_a = ir_int2flt->src;

    u32 r = X64_get_lir_reg(builder, ir_int2flt->dst, X64_REG_CLASS_FLOAT);

    if (ir_a.is_addr) {
        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &ir_a.addr, 0);

        // If src is < 4 bytes, extend it to 4 bytes before converting to fp.
        if (src_size < smallest_src_size) {
            u32 a_ext = X64_next_lir_reg(builder, X64_REG_CLASS_INT);
            bool is_signed = int_kind_signed[src_kind];

            x64_movxx_r_m_funcs[is_signed](builder, xbblock, smallest_src_size, a_ext, src_size, addr);
            x64_int2flt_r_r_funcs[dst_kind](builder, xbblock, r, smallest_src_size, a_ext);
        }
        // Otherwise, just convert src to a fp.
        else {
            x64_int2flt_r_m_funcs[dst_kind](builder, xbblock, r, src_size, addr);
        }
    }
    else {
        u32 a = X64_get_lir_reg(builder, ir_a.reg, X64_REG_CLASS_INT);

        // If src is < 4 bytes, extend it to 4 bytes first.
        if (src_size < smallest_src_size) {
            u32 a_ext = X64_next_lir_reg(builder, X64_REG_CLASS_INT);
            bool is_signed = int_kind_signed[src_kind];

            x64_movxx_r_r_funcs[is_signed](builder, xbblock, smallest_src_size, a_ext, src_size, a);
            x64_int2flt_r_r_funcs[dst_kind](builder, xbblock, r, smallest_src_size, a_ext);
        }
        else {
            x64_int2flt_r_r_funcs[dst_kind](builder, xbblock, r, src_size, a);
        }
    }

    return false;
}

static bool X64_convert_limm_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr, IR_Instr* next_ir_instr)
{
    (void)next_ir_instr;

    // EX: r = 10
    //
    // mov r, 10

    IR_InstrLImm* ir_limm = (IR_InstrLImm*)ir_instr;
    size_t size = ir_limm->type->size;
    u32 r = X64_get_lir_reg(builder, ir_limm->r, X64_REG_CLASS_INT);
    X64_emit_instr_mov_r_i(builder, xbblock, size, r, ir_limm->imm);

    return false;
}

static const X64_LIRCreateFunc x64_convert_ir_funcs[IR_INSTR_KIND_COUNT] = {
    [IR_InstrIntAdd_KIND] = X64_convert_int_add_instr,
    [IR_InstrIntSub_KIND] = X64_convert_int_sub_instr,
    [IR_InstrIntMul_KIND] = X64_convert_int_mul_instr,
    [IR_InstrAnd_KIND] = X64_convert_and_instr,
    [IR_InstrOr_KIND] = X64_convert_or_instr,
    [IR_InstrXor_KIND] = X64_convert_xor_instr,
    [IR_InstrMod_KIND] = X64_convert_mod_instr,
    [IR_InstrDivMod_KIND] = X64_convert_divmod_instr,
    [IR_InstrIntDiv_KIND] = X64_convert_int_div_instr,
    [IR_InstrFltAdd_KIND] = X64_convert_flt_add_instr,
    [IR_InstrFltSub_KIND] = X64_convert_flt_sub_instr,
    [IR_InstrFltMul_KIND] = X64_convert_flt_mul_instr,
    [IR_InstrFltDiv_KIND] = X64_convert_flt_div_instr,
    [IR_InstrSar_KIND] = X64_convert_sar_instr,
    [IR_InstrShl_KIND] = X64_convert_shl_instr,
    [IR_InstrNot_KIND] = X64_convert_neg_instr,
    [IR_InstrNeg_KIND] = X64_convert_not_instr,
    [IR_InstrTrunc_KIND] = X64_convert_trunc_instr,
    [IR_InstrZExt_KIND] = X64_convert_zext_instr,
    [IR_InstrSExt_KIND] = X64_convert_sext_instr,
    [IR_InstrFlt2Flt_KIND] = X64_convert_flt2flt_instr,
    [IR_InstrFlt2Int_KIND] = X64_convert_flt2int_instr,
    [IR_InstrInt2Flt_KIND] = X64_convert_int2flt_instr,
    [IR_InstrLImm_KIND] = X64_convert_limm_instr,
    [IR_InstrLoad_KIND] = X64_fun,
    [IR_InstrLAddr_KIND] = X64_fun,
    [IR_InstrStore_KIND] = X64_fun,
    [IR_InstrIntCmp_KIND] = X64_fun,
    [IR_InstrFltCmp_KIND] = X64_fun,
    [IR_InstrJmp_KIND] = X64_fun,
    [IR_InstrCondJmp_KIND] = X64_fun,
    [IR_InstrRet_KIND] = X64_fun,
    [IR_InstrCall_KIND] = X64_fun,
    [IR_InstrCallIndirect_KIND] = X64_fun,
    [IR_InstrMemcpy_KIND] = X64_fun,
    [IR_InstrMemset_KIND] = X64_fun,
    [IR_InstrSyscall_KIND] = X64_fun,
    [IR_InstrPhi_KIND] = X64_fun,
};

static void X64_convert_ir_ret_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_InstrRet* ir_instr)
{
    Type* ret_type = ir_instr->val.type;

    u32 ax = X64_LIR_REG_COUNT;
    u32 dx = X64_LIR_REG_COUNT;

    if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
        if (type_is_obj_like(ret_type)) {
            X64_MemAddr obj_addr;
            X64_get_lir_addr(builder, xbblock, &obj_addr, &ir_instr->val.addr, (1 << X64_RDI));

            if (X64_is_obj_retarg_large(ret_type->size)) { // Large obj
                // Copy object to the address provided to the procedure.

                X64_MemAddr dst_addr_loc = {
                    // Provided result addr is assumed to be spilled into the first stack slot.
                    .kind = X64_ADDR_SIBD,
                    .sibd = {.base_reg = builder->lreg_rbp, .disp = -PTR_SIZE, .index_reg = X64_LIR_REG_COUNT}};

                u32 rdi = X64_def_phys_reg(builder, X64_RDI);
                X64_emit_instr_mov_r_m(builder, xbblock, PTR_SIZE, rdi, dst_addr_loc);

                u32 rsi = X64_def_phys_reg(builder, X64_RSI);
                X64_emit_instr_lea(builder, xbblock, rsi, obj_addr);

                Scalar num_bytes = {.as_int._u64 = ret_type->size};
                u32 rcx = X64_def_phys_reg(builder, X64_RCX);
                X64_emit_instr_mov_r_i(builder, xbblock, X64_MAX_INT_REG_SIZE, rcx, num_bytes);

                X64_emit_instr_rep_movsb(builder, xbblock, rdi, rsi, rcx);

                // Move provided addr into rax.
                X64_Reg ret_reg = (*x64_target.ret_regs)[X64_REG_CLASS_INT].regs[0];
                ax = X64_def_phys_reg(builder, ret_reg);
                X64_emit_instr_mov_r_r(builder, xbblock, PTR_SIZE, ax, rdi);
            }
            else { // Small obj
                X64_RegClass reg_class = X64_obj_reg_class(ret_type);
                X64_ScratchRegs ret_regs = (*x64_target.ret_regs)[reg_class];

                // Copy first 8 bytes of obj to rax/xmm0.
                ax = X64_def_phys_reg(builder, ret_regs.regs[0]);

                if (reg_class == X64_REG_CLASS_INT) {
                    X64_emit_instr_mov_r_m(builder, xbblock, X64_MAX_INT_REG_SIZE, ax, obj_addr);
                }
                else {
                    assert(reg_class == X64_REG_CLASS_FLOAT);
                    X64_emit_instr_mov_flt_r_m(builder, xbblock, FLOAT_F64, ax, obj_addr);
                }

                if (ret_type->size > X64_MAX_INT_REG_SIZE) {
                    // Copy second 8 bytes of obj to rdx/xmm1.
                    // TODO: Mask off extra copy amount (if obj size < 16 bytes)
                    X64_MemAddr obj_high_addr = obj_addr;
                    obj_high_addr.sibd.disp += X64_MAX_INT_REG_SIZE;

                    dx = X64_def_phys_reg(builder, ret_regs.regs[1]);

                    if (reg_class == X64_REG_CLASS_INT) {
                        X64_emit_instr_mov_r_m(builder, xbblock, X64_MAX_INT_REG_SIZE, dx, obj_high_addr);
                    }
                    else {
                        assert(reg_class == X64_REG_CLASS_FLOAT);
                        X64_emit_instr_mov_flt_r_m(builder, xbblock, FLOAT_F64, dx, obj_high_addr);
                    }
                }
            }
        }
        else {
            X64_RegClass reg_class = ret_type->kind == TYPE_FLOAT ? X64_REG_CLASS_FLOAT : X64_REG_CLASS_INT;
            X64_Reg ret_reg = (*x64_target.ret_regs)[reg_class].regs[0];

            u32 a = X64_get_lir_reg(builder, ir_instr->val.reg, reg_class);
            ax = X64_def_phys_reg(builder, ret_reg);

            if (reg_class == X64_REG_CLASS_INT) {
                X64_emit_instr_mov_r_r(builder, xbblock, ret_type->size, ax, a);
            }
            else {
                assert(reg_class == X64_REG_CLASS_FLOAT);
                X64_emit_instr_mov_flt_r_r(builder, xbblock, ret_type->as_float.kind, ax, a);
            }

            X64_hint_phys_reg(builder, a, ret_reg);
        }
    }

    X64_emit_instr_ret(builder, xbblock, ax, dx);
}

static u32 X64_mov_flt_op_ra_into_reg(X64_LIRBuilder* builder, X64_BBlock* xbblock, FloatKind fkind, IR_Reg ir_r, OpRA ir_a)
{
    u32 r = X64_get_lir_reg(builder, ir_r, X64_REG_CLASS_FLOAT);

    // Ex: movss r, a
    if (ir_a.is_addr) {
        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &ir_a.addr, 0);
        X64_emit_instr_mov_flt_r_m(builder, xbblock, fkind, r, addr);
    }
    else {
        u32 a = X64_get_lir_reg(builder, ir_a.reg, X64_REG_CLASS_FLOAT);
        X64_hint_same_reg(builder, r, a);
        X64_emit_instr_mov_flt_r_r(builder, xbblock, fkind, r, a);
    }

    return r;
}

static IR_Instr* X64_convert_ir_instr(X64_LIRBuilder* builder, X64_BBlock* xbblock, IR_Instr* ir_instr)
{
    IR_Instr* next_instr = ir_instr->next;

    switch (ir_instr->kind) {
    case IR_InstrFltAdd_KIND: {
        break;
    }
    case IR_InstrFltSub_KIND: {
        break;
    }
    case IR_InstrFltMul_KIND: {
        break;
    }
    case IR_InstrFltDiv_KIND: {
        break;
    }
    case IR_InstrIntAdd_KIND:
    case IR_InstrIntSub_KIND:
    case IR_InstrIntMul_KIND:
    case IR_InstrAnd_KIND:
    case IR_InstrOr_KIND:
    case IR_InstrXor_KIND: {
        break;
    }
    case IR_InstrIntDiv_KIND: {
        break;
    }
    case IR_InstrMod_KIND: {
        break;
    }
    case IR_InstrDivMod_KIND: {
        break;
    }
    case IR_InstrSar_KIND:
    case IR_InstrShl_KIND: {
        break;
    }
    case IR_InstrNeg_KIND: {
        break;
    }
    case IR_InstrNot_KIND: {
        break;
    }
    case IR_InstrTrunc_KIND: {
        break;
    }
    case IR_InstrZExt_KIND: {
        break;
    }
    case IR_InstrSExt_KIND: {
        break;
    }
    case IR_InstrFlt2Flt_KIND: {
        break;
    }
    case IR_InstrFlt2Int_KIND: {
        break;
    }
    case IR_InstrInt2Flt_KIND: {
        break;
    }
    case IR_InstrLImm_KIND: {
        break;
    }
    case IR_InstrLoad_KIND: {
        // EX: r = load(base + scale * index + disp)
        //
        // mov r, [base + scale * index + disp]

        IR_InstrLoad* ir_load = (IR_InstrLoad*)ir_instr;

        Type* type = ir_load->type;
        size_t size = type->size;

        X64_MemAddr addr = {0};
        X64_get_lir_addr(builder, xbblock, &addr, &ir_load->addr, 0);

        if (type->kind == TYPE_FLOAT) {
            u32 r = X64_get_lir_reg(builder, ir_load->r, X64_REG_CLASS_FLOAT);
            X64_emit_instr_mov_flt_r_m(builder, xbblock, type->as_float.kind, r, addr);
        }
        else {
            u32 r = X64_get_lir_reg(builder, ir_load->r, X64_REG_CLASS_INT);
            X64_emit_instr_mov_r_m(builder, xbblock, size, r, addr);
        }
        break;
    }
    case IR_InstrLAddr_KIND: {
        // EX: r = laddr(base + scale*index + disp)
        //
        // lea r, [..addr]

        IR_InstrLAddr* ir_laddr = (IR_InstrLAddr*)ir_instr;

        X64_MemAddr addr;
        X64_get_lir_addr(builder, xbblock, &addr, &ir_laddr->addr, 0);

        u32 r = X64_get_lir_reg(builder, ir_laddr->r, X64_REG_CLASS_INT);
        X64_emit_instr_lea(builder, xbblock, r, addr);
        break;
    }
    case IR_InstrStore_KIND: {
        // EX: $addr = a
        //
        // mov [..addr], a

        IR_InstrStore* ir_store = (IR_InstrStore*)ir_instr;

        Type* type = ir_store->type;
        size_t size = type->size;
        OpRI ir_a = ir_store->a;

        X64_MemAddr addr;
        X64_get_lir_addr(builder, xbblock, &addr, &ir_store->addr, 0);

        if (type->kind == TYPE_FLOAT) {
            assert(!ir_a.is_imm);
            u32 a = X64_get_lir_reg(builder, ir_a.reg, X64_REG_CLASS_FLOAT);
            X64_emit_instr_mov_flt_m_r(builder, xbblock, type->as_float.kind, addr, a);
        }
        else if (ir_a.is_imm) {
            X64_emit_instr_mov_m_i(builder, xbblock, size, addr, ir_a.imm);
        }
        else {
            u32 a = X64_get_lir_reg(builder, ir_a.reg, X64_REG_CLASS_INT);
            X64_emit_instr_mov_m_r(builder, xbblock, size, addr, a);
        }
        break;
    }
    case IR_InstrFltCmp_KIND: {
        IR_InstrFltCmp* ir_flt_cmp = (IR_InstrFltCmp*)ir_instr;

        FloatKind fkind = ir_flt_cmp->fkind;
        IR_Reg ir_a = ir_flt_cmp->a;
        OpRA ir_b = ir_flt_cmp->b;

        u32 a = X64_get_lir_reg(builder, ir_a, X64_REG_CLASS_FLOAT);

        if (ir_b.is_addr) {
            X64_MemAddr b = {0};
            X64_get_lir_addr(builder, xbblock, &b, &ir_b.addr, 0);
            X64_emit_instr_flt_cmp_r_m(builder, xbblock, fkind, a, b);
        }
        else {
            u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_FLOAT);
            X64_emit_instr_flt_cmp_r_r(builder, xbblock, fkind, a, b);
        }

        bool combine_next =
            next_instr && (next_instr->kind == IR_InstrCondJmp_KIND) && (((IR_InstrCondJmp*)next_instr)->a == ir_flt_cmp->r);
        ConditionKind float_cond = flt_cond_map[ir_flt_cmp->cond];

        if (combine_next) {
            // Combine this comparison instruction with the next conditional jump.
            //
            // EX: r = a <cond> b
            //     cond_jmp r, <target>
            //
            //     BECOMES:
            //
            //     cmp a, b
            //     jmp_<cond> <target>

            X64_emit_instr_jmpcc(builder, xbblock, float_cond, NULL, NULL);
            ir_instr = next_instr;
        }
        else {
            // EX: r = a <cond> b
            //
            // cmp a, b
            // set_<cond> r

            u32 r = X64_get_lir_reg(builder, ir_flt_cmp->r, X64_REG_CLASS_INT);
            X64_emit_instr_setcc(builder, xbblock, float_cond, r);
        }

        break;
    }
    case IR_InstrIntCmp_KIND: {
        IR_InstrIntCmp* ir_int_cmp = (IR_InstrIntCmp*)ir_instr;

        size_t size = ir_int_cmp->type->size;
        OpRIA ir_a = ir_int_cmp->a;
        OpRIA ir_b = ir_int_cmp->b;

        assert(ir_a.kind != OP_RIA_IMM || ir_b.kind != OP_RIA_IMM); // Only one should be an immediate.

        if (ir_a.kind == OP_RIA_IMM) {
            u32 a = X64_next_lir_reg(builder, X64_REG_CLASS_INT);
            X64_emit_instr_mov_r_i(builder, xbblock, size, a, ir_a.imm);

            // cmp r, r
            if (ir_b.kind == OP_RIA_REG) {
                u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_INT);
                X64_emit_instr_cmp_r_r(builder, xbblock, size, a, b);
            }
            // cmp r, m
            else {
                assert(ir_b.kind == OP_RIA_ADDR);
                X64_MemAddr b = {0};
                X64_get_lir_addr(builder, xbblock, &b, &ir_b.addr, 0);
                X64_emit_instr_cmp_r_m(builder, xbblock, size, a, b);
            }
        }
        else if (ir_a.kind == OP_RIA_REG) {
            u32 a = X64_get_lir_reg(builder, ir_a.reg, X64_REG_CLASS_INT);

            // cmp r, imm
            if (ir_b.kind == OP_RIA_IMM) {
                X64_emit_instr_cmp_r_i(builder, xbblock, size, a, ir_b.imm);
            }
            // cmp r, r
            else if (ir_b.kind == OP_RIA_REG) {
                u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_INT);
                X64_emit_instr_cmp_r_r(builder, xbblock, size, a, b);
            }
            // cmp r, m
            else {
                assert(ir_b.kind == OP_RIA_ADDR);
                X64_MemAddr b = {0};
                X64_get_lir_addr(builder, xbblock, &b, &ir_b.addr, 0);
                X64_emit_instr_cmp_r_m(builder, xbblock, size, a, b);
            }
        }
        else {
            assert(ir_a.kind == OP_RIA_ADDR);
            X64_MemAddr a = {0};
            X64_get_lir_addr(builder, xbblock, &a, &ir_a.addr, 0);

            // cmp m, imm
            if (ir_b.kind == OP_RIA_IMM) {
                X64_emit_instr_cmp_m_i(builder, xbblock, size, a, ir_b.imm);
            }
            // cmp m, r
            else if (ir_b.kind == OP_RIA_REG) {
                u32 b = X64_get_lir_reg(builder, ir_b.reg, X64_REG_CLASS_INT);
                X64_emit_instr_cmp_m_r(builder, xbblock, size, a, b);
            }
            else {
                assert(ir_b.kind == OP_RIA_ADDR);
                X64_MemAddr b_addr = {0};
                X64_get_lir_addr(builder, xbblock, &b_addr, &ir_b.addr, 0);

                u32 b = X64_next_lir_reg(builder, X64_REG_CLASS_INT);
                X64_emit_instr_mov_r_m(builder, xbblock, size, b, b_addr);
                X64_emit_instr_cmp_m_r(builder, xbblock, size, a, b);
            }
        }

        bool combine_next =
            next_instr && (next_instr->kind == IR_InstrCondJmp_KIND) && (((IR_InstrCondJmp*)next_instr)->a == ir_int_cmp->r);

        if (combine_next) {
            // Combine this comparison instruction with the next conditional jump.
            //
            // EX: r = a <cond> b
            //     cond_jmp r, <target>
            //
            //     BECOMES:
            //
            //     cmp a, b
            //     jmp_<cond> <target>

            X64_emit_instr_jmpcc(builder, xbblock, ir_int_cmp->cond, NULL, NULL);
            ir_instr = next_instr;
        }
        else {
            // EX: r = a <cond> b
            //
            // cmp a, b
            // set_<cond> r

            u32 r = X64_get_lir_reg(builder, ir_int_cmp->r, X64_REG_CLASS_INT);
            X64_emit_instr_setcc(builder, xbblock, ir_int_cmp->cond, r);
        }

        break;
    }
    case IR_InstrJmp_KIND: {
        X64_emit_instr_jmp(builder, xbblock, NULL);
        assert(!next_instr);
        break;
    }
    case IR_InstrCondJmp_KIND: {
        // EX: cond_jmp a, <target>
        //
        //     BECOMES
        //
        //     cmp a, 0
        //     jne <target>

        IR_InstrCondJmp* ir_cond_jmp = (IR_InstrCondJmp*)ir_instr;

        u32 a = X64_get_lir_reg(builder, ir_cond_jmp->a, X64_REG_CLASS_INT);
        Scalar zero = {0};

        X64_emit_instr_cmp_r_i(builder, xbblock, 1, a, zero);
        X64_emit_instr_jmpcc(builder, xbblock, COND_NEQ, NULL, NULL);
        assert(!next_instr);
        break;
    }
    case IR_InstrPhi_KIND: {
        IR_InstrPhi* ir_phi = (IR_InstrPhi*)ir_instr;

        // For now, just force all registers in PHI instruction into the same physical register.
        X64_RegClass reg_class = ir_phi->type->kind == TYPE_FLOAT ? X64_REG_CLASS_FLOAT : X64_REG_CLASS_INT;

        u32 r = X64_get_lir_reg(builder, ir_phi->r, reg_class);
        size_t num_args = ir_phi->num_args;
        PhiArg* args = ir_phi->args;

        for (size_t i = 0; i < num_args; i++) {
            u32 x = X64_get_lir_reg(builder, args[i].ireg, reg_class);
            X64_alias_lir_regs(builder, x, r);
        }
        break;
    }
    case IR_InstrMemcpy_KIND: {
        // memcpy(dst, src, size)
        //
        // BECOMES
        //
        // lea rdi, [..dst]
        // lea rsi, [..src]
        // mov rcx, size
        // rep movsb
        IR_InstrMemcpy* ir_memcpy = (IR_InstrMemcpy*)ir_instr;
        X64_MemAddr dst_addr;
        X64_get_lir_addr(builder, xbblock, &dst_addr, &ir_memcpy->dst, 0);

        X64_MemAddr src_addr;
        X64_get_lir_addr(builder, xbblock, &src_addr, &ir_memcpy->src, (1 << X64_RDI));

        X64_emit_memcpy(builder, xbblock, dst_addr, src_addr, ir_memcpy->size);
        break;
    }
    case IR_InstrMemset_KIND: {
        // memset(dst, value, size)
        //
        // BECOMES
        //
        // lea rdi, [..dst]
        // mov al, value
        // mov rcx, size
        // rep stosb
        IR_InstrMemset* ir_memset = (IR_InstrMemset*)ir_instr;
        X64_MemAddr dst_addr;
        X64_get_lir_addr(builder, xbblock, &dst_addr, &ir_memset->dst, 0);

        X64_emit_memset(builder, xbblock, dst_addr, ir_memset->value, ir_memset->size);
        break;
    }
    case IR_InstrSyscall_KIND: {
        // res = syscall6(nr, arg1, arg2, arg3, arg4, arg5, arg6);
        //
        // BECOMES
        //
        // mov rax, {nr}
        // mov rdi, {arg1}
        // mov rsi, {arg2}
        // mov rdx, {arg3}
        // mov r10, {arg4}
        // mov r8, {arg5}
        // mov r9, {arg6}
        //
        // syscall
        // mov {res}, rax
        //
        // CLOBBERS rcx and r11
        //

        IR_InstrSyscall* ir_syscall = (IR_InstrSyscall*)ir_instr;

        u32 rax = X64_def_phys_reg(builder, X64_RAX);
        X64_load_op_ria(builder, xbblock, X64_MAX_INT_REG_SIZE, rax, ir_syscall->nr);

        X64_Reg syscall_arg_regs[6] = {X64_RDI, X64_RSI, X64_RDX, X64_R10, X64_R8, X64_R9};
        u32 lir_args[6] = {0};
        u8 num_args = ir_syscall->count;

        for (u8 i = 0; i < num_args; i += 1) {
            lir_args[i] = X64_def_phys_reg(builder, syscall_arg_regs[i]);
            X64_load_op_ria(builder, xbblock, X64_MAX_INT_REG_SIZE, lir_args[i], ir_syscall->args[i]);
        }

        u32 rcx = X64_def_phys_reg(builder, X64_RCX);
        u32 r11 = X64_def_phys_reg(builder, X64_R11);
        X64_emit_instr_syscall(builder, xbblock, rax, num_args, lir_args, rcx, r11);

        u32 r = X64_get_lir_reg(builder, ir_syscall->r, X64_REG_CLASS_INT);
        X64_hint_same_reg(builder, r, rax);
        X64_emit_instr_mov_r_r(builder, xbblock, X64_MAX_INT_REG_SIZE, r, rax);

        break;
    }
    case IR_InstrRet_KIND: {
        // EX: ret a
        //
        //     BECOMES (for primitive return types)
        //
        //     mov _ax, a  ; if `a` not in `ax`
        //     ret

        X64_convert_ir_ret_instr(builder, xbblock, (IR_InstrRet*)ir_instr);
        break;
    }
    case IR_InstrCallIndirect_KIND:
    case IR_InstrCall_KIND: {
        u32 num_args;
        IR_Value* args;
        Type* proc_type;
        IR_Value ir_r;

        if (ir_instr->kind == IR_InstrCall_KIND) {
            IR_InstrCall* ir_call = (IR_InstrCall*)ir_instr;
            num_args = ir_call->num_args;
            args = ir_call->args;
            proc_type = ir_call->sym->type;
            ir_r = ir_call->r;
        }
        else {
            IR_InstrCallIndirect* ir_calli = (IR_InstrCallIndirect*)ir_instr;
            num_args = ir_calli->num_args;
            args = ir_calli->args;
            proc_type = ir_calli->proc_type;
            ir_r = ir_calli->r;
        }

        Type* ret_type = proc_type->as_proc.ret;
        X64_InstrCallArg* x64_args = alloc_array(builder->arena, X64_InstrCallArg, num_args, true);
        X64_StackArgsInfo stack_info = X64_convert_call_args(builder, xbblock, ret_type, num_args, args, x64_args);

        X64_CallValue r = {0};

        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            if (type_is_obj_like(ret_type)) {
                // The address for the obj assigned to the proc's return value should not use any
                // integer return registers (i.e., rax & rdx).
                X64_get_lir_addr(builder, xbblock, &r.addr, &ir_r.addr, x64_target.ret_reg_mask);
            }
            else {
                X64_RegClass reg_class = ret_type->kind == TYPE_FLOAT ? X64_REG_CLASS_FLOAT : X64_REG_CLASS_INT;

                r.reg = X64_get_lir_reg(builder, ir_r.reg, reg_class);
            }
        }

        X64_Instr* instr;

        if (ir_instr->kind == IR_InstrCall_KIND) {
            IR_InstrCall* ir_call = (IR_InstrCall*)ir_instr;
            instr = X64_emit_instr_call(builder, xbblock, ir_call->sym, r, num_args, x64_args, stack_info);
        }
        else {
            IR_InstrCallIndirect* ir_calli = (IR_InstrCallIndirect*)ir_instr;
            u32 proc_r = X64_get_lir_reg(builder, ir_calli->loc, X64_REG_CLASS_INT);
            instr = X64_emit_instr_call_r(builder, xbblock, proc_type, proc_r, r, num_args, x64_args, stack_info);
        }

        X64_add_call_site(builder, instr);

        break;
    }
    default:
        NIBBLE_FATAL_EXIT("[INTERNAL ERROR]: Unable to convert IR instruction %d to an X64 LIR instruction", ir_instr->kind);
        break;
    }

    return ir_instr->next;
}

static X64_BBlock* X64_make_bblock(X64_LIRBuilder* builder, BBlock* bblock)
{
    X64_BBlock* xbblock = alloc_type(builder->arena, X64_BBlock, true);
    xbblock->id = bblock->id;
    xbblock->flags = bblock->flags;

    for (IR_Instr* it = bblock->first; it;) {
        it = X64_convert_ir_instr(builder, xbblock, it);
    }

    return xbblock;
}

static void X64_emit_lir_instrs(X64_LIRBuilder* builder, size_t num_iregs, size_t num_bblocks, BBlock** bblocks)
{
    //
    // Initialize aux data structures for LIR
    //

    // Data structures used to alias PHI registers.
    builder->lreg_ranges = array_create(builder->arena, X64_LRegRange, 16);
    builder->lreg_aliases = array_create(builder->arena, u32, 16);
    builder->lreg_sizes = array_create(builder->arena, u32, 16);

    builder->call_sites = array_create(builder->arena, X64_Instr*, 8);

    // Array of X64 basic blocks. Basic blocks will be inserted in sorted order.
    builder->num_bblocks = num_bblocks;
    builder->bblocks = alloc_array(builder->arena, X64_BBlock*, num_bblocks, true);

    // Array that maps an IR register to a LIR register.
    builder->reg_map = alloc_array(builder->arena, u32, num_iregs, false);
    memset(builder->reg_map, 0xFF, num_iregs * sizeof(u32));

    // Create an LIR register for the X64 RBP register.
    builder->lreg_rbp = X64_def_phys_reg(builder, X64_RBP);
    builder->stack_reg_mask = x64_target.scratch_reg_mask & (~(x64_target.arg_reg_mask));

    // Create X64 basic blocks.
    for (size_t ii = 0; ii < num_bblocks; ii++) {
        assert(bblocks[ii]->id == (long)ii);
        builder->bblocks[ii] = X64_make_bblock(builder, bblocks[ii]);
    }

    // Connect X64 basic blocks.
    for (size_t ii = 0; ii < num_bblocks; ii++) {
        BBlock* bb = bblocks[ii];
        X64_BBlock* xbb = builder->bblocks[ii];

        assert(bb->id == xbb->id);

        IR_Instr* instr = bb->last;
        X64_Instr* xinstr = xbb->last;

        if (instr->kind == IR_InstrJmp_KIND) {
            assert(xinstr->kind == X64_InstrJmp_KIND);

            BBlock* n = ((IR_InstrJmp*)instr)->target;
            X64_BBlock* xn = builder->bblocks[n->id];

            xinstr->jmp.target = xn;
        }
        else if (instr->kind == IR_InstrCondJmp_KIND) {
            assert(xinstr->kind == X64_InstrJmpCC_KIND);

            BBlock* n_false = ((IR_InstrCondJmp*)instr)->false_bb;
            X64_BBlock* xn_false = builder->bblocks[n_false->id];
            assert(n_false->id == xn_false->id);
            xinstr->jmpcc.false_bb = xn_false;

            BBlock* n_true = ((IR_InstrCondJmp*)instr)->true_bb;
            X64_BBlock* xn_true = builder->bblocks[n_true->id];
            assert(n_true->id == xn_true->id);
            xinstr->jmpcc.true_bb = xn_true;
        }
    }
}
