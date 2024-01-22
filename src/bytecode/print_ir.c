#include "bytecode/module.h"
#include "cstring.h"

static const char* ir_cond_names[] = {
    [COND_U_LT] = "u<", [COND_S_LT] = "s<",    [COND_U_LTEQ] = "u<=", [COND_S_LTEQ] = "s<=", [COND_U_GT] = "u>",
    [COND_S_GT] = "s>", [COND_U_GTEQ] = "u>=", [COND_S_GTEQ] = "s>=", [COND_EQ] = "==",      [COND_NEQ] = "!=",
};

static char* IR_print_imm(Allocator* arena, Scalar imm)
{
    char* dstr = array_create(arena, char, 16);

    ftprint_char_array(&dstr, true, "0x%lx", imm.as_int._u64);

    return dstr;
}

static char* IR_print_reg(Allocator* arena, IR_Reg reg)
{
    char* dstr = NULL;

    if (reg < IR_REG_COUNT) {
        dstr = array_create(arena, char, 8);
        ftprint_char_array(&dstr, true, "r%d", reg);
    }
    else {
        dstr = array_create(arena, char, 2);
        array_push(dstr, '\0');
    }

    return dstr;
}

static char* IR_print_regimm(Allocator* arena, OpRI regimm)
{
    if (regimm.is_imm) {
        return IR_print_imm(arena, regimm.imm);
    }
    else {
        return IR_print_reg(arena, regimm.reg);
    }
}

static char* IR_print_mem(Allocator* arena, MemAddr* addr)
{
    char* dstr = array_create(arena, char, 16);

    bool has_base = addr->base_kind != MEM_BASE_NONE;
    bool has_index = addr->scale && (addr->index_reg < IR_REG_COUNT);
    bool has_disp = addr->disp != 0;

    assert(has_base || has_index);

    ftprint_char_array(&dstr, false, "[");

    if (has_base) {
        if (addr->base_kind == MEM_BASE_REG) {
            ftprint_char_array(&dstr, false, "%s", IR_print_reg(arena, addr->base.reg));
        }
        else if (addr->base_kind == MEM_BASE_MEM_OBJ) {
            MemObj* mem_obj = addr->base.obj;

            while (mem_obj->kind == MEM_OBJ_ALIAS) {
                mem_obj = mem_obj->alias;
            }

            if (mem_obj->kind == MEM_OBJ_SYM) {
                const bool is_local = mem_obj->sym->is_local;
                const char* locality = "local";
                const char* sym_name = mem_obj->sym->name->str;

                if (!is_local) {
                    locality = "global";
                    sym_name = symbol_mangled_name(arena, mem_obj->sym);
                }

                ftprint_char_array(&dstr, false, "%s %s", locality, sym_name);
            }
            else if (mem_obj->kind == MEM_OBJ_ANON_OBJ) {
                ftprint_char_array(&dstr, false, "obj %d", mem_obj->anon_obj->id);
            }
        }
        else if (addr->base_kind == MEM_BASE_FLOAT_LIT) {
            if (addr->base.float_lit->kind == FLOAT_F64) {
                ftprint_char_array(&dstr, false, "%f", addr->base.float_lit->value._f64);
            }
            else {
                ftprint_char_array(&dstr, false, "%f", addr->base.float_lit->value._f32);
            }
        }
        else {
            assert(addr->base_kind == MEM_BASE_STR_LIT);
            ftprint_char_array(&dstr, false, "\"%s\"", cstr_escape(arena, addr->base.str_lit->str, addr->base.str_lit->len, 0));
        }

        if (has_index) {
            char* index_reg_name = IR_print_reg(arena, addr->index_reg);

            if (has_disp)
                ftprint_char_array(&dstr, false, " + %d*%s + %d", addr->scale, index_reg_name, (s32)addr->disp);
            else
                ftprint_char_array(&dstr, false, " + %d*%s", addr->scale, index_reg_name);
        }
        else if (has_disp) {
            ftprint_char_array(&dstr, false, " + %d", (s32)addr->disp);
        }
    }
    else {
        char* index_reg_name = IR_print_reg(arena, addr->index_reg);

        if (has_disp)
            ftprint_char_array(&dstr, false, "%d*%s + %d", addr->scale, index_reg_name, (s32)addr->disp);
        else
            ftprint_char_array(&dstr, false, "%d*%s", addr->scale, index_reg_name);
    }

    ftprint_char_array(&dstr, true, "]");

    return dstr;
}

static char* IR_print_op_ria(Allocator* arena, OpRIA* ria)
{
    switch (ria->kind) {
    case OP_RIA_IMM:
        return IR_print_imm(arena, ria->imm);
    case OP_RIA_REG:
        return IR_print_reg(arena, ria->reg);
    case OP_RIA_ADDR:
        return IR_print_mem(arena, &ria->addr);
    default:
        NIBBLE_FATAL_EXIT("IR_print_op_ria() - Unexpected OpRIAKind %d", ria->kind);
    }

    return NULL;
}

static char* IR_print_op_ra(Allocator* arena, OpRA* ra)
{
    if (ra->is_addr) {
        return IR_print_mem(arena, &ra->addr);
    }

    return IR_print_reg(arena, ra->reg);
}

char* IR_print_instr(Allocator* arena, IR_Instr* instr)
{
    static const char* binary_int_kind_name[] = {
        [IR_InstrIntAdd_KIND] = "iadd", [IR_InstrIntSub_KIND] = "isub", [IR_InstrIntMul_KIND] = "imul", [IR_InstrIntDiv_KIND] = "idiv",
        [IR_InstrMod_KIND] = "mod",     [IR_InstrAnd_KIND] = "and",     [IR_InstrOr_KIND] = "or",       [IR_InstrXor_KIND] = "xor"};
    static const char* binary_flt_kind_name[] = {[IR_InstrFltAdd_KIND] = "fadd",
                                                 [IR_InstrFltSub_KIND] = "fsub",
                                                 [IR_InstrFltMul_KIND] = "fmul",
                                                 [IR_InstrFltDiv_KIND] = "fdiv"};
    static const char* shift_kind_name[] = {[IR_InstrSar_KIND] = "sar", [IR_InstrShl_KIND] = "shl"};
    static const char* unary_kind_name[] = {[IR_InstrNot_KIND] = "not", [IR_InstrNeg_KIND] = "neg"};
    static const char* convert_kind_name[] =
        {[IR_InstrTrunc_KIND] = "trunc", [IR_InstrZExt_KIND] = "zext", [IR_InstrSExt_KIND] = "sext"};
    char* dstr = array_create(arena, char, 16);

    switch (instr->kind) {
    case IR_InstrIntAdd_KIND: {
        IR_InstrIntAdd* i = (IR_InstrIntAdd*)instr;

        const char* op_name = binary_int_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrIntSub_KIND: {
        IR_InstrIntSub* i = (IR_InstrIntSub*)instr;

        const char* op_name = binary_int_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrIntMul_KIND: {
        IR_InstrIntMul* i = (IR_InstrIntMul*)instr;

        const char* op_name = binary_int_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrIntDiv_KIND: {
        IR_InstrIntDiv* i = (IR_InstrIntDiv*)instr;

        const char* op_name = binary_int_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrMod_KIND: {
        IR_InstrMod* i = (IR_InstrMod*)instr;

        const char* op_name = binary_int_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrAnd_KIND: {
        IR_InstrAnd* i = (IR_InstrAnd*)instr;

        const char* op_name = binary_int_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }

    case IR_InstrOr_KIND: {
        IR_InstrOr* i = (IR_InstrOr*)instr;

        const char* op_name = binary_int_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrXor_KIND: {
        IR_InstrXor* i = (IR_InstrXor*)instr;

        const char* op_name = binary_int_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrFltAdd_KIND: {
        IR_InstrFltAdd* i = (IR_InstrFltAdd*)instr;
        const char* op_name = binary_flt_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, float_kind_names[i->fkind], IR_print_reg(arena, i->r),
                           IR_print_op_ra(arena, &i->a), IR_print_op_ra(arena, &i->b));
        break;
    }
    case IR_InstrFltSub_KIND: {
        IR_InstrFltSub* i = (IR_InstrFltSub*)instr;
        const char* op_name = binary_flt_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, float_kind_names[i->fkind], IR_print_reg(arena, i->r),
                           IR_print_op_ra(arena, &i->a), IR_print_op_ra(arena, &i->b));
        break;
    }
    case IR_InstrFltMul_KIND: {
        IR_InstrFltMul* i = (IR_InstrFltMul*)instr;
        const char* op_name = binary_flt_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, float_kind_names[i->fkind], IR_print_reg(arena, i->r),
                           IR_print_op_ra(arena, &i->a), IR_print_op_ra(arena, &i->b));
        break;
    }
    case IR_InstrFltDiv_KIND: {
        IR_InstrFltDiv* i = (IR_InstrFltDiv*)instr;
        const char* op_name = binary_flt_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, float_kind_names[i->fkind], IR_print_reg(arena, i->r),
                           IR_print_op_ra(arena, &i->a), IR_print_op_ra(arena, &i->b));
        break;
    }
    case IR_InstrSar_KIND: {
        IR_InstrSar* i = (IR_InstrSar*)instr;
        const char* op_name = shift_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrShl_KIND: {
        IR_InstrShl* i = (IR_InstrShl*)instr;

        const char* op_name = shift_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrDivMod_KIND: {
        IR_InstrDivMod* i = (IR_InstrDivMod*)instr;

        ftprint_char_array(&dstr, false, "divmod <%s> %s, %s, %s, %s", type_name(i->type), IR_print_reg(arena, i->q),
                           IR_print_reg(arena, i->r), IR_print_op_ria(arena, &i->a), IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrPhi_KIND: {
        IR_InstrPhi* phi = (IR_InstrPhi*)instr;

        ftprint_char_array(&dstr, false, "phi <%s> %s, ", type_name(phi->type), IR_print_reg(arena, phi->r));

        size_t n = phi->num_args;

        for (size_t i = 0; i < n; i++) {
            PhiArg* arg = phi->args + i;

            ftprint_char_array(&dstr, false, "B.%u %s%s", arg->bblock->id, IR_print_reg(arena, arg->ireg), (i == n - 1 ? "" : ", "));
        }
        break;
    }
    case IR_InstrNeg_KIND: {
        IR_InstrNeg* i = (IR_InstrNeg*)instr;

        const char* op_name = unary_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_reg(arena, i->a));
        break;
    }
    case IR_InstrNot_KIND: {
        IR_InstrNot* i = (IR_InstrNot*)instr;

        const char* op_name = unary_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s", op_name, type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_reg(arena, i->a));
        break;
    }
    case IR_InstrTrunc_KIND: {
        IR_InstrTrunc* i = (IR_InstrTrunc*)instr;

        const char* op_name = convert_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, <%s> %s", op_name, type_name(i->dst_type), IR_print_reg(arena, i->r),
                           type_name(i->src_type), IR_print_reg(arena, i->a));
        break;
    }
    case IR_InstrZExt_KIND: {
        IR_InstrZExt* i = (IR_InstrZExt*)instr;

        const char* op_name = convert_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, <%s> %s", op_name, type_name(i->dst_type), IR_print_reg(arena, i->r),
                           type_name(i->src_type), IR_print_reg(arena, i->a));
        break;
    }
    case IR_InstrSExt_KIND: {
        IR_InstrSExt* i = (IR_InstrSExt*)instr;

        const char* op_name = convert_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, <%s> %s", op_name, type_name(i->dst_type), IR_print_reg(arena, i->r),
                           type_name(i->src_type), IR_print_reg(arena, i->a));
        break;
    }
    case IR_InstrFlt2Int_KIND: {
        IR_InstrFlt2Int* i = (IR_InstrFlt2Int*)instr;

        ftprint_char_array(&dstr, false, "flt2int <%s> %s, <%s> %s", int_kind_names[i->dst_kind], IR_print_reg(arena, i->dst),
                           float_kind_names[i->src_kind], IR_print_op_ra(arena, &i->src));
        break;
    }
    case IR_InstrInt2Flt_KIND: {
        IR_InstrInt2Flt* i = (IR_InstrInt2Flt*)instr;

        ftprint_char_array(&dstr, false, "int2flt <%s> %s, <%s> %s", float_kind_names[i->dst_kind], IR_print_reg(arena, i->dst),
                           int_kind_names[i->src_kind], IR_print_op_ra(arena, &i->src));
        break;
    }
    case IR_InstrFlt2Flt_KIND: {
        IR_InstrFlt2Flt* i = (IR_InstrFlt2Flt*)instr;

        ftprint_char_array(&dstr, false, "flt2flt <%s> %s, <%s> %s", float_kind_names[i->dst_kind], IR_print_reg(arena, i->dst),
                           float_kind_names[i->src_kind], IR_print_op_ra(arena, &i->src));
        break;
    }
    case IR_InstrLImm_KIND: {
        IR_InstrLImm* i = (IR_InstrLImm*)instr;

        ftprint_char_array(&dstr, false, "limm <%s> %s, %s", type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_imm(arena, i->imm));
        break;
    }
    case IR_InstrLAddr_KIND: {
        IR_InstrLAddr* i = (IR_InstrLAddr*)instr;

        ftprint_char_array(&dstr, false, "laddr %s, %s", IR_print_reg(arena, i->r), IR_print_mem(arena, &i->addr));
        break;
    }
    case IR_InstrMemcpy_KIND: {
        IR_InstrMemcpy* i = (IR_InstrMemcpy*)instr;

        ftprint_char_array(&dstr, false, "memcpy %s, %s, %s", IR_print_mem(arena, &i->dst), IR_print_mem(arena, &i->src),
                           IR_print_regimm(arena, i->size));

        break;
    }
    case IR_InstrMemset_KIND: {
        IR_InstrMemset* i = (IR_InstrMemset*)instr;

        ftprint_char_array(&dstr, false, "memset %s, %s, %s", IR_print_mem(arena, &i->dst), IR_print_regimm(arena, i->value),
                           IR_print_regimm(arena, i->size));

        break;
    }
    case IR_InstrLoad_KIND: {
        IR_InstrLoad* i = (IR_InstrLoad*)instr;

        ftprint_char_array(&dstr, false, "load <%s> %s, %s", type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_mem(arena, &i->addr));
        break;
    }
    case IR_InstrStore_KIND: {
        IR_InstrStore* i = (IR_InstrStore*)instr;

        ftprint_char_array(&dstr, false, "store <%s> %s, %s", type_name(i->type), IR_print_mem(arena, &i->addr),
                           IR_print_regimm(arena, i->a));
        break;
    }
    case IR_InstrIntCmp_KIND: {
        IR_InstrIntCmp* i = (IR_InstrIntCmp*)instr;

        ftprint_char_array(&dstr, false, "icmp <%s> %s, %s %s %s", type_name(i->type), IR_print_reg(arena, i->r),
                           IR_print_op_ria(arena, &i->a), ir_cond_names[i->cond], IR_print_op_ria(arena, &i->b));
        break;
    }
    case IR_InstrFltCmp_KIND: {
        IR_InstrFltCmp* i = (IR_InstrFltCmp*)instr;

        ftprint_char_array(&dstr, false, "fcmp <%s> %s, %s %s %s", float_kind_names[i->fkind], IR_print_reg(arena, i->r),
                           IR_print_reg(arena, i->a), ir_cond_names[i->cond], IR_print_op_ra(arena, &i->b));
        break;
    }
    case IR_InstrJmp_KIND: {
        IR_InstrJmp* i = (IR_InstrJmp*)instr;

        ftprint_char_array(&dstr, false, "jmp B.%u", i->target->id);
        break;
    }
    case IR_InstrCondJmp_KIND: {
        IR_InstrCondJmp* i = (IR_InstrCondJmp*)instr;

        ftprint_char_array(&dstr, false, "jmpcc B.%u if %s else B.%u", i->true_bb->id, IR_print_reg(arena, i->a), i->false_bb->id);
        break;
    }
    case IR_InstrSwitchCaseJmp_KIND: {
        IR_InstrSwitchCaseJmp* i = (IR_InstrSwitchCaseJmp*)instr;

        ftprint_char_array(&dstr, false, "switch (%s) { ", IR_print_op_ria(arena, &i->val));
        for (u32 k = 0; k < i->num_case_ranges; k++) {
            IR_CaseRange* case_range = &i->case_ranges[k];
            BBlock* target = i->targets[k];

            if (i->case_ranges[k].start == i->case_ranges[k].end) {
                ftprint_char_array(&dstr, false, "case %u -> B.%u ", case_range->start, target->id);
            }
            else {
                ftprint_char_array(&dstr, false, "case %u .. %u -> B.%u ", case_range->start, case_range->end, target->id);
            }
        }

        if (i->num_targets > i->num_case_ranges) {
            BBlock* target = i->targets[i->num_targets - 1];
            ftprint_char_array(&dstr, false, "default -> B.%u ", target->id);
        }
        ftprint_char_array(&dstr, false, "}");
        break;
    }
    case IR_InstrRet_KIND: {
        IR_InstrRet* i = (IR_InstrRet*)instr;
        IR_Value* val = &i->val;

        ftprint_char_array(&dstr, false, "ret <%s>", type_name(val->type));

        if (type_is_obj_like(val->type)) {
            ftprint_char_array(&dstr, false, " %s", IR_print_mem(arena, &val->addr));
        }
        else if (val->type != builtin_types[BUILTIN_TYPE_VOID].type) {
            ftprint_char_array(&dstr, false, " %s", IR_print_reg(arena, val->reg));
        }
        break;
    }
    case IR_InstrSyscall_KIND: {
        IR_InstrSyscall* act_instr = (IR_InstrSyscall*)instr;

        ftprint_char_array(&dstr, false, "syscall%u %s, (%s", act_instr->count, IR_print_reg(arena, act_instr->r),
                           IR_print_op_ria(arena, &act_instr->nr));

        for (u32 i = 0; i < act_instr->count; i += 1) {
            ftprint_char_array(&dstr, false, ", %s", IR_print_op_ria(arena, &act_instr->args[i]));
        }

        ftprint_char_array(&dstr, false, ")");
        break;
    }
    case IR_InstrCall_KIND:
    case IR_InstrCallIndirect_KIND: {
        bool is_indirect = instr->kind == IR_InstrCallIndirect_KIND;

        Type* proc_type;
        const char* proc_name;
        IR_Value r;
        u32 num_args;
        IR_Value* args;

        if (is_indirect) {
            IR_InstrCallIndirect* i = (IR_InstrCallIndirect*)instr;
            proc_type = i->proc_type;
            proc_name = IR_print_reg(arena, i->loc);
            r = i->r;
            num_args = i->num_args;
            args = i->args;
        }
        else {
            IR_InstrCall* i = (IR_InstrCall*)instr;
            proc_type = i->sym->type;
            proc_name = symbol_mangled_name(arena, i->sym);
            r = i->r;
            num_args = i->num_args;
            args = i->args;
        }

        ftprint_char_array(&dstr, false, "call ");

        Type* ret_type = proc_type->as_proc.ret;

        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            ftprint_char_array(&dstr, false, "<%s> %s, ", type_name(proc_type->as_proc.ret),
                               (type_is_obj_like(ret_type) ? IR_print_mem(arena, &r.addr) : IR_print_reg(arena, r.reg)));
        }

        ftprint_char_array(&dstr, false, "%s (", proc_name);

        if (num_args) {
            for (u32 i = 0; i < num_args; i += 1) {
                IR_Value* arg = args + i;

                ftprint_char_array(&dstr, false, "<%s> %s", type_name(arg->type),
                                   (type_is_obj_like(arg->type) ? IR_print_mem(arena, &arg->addr) : IR_print_reg(arena, arg->reg)));

                if (i != num_args - 1)
                    ftprint_char_array(&dstr, false, ", ");
            }
        }

        ftprint_char_array(&dstr, false, ")");
        break;
    }
    default:
        NIBBLE_FATAL_EXIT("UNKNOWN_INSTR %d\n", instr->kind);
    }

    array_push(dstr, '\0');

    return dstr;
}

static void IR_print_bblock(Allocator* arena, BBlock* bblock)
{
    size_t ii = 0;

    ftprint_out("B.%d:", bblock->id);
    ftprint_out((bblock->flags & BBLOCK_IS_LOOP_HDR) ? "    # Loop header\n" : "\n");

    for (IR_Instr* it = bblock->first; it; it = it->next, ii++) {
        ftprint_out("%lu\t%s\n", it->ino, IR_print_instr(arena, it));
    }

    assert(ii == bblock->num_instrs);
}

void IR_print_out_proc(Allocator* arena, Symbol* sym)
{
    if (!sym->as_proc.num_instrs) {
        return;
    }

    ftprint_out("\nproc %s:\n", symbol_mangled_name(arena, sym));
    ftprint_out("num instrs: %d\n", sym->as_proc.num_instrs);

    AllocatorState mem_state = allocator_get_state(arena);
    {
        BBlock** bblocks = sym->as_proc.bblocks;
        size_t n = array_len(bblocks);

        for (size_t i = 0; i < n; i++) {
            IR_print_bblock(arena, bblocks[i]);
        }
    }
    allocator_restore_state(mem_state);
}

static void IR_dump_bblock_dot(Allocator* arena, BBlock* bblock)
{
    size_t ii = 0;

    ftprint_out("\tB%d [", bblock->id);
    if (bblock->flags & BBLOCK_IS_LOOP_HDR) {
        ftprint_out("style=filled, color=lightgrey, label=\"B%d\\n\\n", bblock->id);
    }
    else {
        ftprint_out("label=\"B%d\\n\\n", bblock->id);
    }

    for (IR_Instr* it = bblock->first; it; it = it->next, ii++) {
        ftprint_out("%.3lu: %s\\l", it->ino, IR_print_instr(arena, it));
    }

    assert(ii == bblock->num_instrs);
    ftprint_out("\"]\n");

    IR_Instr* last_instr = bblock->last;

    if (last_instr->kind == IR_InstrJmp_KIND) {
        IR_InstrJmp* jmp = (IR_InstrJmp*)last_instr;
        ftprint_out("\tB%d -> B%d\n", bblock->id, jmp->target->id);
    }
    else if (last_instr->kind == IR_InstrCondJmp_KIND) {
        IR_InstrCondJmp* jmp = (IR_InstrCondJmp*)last_instr;
        ftprint_out("\tB%d -> B%d\n", bblock->id, jmp->true_bb->id);
        ftprint_out("\tB%d -> B%d\n", bblock->id, jmp->false_bb->id);
    }
    else if (last_instr->kind == IR_InstrSwitchCaseJmp_KIND) {
        const IR_InstrSwitchCaseJmp* jmp = (const IR_InstrSwitchCaseJmp*)last_instr;
        for (u32 i = 0; i < jmp->num_targets; i++) {
            ftprint_out("\tB%d -> B%d\n", bblock->id, jmp->targets[i]->id);
        }
    }
    else {
        assert(last_instr->kind == IR_InstrRet_KIND);
    }
}

void IR_dump_proc_dot(Allocator* arena, Symbol* sym)
{
    if (!sym->as_proc.num_instrs) {
        return;
    }

    ftprint_out("\ndigraph %s {\n\tnode [shape=box]\n", symbol_mangled_name(arena, sym));

    AllocatorState mem_state = allocator_get_state(arena);
    {
        BBlock** bblocks = sym->as_proc.bblocks;
        size_t n = array_len(bblocks);

        for (size_t i = 0; i < n; i++) {
            IR_dump_bblock_dot(arena, bblocks[i]);
        }
    }
    allocator_restore_state(mem_state);
    ftprint_out("}\n");
}
