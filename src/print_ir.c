#include "print_ir.h"
#include "cstring.h"

static const char* ir_cond_names[] = {
    [IR_COND_U_LT] = "u<", [IR_COND_S_LT] = "s<", [IR_COND_U_LTEQ] = "u<=", [IR_COND_S_LTEQ] = "s<=",
    [IR_COND_U_GT] = "u>", [IR_COND_S_GT] = "s>", [IR_COND_U_GTEQ] = "u>=", [IR_COND_S_GTEQ] = "s>=",
    [IR_COND_EQ] = "==",   [IR_COND_NEQ] = "!=",
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

static char* IR_print_mem(Allocator* arena, IR_MemAddr* addr)
{
    char* dstr = array_create(arena, char, 16);

    bool has_base = addr->base_kind != IR_MEM_BASE_NONE;
    bool has_index = addr->scale && (addr->index_reg < IR_REG_COUNT);
    bool has_disp = addr->disp != 0;

    assert(has_base || has_index);

    ftprint_char_array(&dstr, false, "[");

    if (has_base) {
        if (addr->base_kind == IR_MEM_BASE_REG)
            ftprint_char_array(&dstr, false, "%s", IR_print_reg(arena, addr->base.reg));
        else if (addr->base_kind == IR_MEM_BASE_SYM)
            ftprint_char_array(&dstr, false, "%s %s", (addr->base.sym->is_local ? "local" : "global"),
                               symbol_mangled_name(arena, addr->base.sym));
        else {
            assert(addr->base_kind == IR_MEM_BASE_STR_LIT);
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

char* IR_print_instr(Allocator* arena, IR_Instr* instr)
{
    char* dstr = array_create(arena, char, 16);

    switch (instr->kind) {
    case IR_INSTR_ADD_R_R: {
        ftprint_char_array(&dstr, false, "add <%s> %s, %s", type_name(instr->add_r_r.type),
                           IR_print_reg(arena, instr->add_r_r.dst), IR_print_reg(arena, instr->add_r_r.src));
        break;
    }
    case IR_INSTR_ADD_R_M: {
        ftprint_char_array(&dstr, false, "add <%s> %s, %s", type_name(instr->add_r_m.type),
                           IR_print_reg(arena, instr->add_r_m.dst), IR_print_mem(arena, &instr->add_r_m.src));
        break;
    }
    case IR_INSTR_ADD_R_I: {
        ftprint_char_array(&dstr, false, "add <%s> %s, %s", type_name(instr->add_r_i.type),
                           IR_print_reg(arena, instr->add_r_i.dst), IR_print_imm(arena, instr->add_r_i.src));
        break;
    }
    case IR_INSTR_SUB_R_R: {
        ftprint_char_array(&dstr, false, "sub <%s> %s, %s", type_name(instr->sub_r_r.type),
                           IR_print_reg(arena, instr->sub_r_r.dst), IR_print_reg(arena, instr->sub_r_r.src));
        break;
    }
    case IR_INSTR_SUB_R_M: {
        ftprint_char_array(&dstr, false, "sub <%s> %s, %s", type_name(instr->sub_r_m.type),
                           IR_print_reg(arena, instr->sub_r_m.dst), IR_print_mem(arena, &instr->sub_r_m.src));
        break;
    }
    case IR_INSTR_SUB_R_I: {
        ftprint_char_array(&dstr, false, "sub <%s> %s, %s", type_name(instr->sub_r_i.type),
                           IR_print_reg(arena, instr->sub_r_i.dst), IR_print_imm(arena, instr->sub_r_i.src));
        break;
    }
    case IR_INSTR_MUL_R_R: {
        ftprint_char_array(&dstr, false, "mul <%s> %s, %s", type_name(instr->mul_r_r.type),
                           IR_print_reg(arena, instr->mul_r_r.dst), IR_print_reg(arena, instr->mul_r_r.src));
        break;
    }
    case IR_INSTR_MUL_R_M: {
        ftprint_char_array(&dstr, false, "mul <%s> %s, %s", type_name(instr->mul_r_m.type),
                           IR_print_reg(arena, instr->mul_r_m.dst), IR_print_mem(arena, &instr->mul_r_m.src));
        break;
    }
    case IR_INSTR_MUL_R_I: {
        ftprint_char_array(&dstr, false, "mul <%s> %s, %s", type_name(instr->mul_r_i.type),
                           IR_print_reg(arena, instr->mul_r_i.dst), IR_print_imm(arena, instr->mul_r_i.src));
        break;
    }
    case IR_INSTR_UDIV_R_R: {
        ftprint_char_array(&dstr, false, "udiv <%s> %s, %s", type_name(instr->div_r_r.type),
                           IR_print_reg(arena, instr->div_r_r.dst), IR_print_reg(arena, instr->div_r_r.src));
        break;
    }
    case IR_INSTR_UDIV_R_M: {
        ftprint_char_array(&dstr, false, "udiv <%s> %s, %s", type_name(instr->div_r_m.type),
                           IR_print_reg(arena, instr->div_r_m.dst), IR_print_mem(arena, &instr->div_r_m.src));
        break;
    }
    case IR_INSTR_UDIV_R_I: {
        ftprint_char_array(&dstr, false, "udiv <%s> %s, %s", type_name(instr->div_r_i.type),
                           IR_print_reg(arena, instr->div_r_i.dst), IR_print_imm(arena, instr->div_r_i.src));
        break;
    }
    case IR_INSTR_SDIV_R_R: {
        ftprint_char_array(&dstr, false, "sdiv <%s> %s, %s", type_name(instr->div_r_r.type),
                           IR_print_reg(arena, instr->div_r_r.dst), IR_print_reg(arena, instr->div_r_r.src));
        break;
    }
    case IR_INSTR_SDIV_R_M: {
        ftprint_char_array(&dstr, false, "sdiv <%s> %s, %s", type_name(instr->div_r_m.type),
                           IR_print_reg(arena, instr->div_r_m.dst), IR_print_mem(arena, &instr->div_r_m.src));
        break;
    }
    case IR_INSTR_SDIV_R_I: {
        ftprint_char_array(&dstr, false, "sdiv <%s> %s, %s", type_name(instr->div_r_i.type),
                           IR_print_reg(arena, instr->div_r_i.dst), IR_print_imm(arena, instr->div_r_i.src));
        break;
    }
    case IR_INSTR_SAR_R_R: {
        ftprint_char_array(&dstr, false, "sar <%s> %s, <%s> %s", type_name(instr->sar_r_r.dst_type),
                           IR_print_reg(arena, instr->sar_r_r.dst), type_name(instr->sar_r_r.src_type),
                           IR_print_reg(arena, instr->sar_r_r.src));
        break;
    }
    case IR_INSTR_SAR_R_M: {
        ftprint_char_array(&dstr, false, "sar <%s> %s, <%s> %s", type_name(instr->sar_r_m.dst_type),
                           IR_print_reg(arena, instr->sar_r_m.dst), type_name(instr->sar_r_m.src_type),
                           IR_print_mem(arena, &instr->sar_r_m.src));
        break;
    }
    case IR_INSTR_SAR_R_I: {
        ftprint_char_array(&dstr, false, "sar <%s> %s, <%s> %s", type_name(instr->sar_r_i.dst_type),
                           IR_print_reg(arena, instr->sar_r_i.dst), type_name(instr->sar_r_i.src_type),
                           IR_print_imm(arena, instr->sar_r_i.src));
        break;
    }
    case IR_INSTR_SHL_R_R: {
        ftprint_char_array(&dstr, false, "shl <%s> %s, <%s> %s", type_name(instr->shl_r_r.dst_type),
                           IR_print_reg(arena, instr->shl_r_r.dst), type_name(instr->shl_r_r.src_type),
                           IR_print_reg(arena, instr->shl_r_r.src));
        break;
    }
    case IR_INSTR_SHL_R_M: {
        ftprint_char_array(&dstr, false, "shl <%s> %s, <%s> %s", type_name(instr->shl_r_m.dst_type),
                           IR_print_reg(arena, instr->shl_r_m.dst), type_name(instr->shl_r_m.src_type),
                           IR_print_mem(arena, &instr->shl_r_m.src));
        break;
    }
    case IR_INSTR_SHL_R_I: {
        ftprint_char_array(&dstr, false, "shl <%s> %s, <%s> %s", type_name(instr->shl_r_i.dst_type),
                           IR_print_reg(arena, instr->shl_r_i.dst), type_name(instr->shl_r_i.src_type),
                           IR_print_imm(arena, instr->shl_r_i.src));
        break;
    }
    case IR_INSTR_NEG: {
        ftprint_char_array(&dstr, false, "neg <%s> %s", type_name(instr->neg.type),
                           IR_print_reg(arena, instr->neg.dst));
        break;
    }
    case IR_INSTR_NOT: {
        ftprint_char_array(&dstr, false, "not <%s> %s", type_name(instr->not .type),
                           IR_print_reg(arena, instr->not .dst));
        break;
    }
    case IR_INSTR_LIMM: {
        ftprint_char_array(&dstr, false, "limm <%s> %s, %s", type_name(instr->limm.type),
                           IR_print_reg(arena, instr->limm.dst), IR_print_imm(arena, instr->limm.src));
        break;
    }
    case IR_INSTR_LADDR: {
        ftprint_char_array(&dstr, false, "laddr %s, %s", IR_print_reg(arena, instr->laddr.dst),
                           IR_print_mem(arena, &instr->laddr.mem));
        break;
    }
    case IR_INSTR_TRUNC_R_R: {
        ftprint_char_array(&dstr, false, "trunc <%s> %s, <%s> %s", type_name(instr->trunc_r_r.dst_type),
                           IR_print_reg(arena, instr->trunc_r_r.dst), type_name(instr->trunc_r_r.src_type),
                           IR_print_reg(arena, instr->trunc_r_r.src));
        break;
    }
    case IR_INSTR_TRUNC_R_M: {
        ftprint_char_array(&dstr, false, "trunc <%s> %s, <%s> %s", type_name(instr->trunc_r_m.dst_type),
                           IR_print_reg(arena, instr->trunc_r_m.dst), type_name(instr->trunc_r_m.src_type),
                           IR_print_mem(arena, &instr->trunc_r_m.src));
        break;
    }
    case IR_INSTR_ZEXT_R_R: {
        ftprint_char_array(&dstr, false, "zext <%s> %s, <%s> %s", type_name(instr->zext_r_r.dst_type),
                           IR_print_reg(arena, instr->zext_r_r.dst), type_name(instr->zext_r_r.src_type),
                           IR_print_reg(arena, instr->zext_r_r.src));
        break;
    }
    case IR_INSTR_ZEXT_R_M: {
        ftprint_char_array(&dstr, false, "zext <%s> %s, <%s> %s", type_name(instr->zext_r_m.dst_type),
                           IR_print_reg(arena, instr->zext_r_m.dst), type_name(instr->zext_r_m.src_type),
                           IR_print_mem(arena, &instr->zext_r_m.src));
        break;
    }
    case IR_INSTR_SEXT_R_R: {
        ftprint_char_array(&dstr, false, "sext <%s> %s, <%s> %s", type_name(instr->sext_r_r.dst_type),
                           IR_print_reg(arena, instr->sext_r_r.dst), type_name(instr->sext_r_r.src_type),
                           IR_print_reg(arena, instr->sext_r_r.src));
        break;
    }
    case IR_INSTR_SEXT_R_M: {
        ftprint_char_array(&dstr, false, "sext <%s> %s, <%s> %s", type_name(instr->sext_r_m.dst_type),
                           IR_print_reg(arena, instr->sext_r_m.dst), type_name(instr->sext_r_m.src_type),
                           IR_print_mem(arena, &instr->sext_r_m.src));
        break;
    }
    case IR_INSTR_LOAD: {
        ftprint_char_array(&dstr, false, "load <%s> %s, %s", type_name(instr->load.type),
                           IR_print_reg(arena, instr->load.dst), IR_print_mem(arena, &instr->load.src));
        break;
    }
    case IR_INSTR_STORE_R: {
        ftprint_char_array(&dstr, false, "store <%s> %s, %s", type_name(instr->store_r.type),
                           IR_print_mem(arena, &instr->store_r.dst), IR_print_reg(arena, instr->store_r.src));
        break;
    }
    case IR_INSTR_STORE_I: {
        ftprint_char_array(&dstr, false, "store <%s> %s, %s", type_name(instr->store_i.type),
                           IR_print_mem(arena, &instr->store_i.dst), IR_print_imm(arena, instr->store_i.src));
        break;
    }
    case IR_INSTR_CMP_R_R: {
        ftprint_char_array(&dstr, false, "cmp <%s> %s, %s", type_name(instr->cmp_r_r.type),
                           IR_print_reg(arena, instr->cmp_r_r.op1), IR_print_reg(arena, instr->cmp_r_r.op2));
        break;
    }
    case IR_INSTR_CMP_R_M: {
        ftprint_char_array(&dstr, false, "cmp <%s> %s, %s", type_name(instr->cmp_r_m.type),
                           IR_print_reg(arena, instr->cmp_r_m.op1), IR_print_mem(arena, &instr->cmp_r_m.op2));
        break;
    }
    case IR_INSTR_CMP_R_I: {
        ftprint_char_array(&dstr, false, "cmp <%s> %s, %s", type_name(instr->cmp_r_i.type),
                           IR_print_reg(arena, instr->cmp_r_i.op1), IR_print_imm(arena, instr->cmp_r_i.op2));
        break;
    }
    case IR_INSTR_CMP_M_R: {
        ftprint_char_array(&dstr, false, "cmp <%s> %s, %s", type_name(instr->cmp_m_r.type),
                           IR_print_mem(arena, &instr->cmp_m_r.op1), IR_print_reg(arena, instr->cmp_m_r.op2));
        break;
    }
    case IR_INSTR_CMP_M_I: {
        ftprint_char_array(&dstr, false, "cmp <%s> %s, %s", type_name(instr->cmp_m_i.type),
                           IR_print_mem(arena, &instr->cmp_m_i.op1), IR_print_imm(arena, instr->cmp_m_i.op2));
        break;
    }
    case IR_INSTR_JMP: {
        ftprint_char_array(&dstr, false, "jmp %u", instr->jmp.jmp_target);
        break;
    }
    case IR_INSTR_JMPCC: {
        ftprint_char_array(&dstr, false, "jmp_%s %u", ir_cond_names[instr->jmpcc.cond], instr->jmpcc.jmp_target);
        break;
    }
    case IR_INSTR_SETCC: {
        ftprint_char_array(&dstr, false, "set_%s %s", ir_cond_names[instr->setcc.cond],
                           IR_print_reg(arena, instr->setcc.dst));
        break;
    }
    case IR_INSTR_RET: {
        ftprint_char_array(&dstr, false, "ret <%s> %s", type_name(instr->ret.type),
                           IR_print_reg(arena, instr->ret.src));
        break;
    }
    case IR_INSTR_CALL: {
        Type* proc_type = instr->call.sym->type;

        ftprint_char_array(&dstr, false, "call ");

        if (proc_type->as_proc.ret != type_void) {
            ftprint_char_array(&dstr, false, "<%s> %s, ", type_name(proc_type->as_proc.ret),
                               IR_print_reg(arena, instr->call.dst));
        }

        ftprint_char_array(&dstr, false, "%s (", symbol_mangled_name(arena, instr->call.sym));

        u32 num_args = instr->call.num_args;
        IR_InstrCallArg* args = instr->call.args;

        if (num_args) {
            for (u32 i = 0; i < num_args; i += 1) {
                IR_InstrCallArg* arg = args + i;

                ftprint_char_array(&dstr, false, "<%s> %s", type_name(arg->type), IR_print_reg(arena, arg->loc));

                if (i != num_args - 1)
                    ftprint_char_array(&dstr, false, ", ");
            }
        }

        ftprint_char_array(&dstr, false, ")");

        break;
    }
    default:
        printf("UNKNOWN_INSTR %d", instr->kind);
        break;
    }

    array_push(dstr, '\0');

    return dstr;
}

void IR_print_out_proc(Allocator* arena, Symbol* sym)
{
    ftprint_out("\nproc %s:\n", symbol_mangled_name(arena, sym));
    ftprint_out("num instrs: %d\n", array_len(sym->as_proc.instrs));

    AllocatorState mem_state = allocator_get_state(arena);
    {
        size_t num_instrs = array_len(sym->as_proc.instrs);

        for (size_t ii = 0; ii < num_instrs; ii += 1)
            ftprint_out("%lu\t%s\n", ii, IR_print_instr(arena, sym->as_proc.instrs[ii]));
    }
    allocator_restore_state(mem_state);
}
