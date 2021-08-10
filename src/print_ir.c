#include "print_ir.h"

static const char* ir_cond_names[] = {
    [IR_COND_U_LT] = "u<",
    [IR_COND_S_LT] = "s<",
    [IR_COND_U_LTEQ] = "u<=",
    [IR_COND_S_LTEQ] = "s<=",
    [IR_COND_EQ] = "==",
    [IR_COND_NEQ] = "!=",
};

static char* IR_print_reg(Allocator* arena, IR_Reg reg)
{
    char* dstr = array_create(arena, char, 8);

    ftprint_char_array(&dstr, true, "r%d", reg);

    return dstr;
}

static char* IR_print_arg(Allocator* arena, IR_InstrArg* arg)
{
    char* dstr = array_create(arena, char, 8);

    switch (arg->kind)
    {
        case IR_ARG_REG:
        {
            if (arg->reg)
                ftprint_char_array(&dstr, false, "r%d", arg->reg);
            else
                ftprint_char_array(&dstr, false, "r<INVALID>");


            break;
        }
        case IR_ARG_IMM:
        {
            ftprint_char_array(&dstr, false, "0x%lx", arg->imm.as_int._u64);
            break;
        }
        default:
            break;
    }

    array_push(dstr, '\0');

    return dstr;
}

static char* IR_print_sibd_addr(Allocator* arena, IR_SIBDAddr addr)
{
    char* dstr = array_create(arena, char, 16);

    bool has_base = addr.base_reg != 0;
    bool has_index = addr.scale && (addr.index_reg != 0);
    bool has_disp = addr.disp != 0;

    assert(has_base || has_index);

    if (has_base)
    {
        char* base_reg_name = IR_print_reg(arena, addr.base_reg);

        if (has_index)
        {
            char* index_reg_name = IR_print_reg(arena, addr.index_reg);

            if (has_disp)
                ftprint_char_array(&dstr, false, "%s + %d*%s + %d", base_reg_name, addr.scale, index_reg_name, (s32)addr.disp);
            else
                ftprint_char_array(&dstr, false, "%s + %d*%s", base_reg_name, addr.scale, index_reg_name);
        }
        else
        {
            if (has_disp)
                ftprint_char_array(&dstr, false, "%s + %d", base_reg_name, (s32)addr.disp);
            else
                ftprint_char_array(&dstr, false, "%s", base_reg_name);
        }
    }
    else
    {
        char* index_reg_name = IR_print_reg(arena, addr.index_reg);

        if (has_disp)
            ftprint_char_array(&dstr, false, "%d*%s + %d", addr.scale, index_reg_name, (s32)addr.disp);
        else
            ftprint_char_array(&dstr, false, "%d*%s", addr.scale, index_reg_name);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* IR_print_instr(Allocator* arena, IR_Instr* instr)
{
    char* dstr = array_create(arena, char, 16);

    switch (instr->kind)
    {
        case IR_INSTR_ADD:
        {
            ftprint_char_array(&dstr, false, "%s = add <%s> %s, %s",
                               IR_print_reg(arena, instr->_add.out_reg),
                               type_name(instr->_add.type),
                               IR_print_arg(arena, &instr->_add.arg1),
                               IR_print_arg(arena, &instr->_add.arg2));
            break;
        }
        case IR_INSTR_SUB:
        {
            ftprint_char_array(&dstr, false, "%s = sub <%s> %s, %s",
                               IR_print_reg(arena, instr->_sub.out_reg),
                               type_name(instr->_sub.type),
                               IR_print_arg(arena, &instr->_sub.arg1),
                               IR_print_arg(arena, &instr->_sub.arg2));
            break;
        }
        case IR_INSTR_SHR:
        {
            ftprint_char_array(&dstr, false, "%s = shr <%s> %s, %s",
                               IR_print_reg(arena, instr->_shr.out_reg),
                               type_name(instr->_sub.type),
                               IR_print_arg(arena, &instr->_shr.arg1),
                               IR_print_arg(arena, &instr->_shr.arg2));
            break;
        }
        case IR_INSTR_STORE:
        {
            ftprint_char_array(&dstr, false, "store <%s> [%s] %s",
                               type_name(instr->_store.type),
                               IR_print_sibd_addr(arena, instr->_store.addr),
                               IR_print_arg(arena, &instr->_store.arg));
            break;
        }
        case IR_INSTR_LOAD:
        {
            ftprint_char_array(&dstr, false, "%s = load <%s> [%s]",
                               IR_print_reg(arena, instr->_load.out_reg),
                               type_name(instr->_load.type),
                               IR_print_sibd_addr(arena, instr->_load.addr));
            break;
        }
        case IR_INSTR_LADDR:
        {
            ftprint_char_array(&dstr, false, "%s = laddr <%s> [%s]",
                               IR_print_reg(arena, instr->_laddr.out_reg),
                               type_name(instr->_laddr.type),
                               IR_print_sibd_addr(arena, instr->_laddr.addr));
            break;
        }
        case IR_INSTR_LADDR_VAR:
        {
            Symbol* sym = instr->_laddr_var.sym;
            char arg_kind = 'g';

            if (sym->as_var.is_arg)
                arg_kind = 'a';
            else if (sym->is_local)
                arg_kind = 'l';

            ftprint_char_array(&dstr, false, "%s = laddr_var <%s> |%c| %s",
                               IR_print_reg(arena, instr->_laddr_var.out_reg),
                               type_name(sym->type),
                               arg_kind, sym->name);
            break;
        }
        case IR_INSTR_RET:
        {
            ftprint_char_array(&dstr, false, "ret <%s> %s", type_name(instr->_ret.type), IR_print_arg(arena, &instr->_ret.ret_arg));
            break;
        }
        case IR_INSTR_TRUNC:
        {
            ftprint_char_array(&dstr, false, "%s = trunc <%s> %s to <%s>",
                               IR_print_reg(arena, instr->_trunc.out_reg),
                               type_name(instr->_trunc.src_type),
                               IR_print_arg(arena, &instr->_trunc.src_arg),
                               type_name(instr->_trunc.dst_type));
            break;
        }
        case IR_INSTR_ZEXT:
        {
            ftprint_char_array(&dstr, false, "%s = zext <%s> %s to <%s>",
                               IR_print_reg(arena, instr->_zext.out_reg),
                               type_name(instr->_zext.src_type),
                               IR_print_arg(arena, &instr->_zext.src_arg),
                               type_name(instr->_zext.dst_type));
            break;
        }
        case IR_INSTR_SEXT:
        {
            ftprint_char_array(&dstr, false, "%s = sext <%s> %s to <%s>",
                               IR_print_reg(arena, instr->_sext.out_reg),
                               type_name(instr->_sext.src_type),
                               IR_print_arg(arena, &instr->_sext.src_arg),
                               type_name(instr->_sext.dst_type));
            break;
        }
        case IR_INSTR_CMP:
        {
            ftprint_char_array(&dstr, false, "%s = cmp_%s <%s> %s, %s",
                               IR_print_reg(arena, instr->_cmp.out_reg),
                               ir_cond_names[instr->_cmp.kind],
                               type_name(instr->_cmp.type),
                               IR_print_arg(arena, &instr->_cmp.arg1),
                               IR_print_arg(arena, &instr->_cmp.arg2));
            break;
        }
        case IR_INSTR_CJMP:
        {
            ftprint_char_array(&dstr, false, "cjmp <i1> %s, %u", 
                               IR_print_reg(arena, instr->_cjmp.cond_reg),
                               instr->_cjmp.jmp_target);
            break;
        }
        case IR_INSTR_JMP:
        {
            ftprint_char_array(&dstr, false, "jmp %u", instr->_jmp.jmp_target);
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
    ftprint_out("\nproc %s:\n", sym->name);

    AllocatorState mem_state = allocator_get_state(arena);
    {
        size_t num_instrs = array_len(sym->as_proc.instrs);

        for (size_t ii = 0; ii < num_instrs; ii += 1)
            ftprint_out("%lu\t%s\n", ii, IR_print_instr(arena, sym->as_proc.instrs[ii]));
    }
    allocator_restore_state(mem_state);
}


