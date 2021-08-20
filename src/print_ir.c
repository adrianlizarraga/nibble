#include "print_ir.h"

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

    if (reg < IR_REG_COUNT)
    {
        dstr = array_create(arena, char, 8);
        ftprint_char_array(&dstr, true, "r%d", reg);
    }
    else
    {
        dstr = array_create(arena, char, 2);
        array_push(dstr, '\0');
    }

    return dstr;
}

static char* IR_print_mem(Allocator* arena, IR_MemAddr* mem_addr)
{
    char* dstr = array_create(arena, char, 16);

    if (mem_addr->kind == IR_MEM_ADDR_SYM)
    {
        Symbol* sym = mem_addr->sym;
        const char* meta = sym->is_local ? "local" : "global";

        ftprint_char_array(&dstr, false, "%s %s", meta, sym->name);
    }
    else
    {
        IR_SIBDAddr addr = mem_addr->sibd;
        bool has_base = addr.base_reg < IR_REG_COUNT;
        bool has_index = addr.scale && (addr.index_reg < IR_REG_COUNT);
        bool has_disp = addr.disp != 0;

        assert(has_base || has_index);

        if (has_base)
        {
            char* base_reg_name = IR_print_reg(arena, addr.base_reg);

            if (has_index)
            {
                char* index_reg_name = IR_print_reg(arena, addr.index_reg);

                if (has_disp)
                    ftprint_char_array(&dstr, false, "%s + %d*%s + %d", base_reg_name, addr.scale, index_reg_name,
                                       (s32)addr.disp);
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
    }

    array_push(dstr, '\0');

    return dstr;
}

static char* IR_print_op_rm(Allocator* arena, IR_OpRM* op)
{
    switch (op->kind)
    {
        case IR_OP_REG:
            return IR_print_reg(arena, op->reg);
        case IR_OP_MEM:
            return IR_print_mem(arena, &op->mem);
        default:
            return NULL;
    }
}

static char* IR_print_op_rmi(Allocator* arena, IR_OpRMI* op)
{
    switch (op->kind)
    {
        case IR_OP_REG:
            return IR_print_reg(arena, op->reg);
        case IR_OP_MEM:
            return IR_print_mem(arena, &op->mem);
        case IR_OP_IMM:
            return IR_print_imm(arena, op->imm);
        default:
            return NULL;
    }
}

static char* IR_print_op_ri(Allocator* arena, IR_OpRI* op)
{
    switch (op->kind)
    {
        case IR_OP_REG:
            return IR_print_reg(arena, op->reg);
        case IR_OP_IMM:
            return IR_print_imm(arena, op->imm);
        default:
            return NULL;
    }
}

char* IR_print_instr(Allocator* arena, IR_Instr* instr)
{
    char* dstr = array_create(arena, char, 16);

    switch (instr->kind)
    {
        case IR_INSTR_ADD:
        {
            ftprint_char_array(&dstr, false, "add <%s> %s, %s", type_name(instr->_add.type),
                               IR_print_op_rm(arena, &instr->_add.dst), IR_print_op_rmi(arena, &instr->_add.src));
            break;
        }
        case IR_INSTR_SUB:
        {
            ftprint_char_array(&dstr, false, "sub <%s> %s, %s", type_name(instr->_sub.type),
                               IR_print_op_rm(arena, &instr->_sub.dst), IR_print_op_rmi(arena, &instr->_sub.src));
            break;
        }
        case IR_INSTR_SHR:
        {
            ftprint_char_array(&dstr, false, "shr <%s> %s, %s", type_name(instr->_shr.type),
                               IR_print_op_rm(arena, &instr->_shr.dst), IR_print_op_rmi(arena, &instr->_shr.src));
            break;
        }
        case IR_INSTR_NEG:
        {
            ftprint_char_array(&dstr, false, "neg <%s> %s", type_name(instr->_neg.type),
                               IR_print_op_rm(arena, &instr->_neg.dst));
            break;
        }
        case IR_INSTR_MOV:
        {
            ftprint_char_array(&dstr, false, "mov <%s> %s, %s", type_name(instr->_mov.type),
                               IR_print_reg(arena, instr->_mov.dst), IR_print_op_ri(arena, &instr->_mov.src));
            break;
        }
        case IR_INSTR_STORE:
        {
            ftprint_char_array(&dstr, false, "store <%s> [%s], %s", type_name(instr->_store.type),
                               IR_print_mem(arena, &instr->_store.dst), IR_print_op_ri(arena, &instr->_store.src));
            break;
        }
        case IR_INSTR_LOAD:
        {
            ftprint_char_array(&dstr, false, "load <%s> %s, [%s]", type_name(instr->_load.type),
                               IR_print_reg(arena, instr->_load.dst), IR_print_mem(arena, &instr->_load.src));
            break;
        }
        case IR_INSTR_LADDR:
        {
            ftprint_char_array(&dstr, false, "laddr %s, [%s]", IR_print_reg(arena, instr->_laddr.dst),
                               IR_print_mem(arena, &instr->_laddr.mem));
            break;
        }
        case IR_INSTR_RET:
        {
            ftprint_char_array(&dstr, false, "ret <%s> %s", type_name(instr->_ret.type),
                               IR_print_reg(arena, instr->_ret.src));
            break;
        }
        case IR_INSTR_TRUNC:
        {
            ftprint_char_array(&dstr, false, "trunc <%s> %s, <%s> %s", type_name(instr->_trunc.dst_type),
                               IR_print_reg(arena, instr->_trunc.dst), type_name(instr->_trunc.src_type),
                               IR_print_op_rm(arena, &instr->_trunc.src));
            break;
        }
        case IR_INSTR_ZEXT:
        {
            ftprint_char_array(&dstr, false, "zext <%s> %s, <%s> %s", type_name(instr->_zext.dst_type),
                               IR_print_reg(arena, instr->_zext.dst), type_name(instr->_zext.src_type),
                               IR_print_op_rm(arena, &instr->_zext.src));
            break;
        }
        case IR_INSTR_SEXT:
        {
            ftprint_char_array(&dstr, false, "sext <%s> %s, <%s> %s", type_name(instr->_sext.dst_type),
                               IR_print_reg(arena, instr->_sext.dst), type_name(instr->_sext.src_type),
                               IR_print_op_rm(arena, &instr->_sext.src));
            break;
        }
        case IR_INSTR_CMP:
        {
            ftprint_char_array(&dstr, false, "cmp <%s> %s, %s", type_name(instr->_cmp.type),
                               IR_print_op_rm(arena, &instr->_cmp.op1), IR_print_op_rmi(arena, &instr->_cmp.op2));
            break;
        }
        case IR_INSTR_JMPCC:
        {
            ftprint_char_array(&dstr, false, "jmp_%s %u", ir_cond_names[instr->_jmpcc.cond], instr->_jmpcc.jmp_target);
            break;
        }
        case IR_INSTR_SETCC:
        {
            ftprint_char_array(&dstr, false, "set_%s %s", ir_cond_names[instr->_setcc.cond],
                               IR_print_op_rm(arena, &instr->_setcc.dst));
            break;
        }
        case IR_INSTR_JMP:
        {
            ftprint_char_array(&dstr, false, "jmp %u", instr->_jmp.jmp_target);
            break;
        }
        case IR_INSTR_CALL:
        {
            Type* proc_type = instr->_call.proc_type;

            ftprint_char_array(&dstr, false, "call ");

            if (proc_type->as_proc.ret != type_void)
            {
                ftprint_char_array(&dstr, false, "<%s> %s, ", type_name(proc_type->as_proc.ret),
                                   IR_print_reg(arena, instr->_call.dst));
            }

            ftprint_char_array(&dstr, false, "%s (", IR_print_op_rm(arena, &instr->_call.proc_loc));

            u32 num_args = instr->_call.num_args;
            IR_InstrCallArg* args = instr->_call.args;

            if (num_args)
            {
                for (u32 i = 0; i < num_args; i += 1)
                {
                    IR_InstrCallArg* arg = args + i;

                    ftprint_char_array(&dstr, false, "<%s> %s", type_name(arg->type), IR_print_op_rm(arena, &arg->loc));

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
    ftprint_out("\nproc %s:\n", sym->name);
    ftprint_out("num instrs: %d\n", array_len(sym->as_proc.instrs));

    AllocatorState mem_state = allocator_get_state(arena);
    {
        size_t num_instrs = array_len(sym->as_proc.instrs);

        for (size_t ii = 0; ii < num_instrs; ii += 1)
            ftprint_out("%lu\t%s\n", ii, IR_print_instr(arena, sym->as_proc.instrs[ii]));
    }
    allocator_restore_state(mem_state);
}
