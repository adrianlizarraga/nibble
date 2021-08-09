#include "print_ir.h"
#include <stdio.h>

static const char* ir_type_names[] = {
    [IR_TYPE_INT8] = "int8",
    [IR_TYPE_INT16] = "int16",
    [IR_TYPE_INT32] = "int32",
    [IR_TYPE_INT64] = "int64",
    [IR_TYPE_F32] = "f32",
    [IR_TYPE_F64] = "f64"
};

static char* IR_print_reg(Allocator* arena, IR_Reg reg)
{
    char* dstr = array_create(arena, char, 8);

    ftprint_char_array(&dstr, true, "r%d", reg);

    return dstr;
}

static char* IR_print_arg(char* buf, size_t blen, IR_InstrArg* arg)
{
    char* dstr = array_create(arena, char, 8);

    switch (arg->kind)
    {
        case IR_ARG_REG:
        {
            if (arg->reg0)
            {
                ftprint_char_array(&dstr, false, "r%d", arg->reg0);
                if (arg->reg1) ftprint_char_array(&dstr, false, " ");
            }

            if (arg->reg1) ftprint_char_array(&dstr, false, "r%d", arg->reg1);

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

char* IR_print_instr(Allocator* arena, IR_Instr* instr)
{
    char* dstr = array_create(arena, char, 16);

    switch (instr->kind)
    {
        case IR_INSTR_ADD:
        {
            ftprint_char_array(&dstr, false, "%s = add <%s> %s, %s\n",
                               IR_print_reg(arena, instr->r),
                               ir_type_names[instr->option.bytes[0]],
                               IR_print_arg(arena, instr->a),
                               IR_print_arg(arena, instr->b));
            break;
        }
        case IR_INSTR_SUB:
        {
            ftprint_char_array(&dstr, false, "%s = sub <%s> %s, %s\n",
                               IR_print_reg(arena, instr->r),
                               ir_type_names[instr->option.bytes[0]],
                               IR_print_arg(arena, instr->a),
                               IR_print_arg(arena, instr->b));
            break;
        }
        case IR_INSTR_NEG:
        {
            ftprint_char_array(&dstr, false, "%s = neg <%s> %s\n",
                               IR_print_reg(arena, instr->r),
                               ir_type_names[instr->option.bytes[0]],
                               IR_print_arg(arena, instr->a));
            break;
        }
        case IR_INSTR_STORE:
        {
            /*
            ftprint_char_array(&dstr, false, "store <%s> [%s] %s\n",
                               ir_type_names[instr->option.bytes[0]],
                               IR_print_sibd_addr(arena, instr->a.reg0, instr->a.reg1, ));
            */
            break;
        }
        case IR_INSTR_LOAD:
        {
            break;
        }
        case IR_INSTR_LADDR:
        {
            break;
        }
        case IR_INSTR_LADDR_VAR:
        {
            break;
        }
        case IR_INSTR_SHR:
        {
            break;
        }
        case IR_INSTR_RET:
        {
            break;
        }
        case IR_INSTR_CMP_EQ:
        {
            break;
        }
        case IR_INSTR_CMP_NE:
        {
            break;
        }
        case IR_INSTR_CJMP:
        {
            break;
        }
        case IR_INSTR_JMP:
        {
            break;
        }
        default:
            printf("UNKNOWN_INSTR %d\n", instr->kind);
            break;
    }

    array_push(dstr, '\0');

    return dstr;
}

