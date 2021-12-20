#include "print_nir.h"
#include "cstring.h"

static const char* nir_cond_names[] = {
    [COND_U_LT] = "u<", [COND_S_LT] = "s<",    [COND_U_LTEQ] = "u<=", [COND_S_LTEQ] = "s<=", [COND_U_GT] = "u>",
    [COND_S_GT] = "s>", [COND_U_GTEQ] = "u>=", [COND_S_GTEQ] = "s>=", [COND_EQ] = "==",      [COND_NEQ] = "!=",
};

static char* NIR_print_imm(Allocator* arena, Scalar imm)
{
    char* dstr = array_create(arena, char, 16);

    ftprint_char_array(&dstr, true, "0x%lx", imm.as_int._u64);

    return dstr;
}

static char* NIR_print_reg(Allocator* arena, NIR_Reg reg)
{
    char* dstr = NULL;

    if (reg < NIR_REG_COUNT) {
        dstr = array_create(arena, char, 8);
        ftprint_char_array(&dstr, true, "r%d", reg);
    }
    else {
        dstr = array_create(arena, char, 2);
        array_push(dstr, '\0');
    }

    return dstr;
}

static char* NIR_print_regimm(Allocator* arena, RegOrImm a)
{
    if (a.is_imm) {
        return NIR_print_imm(arena, a.imm);
    }

    return NIR_print_reg(arena, a.reg);
}

static char* NIR_print_mem(Allocator* arena, MemAddr* addr)
{
    char* dstr = array_create(arena, char, 16);

    bool has_base = addr->base_kind != MEM_BASE_NONE;
    bool has_index = addr->scale && (addr->index_reg < NIR_REG_COUNT);
    bool has_disp = addr->disp != 0;

    assert(has_base || has_index);

    ftprint_char_array(&dstr, false, "[");

    if (has_base) {
        if (addr->base_kind == MEM_BASE_REG)
            ftprint_char_array(&dstr, false, "%s", NIR_print_reg(arena, addr->base.reg));
        else if (addr->base_kind == MEM_BASE_SYM)
            ftprint_char_array(&dstr, false, "%s %s", (addr->base.sym->is_local ? "local" : "global"),
                               symbol_mangled_name(arena, addr->base.sym));
        else {
            assert(addr->base_kind == MEM_BASE_STR_LIT);
            ftprint_char_array(&dstr, false, "\"%s\"", cstr_escape(arena, addr->base.str_lit->str, addr->base.str_lit->len, 0));
        }

        if (has_index) {
            char* index_reg_name = NIR_print_reg(arena, addr->index_reg);

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
        char* index_reg_name = NIR_print_reg(arena, addr->index_reg);

        if (has_disp)
            ftprint_char_array(&dstr, false, "%d*%s + %d", addr->scale, index_reg_name, (s32)addr->disp);
        else
            ftprint_char_array(&dstr, false, "%d*%s", addr->scale, index_reg_name);
    }

    ftprint_char_array(&dstr, true, "]");

    return dstr;
}

char* NIR_print_instr(Allocator* arena, Instr* instr)
{
    static const char* binary_kind_name[] = {
        [INSTR_ADD]  = "add",
        [INSTR_SUB]  = "sub",
        [INSTR_MUL]  = "mul",
        [INSTR_UDIV] = "udiv",
        [INSTR_SDIV] = "sdiv",
        [INSTR_SAR]  = "sar",
        [INSTR_SHL]  = "shl",
        [INSTR_AND]  = "and",
        [INSTR_OR]   = "or",
        [INSTR_XOR]  = "xor"
    };
    static const char* unary_kind_name[] = {
        [INSTR_NOT] = "not",
        [INSTR_NEG] = "neg"
    };
    static const char* convert_kind_name[] = {
        [INSTR_TRUNC] = "trunc",
        [INSTR_ZEXT]  = "zext",
        [INSTR_SEXT]  = "sext"
    };
    char* dstr = array_create(arena, char, 16);

    switch (instr->kind) {
    case INSTR_ADD:
    case INSTR_SUB:
    case INSTR_MUL:
    case INSTR_UDIV:
    case INSTR_SDIV:
    case INSTR_SAR:
    case INSTR_SHL:
    case INSTR_AND:
    case INSTR_OR:
    case INSTR_XOR: {
        const char* op_name = binary_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(instr->binary.type),
                           NIR_print_reg(arena, instr->binary.r), NIR_print_regimm(arena, instr->binary.a),
                           NIR_print_regimm(arena, instr->binary.b));
        break;
    }
    case INSTR_NEG:
    case INSTR_NOT: {
        const char* op_name = unary_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s", op_name, type_name(instr->unary.type), NIR_print_reg(arena, instr->unary.r),
                           NIR_print_reg(arena, instr->unary.a));
        break;
    }
    case INSTR_TRUNC:
    case INSTR_ZEXT:
    case INSTR_SEXT: {
        const char* op_name = convert_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, <%s> %s", op_name, type_name(instr->convert.dst_type),
                           NIR_print_reg(arena, instr->convert.r), type_name(instr->convert.src_type),
                           NIR_print_reg(arena, instr->convert.a));
        break;
    }
    case INSTR_LIMM: {
        ftprint_char_array(&dstr, false, "limm <%s> %s, %s", type_name(instr->limm.type), NIR_print_reg(arena, instr->limm.r),
                           NIR_print_imm(arena, instr->limm.imm));
        break;
    }
    case INSTR_LADDR: {
        ftprint_char_array(&dstr, false, "laddr %s, %s", NIR_print_reg(arena, instr->laddr.r),
                           NIR_print_mem(arena, &instr->laddr.addr));
        break;
    }
    case INSTR_MEMCPY: {
        ftprint_char_array(&dstr, false, "memcpy %s, %s, %llu", NIR_print_mem(arena, &instr->memcpy.dst),
                           NIR_print_mem(arena, &instr->memcpy.src), instr->memcpy.type->size);

        break;
    }
    case INSTR_LOAD: {
        ftprint_char_array(&dstr, false, "load <%s> %s, %s", type_name(instr->load.type), NIR_print_reg(arena, instr->load.r),
                           NIR_print_mem(arena, &instr->load.addr));
        break;
    }
    case INSTR_STORE: {
        ftprint_char_array(&dstr, false, "store <%s> %s, %s", type_name(instr->store.type), NIR_print_mem(arena, &instr->store.addr),
                           NIR_print_regimm(arena, instr->store.a));
        break;
    }
    case INSTR_CMP: {
        ftprint_char_array(&dstr, false, "cmp <%s> %s, %s, %s, %s", type_name(instr->cmp.type), NIR_print_reg(arena, instr->cmp.r),
                           NIR_print_regimm(arena, instr->cmp.a), nir_cond_names[instr->cmp.cond], NIR_print_regimm(arena, instr->cmp.b));
        break;
    }
    case INSTR_JMP: {
        ftprint_char_array(&dstr, false, "jmp %u", *instr->jmp.jmp_target);
        break;
    }
    case INSTR_COND_JMP: {
        ftprint_char_array(&dstr, false, "jmpcc %s, %u", NIR_print_reg(arena, instr->cond_jmp.a), *instr->cond_jmp.jmp_target);
        break;
    }
    case INSTR_RET: {
        ftprint_char_array(&dstr, false, "ret <%s> %s", type_name(instr->ret.type), NIR_print_reg(arena, instr->ret.a));
        break;
    }
    case INSTR_CALL:
    case INSTR_CALL_INDIRECT: {
        bool is_indirect = instr->kind == INSTR_CALL_INDIRECT;

        Type* proc_type;
        const char* proc_name;
        NIR_Reg r;
        u32 num_args;
        InstrCallArg* args;

        if (is_indirect) {
            proc_type = instr->calli.proc_type;
            proc_name = NIR_print_reg(arena, instr->calli.loc);
            r = instr->calli.r;
            num_args = instr->calli.num_args;
            args = instr->calli.args;
        }
        else {
            proc_type = instr->call.sym->type;
            proc_name = symbol_mangled_name(arena, instr->call.sym);
            r = instr->call.r;
            num_args = instr->call.num_args;
            args = instr->call.args;
        }

        ftprint_char_array(&dstr, false, "call ");

        if (proc_type->as_proc.ret != builtin_types[BUILTIN_TYPE_VOID].type) {
            ftprint_char_array(&dstr, false, "<%s> %s, ", type_name(proc_type->as_proc.ret), NIR_print_reg(arena, r));
        }

        ftprint_char_array(&dstr, false, "%s (", proc_name);

        if (num_args) {
            for (u32 i = 0; i < num_args; i += 1) {
                InstrCallArg* arg = args + i;

                ftprint_char_array(&dstr, false, "<%s> %s", type_name(arg->type), NIR_print_reg(arena, arg->loc));

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

void NIR_print_out_proc(Allocator* arena, Symbol* sym)
{
    ftprint_out("\nproc %s:\n", symbol_mangled_name(arena, sym));
    ftprint_out("num instrs: %d\n", array_len(sym->as_proc.nir_instrs));

    AllocatorState mem_state = allocator_get_state(arena);
    {
        size_t num_instrs = array_len(sym->as_proc.nir_instrs);

        for (size_t ii = 0; ii < num_instrs; ii += 1)
            ftprint_out("%lu\t%s\n", ii, NIR_print_instr(arena, sym->as_proc.nir_instrs[ii]));
    }
    allocator_restore_state(mem_state);
}
