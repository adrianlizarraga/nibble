#include "regs.h"

static char* LIR_print_mem(Allocator* arena, X64_MemAddr* addr)
{
    char* dstr = array_create(arena, char, 16);

    ftprint_char_array(&dstr, false, "[");

    switch (addr->kind) {
    case X64_ADDR_GLOBAL_SYM: {
        ftprint_char_array(&dstr, false, "global %s", symbol_mangled_name(arena, addr->global));
        break;
    }
    case X64_ADDR_STR_LIT: {
        ftprint_char_array(&dstr, false, "\"%s\"", cstr_escape(arena, addr->str_lit->str, addr->str_lit->len, 0));
        break;
    }
    case X64_ADDR_SIBD: {
        bool has_base = addr->sibd.base_reg != X64_LIR_REG_COUNT;
        bool has_index = addr->sibd.scale && (addr->sibd.index_reg != X64_LIR_REG_COUNT);
        bool has_disp = addr->sibd.disp != 0;

        if (has_base) {
            ftprint_char_array(&dstr, false, "r%d", addr->sibd.base_reg);

            if (has_index) {
                u32 index_reg = addr->sibd.index_reg;

                if (has_disp)
                    ftprint_char_array(&dstr, false, " + %d*r%d + %d", addr->sibd.scale, index_reg, (s32)addr->sibd.disp);
                else
                    ftprint_char_array(&dstr, false, " + %d*r%d", addr->sibd.scale, index_reg);
            }
            else if (has_disp) {
                ftprint_char_array(&dstr, false, " + %d", (s32)addr->sibd.disp);
            }
        }
        else {
            assert(has_index);

            u32 index_reg = addr->sibd.index_reg;

            if (has_disp)
                ftprint_char_array(&dstr, false, " + %d*r%d + %d", addr->sibd.scale, index_reg, (s32)addr->sibd.disp);
            else
                ftprint_char_array(&dstr, false, " + %d*r%d", addr->sibd.scale, index_reg);
        }
        break;
    }
    default:
        NIBBLE_FATAL_EXIT("[INTERNAL ERROR]: Unable to print unknown X64_MemAddr kind `%d`\n", addr->kind);
        break;
    }

    ftprint_char_array(&dstr, true, "]");

    return dstr;
}

static char* LIR_print_instr(Allocator* arena, X64_Instr* instr)
{
    static const char* binary_r_r_name[] = {
        [X64_INSTR_ADD_R_R] = "add",
        [X64_INSTR_SUB_R_R] = "sub",
        [X64_INSTR_IMUL_R_R] = "imul",
        [X64_INSTR_AND_R_R] = "and",
        [X64_INSTR_OR_R_R] = "or",
        [X64_INSTR_XOR_R_R] = "xor"
    };

    static const char* binary_r_i_name[] = {
        [X64_INSTR_ADD_R_I] = "add",
        [X64_INSTR_SUB_R_I] = "sub",
        [X64_INSTR_IMUL_R_I] = "imul",
        [X64_INSTR_AND_R_I] = "and",
        [X64_INSTR_OR_R_I] = "or",
        [X64_INSTR_XOR_R_I] = "xor"
    };

    static const char* binary_r_m_name[] = {
        [X64_INSTR_ADD_R_M] = "add",
        [X64_INSTR_SUB_R_M] = "sub",
        [X64_INSTR_IMUL_R_M] = "imul",
        [X64_INSTR_AND_R_M] = "and",
        [X64_INSTR_OR_R_M] = "or",
        [X64_INSTR_XOR_R_M] = "xor"
    };

    static const char* shift_r_r_name[] = {
        [X64_INSTR_SAR_R_R] = "sar",
        [X64_INSTR_SHL_R_R] = "shl"
    };

    static const char* shift_r_i_name[] = {
        [X64_INSTR_SAR_R_I] = "sar",
        [X64_INSTR_SHL_R_I] = "shl"
    };

    static const char* unary_name[] = {
        [X64_INSTR_NEG] = "neg",
        [X64_INSTR_NOT] = "not"
    };

    char* dstr = array_create(arena, char, 16);

    switch (instr->kind) {
    case X64_INSTR_ADD_R_R:
    case X64_INSTR_SUB_R_R:
    case X64_INSTR_IMUL_R_R:
    case X64_INSTR_AND_R_R:
    case X64_INSTR_OR_R_R:
    case X64_INSTR_XOR_R_R: {
        u32 size = (u32)instr->binary_r_r.size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, r%d", binary_r_r_name[instr->kind], size,
                           instr->binary_r_r.dst, instr->binary_r_r.src);
        break;
    }
    case X64_INSTR_ADD_R_I:
    case X64_INSTR_SUB_R_I:
    case X64_INSTR_IMUL_R_I:
    case X64_INSTR_AND_R_I:
    case X64_INSTR_OR_R_I:
    case X64_INSTR_XOR_R_I: {
        u32 size = (u32)instr->binary_r_i.size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, 0x%lx", binary_r_i_name[instr->kind], size,
                           instr->binary_r_i.dst, instr->binary_r_i.src.as_int._u64);
        break;
    }
    case X64_INSTR_ADD_R_M:
    case X64_INSTR_SUB_R_M:
    case X64_INSTR_IMUL_R_M:
    case X64_INSTR_AND_R_M:
    case X64_INSTR_OR_R_M:
    case X64_INSTR_XOR_R_M: {
        u32 size = (u32)instr->binary_r_m.size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, %s", binary_r_m_name[instr->kind], size,
                           instr->binary_r_m.dst, LIR_print_mem(arena, &instr->binary_r_m.src));
        break;
    }
    case X64_INSTR_DIV_R:
    case X64_INSTR_IDIV_R: {
        const char* instr_name = instr->kind == X64_INSTR_IDIV_R ? "idiv" : "div";
        u32 size = (u32)instr->div_r.size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d", instr_name, size, instr->div_r.src);
        break;
    }
    case X64_INSTR_DIV_M:
    case X64_INSTR_IDIV_M: {
        const char* instr_name = instr->kind == X64_INSTR_IDIV_M ? "idiv" : "div";
        u32 size = (u32)instr->div_m.size;

        ftprint_char_array(&dstr, false, "%s <%lu> %s", instr_name, size, LIR_print_mem(arena, &instr->div_m.src));
        break;
    }
    case X64_INSTR_SEXT_AX_TO_DX: {
        u32 size = (u32)instr->sext_ax_to_dx.size;
        ftprint_char_array(&dstr, false, "%s", x64_sext_ax_into_dx[size]);
        break;
    }
    case X64_INSTR_SAR_R_R:
    case X64_INSTR_SHL_R_R: {
        u32 dst_size = (u32)instr->shift_r_r.size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, r%d", shift_r_r_name[instr->kind], dst_size, instr->shift_r_r.dst,
                           instr->shift_r_r.src);
        break;
    }
    case X64_INSTR_SAR_R_I:
    case X64_INSTR_SHL_R_I: {
        u32 dst_size = (u32)instr->shift_r_i.size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, 0x%lx", shift_r_i_name[instr->kind], dst_size, instr->shift_r_i.dst,
                           instr->shift_r_i.src.as_int._u8);
        break;
    }
    case X64_INSTR_NEG:
    case X64_INSTR_NOT: {
        u32 size = (u32)instr->unary.size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d", unary_name[instr->kind], size, instr->unary.dst);
        break;
    }
    case X64_INSTR_REP_MOVSB: {
        ftprint_char_array(&dstr, false, "rep movsb");
        break;
    }
    case X64_INSTR_REP_STOSB: {
        ftprint_char_array(&dstr, false, "rep stosb");
        break;
    }
    case X64_INSTR_MOV_R_RH: {
        ftprint_char_array(&dstr, false, "mov <1> r%d, r%d[h]", instr->mov_r_rh.dst, instr->mov_r_rh.src);
        break;
    }
    case X64_INSTR_MOV_R_R: {
        u32 size = (u32)instr->mov_r_r.size;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, r%d", size, instr->mov_r_r.dst, instr->mov_r_r.src);
        break;
    }
    case X64_INSTR_MOV_R_I: {
        u32 size = (u32)instr->mov_r_i.size;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, 0x%lx", size, instr->mov_r_i.dst, instr->mov_r_i.src.as_int._u64);
        break;
    }
    case X64_INSTR_MOV_R_M: {
        u32 size = (u32)instr->mov_r_m.size;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, %s", size, instr->mov_r_m.dst, LIR_print_mem(arena, &instr->mov_r_m.src));
        break;
    }
    case X64_INSTR_MOV_M_R: {
        u32 size = (u32)instr->mov_m_r.size;

        ftprint_char_array(&dstr, false, "mov <%lu> %s, r%d", size, LIR_print_mem(arena, &instr->mov_m_r.dst), instr->mov_m_r.src);
        break;
    }
    case X64_INSTR_MOV_M_I: {
        u32 size = (u32)instr->mov_m_i.size;

        ftprint_char_array(&dstr, false, "mov <%lu> %s, 0x%lx", size, LIR_print_mem(arena, &instr->mov_m_i.dst),
                           instr->mov_m_i.src.as_int._u64);
        break;
    }
    case X64_INSTR_MOVZX_R_R: {
        size_t dst_size = instr->convert_r_r.dst_size;
        size_t src_size = instr->convert_r_r.src_size;

        ftprint_char_array(&dstr, false, "movzx <%lu> r%d, <%lu> r%d", dst_size, instr->convert_r_r.dst,
                           src_size, instr->convert_r_r.src);
        break;
    }
    case X64_INSTR_MOVSX_R_R: {
        size_t dst_size = instr->convert_r_r.dst_size;
        size_t src_size = instr->convert_r_r.src_size;
        const char* movsx = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? "movsxd" : "movsx";

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, <%lu> r%d", movsx, dst_size, instr->convert_r_r.dst,
                           src_size, instr->convert_r_r.src);
        break;
    }
    case X64_INSTR_LEA: {
        ftprint_char_array(&dstr, false, "lea r%d, %s", instr->lea.dst, LIR_print_mem(arena, &instr->lea.mem));
        break;
    }
    case X64_INSTR_CMP_R_R: {
        u32 size = (u32)instr->cmp_r_r.size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, r%d", size, instr->cmp_r_r.op1, instr->cmp_r_r.op2);
        break;
    }
    case X64_INSTR_CMP_R_I: {
        u32 size = (u32)instr->cmp_r_i.size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, 0x%lx", size, instr->cmp_r_i.op1, instr->cmp_r_i.op2.as_int._u64);
        break;
    }
    case X64_INSTR_CMP_R_M: {
        u32 size = (u32)instr->cmp_r_m.size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, %s", size, instr->cmp_r_m.op1, LIR_print_mem(arena, &instr->cmp_r_m.op2));
        break;
    }
    case X64_INSTR_CMP_M_R: {
        u32 size = (u32)instr->cmp_m_r.size;

        ftprint_char_array(&dstr, false, "cmp <%lu> %s, r%d", size, LIR_print_mem(arena, &instr->cmp_m_r.op1), instr->cmp_m_r.op2);
        break;
    }
    case X64_INSTR_CMP_M_I: {
        u32 size = (u32)instr->cmp_m_i.size;

        ftprint_char_array(&dstr, false, "cmp <%lu> %s, 0x%lx", size, LIR_print_mem(arena, &instr->cmp_m_i.op1),
                           instr->cmp_m_i.op2.as_int._u64);
        break;
    }
    case X64_INSTR_JMP: {
        ftprint_char_array(&dstr, false, "jmp B.%ld", instr->jmp.target->id);
        break;
    }
    case X64_INSTR_JMPCC: {
        ftprint_char_array(&dstr, false, "j%s B.%ld else B.%ld", x64_condition_codes[instr->jmpcc.cond],
                           instr->jmpcc.true_bb->id, instr->jmpcc.false_bb->id);
        break;
    }
    case X64_INSTR_SETCC: {
        ftprint_char_array(&dstr, false, "set%s r%d", x64_condition_codes[instr->setcc.cond], instr->setcc.dst);
        break;
    }
    case X64_INSTR_RET: {
        ftprint_char_array(&dstr, false, "ret");

        break;
    }
    case X64_INSTR_CALL:
    case X64_INSTR_CALL_R: {
        Type* proc_type;
        X64_CallValue dst_val;
        u32 num_args;
        X64_InstrCallArg* args;

        if (instr->kind == X64_INSTR_CALL) {
            proc_type = instr->call.sym->type;
            dst_val = instr->call.dst;
            num_args = instr->call.num_args;
            args = instr->call.args;
        }
        else {
            assert(instr->kind == X64_INSTR_CALL_R);
            proc_type = instr->call_r.proc_type;
            dst_val = instr->call_r.dst;
            num_args = instr->call_r.num_args;
            args = instr->call_r.args;
        }

        Type* ret_type = proc_type->as_proc.ret;

        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            if (type_is_obj_like(ret_type)) {
                ftprint_char_array(&dstr, false, "<%lu> %s = ", ret_type->size, LIR_print_mem(arena, &dst_val.addr));
            }
            else {
                ftprint_char_array(&dstr, false, "<%lu> r%d = ", ret_type->size, dst_val.reg);
            }
        }

        if (instr->kind == X64_INSTR_CALL) {
            ftprint_char_array(&dstr, false, "call %s (", symbol_mangled_name(arena, instr->call.sym));
        }
        else {
            ftprint_char_array(&dstr, false, "call r%d (", instr->call_r.proc_loc);
        }

        if (num_args) {
            for (u32 i = 0; i < num_args; i += 1) {
                X64_InstrCallArg* arg = args + i;

                if (type_is_obj_like(arg->type)) {
                    ftprint_char_array(&dstr, false, "<%s> %s", type_name(arg->type), LIR_print_mem(arena, &arg->val.addr));
                }
                else {
                    ftprint_char_array(&dstr, false, "<%s> r%d", type_name(arg->type), arg->val.reg);
                }

                if (i != num_args - 1)
                    ftprint_char_array(&dstr, false, ", ");
            }
        }

        ftprint_char_array(&dstr, false, ")");

        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unknown X64 LIR instruction kind %d\n", instr->kind);
        break;
    }

    array_push(dstr, '\0');

    return dstr;
}

static void LIR_dump_bblock_dot(Allocator* arena, X64_BBlock* bblock)
{
    size_t ii = 0;

    ftprint_out("\tB%d [", bblock->id);
    if (bblock->flags & BBLOCK_IS_LOOP_HDR) {
        ftprint_out("style=filled, color=lightgrey, label=\"B%d\\n\\n", bblock->id);
    }
    else {
        ftprint_out("label=\"B%d\\n\\n", bblock->id);
    }

    for (X64_Instr* it = bblock->first; it; it = it->next, ii++) {
        ftprint_out("%.3lu: %s\\l", it->ino, LIR_print_instr(arena, it));
    }

    assert(ii == bblock->num_instrs);
    ftprint_out("\"]\n");

    X64_Instr* last_instr = bblock->last;

    if (last_instr->kind == X64_INSTR_JMP) {
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->jmp.target->id);
    }
    else if (last_instr->kind == X64_INSTR_JMPCC) {
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->jmpcc.true_bb->id);
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->jmpcc.false_bb->id);
    }
    else {
        assert(last_instr->kind == X64_INSTR_RET);
    }
}

void LIR_dump_proc_dot(Allocator* arena, const char* proc_name, size_t num_xbblocks, X64_BBlock** xbblocks)
{
    ftprint_out("\ndigraph %s {\n\tnode [shape=box]\n", proc_name);

    AllocatorState mem_state = allocator_get_state(arena);
    {
        for (size_t i = 0; i < num_xbblocks; i++) {
            LIR_dump_bblock_dot(arena, xbblocks[i]);
        }
    }
    allocator_restore_state(mem_state);
    ftprint_out("}\n");
}

