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
    case X64_ADDR_FLOAT_LIT: {
        if (addr->float_lit->kind == FLOAT_F64) {
            ftprint_char_array(&dstr, false, "%f", addr->float_lit->value._f64);
        }
        else {
            ftprint_char_array(&dstr, false, "%f", addr->float_lit->value._f32);
        }
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
        [X64_InstrAdd_R_R_KIND] = "add",
        [X64_InstrSub_R_R_KIND] = "sub",
        [X64_InstrIMul_R_R_KIND] = "imul",
        [X64_InstrAnd_R_R_KIND] = "and",
        [X64_InstrOr_R_R_KIND] = "or",
        [X64_InstrXor_R_R_KIND] = "xor"
    };

    static const char* binary_r_i_name[] = {
        [X64_InstrAdd_R_I_KIND] = "add",
        [X64_InstrSub_R_I_KIND] = "sub",
        [X64_InstrIMul_R_I_KIND] = "imul",
        [X64_InstrAnd_R_I_KIND] = "and",
        [X64_InstrOr_R_I_KIND] = "or",
        [X64_InstrXor_R_I_KIND] = "xor"
    };

    static const char* binary_r_m_name[] = {
        [X64_InstrAdd_R_M_KIND] = "add",
        [X64_InstrSub_R_M_KIND] = "sub",
        [X64_InstrIMul_R_M_KIND] = "imul",
        [X64_InstrAnd_R_M_KIND] = "and",
        [X64_InstrOr_R_M_KIND] = "or",
        [X64_InstrXor_R_M_KIND] = "xor"
    };

    static const char* shift_r_r_name[] = {
        [X64_InstrSar_R_R_KIND] = "sar",
        [X64_InstrShl_R_R_KIND] = "shl"
    };

    static const char* shift_r_i_name[] = {
        [X64_InstrSar_R_I_KIND] = "sar",
        [X64_InstrShl_R_I_KIND] = "shl"
    };

    static const char* unary_name[] = {
        [X64_InstrNeg_KIND] = "neg",
        [X64_InstrNot_KIND] = "not"
    };

    char* dstr = array_create(arena, char, 16);

    switch (instr->kind) {
    case X64_InstrAdd_R_R_KIND:
    case X64_InstrSub_R_R_KIND:
    case X64_InstrIMul_R_R_KIND:
    case X64_InstrAnd_R_R_KIND:
    case X64_InstrOr_R_R_KIND:
    case X64_InstrXor_R_R_KIND: {
        X64_InstrBinary_R_R* act_instr = (X64_InstrBinary_R_R*)instr;
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, r%d", binary_r_r_name[instr->kind], size,
                           act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrAdd_R_I_KIND:
    case X64_InstrSub_R_I_KIND:
    case X64_InstrIMul_R_I_KIND:
    case X64_InstrAnd_R_I_KIND:
    case X64_InstrOr_R_I_KIND:
    case X64_InstrXor_R_I_KIND: {
        X64_InstrBinary_R_I* act_instr = (X64_InstrBinary_R_I*)instr;
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, 0x%lx", binary_r_i_name[instr->kind], size,
                           act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case X64_InstrAdd_R_M_KIND:
    case X64_InstrSub_R_M_KIND:
    case X64_InstrIMul_R_M_KIND:
    case X64_InstrAnd_R_M_KIND:
    case X64_InstrOr_R_M_KIND:
    case X64_InstrXor_R_M_KIND: {
        X64_InstrBinary_R_M* act_instr = (X64_InstrBinary_R_M*)instr;
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, %s", binary_r_m_name[instr->kind], size,
                           act_instr->dst, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrDiv_R_KIND:
    case X64_InstrIDiv_R_KIND: {
        X64_InstrBaseDiv_R* act_instr = (X64_InstrBaseDiv_R*)instr;
        const char* instr_name = instr->kind == X64_InstrIDiv_R_KIND ? "idiv" : "div";
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d", instr_name, size, act_instr->src);
        break;
    }
    case X64_InstrDiv_M_KIND:
    case X64_InstrIDiv_M_KIND: {
        X64_InstrBaseDiv_M* act_instr = (X64_InstrBaseDiv_M*)instr;
        const char* instr_name = instr->kind == X64_InstrIDiv_M_KIND ? "idiv" : "div";
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "%s <%lu> %s", instr_name, size, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrAddSS_R_R_KIND: {
        X64_InstrAddSS_R_R* act_instr = (X64_InstrAddSS_R_R*)instr;

        ftprint_char_array(&dstr, false, "addss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrAddSD_R_R_KIND: { // Left off here!
        ftprint_char_array(&dstr, false, "addsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrAddSS_R_M_KIND: {
        ftprint_char_array(&dstr, false, "addss r%d, %s", act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrAddSD_R_M_KIND: {
        ftprint_char_array(&dstr, false, "addsd r%d, %s", act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrSubSS_R_R_KIND: {
        ftprint_char_array(&dstr, false, "subss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrSubSD_R_R_KIND: {
        ftprint_char_array(&dstr, false, "subsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrSubSS_R_M_KIND: {
        ftprint_char_array(&dstr, false, "subss r%d, %s", act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrSubSD_R_M_KIND: {
        ftprint_char_array(&dstr, false, "subsd r%d, %s", act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrMulSS_R_R_KIND: {
        ftprint_char_array(&dstr, false, "mulss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMulSD_R_R_KIND: {
        ftprint_char_array(&dstr, false, "mulsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMulSS_R_M_KIND: {
        ftprint_char_array(&dstr, false, "mulss r%d, %s", act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrMulSD_R_M_KIND: {
        ftprint_char_array(&dstr, false, "mulsd r%d, %s", act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrDivSS_R_R_KIND: {
        ftprint_char_array(&dstr, false, "divss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrDivSD_R_R_KIND: {
        ftprint_char_array(&dstr, false, "divsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrDivSS_R_M_KIND: {
        ftprint_char_array(&dstr, false, "divss r%d, %s", act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrDivSD_R_M_KIND: {
        ftprint_char_array(&dstr, false, "divsd r%d, %s", act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrSExtAxToDx_KIND: {
        const u8 size = act_instr->size;
        ftprint_char_array(&dstr, false, "%s", x64_sext_ax_into_dx[size]);
        break;
    }
    case X64_InstrSar_R_R_KIND:
    case X64_InstrShl_R_R_KIND: {
        const u8 dst_size = act_instr->size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, r%d", shift_r_r_name[instr->kind], dst_size, act_instr->dst,
                           act_instr->src);
        break;
    }
    case X64_InstrSar_R_I_KIND:
    case X64_InstrShl_R_I_KIND: {
        const u8 dst_size = act_instr->size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, 0x%lx", shift_r_i_name[instr->kind], dst_size, act_instr->dst,
                           act_instr->src.as_int._u8);
        break;
    }
    case X64_InstrNeg_KIND:
    case X64_InstrNot_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "%s <%lu> r%d", unary_name[instr->kind], size, act_instr->dst);
        break;
    }
    case X64_InstrRepMovsb_KIND: {
        ftprint_char_array(&dstr, false, "rep movsb");
        break;
    }
    case X64_InstrSyscall_KIND: {
        ftprint_char_array(&dstr, false, "syscall");
        break;
    }
    case X64_InstrRepStosb_KIND: {
        ftprint_char_array(&dstr, false, "rep stosb");
        break;
    }
    case X64_InstrMov_R_RH_KIND: {
        ftprint_char_array(&dstr, false, "mov <1> r%d, r%d[h]", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMov_R_R_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, r%d", size, act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMov_R_I_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, 0x%lx", size, act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case X64_InstrMov_R_M_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, %s", size, act_instr->dst, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrMov_M_R_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "mov <%lu> %s, r%d", size, LIR_print_mem(arena, &act_instr->dst), act_instr->src);
        break;
    }
    case X64_InstrMov_M_I_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "mov <%lu> %s, 0x%lx", size, LIR_print_mem(arena, &act_instr->dst),
                           act_instr->src.as_int._u64);
        break;
    }
    case X64_InstrMovZX_R_R_KIND: {
        u8 dst_size = act_instr->dst_size;
        u8 src_size = act_instr->src_size;

        ftprint_char_array(&dstr, false, "movzx <%lu> r%d, <%lu> r%d", dst_size, act_instr->dst,
                           src_size, act_instr->src);
        break;
    }
    case X64_InstrMovZX_R_M_KIND: {
        u8 dst_size = act_instr->dst_size;
        u8 src_size = act_instr->src_size;

        ftprint_char_array(&dstr, false, "movzx <%lu> r%d, <%lu> %s", dst_size, act_instr->dst,
                           src_size, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrMovSX_R_R_KIND: {
        u8 dst_size = act_instr->dst_size;
        u8 src_size = act_instr->src_size;
        const char* movsx = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? "movsxd" : "movsx";

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, <%lu> r%d", movsx, dst_size, act_instr->dst,
                           src_size, act_instr->src);
        break;
    }
    case X64_InstrMovSX_R_M_KIND: {
        u8 dst_size = act_instr->dst_size;
        u8 src_size = act_instr->src_size;
        const char* movsx = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? "movsxd" : "movsx";

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, <%lu> %s", movsx, dst_size, act_instr->dst,
                           src_size, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrMovSS_R_R_KIND: {
        ftprint_char_array(&dstr, false, "movss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMovSD_R_R_KIND: {
        ftprint_char_array(&dstr, false, "movsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrMovSS_R_M_KIND: {
        ftprint_char_array(&dstr, false, "movss r%d, %s", act_instr->dst, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrMovSD_R_M_KIND: {
        ftprint_char_array(&dstr, false, "movsd r%d, %s", act_instr->dst, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrMovSS_M_R_KIND: {
        ftprint_char_array(&dstr, false, "movss %s, r%d", LIR_print_mem(arena, &act_instr->dst), act_instr->src);
        break;
    }
    case X64_InstrMovSD_M_R_KIND: {
        ftprint_char_array(&dstr, false, "movsd %s, r%d", LIR_print_mem(arena, &act_instr->dst), act_instr->src);
        break;
    }
    case X64_InstrCvtSS2SD_R_R_KIND: {
        ftprint_char_array(&dstr, false, "cvtss2sd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrCvtSD2SS_R_R_KIND: {
        ftprint_char_array(&dstr, false, "cvtsd2ss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case X64_InstrCvtSS2SD_R_M_KIND: {
        ftprint_char_array(&dstr, false, "cvtss2sd r%d, %s", act_instr->dst, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrCvtSD2SS_R_M_KIND: {
        ftprint_char_array(&dstr, false, "cvtsd2ss r%d, %s", act_instr->dst, LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrCvtSS2SI_R_R_KIND: {
        ftprint_char_array(&dstr, false, "cvttss2si <%lu> r%d, r%d", act_instr->dst_size, act_instr->dst,
                           act_instr->src);
        break;
    }
    case X64_InstrCvtSD2SI_R_R_KIND: {
        ftprint_char_array(&dstr, false, "cvttsd2si <%lu> r%d, r%d", act_instr->dst_size, act_instr->dst,
                           act_instr->src);
        break;
    }
    case X64_InstrCvtSS2SI_R_M_KIND: {
        ftprint_char_array(&dstr, false, "cvttss2si <%lu> r%d, %s", act_instr->dst_size, act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrCvtSD2SI_R_M_KIND: {
        ftprint_char_array(&dstr, false, "cvttsd2si <%lu> r%d, %s", act_instr->dst_size, act_instr->dst,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrCvtSI2SS_R_R_KIND: {
        ftprint_char_array(&dstr, false, "cvtsi2ss r%d, <%lu> r%d", act_instr->dst, act_instr->src_size,
                           act_instr->src);
        break;
    }
    case X64_InstrCvtSI2SD_R_R_KIND: {
        ftprint_char_array(&dstr, false, "cvtsi2sd r%d, <%lu> r%d", act_instr->dst, act_instr->src_size,
                           act_instr->src);
        break;
    }
    case X64_InstrCvtSI2SS_R_M_KIND: {
        ftprint_char_array(&dstr, false, "cvtsi2ss r%d, <%lu> %s", act_instr->dst, act_instr->src_size,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrCvtSI2SD_R_M_KIND: {
        ftprint_char_array(&dstr, false, "cvtsi2sd r%d, <%lu> %s", act_instr->dst, act_instr->src_size,
                           LIR_print_mem(arena, &act_instr->src));
        break;
    }
    case X64_InstrLEA_KIND: {
        ftprint_char_array(&dstr, false, "lea r%d, %s", act_instr->dst, LIR_print_mem(arena, &act_instr->mem));
        break;
    }
    case X64_InstrCmp_R_R_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, r%d", size, act_instr->op1, act_instr->op2);
        break;
    }
    case X64_InstrCmp_R_I_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, 0x%lx", size, act_instr->op1, act_instr->op2.as_int._u64);
        break;
    }
    case X64_InstrCmp_R_M_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, %s", size, act_instr->op1, LIR_print_mem(arena, &act_instr->op2));
        break;
    }
    case X64_InstrCmp_M_R_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> %s, r%d", size, LIR_print_mem(arena, &act_instr->op1), act_instr->op2);
        break;
    }
    case X64_InstrCmp_M_I_KIND: {
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> %s, 0x%lx", size, LIR_print_mem(arena, &act_instr->op1),
                           act_instr->op2.as_int._u64);
        break;
    }
    case X64_InstrUComiSS_R_R_KIND: {
        ftprint_char_array(&dstr, false, "ucomiss r%d, r%d", act_instr->op1, act_instr->op2);
        break;
    }
    case X64_InstrUComiSD_R_R_KIND: {
        ftprint_char_array(&dstr, false, "ucomisd r%d, r%d", act_instr->op1, act_instr->op2);
        break;
    }
    case X64_InstrUComiSS_R_M_KIND: {
        ftprint_char_array(&dstr, false, "ucomiss r%d, %s", act_instr->op1, LIR_print_mem(arena, &act_instr->op2));
        break;
    }
    case X64_InstrUComiSD_R_M_KIND: {
        ftprint_char_array(&dstr, false, "ucomisd r%d, %s", act_instr->op1, LIR_print_mem(arena, &act_instr->op2));
        break;
    }
    case X64_InstrJmp_KIND: {
        ftprint_char_array(&dstr, false, "jmp B.%ld", act_instr->target->id);
        break;
    }
    case X64_InstrJmpCC_KIND: {
        ftprint_char_array(&dstr, false, "j%s B.%ld else B.%ld", x64_condition_codes[act_instr->cond],
                           act_instr->true_bb->id, act_instr->false_bb->id);
        break;
    }
    case X64_InstrSetCC_KIND: {
        ftprint_char_array(&dstr, false, "set%s r%d", x64_condition_codes[act_instr->cond], act_instr->dst);
        break;
    }
    case X64_InstrRet_KIND: {
        ftprint_char_array(&dstr, false, "ret");

        break;
    }
    case X64_InstrCall_KIND:
    case X64_InstrCall_R_KIND: {
        Type* proc_type;
        X64_CallValue dst_val;
        u32 num_args;
        X64_InstrCallArg* args;

        if (instr->kind == X64_InstrCall_KIND) {
            proc_type = instr->call.sym->type;
            dst_val = instr->call.dst;
            num_args = instr->call.num_args;
            args = instr->call.args;
        }
        else {
            assert(instr->kind == X64_InstrCall_R_KIND);
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

        if (instr->kind == X64_InstrCall_KIND) {
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

    if (last_instr->kind == X64_InstrJmp_KIND) {
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->jmp.target->id);
    }
    else if (last_instr->kind == X64_InstrJmpCC_KIND) {
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->jmpcc.true_bb->id);
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->jmpcc.false_bb->id);
    }
    else {
        assert(last_instr->kind == X64_InstrRet_KIND);
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

