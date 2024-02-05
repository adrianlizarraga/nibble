#include "x64_gen/print_xir.h"
#include "x64_gen/regs.h"
#include "x64_gen/nasm_gen.h"

static char* XIR_print_mem(Allocator* arena, const XIR_MemAddr* addr)
{
    char* dstr = array_create(arena, char, 16);

    ftprint_char_array(&dstr, false, "[");

    switch (addr->kind) {
    case XIR_ADDR_GLOBAL_SYM: {
        ftprint_char_array(&dstr, false, "global %s", symbol_mangled_name(arena, addr->global));
        break;
    }
    case XIR_ADDR_FLOAT_LIT: {
        if (addr->float_lit->kind == FLOAT_F64) {
            ftprint_char_array(&dstr, false, "%f", addr->float_lit->value._f64);
        }
        else {
            ftprint_char_array(&dstr, false, "%f", addr->float_lit->value._f32);
        }
        break;
    }
    case XIR_ADDR_STR_LIT: {
        ftprint_char_array(&dstr, false, "\"%s\"", cstr_escape(arena, addr->str_lit->str, addr->str_lit->len, 0));
        break;
    }
    case XIR_ADDR_SIBD: {
        bool has_base = addr->sibd.base_reg != XIR_REG_COUNT;
        bool has_index = addr->sibd.scale && (addr->sibd.index_reg != XIR_REG_COUNT);
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
        NIBBLE_FATAL_EXIT("[INTERNAL ERROR]: Unable to print unknown XIR_MemAddr kind `%d`\n", addr->kind);
        break;
    }

    ftprint_char_array(&dstr, true, "]");

    return dstr;
}

char* XIR_print_instr(Allocator* arena, const XIR_Instr* instr)
{
    char* dstr = array_create(arena, char, 16);

    switch (instr->kind) {
    case XIR_InstrAdd_R_R_KIND: {
        const XIR_InstrAdd_R_R* act_instr = (const XIR_InstrAdd_R_R*)instr;

        ftprint_char_array(&dstr, false, "add <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrSub_R_R_KIND: {
        const XIR_InstrSub_R_R* act_instr = (const XIR_InstrSub_R_R*)instr;

        ftprint_char_array(&dstr, false, "sub <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrIMul_R_R_KIND: {
        const XIR_InstrIMul_R_R* act_instr = (const XIR_InstrIMul_R_R*)instr;

        ftprint_char_array(&dstr, false, "imul <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrAnd_R_R_KIND: {
        const XIR_InstrAnd_R_R* act_instr = (const XIR_InstrAnd_R_R*)instr;

        ftprint_char_array(&dstr, false, "and <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrOr_R_R_KIND: {
        const XIR_InstrOr_R_R* act_instr = (const XIR_InstrOr_R_R*)instr;

        ftprint_char_array(&dstr, false, "or <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrXor_R_R_KIND: {
        const XIR_InstrXor_R_R* act_instr = (const XIR_InstrXor_R_R*)instr;

        ftprint_char_array(&dstr, false, "xor <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrAdd_R_I_KIND: {
        const XIR_InstrAdd_R_I* act_instr = (const XIR_InstrAdd_R_I*)instr;

        ftprint_char_array(&dstr, false, "add <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case XIR_InstrSub_R_I_KIND: {
        const XIR_InstrSub_R_I* act_instr = (const XIR_InstrSub_R_I*)instr;

        ftprint_char_array(&dstr, false, "sub <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case XIR_InstrIMul_R_I_KIND: {
        const XIR_InstrIMul_R_I* act_instr = (const XIR_InstrIMul_R_I*)instr;

        ftprint_char_array(&dstr, false, "imul <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case XIR_InstrAnd_R_I_KIND: {
        const XIR_InstrAnd_R_I* act_instr = (const XIR_InstrAnd_R_I*)instr;

        ftprint_char_array(&dstr, false, "and <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case XIR_InstrOr_R_I_KIND: {
        const XIR_InstrOr_R_I* act_instr = (const XIR_InstrOr_R_I*)instr;

        ftprint_char_array(&dstr, false, "or <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case XIR_InstrXor_R_I_KIND: {
        const XIR_InstrXor_R_I* act_instr = (const XIR_InstrXor_R_I*)instr;

        ftprint_char_array(&dstr, false, "xor <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case XIR_InstrAdd_R_M_KIND: {
        const XIR_InstrAdd_R_M* act_instr = (const XIR_InstrAdd_R_M*)instr;

        ftprint_char_array(&dstr, false, "add <%lu> r%d, %s", act_instr->size, act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrSub_R_M_KIND: {
        const XIR_InstrSub_R_M* act_instr = (const XIR_InstrSub_R_M*)instr;

        ftprint_char_array(&dstr, false, "sub <%lu> r%d, %s", act_instr->size, act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrIMul_R_M_KIND: {
        const XIR_InstrIMul_R_M* act_instr = (const XIR_InstrIMul_R_M*)instr;

        ftprint_char_array(&dstr, false, "imul <%lu> r%d, %s", act_instr->size, act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrAnd_R_M_KIND: {
        const XIR_InstrAnd_R_M* act_instr = (const XIR_InstrAnd_R_M*)instr;

        ftprint_char_array(&dstr, false, "and <%lu> r%d, %s", act_instr->size, act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrOr_R_M_KIND: {
        const XIR_InstrOr_R_M* act_instr = (const XIR_InstrOr_R_M*)instr;

        ftprint_char_array(&dstr, false, "or <%lu> r%d, %s", act_instr->size, act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrXor_R_M_KIND: {
        const XIR_InstrXor_R_M* act_instr = (const XIR_InstrXor_R_M*)instr;

        ftprint_char_array(&dstr, false, "xor <%lu> r%d, %s", act_instr->size, act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrDiv_R_KIND: {
        const XIR_InstrDiv_R* act_instr = (const XIR_InstrDiv_R*)instr;

        ftprint_char_array(&dstr, false, "div <%lu> r%d", act_instr->size, act_instr->src);
        break;
    }
    case XIR_InstrIDiv_R_KIND: {
        const XIR_InstrIDiv_R* act_instr = (const XIR_InstrIDiv_R*)instr;

        ftprint_char_array(&dstr, false, "idiv <%lu> r%d", act_instr->size, act_instr->src);
        break;
    }
    case XIR_InstrDiv_M_KIND: {
        const XIR_InstrDiv_M* act_instr = (const XIR_InstrDiv_M*)instr;

        ftprint_char_array(&dstr, false, "div <%lu> %s", act_instr->size, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrIDiv_M_KIND: {
        const XIR_InstrIDiv_M* act_instr = (const XIR_InstrIDiv_M*)instr;

        ftprint_char_array(&dstr, false, "idiv <%lu> %s", act_instr->size, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrAddSS_R_R_KIND: {
        const XIR_InstrAddSS_R_R* act_instr = (const XIR_InstrAddSS_R_R*)instr;

        ftprint_char_array(&dstr, false, "addss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrAddSD_R_R_KIND: {
        const XIR_InstrAddSD_R_R* act_instr = (const XIR_InstrAddSD_R_R*)instr;

        ftprint_char_array(&dstr, false, "addsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrAddSS_R_M_KIND: {
        const XIR_InstrAddSS_R_M* act_instr = (const XIR_InstrAddSS_R_M*)instr;

        ftprint_char_array(&dstr, false, "addss r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrAddSD_R_M_KIND: {
        const XIR_InstrAddSD_R_M* act_instr = (const XIR_InstrAddSD_R_M*)instr;

        ftprint_char_array(&dstr, false, "addsd r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrSubSS_R_R_KIND: {
        const XIR_InstrSubSS_R_R* act_instr = (const XIR_InstrSubSS_R_R*)instr;

        ftprint_char_array(&dstr, false, "subss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrSubSD_R_R_KIND: {
        const XIR_InstrSubSD_R_R* act_instr = (const XIR_InstrSubSD_R_R*)instr;

        ftprint_char_array(&dstr, false, "subsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrSubSS_R_M_KIND: {
        const XIR_InstrSubSS_R_M* act_instr = (const XIR_InstrSubSS_R_M*)instr;

        ftprint_char_array(&dstr, false, "subss r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrSubSD_R_M_KIND: {
        const XIR_InstrSubSD_R_M* act_instr = (const XIR_InstrSubSD_R_M*)instr;

        ftprint_char_array(&dstr, false, "subsd r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrMulSS_R_R_KIND: {
        const XIR_InstrMulSS_R_R* act_instr = (const XIR_InstrMulSS_R_R*)instr;

        ftprint_char_array(&dstr, false, "mulss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMulSD_R_R_KIND: {
        const XIR_InstrMulSD_R_R* act_instr = (const XIR_InstrMulSD_R_R*)instr;

        ftprint_char_array(&dstr, false, "mulsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMulSS_R_M_KIND: {
        const XIR_InstrMulSS_R_M* act_instr = (const XIR_InstrMulSS_R_M*)instr;

        ftprint_char_array(&dstr, false, "mulss r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrMulSD_R_M_KIND: {
        const XIR_InstrMulSD_R_M* act_instr = (const XIR_InstrMulSD_R_M*)instr;

        ftprint_char_array(&dstr, false, "mulsd r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrDivSS_R_R_KIND: {
        const XIR_InstrDivSS_R_R* act_instr = (const XIR_InstrDivSS_R_R*)instr;

        ftprint_char_array(&dstr, false, "divss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrDivSD_R_R_KIND: {
        const XIR_InstrDivSD_R_R* act_instr = (const XIR_InstrDivSD_R_R*)instr;

        ftprint_char_array(&dstr, false, "divsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrDivSS_R_M_KIND: {
        const XIR_InstrDivSS_R_M* act_instr = (const XIR_InstrDivSS_R_M*)instr;

        ftprint_char_array(&dstr, false, "divss r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrDivSD_R_M_KIND: {
        const XIR_InstrDivSD_R_M* act_instr = (const XIR_InstrDivSD_R_M*)instr;

        ftprint_char_array(&dstr, false, "divsd r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrSExtAxToDx_KIND: {
        const XIR_InstrSExtAxToDx* act_instr = (const XIR_InstrSExtAxToDx*)instr;

        ftprint_char_array(&dstr, false, "%s", x64_sext_ax_into_dx[act_instr->size]);
        break;
    }
    case XIR_InstrSar_R_R_KIND: {
        const XIR_InstrSar_R_R* act_instr = (const XIR_InstrSar_R_R*)instr;

        ftprint_char_array(&dstr, false, "sar <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrShl_R_R_KIND: {
        const XIR_InstrShl_R_R* act_instr = (const XIR_InstrShl_R_R*)instr;

        ftprint_char_array(&dstr, false, "shl <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrSar_R_I_KIND: {
        const XIR_InstrSar_R_I* act_instr = (const XIR_InstrSar_R_I*)instr;

        ftprint_char_array(&dstr, false, "sar <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u8);
        break;
    }
    case XIR_InstrShl_R_I_KIND: {
        const XIR_InstrShl_R_I* act_instr = (const XIR_InstrShl_R_I*)instr;

        ftprint_char_array(&dstr, false, "shl <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u8);
        break;
    }
    case XIR_InstrNeg_KIND: {
        const XIR_InstrNeg* act_instr = (const XIR_InstrNeg*)instr;

        ftprint_char_array(&dstr, false, "neg <%lu> r%d", act_instr->size, act_instr->dst);
        break;
    }
    case XIR_InstrNot_KIND: {
        const XIR_InstrNot* act_instr = (const XIR_InstrNot*)instr;

        ftprint_char_array(&dstr, false, "not <%lu> r%d", act_instr->size, act_instr->dst);
        break;
    }
    case XIR_InstrRepMovsb_KIND: {
        ftprint_char_array(&dstr, false, "rep movsb");
        break;
    }
    case XIR_InstrSyscall_KIND: {
        ftprint_char_array(&dstr, false, "syscall");
        break;
    }
    case XIR_InstrRepStosb_KIND: {
        ftprint_char_array(&dstr, false, "rep stosb");
        break;
    }
    case XIR_InstrMov_R_RH_KIND: {
        const XIR_InstrMov_R_RH* act_instr = (const XIR_InstrMov_R_RH*)instr;

        ftprint_char_array(&dstr, false, "mov <1> r%d, r%d[h]", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMov_R_R_KIND: {
        const XIR_InstrMov_R_R* act_instr = (const XIR_InstrMov_R_R*)instr;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, r%d", act_instr->size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMov_R_I_KIND: {
        const XIR_InstrMov_R_I* act_instr = (const XIR_InstrMov_R_I*)instr;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, 0x%lx", act_instr->size, act_instr->dst, act_instr->src.as_int._u64);
        break;
    }
    case XIR_InstrMov_R_M_KIND: {
        const XIR_InstrMov_R_M* act_instr = (const XIR_InstrMov_R_M*)instr;

        ftprint_char_array(&dstr, false, "mov <%lu> r%d, %s", act_instr->size, act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrMov_M_R_KIND: {
        const XIR_InstrMov_M_R* act_instr = (const XIR_InstrMov_M_R*)instr;

        ftprint_char_array(&dstr, false, "mov <%lu> %s, r%d", act_instr->size, XIR_print_mem(arena, &act_instr->dst), act_instr->src);
        break;
    }
    case XIR_InstrMov_M_I_KIND: {
        const XIR_InstrMov_M_I* act_instr = (const XIR_InstrMov_M_I*)instr;

        ftprint_char_array(&dstr, false, "mov <%lu> %s, 0x%lx", act_instr->size, XIR_print_mem(arena, &act_instr->dst),
                           act_instr->src.as_int._u64);
        break;
    }
    case XIR_InstrMovZX_R_R_KIND: {
        const XIR_InstrMovZX_R_R* act_instr = (const XIR_InstrMovZX_R_R*)instr;

        ftprint_char_array(&dstr, false, "movzx <%lu> r%d, <%lu> r%d", act_instr->dst_size, act_instr->dst, act_instr->src_size,
                           act_instr->src);
        break;
    }
    case XIR_InstrMovZX_R_M_KIND: {
        const XIR_InstrMovZX_R_M* act_instr = (const XIR_InstrMovZX_R_M*)instr;

        ftprint_char_array(&dstr, false, "movzx <%lu> r%d, <%lu> %s", act_instr->dst_size, act_instr->dst, act_instr->src_size,
                           XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrMovSX_R_R_KIND: {
        const XIR_InstrMovSX_R_R* act_instr = (const XIR_InstrMovSX_R_R*)instr;
        u8 dst_size = act_instr->dst_size;
        u8 src_size = act_instr->src_size;
        const char* movsx = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? "movsxd" : "movsx";

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, <%lu> r%d", movsx, dst_size, act_instr->dst, src_size, act_instr->src);
        break;
    }
    case XIR_InstrMovSX_R_M_KIND: {
        const XIR_InstrMovSX_R_M* act_instr = (const XIR_InstrMovSX_R_M*)instr;
        u8 dst_size = act_instr->dst_size;
        u8 src_size = act_instr->src_size;
        const char* movsx = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? "movsxd" : "movsx";

        ftprint_char_array(&dstr, false, "%s <%lu> r%d, <%lu> %s", movsx, dst_size, act_instr->dst, src_size,
                           XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrMovSS_R_R_KIND: {
        const XIR_InstrMovSS_R_R* act_instr = (const XIR_InstrMovSS_R_R*)instr;

        ftprint_char_array(&dstr, false, "movss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMovSD_R_R_KIND: {
        const XIR_InstrMovSD_R_R* act_instr = (const XIR_InstrMovSD_R_R*)instr;

        ftprint_char_array(&dstr, false, "movsd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrMovSS_R_M_KIND: {
        const XIR_InstrMovSS_R_M* act_instr = (const XIR_InstrMovSS_R_M*)instr;

        ftprint_char_array(&dstr, false, "movss r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrMovSD_R_M_KIND: {
        const XIR_InstrMovSD_R_M* act_instr = (const XIR_InstrMovSD_R_M*)instr;

        ftprint_char_array(&dstr, false, "movsd r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrMovSS_M_R_KIND: {
        const XIR_InstrMovSS_M_R* act_instr = (const XIR_InstrMovSS_M_R*)instr;

        ftprint_char_array(&dstr, false, "movss %s, r%d", XIR_print_mem(arena, &act_instr->dst), act_instr->src);
        break;
    }
    case XIR_InstrMovSD_M_R_KIND: {
        const XIR_InstrMovSD_M_R* act_instr = (const XIR_InstrMovSD_M_R*)instr;

        ftprint_char_array(&dstr, false, "movsd %s, r%d", XIR_print_mem(arena, &act_instr->dst), act_instr->src);
        break;
    }
    case XIR_InstrCvtSS2SD_R_R_KIND: {
        const XIR_InstrCvtSS2SD_R_R* act_instr = (const XIR_InstrCvtSS2SD_R_R*)instr;

        ftprint_char_array(&dstr, false, "cvtss2sd r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrCvtSD2SS_R_R_KIND: {
        const XIR_InstrCvtSD2SS_R_R* act_instr = (const XIR_InstrCvtSD2SS_R_R*)instr;

        ftprint_char_array(&dstr, false, "cvtsd2ss r%d, r%d", act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrCvtSS2SD_R_M_KIND: {
        const XIR_InstrCvtSS2SD_R_M* act_instr = (const XIR_InstrCvtSS2SD_R_M*)instr;

        ftprint_char_array(&dstr, false, "cvtss2sd r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrCvtSD2SS_R_M_KIND: {
        const XIR_InstrCvtSD2SS_R_M* act_instr = (const XIR_InstrCvtSD2SS_R_M*)instr;

        ftprint_char_array(&dstr, false, "cvtsd2ss r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrCvtSS2SI_R_R_KIND: {
        const XIR_InstrCvtSS2SI_R_R* act_instr = (const XIR_InstrCvtSS2SI_R_R*)instr;

        ftprint_char_array(&dstr, false, "cvttss2si <%lu> r%d, r%d", act_instr->dst_size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrCvtSD2SI_R_R_KIND: {
        const XIR_InstrCvtSD2SI_R_R* act_instr = (const XIR_InstrCvtSD2SI_R_R*)instr;

        ftprint_char_array(&dstr, false, "cvttsd2si <%lu> r%d, r%d", act_instr->dst_size, act_instr->dst, act_instr->src);
        break;
    }
    case XIR_InstrCvtSS2SI_R_M_KIND: {
        const XIR_InstrCvtSS2SI_R_M* act_instr = (const XIR_InstrCvtSS2SI_R_M*)instr;

        ftprint_char_array(&dstr, false, "cvttss2si <%lu> r%d, %s", act_instr->dst_size, act_instr->dst,
                           XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrCvtSD2SI_R_M_KIND: {
        const XIR_InstrCvtSD2SI_R_M* act_instr = (const XIR_InstrCvtSD2SI_R_M*)instr;

        ftprint_char_array(&dstr, false, "cvttsd2si <%lu> r%d, %s", act_instr->dst_size, act_instr->dst,
                           XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrCvtSI2SS_R_R_KIND: {
        const XIR_InstrCvtSI2SS_R_R* act_instr = (const XIR_InstrCvtSI2SS_R_R*)instr;

        ftprint_char_array(&dstr, false, "cvtsi2ss r%d, <%lu> r%d", act_instr->dst, act_instr->src_size, act_instr->src);
        break;
    }
    case XIR_InstrCvtSI2SD_R_R_KIND: {
        const XIR_InstrCvtSI2SD_R_R* act_instr = (const XIR_InstrCvtSI2SD_R_R*)instr;

        ftprint_char_array(&dstr, false, "cvtsi2sd r%d, <%lu> r%d", act_instr->dst, act_instr->src_size, act_instr->src);
        break;
    }
    case XIR_InstrCvtSI2SS_R_M_KIND: {
        const XIR_InstrCvtSI2SS_R_M* act_instr = (const XIR_InstrCvtSI2SS_R_M*)instr;

        ftprint_char_array(&dstr, false, "cvtsi2ss r%d, <%lu> %s", act_instr->dst, act_instr->src_size,
                           XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrCvtSI2SD_R_M_KIND: {
        const XIR_InstrCvtSI2SD_R_M* act_instr = (const XIR_InstrCvtSI2SD_R_M*)instr;

        ftprint_char_array(&dstr, false, "cvtsi2sd r%d, <%lu> %s", act_instr->dst, act_instr->src_size,
                           XIR_print_mem(arena, &act_instr->src));
        break;
    }
    case XIR_InstrLEA_KIND: {
        const XIR_InstrLEA* act_instr = (const XIR_InstrLEA*)instr;

        ftprint_char_array(&dstr, false, "lea r%d, %s", act_instr->dst, XIR_print_mem(arena, &act_instr->mem));
        break;
    }
    case XIR_InstrCmp_R_R_KIND: {
        const XIR_InstrCmp_R_R* act_instr = (const XIR_InstrCmp_R_R*)instr;
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, r%d", size, act_instr->op1, act_instr->op2);
        break;
    }
    case XIR_InstrCmp_R_I_KIND: {
        const XIR_InstrCmp_R_I* act_instr = (const XIR_InstrCmp_R_I*)instr;
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, 0x%lx", size, act_instr->op1, act_instr->op2.as_int._u64);
        break;
    }
    case XIR_InstrCmp_R_M_KIND: {
        const XIR_InstrCmp_R_M* act_instr = (const XIR_InstrCmp_R_M*)instr;
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> r%d, %s", size, act_instr->op1, XIR_print_mem(arena, &act_instr->op2));
        break;
    }
    case XIR_InstrCmp_M_R_KIND: {
        const XIR_InstrCmp_M_R* act_instr = (const XIR_InstrCmp_M_R*)instr;
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> %s, r%d", size, XIR_print_mem(arena, &act_instr->op1), act_instr->op2);
        break;
    }
    case XIR_InstrCmp_M_I_KIND: {
        const XIR_InstrCmp_M_I* act_instr = (const XIR_InstrCmp_M_I*)instr;
        const u8 size = act_instr->size;

        ftprint_char_array(&dstr, false, "cmp <%lu> %s, 0x%lx", size, XIR_print_mem(arena, &act_instr->op1),
                           act_instr->op2.as_int._u64);
        break;
    }
    case XIR_InstrUComiSS_R_R_KIND: {
        const XIR_InstrUComiSS_R_R* act_instr = (const XIR_InstrUComiSS_R_R*)instr;

        ftprint_char_array(&dstr, false, "ucomiss r%d, r%d", act_instr->op1, act_instr->op2);
        break;
    }
    case XIR_InstrUComiSD_R_R_KIND: {
        const XIR_InstrUComiSD_R_R* act_instr = (const XIR_InstrUComiSD_R_R*)instr;

        ftprint_char_array(&dstr, false, "ucomisd r%d, r%d", act_instr->op1, act_instr->op2);
        break;
    }
    case XIR_InstrUComiSS_R_M_KIND: {
        const XIR_InstrUComiSS_R_M* act_instr = (const XIR_InstrUComiSS_R_M*)instr;

        ftprint_char_array(&dstr, false, "ucomiss r%d, %s", act_instr->op1, XIR_print_mem(arena, &act_instr->op2));
        break;
    }
    case XIR_InstrUComiSD_R_M_KIND: {
        const XIR_InstrUComiSD_R_M* act_instr = (const XIR_InstrUComiSD_R_M*)instr;

        ftprint_char_array(&dstr, false, "ucomisd r%d, %s", act_instr->op1, XIR_print_mem(arena, &act_instr->op2));
        break;
    }
    case XIR_InstrJmp_KIND: {
        const XIR_InstrJmp* act_instr = (const XIR_InstrJmp*)instr;

        ftprint_char_array(&dstr, false, "jmp B.%ld", act_instr->target);
        break;
    }
    case XIR_InstrJmpCC_KIND: {
        const XIR_InstrJmpCC* act_instr = (const XIR_InstrJmpCC*)instr;

        ftprint_char_array(&dstr, false, "j%s B.%ld else B.%ld", x64_condition_codes[act_instr->cond], act_instr->true_bb,
                           act_instr->false_bb);
        break;
    }
    case XIR_InstrSetCC_KIND: {
        const XIR_InstrSetCC* act_instr = (const XIR_InstrSetCC*)instr;
        ftprint_char_array(&dstr, false, "set%s r%d", x64_condition_codes[act_instr->cond], act_instr->dst);
        break;
    }
    case XIR_InstrRet_KIND: {
        ftprint_char_array(&dstr, false, "ret");

        break;
    }
    case XIR_InstrCall_KIND:
    case XIR_InstrCall_R_KIND: {
        Type* proc_type;
        XIR_CallValue dst_val;
        u32 num_args;
        XIR_InstrCallArg* args;

        if (instr->kind == XIR_InstrCall_KIND) {
            const XIR_InstrCall* instr_call = (const XIR_InstrCall*)instr;

            proc_type = instr_call->sym->type;
            dst_val = instr_call->dst;
            num_args = instr_call->num_args;
            args = instr_call->args;
        }
        else {
            assert(instr->kind == XIR_InstrCall_R_KIND);
            const XIR_InstrCall_R* instr_call_r = (const XIR_InstrCall_R*)instr;

            proc_type = instr_call_r->proc_type;
            dst_val = instr_call_r->dst;
            num_args = instr_call_r->num_args;
            args = instr_call_r->args;
        }

        Type* ret_type = proc_type->as_proc.ret;

        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            if (type_is_obj_like(ret_type)) {
                ftprint_char_array(&dstr, false, "<%lu> %s = ", ret_type->size, XIR_print_mem(arena, &dst_val.addr));
            }
            else {
                ftprint_char_array(&dstr, false, "<%lu> r%d = ", ret_type->size, dst_val.reg);
            }
        }

        if (instr->kind == XIR_InstrCall_KIND) {
            const XIR_InstrCall* instr_call = (const XIR_InstrCall*)instr;

            ftprint_char_array(&dstr, false, "call %s (", symbol_mangled_name(arena, instr_call->sym));
        }
        else {
            const XIR_InstrCall_R* instr_call_r = (const XIR_InstrCall_R*)instr;

            ftprint_char_array(&dstr, false, "call r%d (", instr_call_r->proc_loc);
        }

        if (num_args) {
            for (u32 i = 0; i < num_args; i += 1) {
                XIR_InstrCallArg* arg = args + i;

                if (type_is_obj_like(arg->type)) {
                    ftprint_char_array(&dstr, false, "<%s> %s", type_name(arg->type), XIR_print_mem(arena, &arg->val.addr));
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

static void XIR_dump_bblock_dot(Allocator* arena, XIR_BBlock* bblock)
{
    size_t ii = 0;

    ftprint_out("\tB%d [", bblock->id);
    if (bblock->flags & BBLOCK_IS_LOOP_HDR) {
        ftprint_out("style=filled, color=lightgrey, label=\"B%d\\n\\n", bblock->id);
    }
    else {
        ftprint_out("label=\"B%d\\n\\n", bblock->id);
    }

    for (XIR_Instr* it = bblock->first; it; it = it->next, ii++) {
        ftprint_out("%.3lu: %s\\l", it->ino, XIR_print_instr(arena, it));
    }

    assert(ii == bblock->num_instrs);
    ftprint_out("\"]\n");

    XIR_Instr* last_instr = bblock->last;

    if (last_instr->kind == XIR_InstrJmp_KIND) {
        XIR_InstrJmp* instr_jmp = (XIR_InstrJmp*)last_instr;

        ftprint_out("\tB%d -> B%d\n", bblock->id, instr_jmp->target);
    }
    else if (last_instr->kind == XIR_InstrJmpCC_KIND) {
        XIR_InstrJmpCC* instr_jmpcc = (XIR_InstrJmpCC*)last_instr;

        ftprint_out("\tB%d -> B%d\n", bblock->id, instr_jmpcc->true_bb);
        ftprint_out("\tB%d -> B%d\n", bblock->id, instr_jmpcc->false_bb);
    }
    else {
        assert(last_instr->kind == XIR_InstrRet_KIND);
    }
}

void XIR_dump_proc_dot(Allocator* arena, const char* proc_name, size_t num_xbblocks, XIR_BBlock** xbblocks)
{
    ftprint_out("\ndigraph %s {\n\tnode [shape=box]\n", proc_name);

    AllocatorState mem_state = allocator_get_state(arena);
    {
        for (size_t i = 0; i < num_xbblocks; i++) {
            XIR_dump_bblock_dot(arena, xbblocks[i]);
        }
    }
    allocator_restore_state(mem_state);
    ftprint_out("}\n");
}
