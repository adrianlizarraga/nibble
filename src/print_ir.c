#include "print_ir.h"
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
        else if (addr->base_kind == MEM_BASE_SYM) {
            ftprint_char_array(&dstr, false, "%s %s", (addr->base.sym->is_local ? "local" : "global"),
                               symbol_mangled_name(arena, addr->base.sym));
        }
        else if (addr->base_kind == MEM_BASE_OBJ) {
            ftprint_char_array(&dstr, false, "obj %d", addr->base.obj->id);
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

char* IR_print_instr(Allocator* arena, Instr* instr)
{
    static const char* binary_kind_name[] = {
        [INSTR_ADD] = "add", [INSTR_SUB] = "sub", [INSTR_MUL] = "mul", [INSTR_UDIV] = "udiv", [INSTR_SDIV] = "sdiv",
        [INSTR_UMOD] = "umod", [INSTR_SMOD] = "smod", [INSTR_SAR] = "sar", [INSTR_SHL] = "shl", [INSTR_AND] = "and",
        [INSTR_OR] = "or",     [INSTR_XOR] = "xor"};
    static const char* unary_kind_name[] = {[INSTR_NOT] = "not", [INSTR_NEG] = "neg"};
    static const char* convert_kind_name[] = {[INSTR_TRUNC] = "trunc", [INSTR_ZEXT] = "zext", [INSTR_SEXT] = "sext"};
    char* dstr = array_create(arena, char, 16);

    switch (instr->kind) {
    case INSTR_ADD:
    case INSTR_SUB:
    case INSTR_MUL:
    case INSTR_UDIV:
    case INSTR_SDIV:
    case INSTR_UMOD:
    case INSTR_SMOD:
    case INSTR_SAR:
    case INSTR_SHL:
    case INSTR_AND:
    case INSTR_OR:
    case INSTR_XOR: {
        const char* op_name = binary_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s, %s", op_name, type_name(instr->binary.type),
                           IR_print_reg(arena, instr->binary.r), IR_print_reg(arena, instr->binary.a),
                           IR_print_reg(arena, instr->binary.b));
        break;
    }
    case INSTR_PHI: {
        ftprint_char_array(&dstr, false, "phi <%s> %s, ", type_name(instr->phi.type), IR_print_reg(arena, instr->phi.r));

        size_t n = instr->phi.num_args;

        for (size_t i = 0; i < n; i++) {
            PhiArg* arg = instr->phi.args + i;

            ftprint_char_array(&dstr, false, "B.%u %s%s", arg->bblock->id, IR_print_reg(arena, arg->ireg), (i == n - 1 ? "" : ", "));
        }
        break;
    }
    case INSTR_NEG:
    case INSTR_NOT: {
        const char* op_name = unary_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, %s", op_name, type_name(instr->unary.type), IR_print_reg(arena, instr->unary.r),
                           IR_print_reg(arena, instr->unary.a));
        break;
    }
    case INSTR_TRUNC:
    case INSTR_ZEXT:
    case INSTR_SEXT: {
        const char* op_name = convert_kind_name[instr->kind];
        ftprint_char_array(&dstr, false, "%s <%s> %s, <%s> %s", op_name, type_name(instr->convert.dst_type),
                           IR_print_reg(arena, instr->convert.r), type_name(instr->convert.src_type),
                           IR_print_reg(arena, instr->convert.a));
        break;
    }
    case INSTR_LIMM: {
        ftprint_char_array(&dstr, false, "limm <%s> %s, %s", type_name(instr->limm.type), IR_print_reg(arena, instr->limm.r),
                           IR_print_imm(arena, instr->limm.imm));
        break;
    }
    case INSTR_LADDR: {
        ftprint_char_array(&dstr, false, "laddr %s, %s", IR_print_reg(arena, instr->laddr.r), IR_print_mem(arena, &instr->laddr.addr));
        break;
    }
    case INSTR_MEMCPY: {
        ftprint_char_array(&dstr, false, "memcpy %s, %s, %llu", IR_print_mem(arena, &instr->memcpy.dst),
                           IR_print_mem(arena, &instr->memcpy.src), instr->memcpy.size);

        break;
    }
    case INSTR_LOAD: {
        ftprint_char_array(&dstr, false, "load <%s> %s, %s", type_name(instr->load.type), IR_print_reg(arena, instr->load.r),
                           IR_print_mem(arena, &instr->load.addr));
        break;
    }
    case INSTR_STORE: {
        ftprint_char_array(&dstr, false, "store <%s> %s, %s", type_name(instr->store.type), IR_print_mem(arena, &instr->store.addr),
                           IR_print_reg(arena, instr->store.a));
        break;
    }
    case INSTR_CMP: {
        ftprint_char_array(&dstr, false, "cmp <%s> %s, %s %s %s", type_name(instr->cmp.type), IR_print_reg(arena, instr->cmp.r),
                           IR_print_reg(arena, instr->cmp.a), ir_cond_names[instr->cmp.cond], IR_print_reg(arena, instr->cmp.b));
        break;
    }
    case INSTR_JMP: {
        ftprint_char_array(&dstr, false, "jmp B.%u", instr->jmp.target->id);
        break;
    }
    case INSTR_COND_JMP: {
        ftprint_char_array(&dstr, false, "jmpcc B.%u if %s else B.%u", instr->cond_jmp.true_bb->id,
                           IR_print_reg(arena, instr->cond_jmp.a), instr->cond_jmp.false_bb->id);
        break;
    }
    case INSTR_RET: {
        IR_Value* val = &instr->ret.val;

        ftprint_char_array(&dstr, false, "ret <%s>", type_name(val->type));

        if (type_is_obj_like(val->type)) {
            ftprint_char_array(&dstr, false, " %s", IR_print_mem(arena, &val->addr));
        }
        else if (val->type != builtin_types[BUILTIN_TYPE_VOID].type) {
            ftprint_char_array(&dstr, false, " %s", IR_print_reg(arena, val->reg));
        }
        break;
    }
    case INSTR_CALL:
    case INSTR_CALL_INDIRECT: {
        bool is_indirect = instr->kind == INSTR_CALL_INDIRECT;

        Type* proc_type;
        const char* proc_name;
        IR_Value r;
        u32 num_args;
        IR_Value* args;

        if (is_indirect) {
            proc_type = instr->calli.proc_type;
            proc_name = IR_print_reg(arena, instr->calli.loc);
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
        ftprint_out("UNKNOWN_INSTR %d\n", instr->kind);
        break;
    }

    array_push(dstr, '\0');

    return dstr;
}

static void IR_print_bblock(Allocator* arena, BBlock* bblock)
{
    size_t ii = 0;

    ftprint_out("B.%d:", bblock->id);
    ftprint_out((bblock->flags & BBLOCK_IS_LOOP_HDR) ? "    # Loop header\n" : "\n");

    for (Instr* it = bblock->first; it; it = it->next, ii++) {
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

    for (Instr* it = bblock->first; it; it = it->next, ii++) {
        ftprint_out("%.3lu: %s\\l", it->ino, IR_print_instr(arena, it));
    }

    assert(ii == bblock->num_instrs);
    ftprint_out("\"]\n");

    Instr* last_instr = bblock->last;

    if (last_instr->kind == INSTR_JMP) {
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->jmp.target->id);
    }
    else if (last_instr->kind == INSTR_COND_JMP) {
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->cond_jmp.true_bb->id);
        ftprint_out("\tB%d -> B%d\n", bblock->id, last_instr->cond_jmp.false_bb->id);
    }
    else {
        assert(last_instr->kind == INSTR_RET);
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
