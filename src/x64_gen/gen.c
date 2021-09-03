#include "stream.h"
#include "x64_gen.h"

#include "x64_gen/regs.c"
#include "x64_gen/reg_alloc.c"

#define X64_INIT_LINE_LEN 128

static const char* x64_reg_names[X64_MAX_INT_REG_SIZE + 1][X64_REG_COUNT] = {
    [1] =
        {
            [X64_RAX] = "al",
            [X64_RCX] = "cl",
            [X64_RDX] = "dl",
            [X64_RBX] = "bl",
            [X64_RSP] = "spl",
            [X64_RBP] = "bpl",
            [X64_RSI] = "sil",
            [X64_RDI] = "dil",
            [X64_R8] = "r8b",
            [X64_R9] = "r9b",
            [X64_R10] = "r10b",
            [X64_R11] = "r11b",
            [X64_R12] = "r12b",
            [X64_R13] = "r13b",
            [X64_R14] = "r14b",
            [X64_R15] = "r15b",
        },
    [2] =
        {
            [X64_RAX] = "ax",
            [X64_RCX] = "cx",
            [X64_RDX] = "dx",
            [X64_RBX] = "bx",
            [X64_RSP] = "sp",
            [X64_RBP] = "bp",
            [X64_RSI] = "si",
            [X64_RDI] = "di",
            [X64_R8] = "r8w",
            [X64_R9] = "r9w",
            [X64_R10] = "r10w",
            [X64_R11] = "r11w",
            [X64_R12] = "r12w",
            [X64_R13] = "r13w",
            [X64_R14] = "r14w",
            [X64_R15] = "r15w",
        },
    [4] =
        {
            [X64_RAX] = "eax",
            [X64_RCX] = "ecx",
            [X64_RDX] = "edx",
            [X64_RBX] = "ebx",
            [X64_RSP] = "esp",
            [X64_RBP] = "ebp",
            [X64_RSI] = "esi",
            [X64_RDI] = "edi",
            [X64_R8] = "r8d",
            [X64_R9] = "r9d",
            [X64_R10] = "r10d",
            [X64_R11] = "r11d",
            [X64_R12] = "r12d",
            [X64_R13] = "r13d",
            [X64_R14] = "r14d",
            [X64_R15] = "r15d",
        },
    [8] =
        {
            [X64_RAX] = "rax",
            [X64_RCX] = "rcx",
            [X64_RDX] = "rdx",
            [X64_RBX] = "rbx",
            [X64_RSP] = "rsp",
            [X64_RBP] = "rbp",
            [X64_RSI] = "rsi",
            [X64_RDI] = "rdi",
            [X64_R8] = "r8",
            [X64_R9] = "r9",
            [X64_R10] = "r10",
            [X64_R11] = "r11",
            [X64_R12] = "r12",
            [X64_R13] = "r13",
            [X64_R14] = "r14",
            [X64_R15] = "r15",
        },
};

static const char* x64_mem_size_label[X64_MAX_INT_REG_SIZE + 1] = {
    [1] = "byte", [2] = "word", [4] = "dword", [8] = "qword"};
static const char* x64_data_size_label[X64_MAX_INT_REG_SIZE + 1] = {[1] = "db", [2] = "dw", [4] = "dd", [8] = "dq"};

static const char* x64_condition_codes[] = {
    [IR_COND_U_LT] = "b", [IR_COND_S_LT] = "l", [IR_COND_U_LTEQ] = "be", [IR_COND_S_LTEQ] = "le",
    [IR_COND_U_GT] = "a", [IR_COND_S_GT] = "g", [IR_COND_U_GTEQ] = "ae", [IR_COND_S_GTEQ] = "ge",
    [IR_COND_EQ] = "e",   [IR_COND_NEQ] = "ne",
};

typedef struct X64_ProcState {
    Symbol* sym;
    u32 id;
    X64_VRegLoc* vreg_map;
} X64_ProcState;

typedef struct X64_Generator {
    BucketList* text_lines;
    BucketList* data_lines;

    X64_ProcState curr_proc;

    Allocator* gen_mem;
    Allocator* tmp_mem;
} X64_Generator;

static X64_VRegLoc X64_vreg_loc(X64_Generator* generator, IR_Reg ir_reg)
{
    X64_VRegLoc reg_loc = generator->curr_proc.vreg_map[ir_reg];

    assert(reg_loc.kind != X64_VREG_LOC_UNASSIGNED);

    return reg_loc;
}

static char** X64_emit_line(BucketList* sstream, Allocator* gen_mem, Allocator* tmp_mem, const char* format,
                            va_list vargs)
{
    char** line_ptr = NULL;

    if (format)
    {
        char* tmp_line = array_create(tmp_mem, char, X64_INIT_LINE_LEN);
        size_t size = ftprintv_char_array(&tmp_line, true, format, vargs);

        line_ptr = sstream_add(sstream, gen_mem, tmp_line, size);
    }
    else
    {
        line_ptr = sstream_add(sstream, gen_mem, NULL, 0);
    }

    return line_ptr;
}

static char** X64_emit_text(X64_Generator* gen, const char* format, ...)
{
    char** line = NULL;
    va_list vargs;

    va_start(vargs, format);
    line = X64_emit_line(gen->text_lines, gen->gen_mem, gen->tmp_mem, format, vargs);
    va_end(vargs);

    return line;
}

static char* X64_get_label(X64_Generator* generator, u32 instr_index)
{
    char* dstr = array_create(generator->tmp_mem, char, 8);
    ftprint_char_array(&dstr, true, "L.%u.%u", generator->curr_proc.id, instr_index);

    return dstr;
}

static char* X64_print_stack_offset(Allocator* arena, s32 offset, u32 size)
{
    char* dstr = array_create(arena, char, 8);

    ftprint_char_array(&dstr, true, "%s [RBP + %d]", x64_mem_size_label[size], offset);

    return dstr;
}

static char* X64_print_imm(Allocator* arena, Scalar imm, u32 size)
{
    char* dstr = array_create(arena, char, 8);

    switch (size)
    {
        case 1:
            ftprint_char_array(&dstr, false, "0x%X", imm.as_int._u8);
            break;
        case 2:
            ftprint_char_array(&dstr, false, "0x%X", imm.as_int._u16);
            break;
        case 4:
            ftprint_char_array(&dstr, false, "0x%X", imm.as_int._u32);
            break;
        case 8:
            ftprint_char_array(&dstr, false, "0x%lX", imm.as_int._u64);
            break;
        default:
            assert(0);
            break;
    }

    array_push(dstr, '\0');

    return dstr;
}

static char** X64_emit_data(X64_Generator* gen, const char* format, ...)
{
    char** line = NULL;
    va_list vargs;

    va_start(vargs, format);
    line = X64_emit_line(gen->data_lines, gen->gen_mem, gen->tmp_mem, format, vargs);
    va_end(vargs);

    return line;
}

static void X64_emit_data_value(X64_Generator* generator, Type* type, Scalar scalar)
{
    if (type->kind == TYPE_INTEGER)
    {
        X64_emit_data(generator, "%s %s\n", x64_data_size_label[type->size],
                      X64_print_imm(generator->tmp_mem, scalar, type->size));
    }
    else
    {
        ftprint_err("Cannot gen NASM data regions for non-int type: %s\n", type_name(type));
        assert(0);
    }
}

static void X64_fill_line(X64_Generator* gen, char** line, const char* format, ...)
{
    char* tmp_line = array_create(gen->tmp_mem, char, X64_INIT_LINE_LEN);
    va_list vargs;

    va_start(vargs, format);
    size_t size = ftprintv_char_array(&tmp_line, true, format, vargs);
    va_end(vargs);

    *line = mem_dup(gen->gen_mem, tmp_line, size + 1, DEFAULT_ALIGN);
}

typedef struct X64_TmpReg {
    X64_Reg reg;
    s32 offset;
    u32 size;
    bool store;
    struct X64_TmpReg* next;
} X64_TmpReg;

typedef struct X64_RegGroup {
    X64_Generator* generator;
    u32 num_tmp_regs;
    X64_TmpReg* first_tmp_reg;
} X64_RegGroup;

static X64_RegGroup X64_begin_reg_group(X64_Generator* generator)
{
    X64_RegGroup group = {
        .generator = generator,
        .num_tmp_regs = 0,
        .first_tmp_reg = NULL,
    };

    return group;
}

static X64_Reg X64_get_reg(X64_RegGroup* group, IR_Reg vreg, u32 size, bool store)
{
    X64_VRegLoc vreg_loc = X64_vreg_loc(group->generator, vreg);

    // If this virtual register was not spilled during allocation, just return its assigned
    // physical register.
    if (vreg_loc.kind == X64_VREG_LOC_REG)
    {
        return vreg_loc.reg;
    }

    // This virtual register was spilled during allocation, so use a temporary physical register which will have
    // to be restored later.
    assert(vreg_loc.kind == X64_VREG_LOC_STACK);
    assert(group->num_tmp_regs < ARRAY_LEN(x64_scratch_regs));

    Allocator* tmp_mem = group->generator->tmp_mem;

    X64_TmpReg* tmp_reg = alloc_type(tmp_mem, X64_TmpReg, true);
    tmp_reg->reg = x64_scratch_regs[group->num_tmp_regs++]; // Use the next scratch register.
    tmp_reg->offset = vreg_loc.offset;
    tmp_reg->size = size;
    tmp_reg->store = store;

    const char* tmp_reg_str = x64_reg_names[size][tmp_reg->reg];

    X64_emit_text(group->generator, "    push %s", tmp_reg_str); // Save scratch register into stack.
    X64_emit_text(group->generator, "    mov %s, %s", tmp_reg_str,
                  X64_print_stack_offset(tmp_mem, vreg_loc.offset, size));

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    return tmp_reg->reg;
}

static void X64_end_reg_group(X64_RegGroup* group)
{
    if (!group->num_tmp_regs)
        return;

    X64_Generator* generator = group->generator;

    // Restore any temporary registers.
    X64_TmpReg* it = group->first_tmp_reg;

    while (it)
    {
        const char* tmp_reg_str = x64_reg_names[it->size][it->reg];

        if (it->store)
        {
            X64_emit_text(generator, "    mov %s, %s", X64_print_stack_offset(generator->tmp_mem, it->offset, it->size),
                          tmp_reg_str);
        }

        X64_emit_text(generator, "    pop %s", tmp_reg_str);
        it = it->next;
    }
}

static size_t X64_assign_scope_stack_offsets(X64_Generator* generator, Scope* scope, size_t offset)
{
    size_t stack_size = offset;

    //
    // Sum sizes of local variables declared in this scope.
    //
    {
        List* head = &scope->sym_list;
        List* it = head->next;

        while (it != head)
        {
            Symbol* sym = list_entry(it, Symbol, lnode);

            if (sym->kind == SYMBOL_VAR)
            {
                stack_size += sym->type->size;
                stack_size = ALIGN_UP(stack_size, sym->type->align);
                sym->as_var.offset = -stack_size;
            }

            it = it->next;
        }
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //
    {
        List* head = &scope->children;
        List* it = head->next;
        size_t child_offset = stack_size;

        while (it != head)
        {
            Scope* child_scope = list_entry(it, Scope, lnode);
            size_t child_size = X64_assign_scope_stack_offsets(generator, child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static size_t X64_assign_proc_stack_offsets(X64_Generator* generator, DeclProc* dproc)
{
    //
    // Sum sizes of local variables declared in this scope.
    //

    size_t stack_size = 0;
    unsigned arg_index = 0;
    unsigned stack_arg_offset = 0x10;

    Scope* scope = dproc->scope;
    List* head = &scope->sym_list;
    List* it = head->next;

    while (it != head)
    {
        Symbol* sym = list_entry(it, Symbol, lnode);

        // Assign stack offsets to procedure params.
        if (arg_index < dproc->num_params)
        {
            assert(sym->kind == SYMBOL_VAR);

            Type* arg_type = sym->type;
            size_t arg_size = arg_type->size;
            size_t arg_align = arg_type->align;
            bool arg_in_reg = (arg_index < ARRAY_LEN(arg_regs)) && (arg_size <= X64_MAX_INT_REG_SIZE);

            // Spill argument register onto the stack.
            if (arg_in_reg)
            {
                Register arg_reg = arg_regs[arg_index];

                stack_size += arg_size;
                stack_size = ALIGN_UP(stack_size, arg_align);
                sym->as_var.offset = -stack_size;

                X64_emit_text(generator, "    mov %s [rbp + %d], %s", x64_mem_size_label[arg_size], sym->as_var.offset,
                              x64_reg_names[arg_size][arg_reg]);

                arg_index += 1;
            }
            else
            {
                sym->as_var.offset = stack_arg_offset;
                stack_arg_offset += arg_size;
                stack_arg_offset = ALIGN_UP(stack_arg_offset, arg_align);
            }
        }
        // Assign stack offsets to local variables in procedure.
        else if (sym->kind == SYMBOL_VAR)
        {
            stack_size += sym->type->size;
            stack_size = ALIGN_UP(stack_size, sym->type->align);
            sym->as_var.offset = -stack_size;
        }

        it = it->next;
    }

    //
    // Recursively compute stack sizes for child scopes. Take the largest.
    //
    {
        List* head = &scope->children;
        List* it = head->next;
        size_t child_offset = stack_size;

        while (it != head)
        {
            Scope* child_scope = list_entry(it, Scope, lnode);
            size_t child_size = X64_assign_scope_stack_offsets(generator, child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static char* X64_print_mem(X64_RegGroup* group, IR_MemAddr* addr, u32 size)
{
    char* dstr = array_create(group->generator->tmp_mem, char, 16);
    const char* mem_label = x64_mem_size_label[size];
    bool has_base = addr->base_kind != IR_MEM_BASE_NONE;
    bool has_index = addr->scale && (addr->index_reg < IR_REG_COUNT);

    assert(has_base || has_index);

    if (has_base)
    {
        X64_Reg base_reg;
        s32 disp = addr->disp;

        if (addr->base_kind == IR_MEM_BASE_SYM)
        {
            Symbol* sym = addr->base.sym;

            // Early exit for global variable addresses.
            if (!sym->is_local)
            {
                ftprint_char_array(&dstr, true, "%s [rel %s]", mem_label, sym->name);
                return dstr;
            }

            disp += addr->base.sym->as_var.offset;
            base_reg = X64_RBP;
        }
        else
        {
            base_reg = X64_get_reg(group, addr->base.reg, size, false);
        }

        bool has_disp = disp != 0;
        const char* base_reg_name = x64_reg_names[X64_MAX_INT_REG_SIZE][base_reg];

        if (has_index)
        {
            X64_Reg index_reg = X64_get_reg(group, addr->index_reg, size, false);
            const char* index_reg_name = x64_reg_names[X64_MAX_INT_REG_SIZE][index_reg];

            if (has_disp)
                ftprint_char_array(&dstr, false, "%s [%s + %d*%s + %d]", mem_label, base_reg_name, addr->scale,
                                   index_reg_name, (s32)disp);
            else
                ftprint_char_array(&dstr, false, "%s [%s + %d*%s]", mem_label, base_reg_name, addr->scale,
                                   index_reg_name);
        }
        else
        {
            if (has_disp)
                ftprint_char_array(&dstr, false, "%s [%s + %d]", mem_label, base_reg_name, (s32)disp);
            else
                ftprint_char_array(&dstr, false, "%s [%s]", mem_label, base_reg_name);
        }
    }
    else
    {
        X64_Reg index_reg = X64_get_reg(group, addr->index_reg, size, false);
        const char* index_reg_name = x64_reg_names[X64_MAX_INT_REG_SIZE][index_reg];

        if (addr->disp)
            ftprint_char_array(&dstr, false, "%s [%d*%s + %d]", mem_label, addr->scale, index_reg_name,
                               (s32)addr->disp);
        else
            ftprint_char_array(&dstr, false, "%s [%d*%s]", mem_label, addr->scale, index_reg_name);
    }

    array_push(dstr, '\0');

    return dstr;
}

static void X64_emit_rr_instr(X64_Generator* generator, const char* instr, bool writes_op1, u32 op1_size,
                              IR_Reg op1_vreg, u32 op2_size, IR_Reg op2_vreg)
{
    X64_VRegLoc op1_loc = X64_vreg_loc(generator, op1_vreg);
    X64_VRegLoc op2_loc = X64_vreg_loc(generator, op2_vreg);

    switch (op1_loc.kind)
    {
        case X64_VREG_LOC_REG:
        {
            switch (op2_loc.kind)
            {
                case X64_VREG_LOC_REG:
                    X64_emit_text(generator, "    %s %s, %s", instr, x64_reg_names[op1_size][op1_loc.reg],
                                  x64_reg_names[op2_size][op2_loc.reg]);
                    break;
                case X64_VREG_LOC_STACK:
                    X64_emit_text(generator, "    %s %s, %s", instr, x64_reg_names[op1_size][op1_loc.reg],
                                  X64_print_stack_offset(generator->tmp_mem, op2_loc.offset, op2_size));
                    break;
                default:
                    assert(0);
                    break;
            }
            break;
        }
        case X64_VREG_LOC_STACK:
        {
            switch (op2_loc.kind)
            {
                case X64_VREG_LOC_REG:
                    X64_emit_text(generator, "    %s %s, %s", instr,
                                  X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size),
                                  x64_reg_names[op2_size][op2_loc.reg]);
                    break;
                case X64_VREG_LOC_STACK:
                {
                    const char* op1_op_str = X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size);
                    const char* op2_op_str = X64_print_stack_offset(generator->tmp_mem, op2_loc.offset, op2_size);
                    const char* tmp_reg_str = x64_reg_names[op1_size][X64_RAX];

                    // Save the contents of a temporary register into the stack.
                    X64_emit_text(generator, "    push %s", tmp_reg_str);

                    // Load dst into the temporary register,
                    X64_emit_text(generator, "    mov %s, %s", tmp_reg_str, op1_op_str);

                    // Execute the instruction using the temporary register as the destination.
                    X64_emit_text(generator, "    %s %s, %s", instr, tmp_reg_str, op2_op_str);

                    // Store the result of the instruction (contents of temporary register) into dst.
                    if (writes_op1)
                    {
                        X64_emit_text(generator, "    mov %s, %s", op1_op_str, tmp_reg_str);
                    }

                    // Restore the contents of the temporary register.
                    X64_emit_text(generator, "    pop %s", tmp_reg_str);

                    break;
                }
                default:
                    assert(0);
                    break;
            }
            break;
        }
        default:
            assert(0);
            break;
    }
}

static void X64_emit_ri_instr(X64_Generator* generator, const char* instr, u32 op1_size, IR_Reg op1_vreg, u32 op2_size,
                              Scalar op2_imm)
{
    X64_VRegLoc op1_loc = X64_vreg_loc(generator, op1_vreg);

    switch (op1_loc.kind)
    {
        case X64_VREG_LOC_REG:
        {
            X64_emit_text(generator, "    %s %s, %s", instr, x64_reg_names[op1_size][op1_loc.reg],
                          X64_print_imm(generator->tmp_mem, op2_imm, op2_size));
            break;
        }
        case X64_VREG_LOC_STACK:
        {
            X64_emit_text(generator, "    %s %s, %s", instr,
                          X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size),
                          X64_print_imm(generator->tmp_mem, op2_imm, op2_size));
            break;
        }
        default:
            assert(0);
            break;
    }
}

static void X64_emit_rm_instr(X64_Generator* generator, const char* instr, bool writes_op1, u32 op1_size,
                              IR_Reg op1_vreg, u32 op2_size, IR_MemAddr* op2_vaddr)
{
    X64_RegGroup tmp_group = X64_begin_reg_group(generator);
    X64_Reg op1_reg = X64_get_reg(&tmp_group, op1_vreg, op1_size, writes_op1);

    X64_emit_text(generator, "    %s %s %s", instr, x64_reg_names[op1_size][op1_reg],
                  X64_print_mem(&tmp_group, op2_vaddr, op2_size));

    X64_end_reg_group(&tmp_group);
}

static void X64_emit_mr_instr(X64_Generator* generator, const char* instr, u32 op1_size, IR_MemAddr* op1_vaddr,
                              u32 op2_size, IR_Reg op2_vreg)
{
    X64_RegGroup tmp_group = X64_begin_reg_group(generator);
    X64_Reg op2_reg = X64_get_reg(&tmp_group, op2_vreg, op2_size, false);

    X64_emit_text(generator, "    %s %s %s", instr, X64_print_mem(&tmp_group, op1_vaddr, op1_size),
                  x64_reg_names[op2_size][op2_reg]);

    X64_end_reg_group(&tmp_group);
}

static void X64_emit_mi_instr(X64_Generator* generator, const char* instr, u32 op1_size, IR_MemAddr* op1_vaddr,
                              u32 op2_size, Scalar op2_imm)
{
    X64_RegGroup tmp_group = X64_begin_reg_group(generator);

    X64_emit_text(generator, "    %s %s %s", instr, X64_print_mem(&tmp_group, op1_vaddr, op1_size),
                  X64_print_imm(generator->tmp_mem, op2_imm, op2_size));

    X64_end_reg_group(&tmp_group);
}

static void X64_gen_instr(X64_Generator* generator, u32 instr_index, bool is_last_instr, IR_Instr* instr)
{
    AllocatorState mem_state = allocator_get_state(generator->tmp_mem);

    if (instr->is_jmp_target)
        X64_emit_text(generator, "    %s:", X64_get_label(generator, instr_index));

    switch (instr->kind)
    {
        case IR_INSTR_ADD_R_R:
        {
            u32 size = (u32)instr->add_r_r.type->size;

            X64_emit_rr_instr(generator, "add", true, size, instr->add_r_r.dst, size, instr->add_r_r.src);
            break;
        }
        case IR_INSTR_ADD_R_M:
        {
            u32 size = (u32)instr->add_r_m.type->size;

            X64_emit_rm_instr(generator, "add", true, size, instr->add_r_m.dst, size, &instr->add_r_m.src);
            break;
        }
        case IR_INSTR_ADD_R_I:
        {
            u32 size = (u32)instr->add_r_i.type->size;

            X64_emit_ri_instr(generator, "add", size, instr->add_r_i.dst, size, instr->add_r_i.src);
            break;
        }
        case IR_INSTR_SUB_R_R:
        {
            u32 size = (u32)instr->sub_r_r.type->size;

            X64_emit_rr_instr(generator, "sub", true, size, instr->sub_r_r.dst, size, instr->sub_r_r.src);
            break;
        }
        case IR_INSTR_SUB_R_M:
        {
            u32 size = (u32)instr->sub_r_m.type->size;

            X64_emit_rm_instr(generator, "sub", true, size, instr->sub_r_m.dst, size, &instr->sub_r_m.src);
            break;
        }
        case IR_INSTR_SUB_R_I:
        {
            u32 size = (u32)instr->sub_r_i.type->size;

            X64_emit_ri_instr(generator, "sub", size, instr->sub_r_i.dst, size, instr->sub_r_i.src);
            break;
        }
        case IR_INSTR_SAR_R_R:
        {
            u32 size = (u32)instr->sar_r_r.type->size;

            X64_emit_rr_instr(generator, "sar", true, size, instr->sar_r_r.dst, size, instr->sar_r_r.src);
            break;
        }
        case IR_INSTR_SAR_R_M:
        {
            u32 size = (u32)instr->sar_r_m.type->size;

            X64_emit_rm_instr(generator, "sar", true, size, instr->sar_r_m.dst, size, &instr->sar_r_m.src);
            break;
        }
        case IR_INSTR_SAR_R_I:
        {
            u32 size = (u32)instr->sar_r_i.type->size;

            X64_emit_ri_instr(generator, "sar", size, instr->sar_r_i.dst, size, instr->sar_r_i.src);
            break;
        }
        case IR_INSTR_NEG:
        {
            Type* type = instr->neg.type;
            X64_VRegLoc dst_loc = X64_vreg_loc(generator, instr->neg.dst);

            if (dst_loc.kind == X64_VREG_LOC_REG)
            {
                X64_emit_text(generator, "    neg %s", x64_reg_names[type->size][dst_loc.reg]);
            }
            else
            {
                assert(dst_loc.kind == X64_VREG_LOC_STACK);
                X64_emit_text(generator, "    neg %s",
                              X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, (u32)type->size));
            }
            break;
        }
        case IR_INSTR_NOT:
        {
            Type* type = instr->not .type;
            X64_VRegLoc dst_loc = X64_vreg_loc(generator, instr->not .dst);

            if (dst_loc.kind == X64_VREG_LOC_REG)
            {
                X64_emit_text(generator, "    not %s", x64_reg_names[type->size][dst_loc.reg]);
            }
            else
            {
                assert(dst_loc.kind == X64_VREG_LOC_STACK);
                X64_emit_text(generator, "    not %s",
                              X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, (u32)type->size));
            }
            break;
        }
        case IR_INSTR_LIMM:
        {
            u32 size = (u32)instr->limm.type->size;

            X64_emit_ri_instr(generator, "mov", size, instr->limm.dst, size, instr->limm.src);
            break;
        }
        case IR_INSTR_LADDR:
        {

            u32 size = (u32)instr->laddr.type->size;

            X64_emit_rm_instr(generator, "lea", true, X64_MAX_INT_REG_SIZE, instr->laddr.dst, size, &instr->laddr.mem);
            break;
        }
        case IR_INSTR_TRUNC_R_R:
        {
            Type* dst_type = instr->trunc_r_r.dst_type;
            Type* src_type = instr->trunc_r_r.src_type;

            X64_emit_rr_instr(generator, "mov", true, dst_type->size, instr->trunc_r_r.dst, src_type->size,
                              instr->trunc_r_r.src);
            break;
        }
        case IR_INSTR_TRUNC_R_M:
        {
            Type* dst_type = instr->trunc_r_m.dst_type;
            Type* src_type = instr->trunc_r_m.src_type;

            X64_emit_rm_instr(generator, "mov", true, dst_type->size, instr->trunc_r_m.dst, src_type->size,
                              &instr->trunc_r_m.src);
            break;
        }
        case IR_INSTR_ZEXT_R_R:
        {
            Type* dst_type = instr->zext_r_r.dst_type;
            Type* src_type = instr->zext_r_r.src_type;

            X64_emit_rr_instr(generator, "movzx", true, dst_type->size, instr->zext_r_r.dst, src_type->size,
                              instr->zext_r_r.src);
            break;
        }
        case IR_INSTR_ZEXT_R_M:
        {
            Type* dst_type = instr->zext_r_m.dst_type;
            Type* src_type = instr->zext_r_m.src_type;

            X64_emit_rm_instr(generator, "movzx", true, dst_type->size, instr->zext_r_m.dst, src_type->size,
                              &instr->zext_r_m.src);
            break;
        }
        case IR_INSTR_SEXT_R_R:
        {
            Type* dst_type = instr->sext_r_r.dst_type;
            Type* src_type = instr->sext_r_r.src_type;
            const char* movsx = src_type->size >= type_u32->size ? "movsxd" : "movsx";

            X64_emit_rr_instr(generator, movsx, true, dst_type->size, instr->sext_r_r.dst, src_type->size,
                              instr->sext_r_r.src);
            break;
        }
        case IR_INSTR_SEXT_R_M:
        {
            Type* dst_type = instr->sext_r_m.dst_type;
            Type* src_type = instr->sext_r_m.src_type;
            const char* movsx = src_type->size >= type_u32->size ? "movsxd" : "movsx";

            X64_emit_rm_instr(generator, movsx, true, dst_type->size, instr->sext_r_m.dst, src_type->size,
                              &instr->sext_r_m.src);
            break;
        }
        case IR_INSTR_LOAD:
        {
            u32 size = (u32)instr->load.type->size;

            X64_emit_rm_instr(generator, "mov", true, size, instr->load.dst, size, &instr->load.src);
            break;
        }
        case IR_INSTR_STORE_R:
        {
            u32 size = (u32)instr->store_r.type->size;

            X64_emit_mr_instr(generator, "mov", size, &instr->store_r.dst, size, instr->store_r.src);
            break;
        }
        case IR_INSTR_STORE_I:
        {
            u32 size = instr->store_i.type->size;

            X64_emit_mi_instr(generator, "mov", size, &instr->store_i.dst, size, instr->store_i.src);
            break;
        }
        case IR_INSTR_CMP_R_R:
        {
            u32 size = (u32)instr->cmp_r_r.type->size;

            X64_emit_rr_instr(generator, "cmp", false, size, instr->cmp_r_r.op1, size, instr->cmp_r_r.op2);
            break;
        }
        case IR_INSTR_CMP_R_M:
        {
            u32 size = (u32)instr->cmp_r_m.type->size;

            X64_emit_rm_instr(generator, "cmp", false, size, instr->cmp_r_m.op1, size, &instr->cmp_r_m.op2);
            break;
        }
        case IR_INSTR_CMP_R_I:
        {
            u32 size = (u32)instr->cmp_r_i.type->size;

            X64_emit_ri_instr(generator, "cmp", size, instr->cmp_r_i.op1, size, instr->cmp_r_i.op2);
            break;
        }
        case IR_INSTR_CMP_M_R:
        {
            u32 size = (u32)instr->cmp_m_r.type->size;

            X64_emit_mr_instr(generator, "cmp", size, &instr->cmp_m_r.op1, size, instr->cmp_m_r.op2);
            break;
        }
        case IR_INSTR_CMP_M_I:
        {
            u32 size = (u32)instr->cmp_m_i.type->size;

            X64_emit_mi_instr(generator, "cmp", size, &instr->cmp_m_i.op1, size, instr->cmp_m_i.op2);
            break;
        }
        case IR_INSTR_JMP:
        {
            X64_emit_text(generator, "    jmp %s", X64_get_label(generator, instr->jmp.jmp_target));
            break;
        }
        case IR_INSTR_JMPCC:
        {
            X64_emit_text(generator, "    j%s %s", x64_condition_codes[instr->jmpcc.cond],
                          X64_get_label(generator, instr->jmpcc.jmp_target));
            break;
        }
        case IR_INSTR_SETCC:
        {
            X64_VRegLoc dst_loc = X64_vreg_loc(generator, instr->setcc.dst);

            if (dst_loc.kind == X64_VREG_LOC_REG)
            {
                X64_emit_text(generator, "    set%s %s", x64_condition_codes[instr->setcc.cond],
                              x64_reg_names[1][dst_loc.reg]);
            }
            else
            {
                assert(dst_loc.kind == X64_VREG_LOC_STACK);
                X64_emit_text(generator, "    set%s %s", x64_condition_codes[instr->setcc.cond],
                              X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, 1));
            }
            break;
        }
        case IR_INSTR_RET:
        {
            Type* ret_type = instr->ret.type;

            if (ret_type != type_void)
            {
                X64_VRegLoc ret_loc = X64_vreg_loc(generator, instr->ret.src);

                assert(ret_loc.kind != X64_VREG_LOC_UNASSIGNED);

                if (ret_loc.kind == X64_VREG_LOC_STACK)
                {
                    X64_emit_text(generator, "    mov %s, %s", x64_reg_names[ret_type->size][X64_RAX],
                                  X64_print_stack_offset(generator->tmp_mem, ret_loc.offset, ret_type->size));
                }
                else if ((ret_loc.kind == X64_VREG_LOC_REG) && (ret_loc.reg != X64_RAX))
                {
                    X64_emit_text(generator, "    mov %s, %s", x64_reg_names[ret_type->size][X64_RAX],
                                  x64_reg_names[ret_type->size][ret_loc.reg]);
                }
            }

            if (!is_last_instr)
                X64_emit_text(generator, "    jmp end.%s", generator->curr_proc.sym->name);

            break;
        }
        default:
            assert(0);
            break;
    }

    allocator_restore_state(mem_state);
}

static void X64_gen_proc(X64_Generator* generator, u32 proc_id, Symbol* sym)
{
    generator->curr_proc.sym = sym;
    generator->curr_proc.id = proc_id;

    AllocatorState mem_state = allocator_get_state(generator->tmp_mem);

    X64_emit_text(generator, "");
    X64_emit_text(generator, "SECTION .text");
    X64_emit_text(generator, "global %s", sym->name);
    X64_emit_text(generator, "%s:", sym->name);

    X64_emit_text(generator, "    push rbp");
    X64_emit_text(generator, "    mov rbp, rsp");

    char** save_regs_inst = X64_emit_text(generator, NULL);
    char** sub_rsp_inst = X64_emit_text(generator, NULL);

    u32 stack_size = X64_assign_proc_stack_offsets(generator, (DeclProc*)sym->decl); // NOTE: Spills argument registers.

    // Register allocation.
    LifetimeInterval* vreg_intervals = sym->as_proc.reg_intervals;
    u32 num_vregs = array_len(vreg_intervals);
    X64_VRegLoc* vreg_locs = alloc_array(generator->tmp_mem, X64_VRegLoc, num_vregs, true);

    X64_RegAllocResult reg_alloc =
        X64_linear_scan_reg_alloc(generator->tmp_mem, num_vregs, vreg_intervals, vreg_locs, stack_size);

    stack_size = reg_alloc.stack_offset;
    generator->curr_proc.vreg_map = vreg_locs;

#ifndef NDEBUG
    ftprint_out("Register allocation for %s:\n", sym->name);
    for (u32 i = 0; i < num_vregs; i += 1)
    {
        X64_VRegLoc* loc = vreg_locs + i;

        if (loc->kind == X64_VREG_LOC_REG)
        {
            ftprint_out("\tr%u -> %s\n", i, x64_reg_names[8][loc->reg]);
        }
        else
        {
            assert(loc->kind == X64_VREG_LOC_STACK);
            ftprint_out("\tr%u -> RBP - %d\n", i, loc->offset);
        }
    }
#endif

    if (stack_size)
        X64_fill_line(generator, sub_rsp_inst, "    sub rsp, %u", stack_size);

    // Generate instructions.
    IR_Instr** instrs = sym->as_proc.instrs;
    size_t num_instrs = array_len(instrs);

    for (size_t i = 0; i < num_instrs; i += 1)
    {
        X64_gen_instr(generator, i, i == num_instrs - 1, instrs[i]);
    }

    // End label
    X64_emit_text(generator, "    end.%s:", sym->name);

    // Save/Restore callee-saved registers.
    char* tmp_line = array_create(generator->tmp_mem, char, X64_INIT_LINE_LEN);

    for (uint32_t r = 0; r < X64_REG_COUNT; r += 1)
    {
        X64_Reg reg = (X64_Reg)r;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg))
        {
            ftprint_char_array(&tmp_line, false, "    push %s\n", reg_names[X64_MAX_INT_REG_SIZE][reg]);
            X64_emit_text(generator, "    pop %s", reg_names[X64_MAX_INT_REG_SIZE][reg]);
        }
    }

    array_push(tmp_line, '\0');

    *save_regs_inst = mem_dup(generator->gen_mem, tmp_line, array_len(tmp_line), DEFAULT_ALIGN);

    if (stack_size)
        X64_emit_text(generator, "    mov rsp, rbp");

    X64_emit_text(generator, "    pop rbp");
    X64_emit_text(generator, "    ret");

    allocator_restore_state(mem_state);
}

static void X64_gen_global_vars(X64_Generator* generator, u32 num_vars, Symbol** vars)
{
    AllocatorState mem_state = allocator_get_state(generator->tmp_mem);
    X64_emit_data(generator, "SECTION .data\n");

    for (u32 i = 0; i < num_vars; i += 1)
    {
        Symbol* sym = vars[i];
        DeclVar* dvar = (DeclVar*)sym->decl;

        X64_emit_data(generator, "ALIGN %d", sym->type->align);
        X64_emit_data(generator, "%s: ", sym->name);

        if (dvar->init)
        {
            X64_emit_data_value(generator, sym->type, dvar->init->const_val);
        }
        else
        {
            Scalar zero_val = {0};
            X64_emit_data_value(generator, sym->type, zero_val);
        }
    }
    allocator_restore_state(mem_state);
}

static void X64_write_output_file(X64_Generator* generator, FILE* out_fd)
{
    ftprint_file(out_fd, false, "; Generated by the Nibble compiler.\n\n");

    for (Bucket* bucket = generator->data_lines->first; bucket; bucket = bucket->next)
    {
        for (size_t i = 0; i < bucket->count; i += 1)
        {
            const char* str = (const char*)bucket->elems[i];

            if (str)
                ftprint_file(out_fd, false, "%s\n", str);
        }
    }

    for (Bucket* bucket = generator->text_lines->first; bucket; bucket = bucket->next)
    {
        for (size_t i = 0; i < bucket->count; i += 1)
        {
            const char* str = (const char*)bucket->elems[i];

            if (str)
                ftprint_file(out_fd, false, "%s\n", str);
        }
    }
}

bool x64_gen_module(Allocator* gen_mem, Allocator* tmp_mem, IR_Module* module, const char* output_file)
{
    FILE* out_fd = fopen(output_file, "w");
    if (!out_fd)
    {
        ftprint_err("Failed to open output file `%s`\n", output_file);
        return false;
    }

    X64_Generator generator = {
        .gen_mem = gen_mem,
        .tmp_mem = tmp_mem,
        .text_lines = new_bucket_list(gen_mem, 256),
        .data_lines = new_bucket_list(gen_mem, 256),
    };

    // Generate global variables.
    X64_gen_global_vars(&generator, module->num_vars, module->vars);

    // Generate instructions for each procedure.
    u32 num_procs = module->num_procs;

    for (u32 i = 0; i < num_procs; i += 1)
    {
        X64_gen_proc(&generator, i, module->procs[i]);
    }

    // Output assembly to file.
    X64_write_output_file(&generator, out_fd);

    fclose(out_fd);

    return true;
}