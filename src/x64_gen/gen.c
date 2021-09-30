#include "stream.h"
#include "x64_gen/gen.h"

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

static const char* x64_mem_size_label[X64_MAX_INT_REG_SIZE + 1] =
    {[1] = "byte", [2] = "word", [4] = "dword", [8] = "qword"};
static const char* x64_data_size_label[X64_MAX_INT_REG_SIZE + 1] = {[1] = "db", [2] = "dw", [4] = "dd", [8] = "dq"};

static const char* x64_condition_codes[] = {
    [IR_COND_U_LT] = "b", [IR_COND_S_LT] = "l", [IR_COND_U_LTEQ] = "be", [IR_COND_S_LTEQ] = "le",
    [IR_COND_U_GT] = "a", [IR_COND_S_GT] = "g", [IR_COND_U_GTEQ] = "ae", [IR_COND_S_GTEQ] = "ge",
    [IR_COND_EQ] = "e",   [IR_COND_NEQ] = "ne",
};

typedef struct X64_ArgInfo {
    IR_InstrCallArg* arg;
    X64_VRegLoc loc;
    u32 reg_index;
    bool in_reg;
    bool save_on_swap;
} X64_ArgInfo;

typedef struct X64_ProcState {
    Symbol* sym;
    u32 id;
    X64_VRegLoc* vreg_map;

    X64_Reg* scratch_regs;
    u32 num_scratch_regs;
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

    if (format) {
        char* tmp_line = array_create(tmp_mem, char, X64_INIT_LINE_LEN);
        size_t size = ftprintv_char_array(&tmp_line, true, format, vargs);

        line_ptr = sstream_add(sstream, gen_mem, tmp_line, size);
    }
    else {
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

    switch (size) {
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
    if (type->kind == TYPE_INTEGER) {
        X64_emit_data(generator, "%s %s\n", x64_data_size_label[type->size],
                      X64_print_imm(generator->tmp_mem, scalar, type->size));
    }
    else {
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
    u64 size;
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
    if (vreg_loc.kind == X64_VREG_LOC_REG) {
        return vreg_loc.reg;
    }

    // This virtual register was spilled during allocation, so use a temporary physical register which will have
    // to be restored later.

    X64_Generator* generator = group->generator;
    u32 num_scratch_regs = generator->curr_proc.num_scratch_regs;
    X64_Reg* scratch_regs = generator->curr_proc.scratch_regs;

    assert(vreg_loc.kind == X64_VREG_LOC_STACK);
    assert(group->num_tmp_regs < num_scratch_regs);

    Allocator* tmp_mem = group->generator->tmp_mem;

    X64_TmpReg* tmp_reg = alloc_type(tmp_mem, X64_TmpReg, true);
    tmp_reg->reg = scratch_regs[group->num_tmp_regs++]; // Use the next scratch register.
    tmp_reg->offset = vreg_loc.offset;
    tmp_reg->size = size;
    tmp_reg->store = store;

    X64_emit_text(group->generator, "    push %s", x64_reg_names[X64_MAX_INT_REG_SIZE][tmp_reg->reg]);
    X64_emit_text(group->generator, "    mov %s, %s", x64_reg_names[size][tmp_reg->reg],
                  X64_print_stack_offset(tmp_mem, vreg_loc.offset, size));

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    return tmp_reg->reg;
}

static void X64_push_reg(X64_RegGroup* group, X64_Reg reg)
{
    assert(reg != X64_REG_COUNT);

    Allocator* tmp_mem = group->generator->tmp_mem;

    X64_TmpReg* tmp_reg = alloc_type(tmp_mem, X64_TmpReg, true);
    tmp_reg->reg = reg;

    X64_emit_text(group->generator, "    push %s", x64_reg_names[X64_MAX_INT_REG_SIZE][tmp_reg->reg]);

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    group->num_tmp_regs += 1;
}

static void X64_end_reg_group(X64_RegGroup* group)
{
    if (!group->num_tmp_regs)
        return;

    X64_Generator* generator = group->generator;

    // Restore any temporary registers.
    X64_TmpReg* it = group->first_tmp_reg;

    while (it) {
        if (it->store) {
            X64_emit_text(generator, "    mov %s, %s", X64_print_stack_offset(generator->tmp_mem, it->offset, it->size),
                          x64_reg_names[it->size][it->reg]);
        }

        X64_emit_text(generator, "    pop %s", x64_reg_names[X64_MAX_INT_REG_SIZE][it->reg]);
        it = it->next;
    }
}

typedef struct X64_StackArgsInfo {
    u64 args_size;
    u64 args_offset;
} X64_StackArgsInfo;

static X64_StackArgsInfo X64_linux_preprocess_call_args(X64_Generator* generator, u32 live_regs, u32 num_args,
                                                        IR_InstrCallArg* args, X64_ArgInfo* arg_infos,
                                                        X64_ArgInfo** arg_info_map)
{
    X64_StackArgsInfo stack_info = {0};
    u32 arg_reg_index = 0;

    // Create info structs (for each arg) that we can use to swap argument registers around.
    for (u32 i = 0; i < num_args; i += 1) {
        IR_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;
        X64_VRegLoc arg_loc = X64_vreg_loc(generator, arg->loc);
        X64_ArgInfo* arg_info = arg_infos + i;

        assert(arg_size <= X64_MAX_INT_REG_SIZE); // TODO: Support structs

        if (arg_reg_index >= x64_target.num_arg_regs) {
            arg_info->in_reg = false;
            stack_info.args_size += ALIGN_UP(arg_size, X64_STACK_WORD_SIZE);
        }
        else {
            arg_info->in_reg = true;
            arg_info->reg_index = arg_reg_index++;
        }

        if (arg_loc.kind == X64_VREG_LOC_REG) {
            bool is_caller_saved = X64_is_caller_saved_reg(arg_loc.reg);
            bool needed_after_call = u32_is_bit_set(live_regs, arg_loc.reg);

            arg_info_map[arg_loc.reg] = arg_info;
            arg_info->save_on_swap = needed_after_call && !is_caller_saved;
        }

        arg_info->arg = arg;
        arg_info->loc = arg_loc;
    }

    return stack_info;
}

static X64_StackArgsInfo X64_windows_preprocess_call_args(X64_Generator* generator, u32 live_regs, u32 num_args,
                                                          IR_InstrCallArg* args, X64_ArgInfo* arg_infos,
                                                          X64_ArgInfo** arg_info_map)
{
    X64_StackArgsInfo stack_info = {.args_size = X64_WINDOWS_SHADOW_SPACE, .args_offset = X64_WINDOWS_SHADOW_SPACE};

    // Create info structs (for each arg) that we can use to swap argument registers around.
    for (u32 i = 0; i < num_args; i += 1) {
        IR_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;
        X64_VRegLoc arg_loc = X64_vreg_loc(generator, arg->loc);
        X64_ArgInfo* arg_info = arg_infos + i;

        assert(arg_size <= X64_MAX_INT_REG_SIZE); // TODO: Support structs

        if (i >= x64_target.num_arg_regs) {
            arg_info->in_reg = false;
            stack_info.args_size += ALIGN_UP(arg_size, X64_STACK_WORD_SIZE);
        }
        else {
            arg_info->in_reg = true;
            arg_info->reg_index = i;
        }

        if (arg_loc.kind == X64_VREG_LOC_REG) {
            bool is_caller_saved = X64_is_caller_saved_reg(arg_loc.reg);
            bool needed_after_call = u32_is_bit_set(live_regs, arg_loc.reg);

            arg_info_map[arg_loc.reg] = arg_info;
            arg_info->save_on_swap = needed_after_call && !is_caller_saved;
        }

        arg_info->arg = arg;
        arg_info->loc = arg_loc;
    }

    return stack_info;
}

static X64_StackArgsInfo X64_preprocess_call_args(X64_Generator* generator, u32 live_regs, u32 num_args,
                                                  IR_InstrCallArg* args, X64_ArgInfo* arg_infos,
                                                  X64_ArgInfo** arg_info_map)
{
    if (x64_target.os == OS_LINUX) {
        return X64_linux_preprocess_call_args(generator, live_regs, num_args, args, arg_infos, arg_info_map);
    }

    return X64_windows_preprocess_call_args(generator, live_regs, num_args, args, arg_infos, arg_info_map);
}

static void X64_place_args_in_regs(X64_RegGroup* save_reg_group, u32 num_args, X64_ArgInfo* arg_infos,
                                   X64_ArgInfo** arg_info_map)
{
    X64_Generator* generator = save_reg_group->generator;

    // Move arguments that should be in registers into the appropriate X64 registers.
    bool keep_going = true;

    while (keep_going) {
        // NOTE: :IMPORTANT: This loops inifinitely if a stack argument is initially assigned an argument
        // register! The register allocator MUST NOT assign an X64 argument register to an argument that will be
        // passed via the stack.
        keep_going = false;

        for (u32 i = 0; i < num_args; i += 1) {
            X64_ArgInfo* arg_info = arg_infos + i;

            if (!arg_info->in_reg) {
                // Skip stack args.
                continue;
            }

            u64 arg_size = arg_info->arg->type->size;

            // This is the X64 register that must contain this argument.
            X64_Reg arg_reg = x64_target.arg_regs[arg_info->reg_index];

            // Check if argument is already in the appropriate register.
            if ((arg_info->loc.kind == X64_VREG_LOC_REG) && (arg_info->loc.reg == arg_reg)) {
                // Do nothing.
                // NOTE: Has already been saved if needed across call.
            }
            else {
                // Move argument into the appropriate register.

                // Need to check if arg_reg is used by another argument.
                X64_ArgInfo* other_arg_info = arg_info_map[arg_reg];

                if (other_arg_info) {
                    assert(other_arg_info->loc.kind == X64_VREG_LOC_REG);
                    assert(other_arg_info->loc.reg == arg_reg);
                    assert(!other_arg_info->save_on_swap);
                    X64_VRegLoc this_loc = arg_info->loc;

                    if (arg_info->loc.kind == X64_VREG_LOC_REG) {
                        // Exchange register contents.

                        if (arg_info->save_on_swap) {
                            X64_push_reg(save_reg_group, arg_info->loc.reg);
                            arg_info->save_on_swap = false;
                        }

                        // Exchange the entire register.
                        X64_emit_text(generator, "    xchg %s, %s", x64_reg_names[X64_MAX_INT_REG_SIZE][this_loc.reg],
                                      x64_reg_names[X64_MAX_INT_REG_SIZE][arg_reg]);

                        arg_info->loc = other_arg_info->loc;
                        arg_info_map[arg_reg] = arg_info;

                        other_arg_info->loc = this_loc;
                        arg_info_map[this_loc.reg] = other_arg_info;
                    }
                    else {
                        assert(arg_info->loc.kind == X64_VREG_LOC_STACK);

                        // Will place in the next pass.
                        keep_going = true;
                    }
                }
                else {
                    // Just move into register.

                    if (arg_info->loc.kind == X64_VREG_LOC_STACK) {
                        X64_emit_text(generator, "    mov %s, %s", x64_reg_names[arg_size][arg_reg],
                                      X64_print_stack_offset(generator->tmp_mem, arg_info->loc.offset, arg_size));

                        arg_info->loc.kind = X64_VREG_LOC_REG;
                        arg_info->loc.reg = arg_reg;
                        arg_info_map[arg_reg] = arg_info;
                    }
                    else {
                        assert(arg_info->loc.kind == X64_VREG_LOC_REG);
                        X64_Reg this_reg = arg_info->loc.reg;

                        X64_emit_text(generator, "    mov %s, %s", x64_reg_names[arg_size][arg_reg],
                                      x64_reg_names[arg_size][this_reg]);

                        arg_info->loc.reg = arg_reg;
                        arg_info_map[arg_reg] = arg_info;

                        // Null out the original register in the map in case it is needed by another arg.
                        arg_info_map[this_reg] = NULL;
                    }
                }
            }
        }
    }
}

static void X64_place_args_in_stack(X64_Generator* generator, X64_StackArgsInfo stack_args_info, u32 num_args,
                                    X64_ArgInfo* arg_infos)
{
    u64 stack_args_size = stack_args_info.args_size;

    // Push stack arguments (if any)
    if (stack_args_size) {
        // Make room in the stack for arguments
        X64_emit_text(generator, "    sub rsp, %d", stack_args_size);

        // 1st pass: Move args that are currently in registers into their stack slots.
        // This ensures that we can freely use RAX as a temporary register in the second pass.
        u64 stack_offset = stack_args_info.args_offset;

        for (u32 i = 0; i < num_args; i += 1) {
            X64_ArgInfo* arg_info = arg_infos + i;

            if (arg_info->in_reg)
                continue; // Skip register args

            u64 arg_size = arg_info->arg->type->size;

            if (arg_info->loc.kind == X64_VREG_LOC_REG) {
                // Move directly into stack slot.
                X64_emit_text(generator, "    mov %s [rsp + %d], %s", x64_mem_size_label[arg_size], stack_offset,
                              x64_reg_names[arg_size][arg_info->loc.reg]);
            }

            stack_offset += ALIGN_UP(arg_size, X64_STACK_WORD_SIZE);
        }

        assert(stack_offset == stack_args_size);

        // 2nd pass: Move args that are currently spilled into their stack slots.
        stack_offset = stack_args_info.args_offset;

        for (u32 i = 0; i < num_args; i += 1) {
            X64_ArgInfo* arg_info = arg_infos + i;

            if (arg_info->in_reg)
                continue; // Skip register args

            u64 arg_size = arg_info->arg->type->size;

            if (arg_info->loc.kind == X64_VREG_LOC_STACK) {
                // Move into RAX.
                X64_emit_text(generator, "    mov %s, %s", x64_reg_names[arg_size][X64_RAX],
                              X64_print_stack_offset(generator->tmp_mem, arg_info->loc.offset, arg_size));

                // Move RAX into stack slot.
                X64_emit_text(generator, "    mov %s [rsp + %d], %s", x64_mem_size_label[arg_size], stack_offset,
                              x64_reg_names[arg_size][X64_RAX]);
            }

            stack_offset += ALIGN_UP(arg_size, X64_STACK_WORD_SIZE);
        }

        assert(stack_offset == stack_args_size);
    }
}

typedef struct X64_StackParamsInfo {
    u64 stack_spill_size; // Spill size below rsp
    List* local_var_iter; // Iterator pointing to the first local variable (if any) of the proc
} X64_StackParamsInfo;

static void X64_linux_assign_proc_param_offsets(X64_Generator* generator, DeclProc* dproc,
                                                X64_StackParamsInfo* stack_params_info)
{
    u64 stack_spill_size = 0;
    u32 index = 0;
    u32 arg_reg_index = 0;
    u64 stack_arg_offset = 0x10;

    Scope* scope = dproc->scope;
    List* head = &scope->sym_list;
    List* it = head->next;

    while (it != head) {
        // Only process params. Local variables are not processed here.
        // TODO: Support varargs
        if (index >= dproc->num_params)
            break;

        Symbol* sym = list_entry(it, Symbol, lnode);

        // Assign stack offsets to procedure params.
        assert(sym->kind == SYMBOL_VAR);

        Type* arg_type = sym->type;
        u64 arg_size = arg_type->size;
        u64 arg_align = arg_type->align;
        bool arg_in_reg = (arg_reg_index < x64_target.num_arg_regs) && (arg_size <= X64_MAX_INT_REG_SIZE);

        // Spill argument register below rsp
        if (arg_in_reg) {
            X64_Reg arg_reg = x64_target.arg_regs[arg_reg_index];

            stack_spill_size += arg_size;
            stack_spill_size = ALIGN_UP(stack_spill_size, arg_align);
            sym->as_var.offset = -stack_spill_size;

            X64_emit_text(generator, "    mov %s [rbp + %d], %s", x64_mem_size_label[arg_size], sym->as_var.offset,
                          x64_reg_names[arg_size][arg_reg]);

            arg_reg_index += 1;
        }
        else {
            sym->as_var.offset = stack_arg_offset;
            stack_arg_offset += arg_size;
            stack_arg_offset = ALIGN_UP(stack_arg_offset, arg_align);
            stack_arg_offset = ALIGN_UP(stack_arg_offset, X64_STACK_WORD_SIZE);
        }

        index += 1;
        it = it->next;
    }

    stack_params_info->stack_spill_size = stack_spill_size;
    stack_params_info->local_var_iter = it;
}

static void X64_windows_assign_proc_param_offsets(X64_Generator* generator, DeclProc* dproc,
                                                  X64_StackParamsInfo* stack_params_info)
{
    u32 index = 0;
    u64 stack_arg_offset = 0x10;

    Scope* scope = dproc->scope;
    List* head = &scope->sym_list;
    List* it = head->next;

    while (it != head) {
        // Only process params. Local variables are not processed here.
        // TODO: Support varargs
        if (index >= dproc->num_params)
            break;

        Symbol* sym = list_entry(it, Symbol, lnode);

        // Assign stack offsets to procedure params.
        assert(sym->kind == SYMBOL_VAR);

        Type* arg_type = sym->type;
        u64 arg_size = arg_type->size;
        u64 arg_align = arg_type->align;
        bool arg_in_reg = (index < x64_target.num_arg_regs) && (arg_size <= X64_MAX_INT_REG_SIZE);

        sym->as_var.offset = stack_arg_offset;
        stack_arg_offset += arg_size;
        stack_arg_offset = ALIGN_UP(stack_arg_offset, arg_align);
        stack_arg_offset = ALIGN_UP(stack_arg_offset, X64_STACK_WORD_SIZE);

        // Spill argument register to the shadow space (32 bytes above return address)
        // Only the first four arguments can be in a register.
        if (arg_in_reg) {
            X64_Reg arg_reg = x64_target.arg_regs[index];

            X64_emit_text(generator, "    mov %s [rbp + %d], %s", x64_mem_size_label[arg_size], sym->as_var.offset,
                          x64_reg_names[arg_size][arg_reg]);
        }

        index += 1;
        it = it->next;
    }

    stack_params_info->stack_spill_size = 0; // Did not spill below rsp; all args are above return address
    stack_params_info->local_var_iter = it;
}

static void X64_assign_proc_param_offsets(X64_Generator* generator, DeclProc* dproc,
                                          X64_StackParamsInfo* stack_params_info)
{
    if (x64_target.os == OS_LINUX) {
        X64_linux_assign_proc_param_offsets(generator, dproc, stack_params_info);
    }
    else {
        assert(x64_target.os == OS_WIN32);
        X64_windows_assign_proc_param_offsets(generator, dproc, stack_params_info);
    }
}

static u64 X64_assign_scope_stack_offsets(X64_Generator* generator, Scope* scope, u64 offset)
{
    u64 stack_size = offset;

    //
    // Sum sizes of local variables declared in this scope.
    //
    {
        List* head = &scope->sym_list;
        List* it = head->next;

        while (it != head) {
            Symbol* sym = list_entry(it, Symbol, lnode);

            if (sym->kind == SYMBOL_VAR) {
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
        u64 child_offset = stack_size;

        while (it != head) {
            Scope* child_scope = list_entry(it, Scope, lnode);
            u64 child_size = X64_assign_scope_stack_offsets(generator, child_scope, child_offset);

            if (child_size > stack_size)
                stack_size = child_size;

            it = it->next;
        }
    }

    return ALIGN_UP(stack_size, X64_STACK_ALIGN);
}

static u64 X64_assign_proc_stack_offsets(X64_Generator* generator, DeclProc* dproc)
{
    Scope* scope = dproc->scope;

    //
    // Spill procedure params into the stack (assign stack offsets to params).
    //

    X64_StackParamsInfo stack_params_info = {0};
    X64_assign_proc_param_offsets(generator, dproc, &stack_params_info);

    u64 stack_size = stack_params_info.stack_spill_size;

    //
    // Assign stack offsets to local variables declared in the procedure's top scope.
    //

    {
        List* it = stack_params_info.local_var_iter;
        List* head = &scope->sym_list;

        while (it != head) {
            Symbol* sym = list_entry(it, Symbol, lnode);

            // Assign stack offsets to local variables in procedure.
            if (sym->kind == SYMBOL_VAR) {
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
        u64 child_offset = stack_size;

        while (it != head) {
            Scope* child_scope = list_entry(it, Scope, lnode);
            u32 child_size = X64_assign_scope_stack_offsets(generator, child_scope, child_offset);

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

    if (has_base) {
        X64_Reg base_reg;
        s32 disp = addr->disp;

        if (addr->base_kind == IR_MEM_BASE_SYM) {
            Symbol* sym = addr->base.sym;

            // Early exit for global variable addresses.
            if (!sym->is_local) {
                ftprint_char_array(&dstr, true, "%s [rel %s]", mem_label, sym->name);
                return dstr;
            }

            disp += addr->base.sym->as_var.offset;
            base_reg = X64_RBP;
        }
        else {
            base_reg = X64_get_reg(group, addr->base.reg, size, false);
        }

        bool has_disp = disp != 0;
        const char* base_reg_name = x64_reg_names[X64_MAX_INT_REG_SIZE][base_reg];

        if (has_index) {
            X64_Reg index_reg = X64_get_reg(group, addr->index_reg, size, false);
            const char* index_reg_name = x64_reg_names[X64_MAX_INT_REG_SIZE][index_reg];

            if (has_disp)
                ftprint_char_array(&dstr, false, "%s [%s + %d*%s + %d]", mem_label, base_reg_name, addr->scale,
                                   index_reg_name, (s32)disp);
            else
                ftprint_char_array(&dstr, false, "%s [%s + %d*%s]", mem_label, base_reg_name, addr->scale,
                                   index_reg_name);
        }
        else {
            if (has_disp)
                ftprint_char_array(&dstr, false, "%s [%s + %d]", mem_label, base_reg_name, (s32)disp);
            else
                ftprint_char_array(&dstr, false, "%s [%s]", mem_label, base_reg_name);
        }
    }
    else {
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

    switch (op1_loc.kind) {
    case X64_VREG_LOC_REG: {
        switch (op2_loc.kind) {
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
    case X64_VREG_LOC_STACK: {
        switch (op2_loc.kind) {
        case X64_VREG_LOC_REG:
            X64_emit_text(generator, "    %s %s, %s", instr,
                          X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size),
                          x64_reg_names[op2_size][op2_loc.reg]);
            break;
        case X64_VREG_LOC_STACK: {
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
            if (writes_op1) {
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

    switch (op1_loc.kind) {
    case X64_VREG_LOC_REG: {
        X64_emit_text(generator, "    %s %s, %s", instr, x64_reg_names[op1_size][op1_loc.reg],
                      X64_print_imm(generator->tmp_mem, op2_imm, op2_size));
        break;
    }
    case X64_VREG_LOC_STACK: {
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

    X64_emit_text(generator, "    %s %s, %s", instr, x64_reg_names[op1_size][op1_reg],
                  X64_print_mem(&tmp_group, op2_vaddr, op2_size));

    X64_end_reg_group(&tmp_group);
}

static void X64_emit_mr_instr(X64_Generator* generator, const char* instr, u32 op1_size, IR_MemAddr* op1_vaddr,
                              u32 op2_size, IR_Reg op2_vreg)
{
    X64_RegGroup tmp_group = X64_begin_reg_group(generator);
    X64_Reg op2_reg = X64_get_reg(&tmp_group, op2_vreg, op2_size, false);

    X64_emit_text(generator, "    %s %s, %s", instr, X64_print_mem(&tmp_group, op1_vaddr, op1_size),
                  x64_reg_names[op2_size][op2_reg]);

    X64_end_reg_group(&tmp_group);
}

static void X64_emit_mi_instr(X64_Generator* generator, const char* instr, u32 op1_size, IR_MemAddr* op1_vaddr,
                              u32 op2_size, Scalar op2_imm)
{
    X64_RegGroup tmp_group = X64_begin_reg_group(generator);

    X64_emit_text(generator, "    %s %s, %s", instr, X64_print_mem(&tmp_group, op1_vaddr, op1_size),
                  X64_print_imm(generator->tmp_mem, op2_imm, op2_size));

    X64_end_reg_group(&tmp_group);
}

static void X64_gen_instr(X64_Generator* generator, u32 live_regs, u32 instr_index, bool is_last_instr, IR_Instr* instr)
{
    AllocatorState mem_state = allocator_get_state(generator->tmp_mem);

    if (instr->is_jmp_target)
        X64_emit_text(generator, "    %s:", X64_get_label(generator, instr_index));

    switch (instr->kind) {
    case IR_INSTR_ADD_R_R: {
        u32 size = (u32)instr->add_r_r.type->size;

        X64_emit_rr_instr(generator, "add", true, size, instr->add_r_r.dst, size, instr->add_r_r.src);
        break;
    }
    case IR_INSTR_ADD_R_M: {
        u32 size = (u32)instr->add_r_m.type->size;

        X64_emit_rm_instr(generator, "add", true, size, instr->add_r_m.dst, size, &instr->add_r_m.src);
        break;
    }
    case IR_INSTR_ADD_R_I: {
        u32 size = (u32)instr->add_r_i.type->size;

        X64_emit_ri_instr(generator, "add", size, instr->add_r_i.dst, size, instr->add_r_i.src);
        break;
    }
    case IR_INSTR_SUB_R_R: {
        u32 size = (u32)instr->sub_r_r.type->size;

        X64_emit_rr_instr(generator, "sub", true, size, instr->sub_r_r.dst, size, instr->sub_r_r.src);
        break;
    }
    case IR_INSTR_SUB_R_M: {
        u32 size = (u32)instr->sub_r_m.type->size;

        X64_emit_rm_instr(generator, "sub", true, size, instr->sub_r_m.dst, size, &instr->sub_r_m.src);
        break;
    }
    case IR_INSTR_SUB_R_I: {
        u32 size = (u32)instr->sub_r_i.type->size;

        X64_emit_ri_instr(generator, "sub", size, instr->sub_r_i.dst, size, instr->sub_r_i.src);
        break;
    }
    case IR_INSTR_MUL_R_R: {
        u32 size = (u32)instr->mul_r_r.type->size;

        X64_emit_rr_instr(generator, "imul", true, size, instr->mul_r_r.dst, size, instr->mul_r_r.src);

        break;
    }
    case IR_INSTR_MUL_R_M: {
        u32 size = (u32)instr->mul_r_m.type->size;

        X64_emit_rm_instr(generator, "imul", true, size, instr->mul_r_m.dst, size, &instr->mul_r_m.src);
        break;
    }
    case IR_INSTR_MUL_R_I: {
        u32 size = (u32)instr->mul_r_i.type->size;

        X64_emit_ri_instr(generator, "imul", size, instr->mul_r_i.dst, size, instr->mul_r_i.src);
        break;
    }
    case IR_INSTR_SAR_R_R: {
        u32 size = (u32)instr->sar_r_r.type->size;

        X64_emit_rr_instr(generator, "sar", true, size, instr->sar_r_r.dst, size, instr->sar_r_r.src);
        break;
    }
    case IR_INSTR_SAR_R_M: {
        u32 size = (u32)instr->sar_r_m.type->size;

        X64_emit_rm_instr(generator, "sar", true, size, instr->sar_r_m.dst, size, &instr->sar_r_m.src);
        break;
    }
    case IR_INSTR_SAR_R_I: {
        u32 size = (u32)instr->sar_r_i.type->size;

        X64_emit_ri_instr(generator, "sar", size, instr->sar_r_i.dst, size, instr->sar_r_i.src);
        break;
    }
    case IR_INSTR_NEG: {
        Type* type = instr->neg.type;
        X64_VRegLoc dst_loc = X64_vreg_loc(generator, instr->neg.dst);

        if (dst_loc.kind == X64_VREG_LOC_REG) {
            X64_emit_text(generator, "    neg %s", x64_reg_names[type->size][dst_loc.reg]);
        }
        else {
            assert(dst_loc.kind == X64_VREG_LOC_STACK);
            X64_emit_text(generator, "    neg %s",
                          X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, (u32)type->size));
        }
        break;
    }
    case IR_INSTR_NOT: {
        Type* type = instr->not .type;
        X64_VRegLoc dst_loc = X64_vreg_loc(generator, instr->not .dst);

        if (dst_loc.kind == X64_VREG_LOC_REG) {
            X64_emit_text(generator, "    not %s", x64_reg_names[type->size][dst_loc.reg]);
        }
        else {
            assert(dst_loc.kind == X64_VREG_LOC_STACK);
            X64_emit_text(generator, "    not %s",
                          X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, (u32)type->size));
        }
        break;
    }
    case IR_INSTR_LIMM: {
        u32 size = (u32)instr->limm.type->size;

        X64_emit_ri_instr(generator, "mov", size, instr->limm.dst, size, instr->limm.src);
        break;
    }
    case IR_INSTR_LADDR: {
        u32 size = (u32)instr->laddr.type->size;

        X64_emit_rm_instr(generator, "lea", true, X64_MAX_INT_REG_SIZE, instr->laddr.dst, size, &instr->laddr.mem);
        break;
    }
    case IR_INSTR_TRUNC_R_R: {
        u32 size = (u32)instr->trunc_r_r.dst_type->size;

        X64_emit_rr_instr(generator, "mov", true, size, instr->trunc_r_r.dst, size, instr->trunc_r_r.src);
        break;
    }
    case IR_INSTR_TRUNC_R_M: {
        u32 size = (u32)instr->trunc_r_r.dst_type->size;

        X64_emit_rm_instr(generator, "mov", true, size, instr->trunc_r_m.dst, size, &instr->trunc_r_m.src);
        break;
    }
    case IR_INSTR_ZEXT_R_R: {
        Type* dst_type = instr->zext_r_r.dst_type;
        Type* src_type = instr->zext_r_r.src_type;

        X64_emit_rr_instr(generator, "movzx", true, dst_type->size, instr->zext_r_r.dst, src_type->size,
                          instr->zext_r_r.src);
        break;
    }
    case IR_INSTR_ZEXT_R_M: {
        Type* dst_type = instr->zext_r_m.dst_type;
        Type* src_type = instr->zext_r_m.src_type;

        X64_emit_rm_instr(generator, "movzx", true, dst_type->size, instr->zext_r_m.dst, src_type->size,
                          &instr->zext_r_m.src);
        break;
    }
    case IR_INSTR_SEXT_R_R: {
        Type* dst_type = instr->sext_r_r.dst_type;
        Type* src_type = instr->sext_r_r.src_type;
        const char* movsx = src_type->size >= type_u32->size ? "movsxd" : "movsx";

        X64_emit_rr_instr(generator, movsx, true, dst_type->size, instr->sext_r_r.dst, src_type->size,
                          instr->sext_r_r.src);
        break;
    }
    case IR_INSTR_SEXT_R_M: {
        Type* dst_type = instr->sext_r_m.dst_type;
        Type* src_type = instr->sext_r_m.src_type;
        const char* movsx = src_type->size >= type_u32->size ? "movsxd" : "movsx";

        X64_emit_rm_instr(generator, movsx, true, dst_type->size, instr->sext_r_m.dst, src_type->size,
                          &instr->sext_r_m.src);
        break;
    }
    case IR_INSTR_LOAD: {
        u32 size = (u32)instr->load.type->size;

        X64_emit_rm_instr(generator, "mov", true, size, instr->load.dst, size, &instr->load.src);
        break;
    }
    case IR_INSTR_STORE_R: {
        u32 size = (u32)instr->store_r.type->size;

        X64_emit_mr_instr(generator, "mov", size, &instr->store_r.dst, size, instr->store_r.src);
        break;
    }
    case IR_INSTR_STORE_I: {
        u32 size = instr->store_i.type->size;

        X64_emit_mi_instr(generator, "mov", size, &instr->store_i.dst, size, instr->store_i.src);
        break;
    }
    case IR_INSTR_CMP_R_R: {
        u32 size = (u32)instr->cmp_r_r.type->size;

        X64_emit_rr_instr(generator, "cmp", false, size, instr->cmp_r_r.op1, size, instr->cmp_r_r.op2);
        break;
    }
    case IR_INSTR_CMP_R_M: {
        u32 size = (u32)instr->cmp_r_m.type->size;

        X64_emit_rm_instr(generator, "cmp", false, size, instr->cmp_r_m.op1, size, &instr->cmp_r_m.op2);
        break;
    }
    case IR_INSTR_CMP_R_I: {
        u32 size = (u32)instr->cmp_r_i.type->size;

        X64_emit_ri_instr(generator, "cmp", size, instr->cmp_r_i.op1, size, instr->cmp_r_i.op2);
        break;
    }
    case IR_INSTR_CMP_M_R: {
        u32 size = (u32)instr->cmp_m_r.type->size;

        X64_emit_mr_instr(generator, "cmp", size, &instr->cmp_m_r.op1, size, instr->cmp_m_r.op2);
        break;
    }
    case IR_INSTR_CMP_M_I: {
        u32 size = (u32)instr->cmp_m_i.type->size;

        X64_emit_mi_instr(generator, "cmp", size, &instr->cmp_m_i.op1, size, instr->cmp_m_i.op2);
        break;
    }
    case IR_INSTR_JMP: {
        X64_emit_text(generator, "    jmp %s", X64_get_label(generator, instr->jmp.jmp_target));
        break;
    }
    case IR_INSTR_JMPCC: {
        X64_emit_text(generator, "    j%s %s", x64_condition_codes[instr->jmpcc.cond],
                      X64_get_label(generator, instr->jmpcc.jmp_target));
        break;
    }
    case IR_INSTR_SETCC: {
        X64_VRegLoc dst_loc = X64_vreg_loc(generator, instr->setcc.dst);

        if (dst_loc.kind == X64_VREG_LOC_REG) {
            X64_emit_text(generator, "    set%s %s", x64_condition_codes[instr->setcc.cond],
                          x64_reg_names[1][dst_loc.reg]);
        }
        else {
            assert(dst_loc.kind == X64_VREG_LOC_STACK);
            X64_emit_text(generator, "    set%s %s", x64_condition_codes[instr->setcc.cond],
                          X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, 1));
        }
        break;
    }
    case IR_INSTR_RET: {
        Type* ret_type = instr->ret.type;

        if (ret_type != type_void) {
            X64_VRegLoc ret_loc = X64_vreg_loc(generator, instr->ret.src);

            assert(ret_loc.kind != X64_VREG_LOC_UNASSIGNED);

            if (ret_loc.kind == X64_VREG_LOC_STACK) {
                X64_emit_text(generator, "    mov %s, %s", x64_reg_names[ret_type->size][X64_RAX],
                              X64_print_stack_offset(generator->tmp_mem, ret_loc.offset, ret_type->size));
            }
            else if ((ret_loc.kind == X64_VREG_LOC_REG) && (ret_loc.reg != X64_RAX)) {
                X64_emit_text(generator, "    mov %s, %s", x64_reg_names[ret_type->size][X64_RAX],
                              x64_reg_names[ret_type->size][ret_loc.reg]);
            }
        }

        if (!is_last_instr)
            X64_emit_text(generator, "    jmp end.%s", generator->curr_proc.sym->name);

        break;
    }
    case IR_INSTR_CALL: {
        u32 num_args = instr->call.num_args;
        IR_InstrCallArg* args = instr->call.args;

        // NOTE: Stack frame must be 16-byte aligned before procedure call.
        // If the number of stack args + caller-saved regs is not even (16-byte aligned),
        // we MUST subtract 8 from stack BEFORE pushing anything into stack
        // See: https://godbolt.org/z/cM9Encdsc
        char** rsp_align_instr = X64_emit_text(generator, NULL);

        // Group used to track the registers that we are saving in the stack before the call.
        X64_RegGroup group = X64_begin_reg_group(generator);

        // Save caller-saved registers that are needed after the procedure call.
        for (u32 r = 0; r < X64_REG_COUNT; r += 1) {
            X64_Reg reg = (X64_Reg)r;
            bool is_caller_saved = X64_is_caller_saved_reg(reg);
            bool preserve = u32_is_bit_set(live_regs, reg);

            if (is_caller_saved && preserve) {
                X64_push_reg(&group, reg);
            }
        }

        // Pre-process call arguments:
        //   - Determine which arguments will be passed via registers and which will be passed via the stack.
        //   - |arg_infos| is an array of |num_args| elements. Each element has information on where the corresponding
        //     argument will be passed, and if the argument should be saved on the stack if it needs to be swapped.
        //   - |arg_info_map| maps an x64 register to the arg_info element that currently occupies it.
        X64_ArgInfo* arg_infos = alloc_array(generator->tmp_mem, X64_ArgInfo, num_args, true);
        X64_ArgInfo* arg_info_map[X64_REG_COUNT] = {0};
        X64_StackArgsInfo stack_args_info =
            X64_preprocess_call_args(generator, live_regs, num_args, args, arg_infos, arg_info_map);

        // Place arguments in the appropriate locations.
        X64_place_args_in_regs(&group, num_args, arg_infos,
                               arg_info_map); // Will save swapped callee-saved regs in the stack
        X64_place_args_in_stack(generator, stack_args_info, num_args, arg_infos);

        // Align stack before call.
        u64 total_stack_size = stack_args_info.args_size + group.num_tmp_regs * X64_MAX_INT_REG_SIZE;
        u64 align_stack_size = 0;

        if (total_stack_size & (X64_STACK_ALIGN - 1)) {
            align_stack_size = X64_STACK_WORD_SIZE;

            X64_fill_line(generator, rsp_align_instr, "    sub rsp, %lu", align_stack_size);
            total_stack_size += align_stack_size;
        }

        // Stack should now be aligned properly for procedure call.
        assert((total_stack_size & (X64_STACK_ALIGN - 1)) == 0);

        X64_emit_text(generator, "    call %s", instr->call.sym->name);

        // Move return value (if any) to appropriate register.
        Type* ret_type = instr->call.sym->type->as_proc.ret;

        if (ret_type != type_void) {
            X64_VRegLoc dst_loc = X64_vreg_loc(generator, instr->call.dst);

            if (dst_loc.kind == X64_VREG_LOC_STACK) {
                // Move result (in RAX) to stack offset.
                X64_emit_text(generator, "    mov %s, %s",
                              X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, ret_type->size),
                              x64_reg_names[ret_type->size][X64_RAX]);
            }
            else {
                assert(dst_loc.kind == X64_VREG_LOC_REG);

                if (dst_loc.reg != X64_RAX) {
                    // Move result (in RAX) to allocated result register.
                    X64_emit_text(generator, "    mov %s, %s", x64_reg_names[ret_type->size][dst_loc.reg],
                                  x64_reg_names[ret_type->size][X64_RAX]);
                }
            }
        }

        // Clean up stack args
        if (stack_args_info.args_size) {
            X64_emit_text(generator, "    add rsp, %u", stack_args_info.args_size);
        }

        // Restore saved registers.
        X64_end_reg_group(&group);

        // Clean up any initial stack alignment
        if (align_stack_size) {
            X64_emit_text(generator, "    add rsp, %u", align_stack_size);
        }

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

    bool is_nonleaf = sym->as_proc.is_nonleaf;

    // Set different scratch register order for leaf vs nonleaf procedures.
    if (is_nonleaf) {
        generator->curr_proc.scratch_regs = x64_target.nonleaf_scratch_regs;
        generator->curr_proc.num_scratch_regs = x64_target.num_nonleaf_scratch_regs;
    }
    else {
        generator->curr_proc.scratch_regs = x64_target.leaf_scratch_regs;
        generator->curr_proc.num_scratch_regs = x64_target.num_leaf_scratch_regs;
    }

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
        X64_linear_scan_reg_alloc(generator->tmp_mem, num_vregs, vreg_intervals, vreg_locs,
                                  generator->curr_proc.num_scratch_regs, generator->curr_proc.scratch_regs, stack_size);

    stack_size = reg_alloc.stack_offset;
    generator->curr_proc.vreg_map = vreg_locs;

#if 0
    ftprint_out("Register allocation for %s (%s):\n", sym->name, is_nonleaf ? "nonleaf": "leaf");
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

    X64_VRegIntervalList active = {.arena = generator->tmp_mem};
    active.sentinel.next = &active.sentinel;
    active.sentinel.prev = &active.sentinel;
    u32 live_regs = 0;
    u32 first_interval = 0;

    for (size_t instr_index = 0; instr_index < num_instrs; instr_index += 1) {
        // Expire old intervals
        X64_VRegInterval* head = &active.sentinel;
        X64_VRegInterval* it = head->next;

        while (it != head) {
            X64_VRegInterval* next = it->next;

            // This comparison uses > instead of >= so that registers that are NOT
            // specified by the used_caller_regs mask are guaranteed to NOT be used after this instruction.
            // This information is used by call instructions to determine if arguments in caller-saved registers
            // need to be preserved across procedure calls.
            if (it->interval.end > instr_index)
                break;

            X64_vreg_interval_list_rm(&active, it);

            X64_VRegLoc* loc = vreg_locs + it->index;

            if (loc->kind == X64_VREG_LOC_REG)
                u32_unset_bit(&live_regs, loc->reg);

            it = next;
        }

        // Generate x64 instructions
        X64_gen_instr(generator, live_regs, instr_index, instr_index == num_instrs - 1, instrs[instr_index]);

        // Add new active intervals.
        for (u32 j = first_interval; j < num_vregs; j += 1) {
            LifetimeInterval* interval = vreg_intervals + j;

            if (instr_index < interval->start) {
                first_interval = j;
                break;
            }

            X64_vreg_interval_list_add(&active, interval, j);

            X64_VRegLoc* loc = vreg_locs + j;

            if (loc->kind == X64_VREG_LOC_REG)
                u32_set_bit(&live_regs, loc->reg);
        }
    }

    // End label
    X64_emit_text(generator, "    end.%s:", sym->name);

    // Save/Restore callee-saved registers.
    char* tmp_line = array_create(generator->tmp_mem, char, X64_INIT_LINE_LEN);

    for (uint32_t r = 0; r < X64_REG_COUNT; r += 1) {
        X64_Reg reg = (X64_Reg)r;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg)) {
            ftprint_char_array(&tmp_line, false, "    push %s\n", x64_reg_names[X64_MAX_INT_REG_SIZE][reg]);
            X64_emit_text(generator, "    pop %s", x64_reg_names[X64_MAX_INT_REG_SIZE][reg]);
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

    for (u32 i = 0; i < num_vars; i += 1) {
        Symbol* sym = vars[i];
        DeclVar* dvar = (DeclVar*)sym->decl;

        X64_emit_data(generator, "ALIGN %d", sym->type->align);
        X64_emit_data(generator, "%s: ", sym->name);

        if (dvar->init) {
            X64_emit_data_value(generator, sym->type, dvar->init->const_val);
        }
        else {
            Scalar zero_val = {0};
            X64_emit_data_value(generator, sym->type, zero_val);
        }
    }
    allocator_restore_state(mem_state);
}

static void X64_write_output_file(X64_Generator* generator, FILE* out_fd)
{
    ftprint_file(out_fd, false, "; Generated by the Nibble compiler.\n\n");

    for (Bucket* bucket = generator->data_lines->first; bucket; bucket = bucket->next) {
        for (size_t i = 0; i < bucket->count; i += 1) {
            const char* str = (const char*)bucket->elems[i];

            if (str)
                ftprint_file(out_fd, false, "%s\n", str);
        }
    }

    for (Bucket* bucket = generator->text_lines->first; bucket; bucket = bucket->next) {
        for (size_t i = 0; i < bucket->count; i += 1) {
            const char* str = (const char*)bucket->elems[i];

            if (str)
                ftprint_file(out_fd, false, "%s\n", str);
        }
    }
}

bool x64_gen_module(Allocator* gen_mem, Allocator* tmp_mem, IR_Module* module, const char* output_file)
{
    FILE* out_fd = fopen(output_file, "w");
    if (!out_fd) {
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

    for (u32 i = 0; i < num_procs; i += 1) {
        X64_gen_proc(&generator, i, module->procs[i]);
    }

    // Output assembly to file.
    X64_write_output_file(&generator, out_fd);

    fclose(out_fd);

    return true;
}
