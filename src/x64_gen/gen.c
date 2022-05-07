#include "stream.h"
#include "x64_gen/gen.h"

#include "x64_gen/regs.c"
#include "x64_gen/lir.c"
#include "x64_gen/convert_ir.c"
#include "x64_gen/livevar.c"
#include "x64_gen/reg_alloc.c"
#include "x64_gen/print_lir.c"

#define X64_ASM_LINE_LEN 64
#define X64_STR_LIT_PRE "__nibble_str_lit_"
#define X64_FLOAT_LIT_PRE "__nibble_float_lit_"

static const char* x64_reg_h_names[X64_REG_COUNT] = {[X64_RAX] = "ah", [X64_RCX] = "ch", [X64_RDX] = "dh", [X64_RBX] = "bh"};

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

typedef enum X64_SIBDAddrKind {
    X64_SIBD_ADDR_GLOBAL,
    X64_SIBD_ADDR_LOCAL,
    X64_SIBD_ADDR_STR_LIT,
    X64_SIBD_ADDR_FLOAT_LIT,
} X64_SIBDAddrKind;

typedef struct X64_SIBDAddr {
    X64_SIBDAddrKind kind;
    union {
        Symbol* global;
        struct {
            X64_Reg base_reg;
            X64_Reg index_reg;
            s32 disp;
            u8 scale;
        } local;
        StrLit* str_lit;
        FloatLit* float_lit;
    };
} X64_SIBDAddr;

typedef struct X64_ProcState {
    Symbol* sym;
    u32 id;
    X64_LIRBuilder* builder;

    X64_ScratchRegs (*scratch_regs)[X64_REG_CLASS_COUNT];
    List text_lines;
} X64_ProcState;

typedef struct X64_AsmLine {
    char* text; // NOTE: stretchy buffer
    ListNode lnode;
} X64_AsmLine;

typedef struct X64_Generator {
    FILE* out_fd;
    List data_lines;
    X64_ProcState curr_proc;
    Allocator* gen_mem;
    Allocator* tmp_mem;
} X64_Generator;

static X64_LRegLoc X64_lreg_loc(X64_Generator* generator, u32 lreg)
{
    u32 rng_idx = X64_find_alias_reg(generator->curr_proc.builder, lreg);
    X64_LRegLoc reg_loc = generator->curr_proc.builder->lreg_ranges[rng_idx].loc;

    assert(reg_loc.kind != X64_LREG_LOC_UNASSIGNED);

    return reg_loc;
}

static void X64_output_asm_lines(FILE* out_fd, ListNode* head)
{
    for (ListNode* it = head->next; it != head; it = it->next) {
        X64_AsmLine* asm_line = list_entry(it, X64_AsmLine, lnode);

        assert(array_back(asm_line->text) == '\0');
        ftprint_file(out_fd, false, "%s\n", asm_line->text);
    }
}

static ListNode* X64_add_asm_line(Allocator* alloc, ListNode* prev, const char* format, va_list vargs)
{
    X64_AsmLine* asm_line = alloc_type(alloc, X64_AsmLine, true);
    asm_line->text = array_create(alloc, char, X64_ASM_LINE_LEN);

    ftprintv_char_array(&asm_line->text, true, format, vargs);

    list_add(prev, &asm_line->lnode);

    return &asm_line->lnode;
}

static ListNode* X64_emit_text(X64_Generator* gen, const char* format, ...)
{
    va_list vargs;

    va_start(vargs, format);
    ListNode* node = X64_add_asm_line(gen->gen_mem, gen->curr_proc.text_lines.prev, format, vargs);
    va_end(vargs);

    return node;
}

static ListNode* X64_insert_text(X64_Generator* gen, ListNode* prev, const char* format, ...)
{
    va_list vargs;

    va_start(vargs, format);
    ListNode* node = X64_add_asm_line(gen->gen_mem, prev, format, vargs);
    va_end(vargs);

    return node;
}

static void X64_emit_data(X64_Generator* gen, const char* format, ...)
{
    va_list vargs;

    va_start(vargs, format);
    X64_add_asm_line(gen->gen_mem, gen->data_lines.prev, format, vargs);
    va_end(vargs);
}

static char* X64_get_label(X64_Generator* generator, u32 bblock_id)
{
    char* dstr = array_create(generator->tmp_mem, char, 8);
    ftprint_char_array(&dstr, true, "L.%u.%u", generator->curr_proc.id, bblock_id);

    return dstr;
}

static char* X64_print_stack_offset(Allocator* arena, s32 offset, u32 size)
{
    char* dstr = array_create(arena, char, 8);

    ftprint_char_array(&dstr, true, "%s [rbp + %d]", x64_mem_size_label[size], offset);

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

static void X64_print_global_val(Allocator* allocator, ConstExpr* const_expr, char** line);

static void X64_print_global_zero_bytes(char** line, size_t size)
{
    ftprint_char_array(line, false, "%s ", x64_data_size_label[1]);

    for (size_t j = 0; j < size; j += 1) {
        char inner_sep = (j == size - 1) ? '\n' : ',';
        ftprint_char_array(line, false, "0x%.2X%c", 0, inner_sep);
    }
}

static void X64_print_global_arr_init(Allocator* allocator, ConstExpr* const_expr, char** line)
{
    Type* type = const_expr->type;
    assert(type->kind == TYPE_ARRAY);

    Type* elem_type = type->as_array.base;
    size_t num_elems = type->as_array.len;
    ConstExpr** init_vals = alloc_array(allocator, ConstExpr*, num_elems, true); // Initialized to NULL

    // Iterate through initializers and overwrite appropriate elements in init_vals array with
    // the specified initializer value.
    ConstArrayInitzer* init = &const_expr->array_initzer;

    for (size_t i = 0; i < init->num_initzers; i += 1) {
        ConstArrayMemberInitzer* initzer = init->initzers + i;

        init_vals[initzer->index] = &initzer->const_expr;
    }

    // Print an initial value for each element.
    for (u64 i = 0; i < num_elems; i += 1) {
        if (init_vals[i]) {
            X64_print_global_val(allocator, init_vals[i], line);
        }
        else {
            X64_print_global_zero_bytes(line, elem_type->size);
        }
    }
}

static void X64_print_global_struct_init(Allocator* allocator, ConstExpr* const_expr, char** line)
{
    Type* type = const_expr->type;
    assert(type->kind == TYPE_STRUCT);

    TypeAggregateBody* type_agg = &type->as_struct.body;
    ConstExpr** field_exprs = const_expr->struct_initzer.field_exprs;

    TypeAggregateField* fields = type_agg->fields;
    size_t num_fields = type_agg->num_fields;
    size_t offset = 0; // Tracks the struct byte offset that has been initialized.

    // Init fields.
    for (size_t i = 0; i < num_fields; i++) {
        TypeAggregateField* field = fields + i;
        size_t field_size = field->type->size;
        size_t padding = field->offset - offset;

        // Fill padding with zeros.
        if (padding) {
            X64_print_global_zero_bytes(line, padding);
            offset = field->offset;
        }

        // Init field with specified value or zero.
        if (field_exprs[i]) {
            X64_print_global_val(allocator, field_exprs[i], line);
        }
        else {
            X64_print_global_zero_bytes(line, field_size);
        }

        offset += field_size;
    }

    // Clear padding after last field.
    size_t padding = type->size - offset;

    if (padding) {
        X64_print_global_zero_bytes(line, padding);
    }
}

static void X64_print_global_union_init(Allocator* allocator, ConstExpr* const_expr, char** line)
{
    Type* type = const_expr->type;
    assert(type->kind == TYPE_UNION);

    TypeAggregateField* field = &type->as_union.body.fields[const_expr->union_initzer.field_index];
    ConstExpr* field_expr = const_expr->union_initzer.field_expr;

    if (field_expr) {
        X64_print_global_val(allocator, field_expr, line);

        size_t padding = type->size - field->type->size;

        if (padding) {
            X64_print_global_zero_bytes(line, padding);
        }
    }
    else {
        X64_print_global_zero_bytes(line, type->size);
    }
}

static void X64_print_global_int_bytes(Scalar imm, size_t size, char** line)
{
    u64 elem_val = imm.as_int._u64;
    u64 mask = 0xFFLL;

    ftprint_char_array(line, false, "%s ", x64_data_size_label[1]);

    // Print each byte of the value (comma-separated)
    for (size_t i = 0; i < size; i += 1) {
        u64 val = (elem_val & mask) >> (i << 3);
        char sep = (i == size - 1) ? '\n' : ',';

        ftprint_char_array(line, false, "0x%.2X%c", val, sep);

        mask = mask << 8;
    }
}

static void X64_print_global_val(Allocator* allocator, ConstExpr* const_expr, char** line)
{
    switch (const_expr->kind) {
    case CONST_EXPR_NONE: {
        size_t size = const_expr->type->size;

        ftprint_char_array(line, false, "%s ", x64_data_size_label[1]);

        // Print each byte of the value (comma-separated)
        for (size_t i = 0; i < size; i += 1) {
            char sep = (i == size - 1) ? '\n' : ',';

            ftprint_char_array(line, false, "0x00%c", sep);
        }
        break;
    }
    case CONST_EXPR_IMM: {
        X64_print_global_int_bytes(const_expr->imm, const_expr->type->size, line);
        break;
    }
    case CONST_EXPR_MEM_ADDR: {
        ConstAddr* addr = &const_expr->addr;

        if (addr->kind == CONST_ADDR_SYM) {
            ftprint_char_array(line, false, "%s %s", x64_data_size_label[const_expr->type->size],
                               symbol_mangled_name(allocator, addr->sym));
        }
        else {
            assert(addr->kind == CONST_ADDR_STR_LIT);
            ftprint_char_array(line, false, "%s %s_%llu", x64_data_size_label[const_expr->type->size], X64_STR_LIT_PRE,
                               addr->str_lit->id);
        }

        if (addr->disp) {
            ftprint_char_array(line, false, " + %d", (s32)addr->disp);
        }

        ftprint_char_array(line, false, "\n");
        break;
    }
    case CONST_EXPR_STR_LIT: {
        StrLit* str_lit = const_expr->str_lit;
        size_t len = str_lit->len;
        const char* str = str_lit->str;

        ftprint_char_array(line, false, "%s ", x64_data_size_label[1]);

        for (size_t i = 0; i < len; i += 1) {
            ftprint_char_array(line, false, "0x%.2X,", str[i]);
        }

        ftprint_char_array(line, false, "0x00\n");

        break;
    }
    case CONST_EXPR_FLOAT_LIT: {
        FloatLit* float_lit = const_expr->float_lit;
        Scalar imm = {.as_float = float_lit->value};
        size_t size = float_kind_sizes[float_lit->kind];

        X64_print_global_int_bytes(imm, size, line);
        break;
    }
    case CONST_EXPR_PROC: {
        ftprint_char_array(line, false, "%s %s", x64_data_size_label[const_expr->type->size],
                           symbol_mangled_name(allocator, const_expr->sym));
        break;
    }
    case CONST_EXPR_ARRAY_INIT: {
        X64_print_global_arr_init(allocator, const_expr, line);
        break;
    }
    case CONST_EXPR_STRUCT_INIT: {
        X64_print_global_struct_init(allocator, const_expr, line);
        break;
    }
    case CONST_EXPR_UNION_INIT: {
        X64_print_global_union_init(allocator, const_expr, line);
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64_emit_global_data(X64_Generator* generator, Symbol* sym)
{
    assert(sym->kind == SYMBOL_VAR);

    Allocator* tmp_mem = generator->tmp_mem;
    Type* type = sym->type;

    X64_emit_data(generator, "ALIGN %d", type->align);
    X64_emit_data(generator, "%s: ", symbol_mangled_name(tmp_mem, sym));

    AllocatorState mem_state = allocator_get_state(tmp_mem);
    char* line = array_create(tmp_mem, char, type->size << 3);

    X64_print_global_val(tmp_mem, &sym->as_var.const_expr, &line);

    array_push(line, '\0');
    X64_emit_data(generator, "%s\n", line);
    allocator_restore_state(mem_state);
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
    u32 used_tmp_reg_mask;
} X64_RegGroup;

static X64_RegGroup X64_begin_reg_group(X64_Generator* generator)
{
    X64_RegGroup group = {
        .generator = generator,
    };

    return group;
}

static X64_Reg X64_get_reg(X64_RegGroup* group, u32 lreg, u32 size, bool store, u32 banned_regs)
{
    X64_LRegLoc lreg_loc = X64_lreg_loc(group->generator, lreg);

    // If this virtual register was not spilled during allocation, just return its assigned
    // physical register.
    if (lreg_loc.kind == X64_LREG_LOC_REG) {
        return lreg_loc.reg;
    }

    // This virtual register was spilled during allocation, so use a temporary physical register which will have
    // to be restored later.

    X64_Generator* generator = group->generator;
    X64_ScratchRegs* int_scratch_regs = &(*generator->curr_proc.scratch_regs)[X64_REG_CLASS_INT];
    u32 num_scratch_regs = int_scratch_regs->num_regs;
    X64_Reg* scratch_regs = int_scratch_regs->regs;

    assert(lreg_loc.kind == X64_LREG_LOC_STACK);
    assert(group->num_tmp_regs < num_scratch_regs);

    X64_Reg x64_reg = X64_REG_COUNT;

    // Try to use a scratch register that is not currently being used as a tmp register and is not banned.
    for (u32 r = 0; r < num_scratch_regs; r += 1) {
        X64_Reg reg = scratch_regs[r];

        bool is_used_as_tmp = u32_is_bit_set(group->used_tmp_reg_mask, reg);
        bool is_banned = u32_is_bit_set(banned_regs, reg);

        if (!is_used_as_tmp && !is_banned) {
            x64_reg = reg;
            break;
        }
    }

    assert(x64_reg != X64_REG_COUNT);

    // Record register in group
    group->num_tmp_regs += 1;
    u32_set_bit(&group->used_tmp_reg_mask, x64_reg);

    Allocator* tmp_mem = group->generator->tmp_mem;

    X64_TmpReg* tmp_reg = alloc_type(tmp_mem, X64_TmpReg, true);
    tmp_reg->reg = x64_reg;
    tmp_reg->offset = lreg_loc.offset;
    tmp_reg->size = size;
    tmp_reg->store = store;

    X64_emit_text(group->generator, "  push %s", x64_reg_names[X64_MAX_INT_REG_SIZE][tmp_reg->reg]);
    X64_emit_text(group->generator, "  mov %s, %s", x64_reg_names[size][tmp_reg->reg],
                  X64_print_stack_offset(tmp_mem, lreg_loc.offset, size));

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    return tmp_reg->reg;
}

static void X64_save_reg(X64_RegGroup* group, X64_Reg reg)
{
    assert(reg != X64_REG_COUNT);
    assert(!u32_is_bit_set(group->used_tmp_reg_mask, reg));

    Allocator* tmp_mem = group->generator->tmp_mem;

    X64_TmpReg* tmp_reg = alloc_type(tmp_mem, X64_TmpReg, true);
    tmp_reg->reg = reg;

    X64_emit_text(group->generator, "  push %s", x64_reg_names[X64_MAX_INT_REG_SIZE][tmp_reg->reg]);

    // Add scratch register to the list (stack) of regs in group.
    tmp_reg->next = group->first_tmp_reg;
    group->first_tmp_reg = tmp_reg;

    // Record register in group
    u32_set_bit(&group->used_tmp_reg_mask, tmp_reg->reg);

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
            X64_emit_text(generator, "  mov %s, %s", X64_print_stack_offset(generator->tmp_mem, it->offset, it->size),
                          x64_reg_names[it->size][it->reg]);
        }

        X64_emit_text(generator, "  pop %s", x64_reg_names[X64_MAX_INT_REG_SIZE][it->reg]);
        it = it->next;
    }
}

static ListNode* X64_print_push_reg(X64_Generator* gen, ListNode* prev, X64_Reg reg)
{
    ListNode* node;

    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        node = X64_insert_text(gen, prev, "  push %s", x64_reg_names[X64_MAX_INT_REG_SIZE][reg]);
    }
    else {
        node = X64_insert_text(gen, prev, "  sub rsp, 16");
        node = X64_insert_text(gen, node, "  movdqu oword [rsp], %s", x64_fp_reg_names[reg]);
    }

    return node;
}

static ListNode* X64_print_pop_reg(X64_Generator* gen, ListNode* prev, X64_Reg reg)
{
    ListNode* node;

    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        node = X64_insert_text(gen, prev, "  pop %s", x64_reg_names[X64_MAX_INT_REG_SIZE][reg]);
    }
    else {
        node = X64_insert_text(gen, prev, "  movdqu %s, oword [rsp]", x64_fp_reg_names[reg]);
        node = X64_insert_text(gen, node, "  add rsp, 16");
    }

    return node;
}

typedef struct X64_StackParamsInfo {
    u64 stack_spill_size; // Spill size below rsp
    List* local_var_iter; // Iterator pointing to the first local variable (if any) of the proc
} X64_StackParamsInfo;

typedef struct X64_LinuxAssignParamState {
    u64 stack_spill_size;
    u64 stack_arg_offset;
} X64_LinuxAssignParamState;

static s32 X64_consume_stack_arg(u64* stack_arg_offset, u64 arg_size, u64 arg_align)
{
    s32 offset = (s32)*stack_arg_offset;

    *stack_arg_offset += arg_size;
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, arg_align);
    *stack_arg_offset = ALIGN_UP(*stack_arg_offset, X64_STACK_WORD_SIZE);

    return offset;
}

static s32 X64_linux_spill_reg(X64_Generator* generator, X64_LinuxAssignParamState* state, u64 size, u64 align, X64_Reg preg)
{
    state->stack_spill_size += size;
    state->stack_spill_size = ALIGN_UP(state->stack_spill_size, align);
    s32 offset = -state->stack_spill_size;

    X64_emit_text(generator, "  mov %s [rbp + %d], %s", x64_mem_size_label[size], offset, x64_reg_names[size][preg]);

    return offset;
}

static void X64_linux_assign_proc_param_offsets(X64_Generator* generator, Symbol* sproc, X64_StackParamsInfo* stack_params_info)
{
    DeclProc* dproc = (DeclProc*)sproc->decl;
    Type* ret_type = sproc->type->as_proc.ret;

    u32 index = 0;
    u32 arg_reg_index = 0;
    X64_LinuxAssignParamState state = {.stack_arg_offset = 0x10};

    // For procs that return a large struct by value:
    // Spill the first argument, which contains a pointer to the return value's memory address, into the stack.
    // We need to spill (remember) this address so that the procedure can return it, as per the X64 calling conventions.
    if (type_is_obj_like(ret_type) && X64_linux_is_obj_retarg_large(ret_type->size)) {
        X64_linux_spill_reg(generator, &state, X64_MAX_INT_REG_SIZE, X64_MAX_INT_REG_SIZE, x64_target.arg_regs[arg_reg_index++]);
    }

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

        if (type_is_obj_like(arg_type)) {
            u32 rem_regs = x64_target.num_arg_regs - arg_reg_index;

            if ((arg_size <= X64_MAX_INT_REG_SIZE) && (rem_regs >= 1)) {
                X64_Reg arg_reg = x64_target.arg_regs[arg_reg_index++];
                sym->as_var.offset = X64_linux_spill_reg(generator, &state, X64_MAX_INT_REG_SIZE, arg_align, arg_reg);
            }
            else if ((arg_size <= (X64_MAX_INT_REG_SIZE << 1)) && (rem_regs >= 2)) {
                X64_Reg low_reg = x64_target.arg_regs[arg_reg_index++];
                X64_Reg high_reg = x64_target.arg_regs[arg_reg_index++];

                X64_linux_spill_reg(generator, &state, X64_MAX_INT_REG_SIZE, arg_align, high_reg);
                sym->as_var.offset = X64_linux_spill_reg(generator, &state, X64_MAX_INT_REG_SIZE, arg_align, low_reg);
            }
            else {
                sym->as_var.offset = X64_consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }
        else {
            // Spill argument register below rsp
            if (arg_reg_index < x64_target.num_arg_regs) {
                X64_Reg arg_reg = x64_target.arg_regs[arg_reg_index++];
                sym->as_var.offset = X64_linux_spill_reg(generator, &state, arg_size, arg_align, arg_reg);
            }
            else {
                sym->as_var.offset = X64_consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }

        index += 1;
        it = it->next;
    }

    stack_params_info->stack_spill_size = state.stack_spill_size;
    stack_params_info->local_var_iter = it;
}

static void X64_windows_assign_proc_param_offsets(X64_Generator* generator, Symbol* sproc, X64_StackParamsInfo* stack_params_info)
{
    DeclProc* dproc = (DeclProc*)sproc->decl;
    Type* ret_type = sproc->type->as_proc.ret;

    u32 index = 0;
    u64 stack_arg_offset = X64_STACK_ARG_RBP_OFFSET;

    bool has_dst_arg = type_is_obj_like(ret_type) && X64_windows_is_obj_retarg_large(ret_type->size);

    // For procs that return a large struct by value:
    // Spill the first argument, which contains a pointer to the return value's memory address, into the stack.
    // We need to spill (remember) this address so that the procedure can return it, as per the X64 calling conventions.
    if (has_dst_arg) {
        X64_Reg arg_reg = x64_target.arg_regs[index++];

        X64_emit_text(generator, "  mov %s [rbp + %d], %s", x64_mem_size_label[X64_MAX_INT_REG_SIZE],
                      X64_consume_stack_arg(&stack_arg_offset, X64_MAX_INT_REG_SIZE, X64_MAX_INT_REG_SIZE),
                      x64_reg_names[X64_MAX_INT_REG_SIZE][arg_reg]);
    }

    Scope* scope = dproc->scope;
    List* head = &scope->sym_list;
    List* it = head->next;

    while (it != head) {
        // Only process params. Local variables are not processed here.
        // TODO: Support varargs
        if (index >= (dproc->num_params + has_dst_arg))
            break;

        Symbol* sym = list_entry(it, Symbol, lnode);

        // Assign stack offsets to procedure params.
        assert(sym->kind == SYMBOL_VAR);

        Type* arg_type = sym->type;
        u64 slot_size = arg_type->size;
        u64 slot_align = arg_type->align;

        if (type_is_obj_like(arg_type)) {
            if (X64_windows_is_obj_retarg_large(slot_size)) {
                // NOTE: Passing the object's address!
                slot_size = X64_MAX_INT_REG_SIZE;
                slot_align = X64_MAX_INT_REG_SIZE;

                sym->as_var.is_ptr = true;
                sym->as_var.offset = X64_consume_stack_arg(&stack_arg_offset, slot_size, slot_align);
            }
            else {
                sym->as_var.is_ptr = false;
                sym->as_var.offset = X64_consume_stack_arg(&stack_arg_offset, slot_size, slot_align);
            }
        }
        else {
            sym->as_var.is_ptr = false;
            sym->as_var.offset = X64_consume_stack_arg(&stack_arg_offset, slot_size, slot_align);
        }

        assert(slot_size <= X64_MAX_INT_REG_SIZE);

        // Spill argument register to the shadow space (32 bytes above return address)
        // Only the first four arguments can be in a register.
        if (index < x64_target.num_arg_regs) {
            X64_Reg arg_reg = x64_target.arg_regs[index];

            X64_emit_text(generator, "  mov %s [rbp + %d], %s", x64_mem_size_label[slot_size], sym->as_var.offset,
                          x64_reg_names[slot_size][arg_reg]);
        }

        index += 1;
        it = it->next;
    }

    stack_params_info->stack_spill_size = 0; // Did not spill below rsp; all args are above return address
    stack_params_info->local_var_iter = it;
}

static void X64_assign_proc_param_offsets(X64_Generator* generator, Symbol* sproc, X64_StackParamsInfo* stack_params_info)
{
    if (x64_target.os == OS_LINUX) {
        X64_linux_assign_proc_param_offsets(generator, sproc, stack_params_info);
    }
    else {
        assert(x64_target.os == OS_WIN32);
        X64_windows_assign_proc_param_offsets(generator, sproc, stack_params_info);
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
    // Sum sizes of anonymous objects in this scope.
    //
    {
        List* head = &scope->obj_list;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

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

static u64 X64_assign_proc_stack_offsets(X64_Generator* generator, Symbol* sproc)
{
    DeclProc* dproc = (DeclProc*)sproc->decl;
    Scope* scope = dproc->scope;

    //
    // Spill procedure params into the stack (assign stack offsets to params).
    //

    X64_StackParamsInfo stack_params_info = {0};
    X64_assign_proc_param_offsets(generator, sproc, &stack_params_info);

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
    // Sum sizes of `TEMPORARY` anonymous objects in the procedure's top scope.
    //
    {
        List* head = &sproc->as_proc.tmp_objs;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

            it = it->next;
        }
    }

    //
    // Sum sizes of anonymous objects in the procedure's top scope.
    //
    {
        List* head = &scope->obj_list;
        List* it = head->next;

        while (it != head) {
            AnonObj* obj = list_entry(it, AnonObj, lnode);

            stack_size += obj->size;
            stack_size = ALIGN_UP(stack_size, obj->align);
            obj->offset = -stack_size;

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

static u32 X64_get_sibd_addr(X64_Generator* generator, X64_SIBDAddr* sibd_addr, X64_MemAddr* vaddr)
{
    u32 used_regs = 0;

    if (vaddr->kind == X64_ADDR_GLOBAL_SYM) {
        sibd_addr->kind = X64_SIBD_ADDR_GLOBAL;
        sibd_addr->global = vaddr->global;
    }
    else if (vaddr->kind == X64_ADDR_STR_LIT) {
        sibd_addr->kind = X64_SIBD_ADDR_STR_LIT;
        sibd_addr->str_lit = vaddr->str_lit;
    }
    else if (vaddr->kind == X64_ADDR_FLOAT_LIT) {
        sibd_addr->kind = X64_SIBD_ADDR_FLOAT_LIT;
        sibd_addr->float_lit = vaddr->float_lit;
    }
    else {
        assert(vaddr->kind == X64_ADDR_SIBD);
        bool has_base = vaddr->sibd.base_reg != (u32)-1;
        bool has_index = vaddr->sibd.scale && (vaddr->sibd.index_reg != (u32)-1);
        assert(has_base || has_index);

        sibd_addr->kind = X64_SIBD_ADDR_LOCAL;
        sibd_addr->local.disp = vaddr->sibd.disp;
        sibd_addr->local.scale = vaddr->sibd.scale;

        if (has_base) {
            X64_LRegLoc base_loc = X64_lreg_loc(generator, vaddr->sibd.base_reg);
            assert(base_loc.kind == X64_LREG_LOC_REG);

            sibd_addr->local.base_reg = base_loc.reg;
            u32_set_bit(&used_regs, base_loc.reg);

            if (has_index) {
                X64_LRegLoc index_loc = X64_lreg_loc(generator, vaddr->sibd.index_reg);
                assert(index_loc.kind == X64_LREG_LOC_REG);

                sibd_addr->local.index_reg = index_loc.reg;
                u32_set_bit(&used_regs, index_loc.reg);
            }
            else {
                sibd_addr->local.index_reg = X64_REG_COUNT;
            }
        }
        else {
            X64_LRegLoc index_loc = X64_lreg_loc(generator, vaddr->sibd.index_reg);
            assert(index_loc.kind == X64_LREG_LOC_REG);

            sibd_addr->local.base_reg = X64_REG_COUNT;
            sibd_addr->local.index_reg = index_loc.reg;
            u32_set_bit(&used_regs, index_loc.reg);
        }
    }

    return used_regs;
}

static char* X64_print_sibd_addr(Allocator* allocator, X64_SIBDAddr* addr, u32 mem_label_size)
{
    assert(mem_label_size <= X64_MAX_INT_REG_SIZE);
    char* dstr = array_create(allocator, char, 16);
    const char* mem_label = mem_label_size ? x64_mem_size_label[mem_label_size] : "";

    if (addr->kind == X64_SIBD_ADDR_STR_LIT) {
        ftprint_char_array(&dstr, true, "[rel %s_%llu]", X64_STR_LIT_PRE, addr->str_lit->id);
    }
    else if (addr->kind == X64_SIBD_ADDR_FLOAT_LIT) {
        ftprint_char_array(&dstr, true, "[rel %s_%llu]", X64_FLOAT_LIT_PRE, addr->float_lit->id);
    }
    else if (addr->kind == X64_SIBD_ADDR_GLOBAL) {
        ftprint_char_array(&dstr, true, "%s [rel %s]", mem_label, symbol_mangled_name(allocator, addr->global));
    }
    else {
        assert(addr->kind == X64_SIBD_ADDR_LOCAL);
        bool has_base = addr->local.base_reg < X64_REG_COUNT;
        bool has_index = addr->local.scale && (addr->local.index_reg < X64_REG_COUNT);
        bool has_disp = addr->local.disp != 0;

        if (has_base) {
            const char* base_reg_name = x64_reg_names[X64_MAX_INT_REG_SIZE][addr->local.base_reg];

            if (has_index) {
                const char* index_reg_name = x64_reg_names[X64_MAX_INT_REG_SIZE][addr->local.index_reg];

                if (has_disp)
                    ftprint_char_array(&dstr, false, "%s [%s + %d*%s + %d]", mem_label, base_reg_name, addr->local.scale,
                                       index_reg_name, (s32)addr->local.disp);
                else
                    ftprint_char_array(&dstr, false, "%s [%s + %d*%s]", mem_label, base_reg_name, addr->local.scale, index_reg_name);
            }
            else {
                if (has_disp)
                    ftprint_char_array(&dstr, false, "%s [%s + %d]", mem_label, base_reg_name, (s32)addr->local.disp);
                else
                    ftprint_char_array(&dstr, false, "%s [%s]", mem_label, base_reg_name);
            }
        }
        else {
            const char* index_reg_name = x64_reg_names[X64_MAX_INT_REG_SIZE][addr->local.index_reg];

            if (addr->local.disp)
                ftprint_char_array(&dstr, false, "%s [%d*%s + %d]", mem_label, addr->local.scale, index_reg_name,
                                   (s32)addr->local.disp);
            else
                ftprint_char_array(&dstr, false, "%s [%d*%s]", mem_label, addr->local.scale, index_reg_name);
        }

        array_push(dstr, '\0');
    }

    return dstr;
}

static size_t X64_cpy_reg_to_mem(X64_Generator* generator, X64_SIBDAddr* dst, X64_Reg src, size_t size)
{
    static char pow2_sizes[8] = {
        [1] = 1, [2] = 2, [3] = 2, [4] = 4, [5] = 4, [6] = 4, [7] = 4,
    };

    size_t rem_amnt = size;

    // If need to copy more than 8 bytes, just copy entire register into memory, and then return.
    if (rem_amnt >= X64_MAX_INT_REG_SIZE) {
        X64_emit_text(generator, "  mov %s, %s", X64_print_sibd_addr(generator->tmp_mem, dst, X64_MAX_INT_REG_SIZE),
                      x64_reg_names[X64_MAX_INT_REG_SIZE][src]);

        // Move dst addr forward.
        dst->local.disp += X64_MAX_INT_REG_SIZE;

        return rem_amnt - X64_MAX_INT_REG_SIZE;
    }

    // Have to copy less than 8 bytes. Copy in chunks of powers-of-two.
    assert(rem_amnt < X64_MAX_INT_REG_SIZE);

    while (rem_amnt) {
        // Calc the largest power of 2 that is less than or equal to min(8, rem_amnt).
        size_t n = pow2_sizes[rem_amnt];

        // Copy that amount into memory.
        X64_emit_text(generator, "  mov %s, %s", X64_print_sibd_addr(generator->tmp_mem, dst, n), x64_reg_names[n][src]);

        // Move dst addr forward.
        dst->local.disp += n;

        size_t new_rem_amnt = rem_amnt - n;

        // Shift src register right to discard copied bits.
        if (new_rem_amnt) {
            X64_emit_text(generator, "  sar %s, %d", x64_reg_names[X64_MAX_INT_REG_SIZE][src], n << 3);
        }

        rem_amnt = new_rem_amnt;
    }

    return rem_amnt;
}

static void X64_emit_rr_instr(X64_Generator* generator, const char* instr, bool writes_op1, u32 op1_size, u32 op1_lreg, u32 op2_size,
                              u32 op2_lreg)
{
    X64_LRegLoc op1_loc = X64_lreg_loc(generator, op1_lreg);
    X64_LRegLoc op2_loc = X64_lreg_loc(generator, op2_lreg);

    switch (op1_loc.kind) {
    case X64_LREG_LOC_REG: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG:
            X64_emit_text(generator, "  %s %s, %s", instr, x64_reg_names[op1_size][op1_loc.reg], x64_reg_names[op2_size][op2_loc.reg]);
            break;
        case X64_LREG_LOC_STACK:
            X64_emit_text(generator, "  %s %s, %s", instr, x64_reg_names[op1_size][op1_loc.reg],
                          X64_print_stack_offset(generator->tmp_mem, op2_loc.offset, op2_size));
            break;
        default:
            assert(0);
            break;
        }
        break;
    }
    case X64_LREG_LOC_STACK: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG:
            X64_emit_text(generator, "  %s %s, %s", instr, X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size),
                          x64_reg_names[op2_size][op2_loc.reg]);
            break;
        case X64_LREG_LOC_STACK: {
            const char* op1_op_str = X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size);
            const char* op2_op_str = X64_print_stack_offset(generator->tmp_mem, op2_loc.offset, op2_size);
            const char* tmp_reg_str = x64_reg_names[op1_size][X64_RAX];
            const char* tmp_reg_str_lg = x64_reg_names[X64_MAX_INT_REG_SIZE][X64_RAX]; // Can only push 64bit vals into stack.

            // Save the contents of a temporary register into the stack.
            X64_emit_text(generator, "  push %s", tmp_reg_str_lg);

            // Load dst into the temporary register,
            X64_emit_text(generator, "  mov %s, %s", tmp_reg_str, op1_op_str);

            // Execute the instruction using the temporary register as the destination.
            X64_emit_text(generator, "  %s %s, %s", instr, tmp_reg_str, op2_op_str);

            // Store the result of the instruction (contents of temporary register) into dst.
            if (writes_op1) {
                X64_emit_text(generator, "  mov %s, %s", op1_op_str, tmp_reg_str);
            }

            // Restore the contents of the temporary register.
            X64_emit_text(generator, "  pop %s", tmp_reg_str_lg);

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

static void X64_emit_ri_instr(X64_Generator* generator, const char* instr, u32 op1_size, u32 op1_lreg, u32 op2_size, Scalar op2_imm)
{
    X64_LRegLoc op1_loc = X64_lreg_loc(generator, op1_lreg);

    switch (op1_loc.kind) {
    case X64_LREG_LOC_REG: {
        X64_emit_text(generator, "  %s %s, %s", instr, x64_reg_names[op1_size][op1_loc.reg],
                      X64_print_imm(generator->tmp_mem, op2_imm, op2_size));
        break;
    }
    case X64_LREG_LOC_STACK: {
        X64_emit_text(generator, "  %s %s, %s", instr, X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size),
                      X64_print_imm(generator->tmp_mem, op2_imm, op2_size));
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64_emit_rm_instr(X64_Generator* generator, const char* instr, bool writes_op1, u32 op1_size, u32 op1_lreg, u32 op2_size,
                              X64_MemAddr* op2_vaddr)
{
    X64_SIBDAddr op2_addr = {0};
    u32 used_regs = X64_get_sibd_addr(generator, &op2_addr, op2_vaddr);

    X64_RegGroup tmp_group = X64_begin_reg_group(generator);
    X64_Reg op1_reg = X64_get_reg(&tmp_group, op1_lreg, op1_size, writes_op1, used_regs);
    assert(op1_size <= 8 && IS_POW2(op1_size));

    X64_emit_text(generator, "  %s %s, %s", instr, x64_reg_names[op1_size][op1_reg],
                  X64_print_sibd_addr(generator->tmp_mem, &op2_addr, op2_size));

    X64_end_reg_group(&tmp_group);
}

static void X64_emit_mr_instr(X64_Generator* generator, const char* instr, u32 op1_size, X64_MemAddr* op1_vaddr, u32 op2_size,
                              u32 op2_lreg)
{
    X64_SIBDAddr op1_addr = {0};
    u32 used_regs = X64_get_sibd_addr(generator, &op1_addr, op1_vaddr);

    X64_RegGroup tmp_group = X64_begin_reg_group(generator);
    X64_Reg op2_reg = X64_get_reg(&tmp_group, op2_lreg, op2_size, false, used_regs);
    assert(op2_size <= 8 && IS_POW2(op2_size));

    X64_emit_text(generator, "  %s %s, %s", instr, X64_print_sibd_addr(generator->tmp_mem, &op1_addr, op1_size),
                  x64_reg_names[op2_size][op2_reg]);

    X64_end_reg_group(&tmp_group);
}

static void X64_emit_mi_instr(X64_Generator* generator, const char* instr, u32 op1_size, X64_MemAddr* op1_vaddr, u32 op2_size,
                              Scalar op2_imm)
{
    X64_SIBDAddr op1_addr = {0};
    X64_get_sibd_addr(generator, &op1_addr, op1_vaddr);

    X64_emit_text(generator, "  %s %s, %s", instr, X64_print_sibd_addr(generator->tmp_mem, &op1_addr, op1_size),
                  X64_print_imm(generator->tmp_mem, op2_imm, op2_size));
}

static void X64_place_args_in_regs(X64_Generator* generator, u32 num_args, X64_InstrCallArg* args)
{
    for (u32 i = 0; i < num_args; i++) {
        X64_InstrCallArg* arg = args + i;
        size_t arg_size = arg->type->size;

        if (type_is_obj_like(arg->type)) { // Argument is a struct/union/array object.
            X64_ObjArgSlot* slot = &arg->slot.obj;

            if (!slot->num_regs) {
                continue;
            }

            // Move object address into the appropriate argument register.
            if (slot->as_ptr) {
                assert(slot->num_regs == 1);
                X64_emit_text(generator, "  lea %s, [rsp + %d]", x64_reg_names[X64_MAX_INT_REG_SIZE][slot->pregs[0]],
                              slot->ptr_sp_offset);
            }
            // Copy 64-bit chunks of the struct object into the appropriate argument registers.
            else {
                X64_SIBDAddr addr = {0};
                X64_get_sibd_addr(generator, &addr, &arg->val.addr);

                assert(addr.kind == X64_SIBD_ADDR_LOCAL);

                size_t copy_amnt = 0;

                for (unsigned ii = 0; ii < slot->num_regs; ii++) {
                    X64_emit_text(generator, "  mov %s, %s", x64_reg_names[X64_MAX_INT_REG_SIZE][slot->pregs[ii]],
                                  X64_print_sibd_addr(generator->tmp_mem, &addr, X64_MAX_INT_REG_SIZE));
                    addr.local.disp += X64_MAX_INT_REG_SIZE;
                    copy_amnt += X64_MAX_INT_REG_SIZE;
                }
            }
        }
        else { // Argument is a primitive type
            X64_PrimArgSlot* slot = &arg->slot.prim;

            if (!slot->in_reg) {
                continue;
            }

            X64_LRegLoc loc = X64_lreg_loc(generator, arg->val.reg);

            if (loc.kind == X64_LREG_LOC_STACK) {
                assert(slot->preg < X64_REG_COUNT);
                X64_emit_text(generator, "  mov %s, %s", x64_reg_names[arg_size][slot->preg],
                              X64_print_stack_offset(generator->tmp_mem, loc.offset, arg_size));
            }
            else {
                assert(loc.kind == X64_LREG_LOC_REG);
                assert(loc.reg == slot->preg);
            }
        }
    }
}

static void X64_linux_place_struct_args_in_stack(X64_Generator* generator, u32 num_args, X64_InstrCallArg* args)
{
    bool pushed_cpy_state = false;

    for (u32 i = 0; i < num_args; i += 1) {
        X64_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;

        if (type_is_obj_like(arg->type)) {
            // Argument is a struct/union/array object.
            X64_ObjArgSlot* slot = &arg->slot.obj;

            assert(!slot->as_ptr);

            if (slot->num_regs) {
                continue;
            }

            // TODO: There's no need to push all (rdi, rsi, rcx) if not used.
            if (!pushed_cpy_state) {
                X64_emit_text(generator, "  push rdi");
                X64_emit_text(generator, "  push rsi");
                X64_emit_text(generator, "  push rcx");
                pushed_cpy_state = true;
            }

            const u32 sp_begin = X64_MAX_INT_REG_SIZE * 3;

            // Copy obj into its location in the stack.
            X64_SIBDAddr src_addr = {0};
            X64_get_sibd_addr(generator, &src_addr, &arg->val.addr);
            assert(src_addr.kind == X64_SIBD_ADDR_LOCAL);

            X64_emit_text(generator, "  lea rdi, [rsp + %d]", slot->sp_offset + sp_begin);
            X64_emit_text(generator, "  lea rsi, %s", X64_print_sibd_addr(generator->tmp_mem, &src_addr, 0));
            X64_emit_text(generator, "  mov rcx, 0x%lx", arg_size);
            X64_emit_text(generator, "  rep movsb");
        }
    }

    if (pushed_cpy_state) {
        X64_emit_text(generator, "  pop rcx");
        X64_emit_text(generator, "  pop rsi");
        X64_emit_text(generator, "  pop rdi");
    }
}

static void X64_windows_place_struct_args_in_stack(X64_Generator* generator, u32 num_args, X64_InstrCallArg* args)
{
    bool pushed_cpy_state = false;

    for (u32 i = 0; i < num_args; i += 1) {
        X64_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;

        if (type_is_obj_like(arg->type)) {
            // Argument is a struct/union/array object.
            X64_ObjArgSlot* slot = &arg->slot.obj;

            if (!slot->as_ptr) {
                continue;
            }

            assert(X64_windows_is_obj_retarg_large(arg_size));

            // TODO: There's no need to push all (rdi, rsi, rcx) if not used.
            if (!pushed_cpy_state) {
                X64_emit_text(generator, "  push rdi");
                X64_emit_text(generator, "  push rsi");
                X64_emit_text(generator, "  push rcx");
                pushed_cpy_state = true;
            }

            const u32 sp_begin = X64_MAX_INT_REG_SIZE * 3;

            // Copy obj into its location in the stack.
            X64_SIBDAddr src_addr = {0};
            X64_get_sibd_addr(generator, &src_addr, &arg->val.addr);
            assert(src_addr.kind == X64_SIBD_ADDR_LOCAL);

            X64_emit_text(generator, "  lea rdi, [rsp + %d]", slot->ptr_sp_offset + sp_begin);

            // Move object pointer into stack slot.
            if (!slot->num_regs) {
                X64_emit_text(generator, "  mov %s [rsp + %d], rdi", x64_mem_size_label[X64_MAX_INT_REG_SIZE],
                              slot->sp_offset + sp_begin);
            }

            X64_emit_text(generator, "  lea rsi, %s", X64_print_sibd_addr(generator->tmp_mem, &src_addr, 0));
            X64_emit_text(generator, "  mov rcx, 0x%lx", arg_size);
            X64_emit_text(generator, "  rep movsb");
        }
    }

    if (pushed_cpy_state) {
        X64_emit_text(generator, "  pop rcx");
        X64_emit_text(generator, "  pop rsi");
        X64_emit_text(generator, "  pop rdi");
    }
}

static void X64_place_args_in_stack(X64_Generator* generator, u32 num_args, X64_InstrCallArg* args)
{
    // 1st pass: Copy struct arguments into the stack.
    if (x64_target.os == OS_LINUX) {
        X64_linux_place_struct_args_in_stack(generator, num_args, args);
    }
    else {
        assert(x64_target.os == OS_WIN32);
        X64_windows_place_struct_args_in_stack(generator, num_args, args);
    }

    // 2nd pass: Copy primitive args that are currently in registers into their stack slots.
    // This ensures that we can freely use RAX as a temporary register in the next pass.
    for (u32 i = 0; i < num_args; i += 1) {
        X64_InstrCallArg* arg = args + i;
        u64 arg_size = arg->type->size;

        if (!type_is_obj_like(arg->type)) {
            X64_PrimArgSlot* slot = &arg->slot.prim;

            if (slot->in_reg) {
                continue; // Skip register args
            }

            X64_LRegLoc loc = X64_lreg_loc(generator, arg->val.reg);

            // Move directly into stack slot.
            if (loc.kind == X64_LREG_LOC_REG) {
                X64_emit_text(generator, "  mov %s [rsp + %d], %s", x64_mem_size_label[arg_size], slot->sp_offset,
                              x64_reg_names[arg_size][loc.reg]);
            }
        }
    }

    // 3rd pass: Copy primitive args that are currently spilled into the stack frame.
    for (u32 i = 0; i < num_args; i += 1) {
        X64_InstrCallArg* arg = args + i;

        if (type_is_obj_like(arg->type)) {
            continue;
        }

        X64_PrimArgSlot* slot = &arg->slot.prim;

        if (slot->in_reg) {
            continue; // Skip register args
        }

        u64 arg_size = arg->type->size;
        X64_LRegLoc loc = X64_lreg_loc(generator, arg->val.reg);

        if (loc.kind == X64_LREG_LOC_STACK) {
            // Move into RAX.
            X64_emit_text(generator, "  mov %s, %s", x64_reg_names[arg_size][X64_RAX],
                          X64_print_stack_offset(generator->tmp_mem, loc.offset, arg_size));

            // Move RAX into stack slot.
            X64_emit_text(generator, "  mov %s [rsp + %d], %s", x64_mem_size_label[arg_size], slot->sp_offset,
                          x64_reg_names[arg_size][X64_RAX]);
        }
    }
}

static void X64_linux_cpy_ret_small_obj(X64_Generator* generator, Type* ret_type, X64_CallValue* dst_val)
{
    // Procedure returned a small struct/union/array object in registers.
    // Copy into appropriate memory location.
    if (!X64_linux_is_obj_retarg_large(ret_type->size)) {
        X64_SIBDAddr obj_addr = {0};
        X64_get_sibd_addr(generator, &obj_addr, &dst_val->addr);

        // Copy RAX into the first 8 bytes of struct memory.
        size_t rem_amnt = X64_cpy_reg_to_mem(generator, &obj_addr, X64_RAX, ret_type->size);

        // Copy RDX into the second 8 bytes of struct memory.
        if (rem_amnt) {
            rem_amnt = X64_cpy_reg_to_mem(generator, &obj_addr, X64_RDX, rem_amnt);
            assert(!rem_amnt);
        }
    }
}

static void X64_windows_cpy_ret_small_obj(X64_Generator* generator, Type* ret_type, X64_CallValue* dst_val)
{
    // Procedure returned a small struct/union/array object in RAX.
    // Copy into appropriate memory location.
    if (!X64_windows_is_obj_retarg_large(ret_type->size)) {
        X64_SIBDAddr obj_addr = {0};
        X64_get_sibd_addr(generator, &obj_addr, &dst_val->addr);

        // Copy RAX into the first 8 bytes of struct memory.
        size_t rem_amnt = X64_cpy_reg_to_mem(generator, &obj_addr, X64_RAX, ret_type->size);
        assert(!rem_amnt);
    }
}

static void X64_gen_instr(X64_Generator* generator, X64_Instr* instr, bool last_instr, long bblock_id)
{
    static const char* binary_r_r_name[] = {[X64_INSTR_ADD_R_R] = "add", [X64_INSTR_SUB_R_R] = "sub", [X64_INSTR_IMUL_R_R] = "imul",
                                            [X64_INSTR_AND_R_R] = "and", [X64_INSTR_OR_R_R] = "or",   [X64_INSTR_XOR_R_R] = "xor"};

    static const char* binary_r_i_name[] = {[X64_INSTR_ADD_R_I] = "add", [X64_INSTR_SUB_R_I] = "sub", [X64_INSTR_IMUL_R_I] = "imul",
                                            [X64_INSTR_AND_R_I] = "and", [X64_INSTR_OR_R_I] = "or",   [X64_INSTR_XOR_R_I] = "xor"};

    static const char* binary_r_m_name[] = {[X64_INSTR_ADD_R_M] = "add", [X64_INSTR_SUB_R_M] = "sub", [X64_INSTR_IMUL_R_M] = "imul",
                                            [X64_INSTR_AND_R_M] = "and", [X64_INSTR_OR_R_M] = "or",   [X64_INSTR_XOR_R_M] = "xor"};

    static const char* shift_r_r_name[] = {[X64_INSTR_SAR_R_R] = "sar", [X64_INSTR_SHL_R_R] = "shl"};

    static const char* shift_r_i_name[] = {[X64_INSTR_SAR_R_I] = "sar", [X64_INSTR_SHL_R_I] = "shl"};

    static const char* unary_name[] = {[X64_INSTR_NEG] = "neg", [X64_INSTR_NOT] = "not"};

    AllocatorState mem_state = allocator_get_state(generator->tmp_mem);

    switch (instr->kind) {
    case X64_INSTR_ADD_R_R:
    case X64_INSTR_SUB_R_R:
    case X64_INSTR_IMUL_R_R:
    case X64_INSTR_AND_R_R:
    case X64_INSTR_OR_R_R:
    case X64_INSTR_XOR_R_R: {
        u32 size = (u32)instr->binary_r_r.size;

        X64_emit_rr_instr(generator, binary_r_r_name[instr->kind], true, size, instr->binary_r_r.dst, size, instr->binary_r_r.src);
        break;
    }
    case X64_INSTR_ADD_R_I:
    case X64_INSTR_SUB_R_I:
    case X64_INSTR_IMUL_R_I:
    case X64_INSTR_AND_R_I:
    case X64_INSTR_OR_R_I:
    case X64_INSTR_XOR_R_I: {
        u32 size = (u32)instr->binary_r_i.size;

        X64_emit_ri_instr(generator, binary_r_i_name[instr->kind], size, instr->binary_r_i.dst, size, instr->binary_r_i.src);
        break;
    }
    case X64_INSTR_ADD_R_M:
    case X64_INSTR_SUB_R_M:
    case X64_INSTR_IMUL_R_M:
    case X64_INSTR_AND_R_M:
    case X64_INSTR_OR_R_M:
    case X64_INSTR_XOR_R_M: {
        u32 size = (u32)instr->binary_r_m.size;

        X64_emit_rm_instr(generator, binary_r_m_name[instr->kind], true, size, instr->binary_r_m.dst, size, &instr->binary_r_m.src);
        break;
    }
    case X64_INSTR_DIV_R:
    case X64_INSTR_IDIV_R: {
        const char* instr_name = instr->kind == X64_INSTR_IDIV_R ? "idiv" : "div";
        u32 size = (u32)instr->div_r.size;

        X64_LRegLoc src_loc = X64_lreg_loc(generator, instr->div_r.src);
        bool src_is_reg = src_loc.kind == X64_LREG_LOC_REG;
        const char* src_op_str =
            src_is_reg ? x64_reg_names[size][src_loc.reg] : X64_print_stack_offset(generator->tmp_mem, src_loc.offset, size);

        X64_emit_text(generator, "  %s %s", instr_name, src_op_str);

        break;
    }
    case X64_INSTR_DIV_M:
    case X64_INSTR_IDIV_M: {
        const char* instr_name = instr->kind == X64_INSTR_IDIV_M ? "idiv" : "div";
        u32 size = (u32)instr->div_m.size;
        X64_SIBDAddr op_addr = {0};

        X64_get_sibd_addr(generator, &op_addr, &instr->div_m.src);
        X64_emit_text(generator, "  %s %s", instr_name, X64_print_sibd_addr(generator->tmp_mem, &op_addr, size));

        break;
    }
    case X64_INSTR_SEXT_AX_TO_DX: {
        u32 size = (u32)instr->sext_ax_to_dx.size;
        X64_emit_text(generator, "  %s", x64_sext_ax_into_dx[size]);
        break;
    }
    case X64_INSTR_SAR_R_R:
    case X64_INSTR_SHL_R_R: {
        u32 dst_size = (u32)instr->shift_r_r.size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, instr->shift_r_r.dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, instr->shift_r_r.src);
        bool dst_in_reg = (dst_loc.kind == X64_LREG_LOC_REG);

        assert(src_loc.kind == X64_LREG_LOC_REG && src_loc.reg == X64_RCX);

        const char* dst_op_str =
            dst_in_reg ? x64_reg_names[dst_size][dst_loc.reg] : X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, dst_size);

        X64_emit_text(generator, "  %s %s, %s", shift_r_r_name[instr->kind], dst_op_str, x64_reg_names[1][X64_RCX]);
        break;
    }
    case X64_INSTR_SAR_R_I:
    case X64_INSTR_SHL_R_I: {
        u32 dst_size = (u32)instr->shift_r_i.size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, instr->shift_r_i.dst);
        bool dst_in_reg = (dst_loc.kind == X64_LREG_LOC_REG);
        const char* dst_op_str =
            dst_in_reg ? x64_reg_names[dst_size][dst_loc.reg] : X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, dst_size);

        X64_emit_text(generator, "  %s %s, %d", shift_r_i_name[instr->kind], dst_op_str, instr->shift_r_i.src.as_int._u8);
        break;
    }
    case X64_INSTR_NEG:
    case X64_INSTR_NOT: {
        u32 size = (u32)instr->unary.size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, instr->unary.dst);
        bool dst_in_reg = (dst_loc.kind == X64_LREG_LOC_REG);
        const char* dst_op_str =
            dst_in_reg ? x64_reg_names[size][dst_loc.reg] : X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, size);

        X64_emit_text(generator, "  %s %s", unary_name[instr->kind], dst_op_str);
        break;
    }
    case X64_INSTR_REP_MOVSB: {
        X64_emit_text(generator, "  rep movsb");
        break;
    }
    case X64_INSTR_REP_STOSB: {
        X64_emit_text(generator, "  rep stosb");
        break;
    }
    case X64_INSTR_MOV_R_RH: {
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, instr->mov_r_rh.dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, instr->mov_r_rh.src);

        assert(src_loc.kind == X64_LREG_LOC_REG);

        const char* src_h_name = x64_reg_h_names[src_loc.reg];

        assert(src_h_name);

        switch (dst_loc.kind) {
        case X64_LREG_LOC_REG: {
            X64_emit_text(generator, "  mov %s, %s", x64_reg_names[1][dst_loc.reg], src_h_name);
            break;
        }
        case X64_LREG_LOC_STACK: {
            X64_emit_text(generator, "  mov %s, %s", X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, 1), src_h_name);
            break;
        }
        default:
            NIBBLE_FATAL_EXIT("Invalid dst_loc.kind in X64_INSTR_MOV_R_RH generation.");
            break;
        }
        break;
    }
    case X64_INSTR_MOV_R_R: {
        u32 size = (u32)instr->mov_r_r.size;

        X64_LRegLoc dst_loc = X64_lreg_loc(generator, instr->mov_r_r.dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, instr->mov_r_r.src);

        bool same_ops =
            (dst_loc.kind == src_loc.kind) && (((dst_loc.kind == X64_LREG_LOC_REG) && (dst_loc.reg == src_loc.reg)) ||
                                               ((dst_loc.kind == X64_LREG_LOC_STACK) && (dst_loc.offset == src_loc.offset)));

        if (!same_ops) {
            X64_emit_rr_instr(generator, "mov", true, size, instr->mov_r_r.dst, size, instr->mov_r_r.src);
        }
        break;
    }
    case X64_INSTR_MOV_R_I: {
        u32 size = (u32)instr->mov_r_i.size;

        X64_emit_ri_instr(generator, "mov", size, instr->mov_r_i.dst, size, instr->mov_r_i.src);
        break;
    }
    case X64_INSTR_MOV_R_M: {
        u32 size = (u32)instr->mov_r_m.size;

        X64_emit_rm_instr(generator, "mov", true, size, instr->mov_r_m.dst, size, &instr->mov_r_m.src);
        break;
    }
    case X64_INSTR_MOV_M_R: {
        u32 size = (u32)instr->mov_m_r.size;

        X64_emit_mr_instr(generator, "mov", size, &instr->mov_m_r.dst, size, instr->mov_m_r.src);
        break;
    }
    case X64_INSTR_MOV_M_I: {
        u32 size = (u32)instr->mov_m_i.size;

        X64_emit_mi_instr(generator, "mov", size, &instr->mov_m_i.dst, size, instr->mov_m_i.src);
        break;
    }
    case X64_INSTR_MOVZX_R_R: {
        size_t dst_size = instr->convert_r_r.dst_size;
        size_t src_size = instr->convert_r_r.src_size;

        // There is no encoding for an instruction that zero-extends a 4-byte source to an 8-byte destination! Also, note that
        // an instruction like mov eax, __ clears the upper 4 bytes of eax.
        // See: https://stackoverflow.com/a/51394642
        if (src_size != 4) {
            X64_emit_rr_instr(generator, "movzx", true, dst_size, instr->convert_r_r.dst, src_size, instr->convert_r_r.src);
        }
        // EX: Instead of movzx rax, edi (invalid), use mov eax, edi to zero-extend edi into rax.
        else {
            assert(dst_size == X64_MAX_INT_REG_SIZE);

            // NOTE: Not necessary if a previous instruction already cleared the upper 4-bytes of the dest reg with a mov instruction.
            // We would need to track the "zxt" state of all registers: if mov rx, _ => rx is "zxt", otherwise if <not_mov> rx, _ =>
            // rx is NOT "zxt".
            X64_emit_rr_instr(generator, "mov", true, 4, instr->convert_r_r.dst, 4, instr->convert_r_r.src);
        }
        break;
    }
    case X64_INSTR_MOVSX_R_R: {
        size_t dst_size = instr->convert_r_r.dst_size;
        size_t src_size = instr->convert_r_r.src_size;
        const char* movsx = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? "movsxd" : "movsx";

        X64_emit_rr_instr(generator, movsx, true, dst_size, instr->convert_r_r.dst, src_size, instr->convert_r_r.src);
        break;
    }
    case X64_INSTR_MOVSS_R_M: {
        X64_SIBDAddr src = {0};
        X64_get_sibd_addr(generator, &src, &instr->movfp_r_m.src);

        X64_LRegLoc dst_loc = X64_lreg_loc(generator, instr->movfp_r_m.dst);

        // TODO: Handle case when dst is not allocated into an xmm register.
        // Modify X64_get_reg() and X64_emit_rm_instr() to handle floating-point class of registers.
        assert(dst_loc.kind == X64_LREG_LOC_REG);

        X64_emit_text(generator, "  movss %s, %s", x64_fp_reg_names[dst_loc.reg],
                      X64_print_sibd_addr(generator->tmp_mem, &src, float_kind_sizes[FLOAT_F32]));

        break;
    }
    case X64_INSTR_MOVSD_R_M: {
        X64_SIBDAddr src = {0};
        X64_get_sibd_addr(generator, &src, &instr->movfp_r_m.src);

        X64_LRegLoc dst_loc = X64_lreg_loc(generator, instr->movfp_r_m.dst);

        // TODO: Handle case when dst is not allocated into an xmm register.
        // Modify X64_get_reg() and X64_emit_rm_instr() to handle floating-point class of registers.
        assert(dst_loc.kind == X64_LREG_LOC_REG);

        X64_emit_text(generator, "  movsd %s, %s", x64_fp_reg_names[dst_loc.reg],
                      X64_print_sibd_addr(generator->tmp_mem, &src, float_kind_sizes[FLOAT_F64]));

        break;
    }
    case X64_INSTR_MOVSS_M_R: {
        X64_SIBDAddr dst = {0};
        X64_get_sibd_addr(generator, &dst, &instr->movfp_m_r.dst);

        X64_LRegLoc src_loc = X64_lreg_loc(generator, instr->movfp_m_r.src);

        // TODO: Handle case when src is not allocated into an xmm register.
        // Modify X64_get_reg() and X64_emit_rm_instr() to handle floating-point class of registers.
        assert(src_loc.kind == X64_LREG_LOC_REG);

        X64_emit_text(generator, "  movss %s, %s", X64_print_sibd_addr(generator->tmp_mem, &dst, float_kind_sizes[FLOAT_F32]),
                      x64_fp_reg_names[src_loc.reg]);

        break;
    }
    case X64_INSTR_MOVSD_M_R: {
        X64_SIBDAddr dst = {0};
        X64_get_sibd_addr(generator, &dst, &instr->movfp_m_r.dst);

        X64_LRegLoc src_loc = X64_lreg_loc(generator, instr->movfp_m_r.src);

        // TODO: Handle case when src is not allocated into an xmm register.
        // Modify X64_get_reg() and X64_emit_rm_instr() to handle floating-point class of registers.
        assert(src_loc.kind == X64_LREG_LOC_REG);

        X64_emit_text(generator, "  movsd %s, %s", X64_print_sibd_addr(generator->tmp_mem, &dst, float_kind_sizes[FLOAT_F64]),
                      x64_fp_reg_names[src_loc.reg]);

        break;
    }
    case X64_INSTR_LEA: {
        X64_emit_rm_instr(generator, "lea", true, X64_MAX_INT_REG_SIZE, instr->lea.dst, 0, &instr->lea.mem);
        break;
    }
    case X64_INSTR_CMP_R_R: {
        u32 size = (u32)instr->cmp_r_r.size;

        X64_emit_rr_instr(generator, "cmp", false, size, instr->cmp_r_r.op1, size, instr->cmp_r_r.op2);
        break;
    }
    case X64_INSTR_CMP_R_I: {
        u32 size = (u32)instr->cmp_r_i.size;

        X64_emit_ri_instr(generator, "cmp", size, instr->cmp_r_i.op1, size, instr->cmp_r_i.op2);
        break;
    }
    case X64_INSTR_CMP_R_M: {
        u32 size = (u32)instr->cmp_r_m.size;

        X64_emit_rm_instr(generator, "cmp", false, size, instr->cmp_r_m.op1, size, &instr->cmp_r_m.op2);
        break;
    }
    case X64_INSTR_CMP_M_R: {
        u32 size = (u32)instr->cmp_m_r.size;

        X64_emit_mr_instr(generator, "cmp", size, &instr->cmp_m_r.op1, size, instr->cmp_m_r.op2);
        break;
    }
    case X64_INSTR_CMP_M_I: {
        u32 size = (u32)instr->cmp_m_i.size;

        X64_emit_mi_instr(generator, "cmp", size, &instr->cmp_m_i.op1, size, instr->cmp_m_i.op2);
        break;
    }
    case X64_INSTR_JMP: {
        long target_id = instr->jmp.target->id;

        if (target_id != bblock_id + 1) {
            X64_emit_text(generator, "  jmp %s", X64_get_label(generator, target_id));
        }
        break;
    }
    case X64_INSTR_JMPCC: {
        X64_emit_text(generator, "  j%s %s", x64_condition_codes[instr->jmpcc.cond],
                      X64_get_label(generator, instr->jmpcc.true_bb->id));
        break;
    }
    case X64_INSTR_SETCC: {
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, instr->setcc.dst);

        if (dst_loc.kind == X64_LREG_LOC_REG) {
            X64_emit_text(generator, "  set%s %s", x64_condition_codes[instr->setcc.cond], x64_reg_names[1][dst_loc.reg]);
        }
        else {
            assert(dst_loc.kind == X64_LREG_LOC_STACK);
            X64_emit_text(generator, "  set%s %s", x64_condition_codes[instr->setcc.cond],
                          X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, 1));
        }
        break;
    }
    case X64_INSTR_RET: {
        if (!last_instr) { // Not the last instruction
            X64_emit_text(generator, "  jmp end.%s", symbol_mangled_name(generator->tmp_mem, generator->curr_proc.sym));
        }

        break;
    }
    case X64_INSTR_CALL:
    case X64_INSTR_CALL_R: {
        Type* proc_type;
        X64_CallValue dst_val;
        u32 num_args;
        X64_InstrCallArg* args;
        X64_StackArgsInfo stack_args_info;
        unsigned save_reg_mask;

        if (instr->kind == X64_INSTR_CALL) {
            proc_type = instr->call.sym->type;
            dst_val = instr->call.dst;
            num_args = instr->call.num_args;
            args = instr->call.args;
            stack_args_info = instr->call.stack_info;
            save_reg_mask = instr->call.save_reg_mask;
        }
        else {
            assert(instr->kind == X64_INSTR_CALL_R);
            proc_type = instr->call_r.proc_type;
            dst_val = instr->call_r.dst;
            num_args = instr->call_r.num_args;
            args = instr->call_r.args;
            stack_args_info = instr->call_r.stack_info;
            save_reg_mask = instr->call_r.save_reg_mask;
        }

        // NOTE: Stack frame must be 16-byte aligned before procedure call.
        // If the number of stack args + caller-saved regs is not even (16-byte aligned),
        // we MUST subtract 8 from stack BEFORE pushing anything into stack
        // See: https://godbolt.org/z/cM9Encdsc
        ListNode* rsp_align_loc = generator->curr_proc.text_lines.prev;

        // NOTE: No need to save caller-saved registers before call because the register allocator currently
        // spills any values needed across procedure calls.
        X64_RegGroup group = X64_begin_reg_group(generator);

        // Save caller-saved registers needed across the call.
        u32 r = 0;

        while (save_reg_mask) {
            if (save_reg_mask & 0x1) {
                X64_save_reg(&group, (X64_Reg)r);
            }

            save_reg_mask >>= 1;
            r++;
        }

        // If the called procedure returns a "large" object by value, provide the address to the destination
        // memory location as the first argument.
        Type* ret_type = proc_type->as_proc.ret;

        if (type_is_obj_like(ret_type) && X64_is_obj_retarg_large(ret_type->size)) {
            X64_Reg dst_reg = x64_target.arg_regs[0];
            X64_SIBDAddr obj_addr = {0};
            X64_get_sibd_addr(generator, &obj_addr, &dst_val.addr);

            X64_emit_text(generator, "  lea %s, %s", x64_reg_names[X64_MAX_INT_REG_SIZE][dst_reg],
                          X64_print_sibd_addr(generator->tmp_mem, &obj_addr, 0));
        }

        // Place arguments in the appropriate locations.
        // For register args, it is expected that the register allocator either placed the arg in the correct register or spilled it.
        // For stack args, it is expected that the register allocator either placed the arg in a non-argument register or spilled it.
        if (stack_args_info.size) {
            X64_emit_text(generator, "  sub rsp, %d", stack_args_info.size);
        }

        X64_place_args_in_regs(generator, num_args, args);

        if (stack_args_info.size) {
            X64_place_args_in_stack(generator, num_args, args);
        }

        // Align stack before call.
        u64 total_stack_size = stack_args_info.size + group.num_tmp_regs * X64_MAX_INT_REG_SIZE;
        u64 align_stack_size = 0;

        if (total_stack_size & (X64_STACK_ALIGN - 1)) {
            align_stack_size = X64_STACK_WORD_SIZE;

            X64_insert_text(generator, rsp_align_loc, "  sub rsp, %lu", align_stack_size);
            total_stack_size += align_stack_size;
        }

        // Stack should now be aligned properly for procedure call.
        assert((total_stack_size & (X64_STACK_ALIGN - 1)) == 0);

        if (instr->kind == X64_INSTR_CALL) {
            X64_emit_text(generator, "  call %s", symbol_mangled_name(generator->tmp_mem, instr->call.sym));
        }
        else {
            X64_LRegLoc proc_reg_loc = X64_lreg_loc(generator, instr->call_r.proc_loc);
            const char* call_op_str = proc_reg_loc.kind == X64_LREG_LOC_REG ?
                                          x64_reg_names[PTR_SIZE][proc_reg_loc.reg] :
                                          X64_print_stack_offset(generator->tmp_mem, proc_reg_loc.offset, PTR_SIZE);

            X64_emit_text(generator, "  call %s", call_op_str);
        }

        // Move return value (if any) to appropriate register.
        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            if (!type_is_obj_like(ret_type)) {
                // Returns a primitive type.

                X64_LRegLoc dst_loc = X64_lreg_loc(generator, dst_val.reg);

                if (dst_loc.kind == X64_LREG_LOC_STACK) {
                    // Move result (in RAX) to stack offset.
                    X64_emit_text(generator, "  mov %s, %s",
                                  X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, ret_type->size),
                                  x64_reg_names[ret_type->size][X64_RAX]);
                }
                else {
                    assert(dst_loc.kind == X64_LREG_LOC_REG);

                    if (dst_loc.reg != X64_RAX) {
                        // Move result (in RAX) to allocated result register.
                        X64_emit_text(generator, "  mov %s, %s", x64_reg_names[ret_type->size][dst_loc.reg],
                                      x64_reg_names[ret_type->size][X64_RAX]);
                    }
                }
            }
            else if (x64_target.os == OS_LINUX) {
                X64_linux_cpy_ret_small_obj(generator, ret_type, &dst_val);
            }
            else {
                assert(x64_target.os == OS_WIN32);
                X64_windows_cpy_ret_small_obj(generator, ret_type, &dst_val);
            }
        }

        if (group.num_tmp_regs) {
            // Clean up stack args
            if (stack_args_info.size) {
                X64_emit_text(generator, "  add rsp, %u", stack_args_info.size);
            }

            // Restore saved registers.
            X64_end_reg_group(&group);

            // Clean up any initial stack alignment
            if (align_stack_size) {
                X64_emit_text(generator, "  add rsp, %u", align_stack_size);
            }
        }
        else {
            size_t cleanup_amount = stack_args_info.size + align_stack_size;

            if (cleanup_amount) {
                X64_emit_text(generator, "  add rsp, %lu", cleanup_amount); // Clean up stack args + alignment
            }
        }

        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unknown X64 LIR instruction kind %d at IP %u\n", instr->kind, instr->ino);
        break;
    }

    allocator_restore_state(mem_state);
}

static void X64_gen_proc(X64_Generator* generator, u32 proc_id, Symbol* sym)
{
    DeclProc* decl = (DeclProc*)sym->decl;

    if (decl->is_incomplete) {
        return;
    }

    generator->curr_proc.sym = sym;
    generator->curr_proc.id = proc_id;

    list_head_init(&generator->curr_proc.text_lines);

    bool is_nonleaf = sym->as_proc.is_nonleaf;

    // Set different scratch register order for leaf vs nonleaf procedures.
    generator->curr_proc.scratch_regs = is_nonleaf ? x64_target.nonleaf_scratch_regs : x64_target.leaf_scratch_regs;

    AllocatorState tmp_mem_state = allocator_get_state(generator->tmp_mem);
    AllocatorState gen_mem_state = allocator_get_state(generator->gen_mem);

    const char* proc_mangled = symbol_mangled_name(generator->tmp_mem, sym);
    X64_emit_text(generator, "");
    X64_emit_text(generator, "global %s", proc_mangled);
    X64_emit_text(generator, "%s:", proc_mangled);

    X64_emit_text(generator, "  push rbp");
    X64_emit_text(generator, "  mov rbp, rsp");

    ListNode* sub_rsp_prev = generator->curr_proc.text_lines.prev; // sub rsp, <stack_size>

    u32 stack_size = X64_assign_proc_stack_offsets(generator, sym); // NOTE: Spills argument registers.

    // Register allocation.
    BBlock** ir_bblocks = sym->as_proc.bblocks;
    size_t num_ir_bblocks = array_len(ir_bblocks);
    X64_LIRBuilder builder = {.arena = generator->tmp_mem};

    X64_emit_lir_instrs(&builder, sym->as_proc.num_regs, num_ir_bblocks, ir_bblocks);

#ifdef NIBBLE_PRINT_IRS
    LIR_dump_proc_dot(generator->tmp_mem, proc_mangled, builder.num_bblocks, builder.bblocks);
#endif

    X64_compute_live_intervals(&builder);

    X64_RegAllocResult reg_alloc = X64_linear_scan_reg_alloc(&builder, generator->curr_proc.scratch_regs, stack_size);

    if (!reg_alloc.success) {
        NIBBLE_FATAL_EXIT("Register allocation for procedure `%s` failed.", sym->name->str);
        return;
    }

    stack_size = reg_alloc.stack_offset;
    generator->curr_proc.builder = &builder;

#if 0
    u32 num_lreg_ranges = array_len(builder.lreg_ranges);
    ftprint_out("Register allocation for %s (%s):\n", sym->name->str, is_nonleaf ? "nonleaf": "leaf");
    for (u32 i = 0; i < num_lreg_ranges; i += 1)
    {
        if (X64_find_alias_reg(&builder, i) != i) continue;

        X64_LRegRange* rng = builder.lreg_ranges + i;
        X64_LRegLoc* loc = &rng->loc;

        if (loc->kind == X64_LREG_LOC_REG)
        {
            ftprint_out("\tr%u -> %s", i, x64_reg_names[8][loc->reg]);
        }
        else
        {
            assert(loc->kind == X64_LREG_LOC_STACK);
            ftprint_out("\tr%u -> RBP + %d", i, loc->offset);
        }

        ftprint_out(", [%u - %u]\n", rng->start, rng->end);
    }
#endif
    if (stack_size) {
        X64_insert_text(generator, sub_rsp_prev, "  sub rsp, %u", stack_size);
    }

    ListNode* save_regs_loc = generator->curr_proc.text_lines.prev;

    // Generate instructions.
    for (size_t ii = 0; ii < builder.num_bblocks; ii++) {
        X64_BBlock* bb = builder.bblocks[ii];
        bool last_bb = ii == builder.num_bblocks - 1;

        X64_emit_text(generator, "  %s:", X64_get_label(generator, bb->id));

        for (X64_Instr* instr = bb->first; instr; instr = instr->next) {
            bool last_instr = last_bb && !instr->next;

            X64_gen_instr(generator, instr, last_instr, bb->id);
        }
    }

    // End label
    X64_emit_text(generator, "  end.%s:", proc_mangled);

    // Patch instruction to save callee-saved registers.
    for (uint32_t r = 0; r < X64_REG_COUNT; r += 1) {
        X64_Reg reg = (X64_Reg)r;

        if (reg == X64_RBP || reg == X64_RSP)
            continue;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg)) {
            save_regs_loc = X64_print_push_reg(generator, save_regs_loc, reg);
        }
    }

    // Restore callee-saved registers.
    // NOTE: Iterating in the reverse order as the corresponding pushes.
    ListNode* pop_regs_loc = generator->curr_proc.text_lines.prev;

    for (uint32_t r = X64_REG_COUNT; r-- > 0;) {
        X64_Reg reg = (X64_Reg)r;

        if (reg == X64_RBP || reg == X64_RSP)
            continue;

        if (u32_is_bit_set(reg_alloc.used_callee_regs, reg)) {
            pop_regs_loc = X64_print_pop_reg(generator, pop_regs_loc, reg);
        }
    }

    assert(pop_regs_loc == generator->curr_proc.text_lines.prev);

    // Postamble
    X64_emit_text(generator, "  mov rsp, rbp");
    X64_emit_text(generator, "  pop rbp");
    X64_emit_text(generator, "  ret");

    // Write instruction text to file.
    X64_output_asm_lines(generator->out_fd, &generator->curr_proc.text_lines);

    allocator_restore_state(tmp_mem_state);
    allocator_restore_state(gen_mem_state);
}

static void X64_gen_global_vars(X64_Generator* generator, BucketList* vars, BucketList* str_lits, BucketList* float_lits)
{
    AllocatorState mem_state = allocator_get_state(generator->tmp_mem);

    X64_emit_data(generator, "SECTION .rodata\n");

    // Emit static/const string literals
    size_t num_str_lits = str_lits->num_elems;

    for (size_t i = 0; i < num_str_lits; i++) {
        void** str_lit_ptr = bucket_list_get_elem_packed(str_lits, i);
        assert(str_lit_ptr);

        StrLit* str_lit = (StrLit*)(*str_lit_ptr);

        assert(str_lit->used);

        const char* escaped_str = cstr_escape(generator->tmp_mem, str_lit->str, str_lit->len, '`');

        X64_emit_data(generator, "%s_%llu: ", X64_STR_LIT_PRE, str_lit->id);
        X64_emit_data(generator, "db `%s\\0`", escaped_str);
    }

    // Emit static/const float literals
    size_t num_float_lits = float_lits->num_elems;

    for (size_t i = 0; i < num_float_lits; i++) {
        void** float_lit_ptr = bucket_list_get_elem_packed(float_lits, i);
        assert(float_lit_ptr);

        FloatLit* float_lit = (FloatLit*)(*float_lit_ptr);

        assert(float_lit->used);

        X64_emit_data(generator, "%s_%llu: ", X64_FLOAT_LIT_PRE, float_lit->id);

        if (float_lit->kind == FLOAT_F64) {
            X64_emit_data(generator, "%s %f", x64_data_size_label[8], float_lit->value._f64);
        }
        else {
            assert(float_lit->kind == FLOAT_F32);
            X64_emit_data(generator, "%s %f", x64_data_size_label[4], float_lit->value._f32);
        }
    }

    X64_emit_data(generator, "\nSECTION .data\n");

    size_t num_vars = vars->num_elems;

    for (u32 i = 0; i < num_vars; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(vars, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);

        X64_emit_global_data(generator, sym);
    }
    allocator_restore_state(mem_state);
}

bool x64_gen_module(Allocator* gen_mem, Allocator* tmp_mem, BucketList* vars, BucketList* procs, BucketList* str_lits,
                    BucketList* float_lits, const char* output_file)
{
    FILE* out_fd = fopen(output_file, "w");
    if (!out_fd) {
        ftprint_err("Failed to open output file `%s`\n", output_file);
        return false;
    }

    X64_Generator generator = {.out_fd = out_fd, .gen_mem = gen_mem, .tmp_mem = tmp_mem};

    // Write top comment to file.
    ftprint_file(out_fd, false, "; Generated by the Nibble compiler.\n\n");

    AllocatorState gen_mem_state = allocator_get_state(gen_mem);
    {
        list_head_init(&generator.data_lines);

        // Generate global variables.
        X64_gen_global_vars(&generator, vars, str_lits, float_lits);

        // Write data to file.
        X64_output_asm_lines(out_fd, &generator.data_lines);
    }
    allocator_restore_state(gen_mem_state);

    // Write startup/builtin code.
    ftprint_file(out_fd, false, "%s\n\n", x64_target.startup_code);

    // Generate instructions for each procedure.
    size_t num_procs = procs->num_elems;

    for (size_t i = 0; i < num_procs; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);

        X64_gen_proc(&generator, i, sym);
    }

    fclose(out_fd);

    return true;
}
