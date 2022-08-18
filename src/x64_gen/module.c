#include "stream.h"
#include "x64_gen/module.h"

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

static const char* x64_int_reg_names[X64_MAX_INT_REG_SIZE + 1][X64_REG_COUNT] = {
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

static const char* X64_reg_name(X64_Reg reg, u32 size)
{
    X64_RegClass reg_class = x64_reg_classes[reg];

    if (reg_class == X64_REG_CLASS_INT) {
        return x64_int_reg_names[size][reg];
    }

    return x64_flt_reg_names[reg];
}

static const char* X64_float_lit_mangled_name(Allocator* alloc, FloatLit* float_lit)
{
    char* dstr = array_create(alloc, char, 16);
    ftprint_char_array(&dstr, true, "%s_%llu", X64_FLOAT_LIT_PRE, float_lit->id);

    return dstr;
}

#define IS_LREG_IN_REG(k) ((k) == X64_LREG_LOC_REG)
#define IS_LREG_IN_STACK(k) ((k) == X64_LREG_LOC_STACK)

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

static void X64_emit_global_data(X64_Generator* generator, const char* name, ConstExpr* const_expr)
{
    Allocator* tmp_mem = generator->tmp_mem;
    Type* type = const_expr->type;

    X64_emit_data(generator, "ALIGN %d", type->align);
    X64_emit_data(generator, "%s: ", name);

    AllocatorState mem_state = allocator_get_state(tmp_mem);
    char* line = array_create(tmp_mem, char, type->size << 3);

    X64_print_global_val(tmp_mem, const_expr, &line);

    array_push(line, '\0');
    X64_emit_data(generator, "%s\n", line);
    allocator_restore_state(mem_state);
}

static ListNode* X64_print_push_reg(X64_Generator* gen, ListNode* prev, X64_Reg reg)
{
    ListNode* node;

    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        node = X64_insert_text(gen, prev, "  push %s", x64_int_reg_names[X64_MAX_INT_REG_SIZE][reg]);
    }
    else {
        node = X64_insert_text(gen, prev, "  sub rsp, 16");
        node = X64_insert_text(gen, node, "  movdqu oword [rsp], %s", x64_flt_reg_names[reg]);
    }

    return node;
}

static ListNode* X64_print_pop_reg(X64_Generator* gen, ListNode* prev, X64_Reg reg)
{
    ListNode* node;

    if (x64_reg_classes[reg] == X64_REG_CLASS_INT) {
        node = X64_insert_text(gen, prev, "  pop %s", x64_int_reg_names[X64_MAX_INT_REG_SIZE][reg]);
    }
    else {
        node = X64_insert_text(gen, prev, "  movdqu %s, oword [rsp]", x64_flt_reg_names[reg]);
        node = X64_insert_text(gen, node, "  add rsp, 16");
    }

    return node;
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

static X64_Reg X64_get_reg(X64_RegGroup* group, X64_RegClass reg_class, u32 lreg, u32 size, bool store, u32 banned_regs)
{
    X64_LRegLoc lreg_loc = X64_lreg_loc(group->generator, lreg);

    // If this virtual register was not spilled during allocation, just return its assigned
    // physical register.
    if (IS_LREG_IN_REG(lreg_loc.kind)) {
        return lreg_loc.reg;
    }

    // This virtual register was spilled during allocation, so use a temporary physical register which will have
    // to be restored later.

    X64_Generator* generator = group->generator;
    X64_ScratchRegs* x64_scratch_regs = &(*generator->curr_proc.scratch_regs)[reg_class];
    u32 num_scratch_regs = x64_scratch_regs->num_regs;
    X64_Reg* scratch_regs = x64_scratch_regs->regs;

    assert(IS_LREG_IN_STACK(lreg_loc.kind));
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

    X64_print_push_reg(group->generator, group->generator->curr_proc.text_lines.prev, tmp_reg->reg);

    if (reg_class == X64_REG_CLASS_INT) {
        X64_emit_text(group->generator, "  mov %s, %s", x64_int_reg_names[size][tmp_reg->reg],
                      X64_print_stack_offset(tmp_mem, lreg_loc.offset, size));
    }
    else {
        assert(reg_class == X64_REG_CLASS_FLOAT);
        const char* mov_flt_instr = size == float_kind_sizes[FLOAT_F64] ? "movsd" : "movss";

        X64_emit_text(group->generator, "  %s %s, %s", mov_flt_instr, x64_flt_reg_names[tmp_reg->reg],
                      X64_print_stack_offset(tmp_mem, lreg_loc.offset, size));
    }

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

    X64_print_push_reg(group->generator, group->generator->curr_proc.text_lines.prev, tmp_reg->reg);

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
            X64_RegClass reg_class = x64_reg_classes[it->reg];

            if (reg_class == X64_REG_CLASS_INT) {
                X64_emit_text(generator, "  mov %s, %s", X64_print_stack_offset(generator->tmp_mem, it->offset, it->size),
                              x64_int_reg_names[it->size][it->reg]);
            }
            else {
                assert(reg_class == X64_REG_CLASS_FLOAT);
                const char* mov_flt_instr = (it->size == float_kind_sizes[FLOAT_F64]) ? "movsd" : "movss";

                X64_emit_text(group->generator, "  %s %s, %s", mov_flt_instr,
                              X64_print_stack_offset(generator->tmp_mem, it->offset, it->size), x64_flt_reg_names[it->reg]);
            }
        }

        X64_print_pop_reg(generator, generator->curr_proc.text_lines.prev, it->reg);
        it = it->next;
    }
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

static s32 X64_spill_reg(X64_Generator* generator, X64_LinuxAssignParamState* state, u64 size, u64 align, X64_Reg preg)
{
    state->stack_spill_size += size;
    state->stack_spill_size = ALIGN_UP(state->stack_spill_size, align);
    s32 offset = -state->stack_spill_size;

    X64_RegClass reg_class = x64_reg_classes[preg];
    const char* mov_name = NULL;
    const char* reg_name = NULL;

    if (reg_class == X64_REG_CLASS_INT) {
        mov_name = "mov";
        reg_name = x64_int_reg_names[size][preg];
    }
    else if (reg_class == X64_REG_CLASS_FLOAT) {
        mov_name = size == float_kind_sizes[FLOAT_F64] ? "movsd" : "movss";
        reg_name = x64_flt_reg_names[preg];
    }
    else {
        NIBBLE_FATAL_EXIT("X64_spill_reg(): Unexpected register class for register '%d'.", preg);
    }

    X64_emit_text(generator, "  %s %s [rbp + %d], %s", mov_name, x64_mem_size_label[size], offset, reg_name);

    return offset;
}

static void X64_assign_proc_param_offsets(X64_Generator* generator, Symbol* sproc, X64_StackParamsInfo* stack_params_info)
{
    DeclProc* dproc = (DeclProc*)sproc->decl;
    Type* ret_type = sproc->type->as_proc.ret;

    u32 index = 0;
    u32 arg_reg_indices[X64_REG_CLASS_COUNT] = {0};
    X64_LinuxAssignParamState state = {.stack_arg_offset = 0x10};

    // For procs that return a large struct by value:
    // Spill the first argument, which contains a pointer to the return value's memory address, into the stack.
    // We need to spill (remember) this address so that the procedure can return it, as per the X64 calling conventions.
    if (type_is_obj_like(ret_type) && X64_is_obj_retarg_large(ret_type->size)) {
        X64_ScratchRegs arg_int_regs = (*x64_target.arg_regs)[X64_REG_CLASS_INT];

        X64_spill_reg(generator, &state, X64_MAX_INT_REG_SIZE, X64_MAX_INT_REG_SIZE,
                      arg_int_regs.regs[arg_reg_indices[X64_REG_CLASS_INT]]);
        arg_reg_indices[X64_REG_CLASS_INT] += 1;
    }

    Scope* scope = dproc->scope;
    List* head = &scope->sym_list;
    List* it = head->next;

    while (it != head) {
        // Only process params. Local variables are not processed here.
        if (index >= dproc->num_params)
            break;

        Symbol* sym = list_entry(it, Symbol, lnode);

        // Assign stack offsets to procedure params.
        assert(sym->kind == SYMBOL_VAR);

        Type* arg_type = sym->type;
        u64 arg_size = arg_type->size;
        u64 arg_align = arg_type->align;

        if (type_is_obj_like(arg_type)) {
            X64_RegClass reg_class = X64_obj_reg_class(arg_type);
            X64_ScratchRegs arg_regs = (*x64_target.arg_regs)[reg_class];
            u32* arg_reg_index = &arg_reg_indices[reg_class];

            u32 rem_regs = arg_regs.num_regs - *arg_reg_index;

            if ((arg_size <= X64_MAX_INT_REG_SIZE) && (rem_regs >= 1)) {
                X64_Reg arg_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                sym->as_var.offset = X64_spill_reg(generator, &state, X64_MAX_INT_REG_SIZE, arg_align, arg_reg);
            }
            else if ((arg_size <= (X64_MAX_INT_REG_SIZE << 1)) && (rem_regs >= 2)) {
                X64_Reg low_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;
                X64_Reg high_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                X64_spill_reg(generator, &state, X64_MAX_INT_REG_SIZE, arg_align, high_reg);
                sym->as_var.offset = X64_spill_reg(generator, &state, X64_MAX_INT_REG_SIZE, arg_align, low_reg);
            }
            else {
                sym->as_var.offset = X64_consume_stack_arg(&state.stack_arg_offset, arg_size, arg_align);
            }
        }
        else {
            X64_RegClass reg_class = arg_type->kind == TYPE_FLOAT ? X64_REG_CLASS_FLOAT : X64_REG_CLASS_INT;
            X64_ScratchRegs arg_regs = (*x64_target.arg_regs)[reg_class];
            u32* arg_reg_index = &arg_reg_indices[reg_class];

            // Spill argument register below rsp
            if (*arg_reg_index < arg_regs.num_regs) {
                X64_Reg arg_reg = arg_regs.regs[*arg_reg_index];
                *arg_reg_index += 1;

                sym->as_var.offset = X64_spill_reg(generator, &state, arg_size, arg_align, arg_reg);
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
            assert(IS_LREG_IN_REG(base_loc.kind));

            sibd_addr->local.base_reg = base_loc.reg;
            u32_set_bit(&used_regs, base_loc.reg);

            if (has_index) {
                X64_LRegLoc index_loc = X64_lreg_loc(generator, vaddr->sibd.index_reg);
                assert(IS_LREG_IN_REG(index_loc.kind));

                sibd_addr->local.index_reg = index_loc.reg;
                u32_set_bit(&used_regs, index_loc.reg);
            }
            else {
                sibd_addr->local.index_reg = X64_REG_COUNT;
            }
        }
        else {
            X64_LRegLoc index_loc = X64_lreg_loc(generator, vaddr->sibd.index_reg);
            assert(IS_LREG_IN_REG(index_loc.kind));

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
            const char* base_reg_name = x64_int_reg_names[X64_MAX_INT_REG_SIZE][addr->local.base_reg];

            if (has_index) {
                const char* index_reg_name = x64_int_reg_names[X64_MAX_INT_REG_SIZE][addr->local.index_reg];

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
            const char* index_reg_name = x64_int_reg_names[X64_MAX_INT_REG_SIZE][addr->local.index_reg];

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
    const X64_RegClass src_reg_class = x64_reg_classes[src];

    // If need to copy 8 or more bytes, just copy entire register into memory, and then return.
    if (rem_amnt >= X64_MAX_INT_REG_SIZE) {
        const char* src_reg_name = X64_reg_name(src, X64_MAX_INT_REG_SIZE);
        const char* mov_instr_name = src_reg_class == X64_REG_CLASS_FLOAT ? "movsd" : "mov";

        X64_emit_text(generator, "  %s %s, %s", mov_instr_name, X64_print_sibd_addr(generator->tmp_mem, dst, X64_MAX_INT_REG_SIZE),
                      src_reg_name);

        // Move dst addr forward.
        dst->local.disp += X64_MAX_INT_REG_SIZE;

        return rem_amnt - X64_MAX_INT_REG_SIZE;
    }

    // Have to copy less than 8 bytes. Copy in chunks of powers-of-two.
    assert(rem_amnt < X64_MAX_INT_REG_SIZE);

    if (src_reg_class == X64_REG_CLASS_INT) {
        while (rem_amnt) {
            // Calc the largest power of 2 that is less than or equal to min(8, rem_amnt).
            size_t n = pow2_sizes[rem_amnt];

            // Copy that amount into memory.
            X64_emit_text(generator, "  mov %s, %s", X64_print_sibd_addr(generator->tmp_mem, dst, n), x64_int_reg_names[n][src]);

            // Move dst addr forward.
            dst->local.disp += n;

            size_t new_rem_amnt = rem_amnt - n;

            // Shift src register right to discard copied bits.
            if (new_rem_amnt) {
                X64_emit_text(generator, "  sar %s, %d", x64_int_reg_names[X64_MAX_INT_REG_SIZE][src], n << 3);
            }

            rem_amnt = new_rem_amnt;
        }
    }
    else {
        assert(src_reg_class == X64_REG_CLASS_FLOAT);

        if (rem_amnt == float_kind_sizes[FLOAT_F32]) {
            X64_emit_text(generator, "  movss %s, %s", X64_print_sibd_addr(generator->tmp_mem, dst, rem_amnt), x64_flt_reg_names[src]);

            rem_amnt = 0;
        }
        else {
            NIBBLE_FATAL_EXIT("X64_cpy_reg_to_mem(): Cannot copy %d bytes from XMM register.", rem_amnt);
        }
    }

    return rem_amnt;
}

static void X64_emit_flt_cmp_rr_instr(X64_Generator* generator, const char* instr_name, FloatKind fkind, u32 op1_lreg, u32 op2_lreg)
{
    X64_LRegLoc op2_loc = X64_lreg_loc(generator, op2_lreg);
    const size_t op_size = float_kind_sizes[fkind];

    u32 banned_op1_regs = 0;
    const char* op2_name = NULL;

    if (IS_LREG_IN_REG(op2_loc.kind)) {
        banned_op1_regs = (1 << op2_loc.reg);
        op2_name = x64_flt_reg_names[op2_loc.reg];
    }
    else if (IS_LREG_IN_STACK(op2_loc.kind)) {
        op2_name = X64_print_stack_offset(generator->tmp_mem, op2_loc.offset, op_size);
    }
    else {
        NIBBLE_FATAL_EXIT("X64_emit_flt_cmp_rr_instr(): Unexpected op2 X64_LRegLoc kind '%d'.", op2_loc.kind);
    }

    X64_RegGroup tmp_group = X64_begin_reg_group(generator);
    X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, op1_lreg, op_size, false, banned_op1_regs);
    const char* op1_reg_name = x64_flt_reg_names[op1_reg];

    X64_emit_text(generator, "  %s %s, %s", instr_name, op1_reg_name, op2_name);
    X64_end_reg_group(&tmp_group);
}

static void X64_emit_flt2int_rr_instr(X64_Generator* generator, const char* instr_name, u8 src_size, u8 dst_size, u32 src_lreg,
                                      u32 dst_lreg)
{
    X64_LRegLoc dst_loc = X64_lreg_loc(generator, dst_lreg);
    X64_LRegLoc src_loc = X64_lreg_loc(generator, src_lreg);
    u32 dst_si_size = dst_size <= 4 ? 4 : 8; // TODO: No magic allowed.

    if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
        const char* dst_reg_name = x64_int_reg_names[dst_si_size][dst_loc.reg];
        const char* src_reg_name = x64_flt_reg_names[src_loc.reg];

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_reg_name);
    }
    else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
        const char* dst_reg_name = x64_int_reg_names[dst_si_size][dst_loc.reg];
        const char* src_addr_name = X64_print_stack_offset(generator->tmp_mem, src_loc.offset, src_size);

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_addr_name);
    }
    else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, dst_lreg, dst_size, true, (1 << src_loc.reg));
        const char* dst_reg_name = x64_int_reg_names[dst_si_size][dst_reg];
        const char* src_reg_name = x64_flt_reg_names[src_loc.reg];

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_reg_name);
        X64_end_reg_group(&tmp_group);
    }
    else {
        assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, dst_lreg, dst_size, true, 0);
        const char* dst_reg_name = x64_int_reg_names[dst_si_size][dst_reg];
        const char* src_addr_name = X64_print_stack_offset(generator->tmp_mem, src_loc.offset, src_size);

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_addr_name);
        X64_end_reg_group(&tmp_group);
    }
}

static void X64_emit_int2flt_rr_instr(X64_Generator* generator, const char* instr_name, u8 src_size, u8 dst_size, u32 src_lreg,
                                      u32 dst_lreg)
{
    assert(src_size >= 4);

    X64_LRegLoc dst_loc = X64_lreg_loc(generator, dst_lreg);
    X64_LRegLoc src_loc = X64_lreg_loc(generator, src_lreg);

    if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
        const char* dst_reg_name = x64_flt_reg_names[dst_loc.reg];
        const char* src_reg_name = x64_int_reg_names[src_size][src_loc.reg];

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_reg_name);
    }
    else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
        const char* dst_reg_name = x64_flt_reg_names[dst_loc.reg];
        const char* src_addr_name = X64_print_stack_offset(generator->tmp_mem, src_loc.offset, src_size);

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_addr_name);
    }
    else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, dst_lreg, dst_size, true, (1 << src_loc.reg));
        const char* dst_reg_name = x64_flt_reg_names[dst_reg];
        const char* src_reg_name = x64_int_reg_names[src_size][src_loc.reg];

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_reg_name);
        X64_end_reg_group(&tmp_group);
    }
    else {
        assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, dst_lreg, dst_size, true, 0);
        const char* dst_reg_name = x64_flt_reg_names[dst_reg];
        const char* src_addr_name = X64_print_stack_offset(generator->tmp_mem, src_loc.offset, src_size);

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_addr_name);
        X64_end_reg_group(&tmp_group);
    }
}

static void X64_emit_flt2flt_rr_instr(X64_Generator* generator, const char* instr_name, u8 src_size, u8 dst_size, u32 src_lreg,
                                      u32 dst_lreg)
{
    X64_LRegLoc dst_loc = X64_lreg_loc(generator, dst_lreg);
    X64_LRegLoc src_loc = X64_lreg_loc(generator, src_lreg);

    if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
        const char* dst_reg_name = x64_flt_reg_names[dst_loc.reg];
        const char* src_reg_name = x64_flt_reg_names[src_loc.reg];

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_reg_name);
    }
    else if (IS_LREG_IN_REG(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind)) {
        const char* dst_reg_name = x64_flt_reg_names[dst_loc.reg];
        const char* src_addr_name = X64_print_stack_offset(generator->tmp_mem, src_loc.offset, src_size);

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_addr_name);
    }
    else if (IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_REG(src_loc.kind)) {
        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, dst_lreg, dst_size, true, (1 << src_loc.reg));
        const char* dst_reg_name = x64_flt_reg_names[dst_reg];
        const char* src_reg_name = x64_flt_reg_names[src_loc.reg];

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_reg_name);
        X64_end_reg_group(&tmp_group);
    }
    else {
        assert(IS_LREG_IN_STACK(dst_loc.kind) && IS_LREG_IN_STACK(src_loc.kind));
        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, dst_lreg, dst_size, true, 0);
        const char* dst_reg_name = x64_flt_reg_names[dst_reg];
        const char* src_addr_name = X64_print_stack_offset(generator->tmp_mem, src_loc.offset, src_size);

        X64_emit_text(generator, "  %s %s, %s", instr_name, dst_reg_name, src_addr_name);
        X64_end_reg_group(&tmp_group);
    }
}

static void X64_emit_rr_instr(X64_Generator* generator, const char* instr, bool writes_op1, X64_RegClass reg_class, u32 op1_size,
                              u32 op1_lreg, u32 op2_size, u32 op2_lreg)
{
    X64_LRegLoc op1_loc = X64_lreg_loc(generator, op1_lreg);
    X64_LRegLoc op2_loc = X64_lreg_loc(generator, op2_lreg);

    switch (op1_loc.kind) {
    case X64_LREG_LOC_REG: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG: {
            const char* r1 = X64_reg_name(op1_loc.reg, op1_size);
            const char* r2 = X64_reg_name(op2_loc.reg, op2_size);
            X64_emit_text(generator, "  %s %s, %s", instr, r1, r2);
            break;
        }
        case X64_LREG_LOC_STACK: {
            const char* r1 = X64_reg_name(op1_loc.reg, op1_size);
            const char* addr2 = X64_print_stack_offset(generator->tmp_mem, op2_loc.offset, op2_size);
            X64_emit_text(generator, "  %s %s, %s", instr, r1, addr2);
            break;
        }
        default:
            assert(0);
            break;
        }
        break;
    }
    case X64_LREG_LOC_STACK: {
        switch (op2_loc.kind) {
        case X64_LREG_LOC_REG: {
            const char* addr1 = X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size);
            const char* r2 = X64_reg_name(op2_loc.reg, op2_size);
            X64_emit_text(generator, "  %s %s, %s", instr, addr1, r2);
            break;
        }
        case X64_LREG_LOC_STACK: {
            const char* op1_op_str = X64_print_stack_offset(generator->tmp_mem, op1_loc.offset, op1_size);
            const char* op2_op_str = X64_print_stack_offset(generator->tmp_mem, op2_loc.offset, op2_size);

            X64_Reg tmp_reg = (reg_class == X64_REG_CLASS_INT) ? X64_RAX : X64_XMM0;
            const char* tmp_reg_str = X64_reg_name(tmp_reg, op1_size);

            // Save the contents of a temporary register into the stack.
            X64_print_push_reg(generator, generator->curr_proc.text_lines.prev, tmp_reg);

            // Load dst into the temporary register,
            const char* mov_instr =
                (reg_class == X64_REG_CLASS_FLOAT) ? (op1_size == float_kind_sizes[FLOAT_F64] ? "movsd" : "movss") : "mov";
            X64_emit_text(generator, "  %s %s, %s", mov_instr, tmp_reg_str, op1_op_str);

            // Execute the instruction using the temporary register as the destination.
            X64_emit_text(generator, "  %s %s, %s", instr, tmp_reg_str, op2_op_str);

            // Store the result of the instruction (contents of temporary register) into dst.
            if (writes_op1) {
                X64_emit_text(generator, "  %s %s, %s", mov_instr, op1_op_str, tmp_reg_str);
            }

            // Restore the contents of the temporary register.
            X64_print_pop_reg(generator, generator->curr_proc.text_lines.prev, tmp_reg);

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
        X64_emit_text(generator, "  %s %s, %s", instr, x64_int_reg_names[op1_size][op1_loc.reg],
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

static void X64_emit_rm_instr(X64_Generator* generator, const char* instr, bool writes_op1, X64_RegClass reg_class, u32 op1_size,
                              u32 op1_lreg, u32 op2_size, X64_MemAddr* op2_vaddr)
{
    X64_SIBDAddr op2_addr = {0};
    u32 used_regs = X64_get_sibd_addr(generator, &op2_addr, op2_vaddr);

    X64_RegGroup tmp_group = X64_begin_reg_group(generator);
    X64_Reg op1_reg = X64_get_reg(&tmp_group, reg_class, op1_lreg, op1_size, writes_op1, used_regs);
    assert(op1_size <= 8 && IS_POW2(op1_size));

    const char* reg_name = reg_class == X64_REG_CLASS_INT ? x64_int_reg_names[op1_size][op1_reg] : x64_flt_reg_names[op1_reg];
    X64_emit_text(generator, "  %s %s, %s", instr, reg_name, X64_print_sibd_addr(generator->tmp_mem, &op2_addr, op2_size));

    X64_end_reg_group(&tmp_group);
}

static void X64_emit_mr_instr(X64_Generator* generator, const char* instr, u32 op1_size, X64_MemAddr* op1_vaddr,
                              X64_RegClass reg_class, u32 op2_size, u32 op2_lreg)
{
    X64_SIBDAddr op1_addr = {0};
    u32 used_regs = X64_get_sibd_addr(generator, &op1_addr, op1_vaddr);

    X64_RegGroup tmp_group = X64_begin_reg_group(generator);
    X64_Reg op2_reg = X64_get_reg(&tmp_group, reg_class, op2_lreg, op2_size, false, used_regs);
    assert(op2_size <= 8 && IS_POW2(op2_size));

    const char* reg_name = reg_class == X64_REG_CLASS_INT ? x64_int_reg_names[op2_size][op2_reg] : x64_flt_reg_names[op2_reg];
    X64_emit_text(generator, "  %s %s, %s", instr, X64_print_sibd_addr(generator->tmp_mem, &op1_addr, op1_size), reg_name);

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
                assert(x64_reg_classes[slot->pregs[0]] == X64_REG_CLASS_INT);
                X64_emit_text(generator, "  lea %s, [rsp + %d]", x64_int_reg_names[X64_MAX_INT_REG_SIZE][slot->pregs[0]],
                              slot->ptr_sp_offset);
            }
            // Copy 64-bit chunks of the struct object into the appropriate argument registers.
            else {
                X64_SIBDAddr addr = {0};
                X64_get_sibd_addr(generator, &addr, &arg->val.addr);

                assert(addr.kind == X64_SIBD_ADDR_LOCAL);

                for (unsigned ii = 0; ii < slot->num_regs; ii++) {
                    X64_RegClass reg_class = x64_reg_classes[slot->pregs[ii]];

                    if (reg_class == X64_REG_CLASS_INT) {
                        X64_emit_text(generator, "  mov %s, %s", x64_int_reg_names[X64_MAX_INT_REG_SIZE][slot->pregs[ii]],
                                      X64_print_sibd_addr(generator->tmp_mem, &addr, X64_MAX_INT_REG_SIZE));
                    }
                    else {
                        assert(reg_class == X64_REG_CLASS_FLOAT);
                        X64_emit_text(generator, "  movsd %s, %s", x64_flt_reg_names[slot->pregs[ii]],
                                      X64_print_sibd_addr(generator->tmp_mem, &addr, X64_MAX_INT_REG_SIZE));
                    }

                    addr.local.disp += X64_MAX_INT_REG_SIZE;
                }
            }
        }
        else { // Argument is a primitive type
            X64_PrimArgSlot* slot = &arg->slot.prim;

            if (!slot->in_reg) {
                continue;
            }

            X64_LRegLoc loc = X64_lreg_loc(generator, arg->val.reg);

            if (IS_LREG_IN_STACK(loc.kind)) {
                assert(slot->preg < X64_REG_COUNT);
                X64_RegClass reg_class = x64_reg_classes[slot->preg];

                if (reg_class == X64_REG_CLASS_INT) {
                    X64_emit_text(generator, "  mov %s, %s", x64_int_reg_names[arg_size][slot->preg],
                                  X64_print_stack_offset(generator->tmp_mem, loc.offset, arg_size));
                }
                else {
                    assert(reg_class == X64_REG_CLASS_FLOAT);
                    const char* mov_name = arg_size == float_kind_sizes[FLOAT_F64] ? "movsd" : "movss";

                    X64_emit_text(generator, "  %s %s, %s", mov_name, x64_flt_reg_names[slot->preg],
                                  X64_print_stack_offset(generator->tmp_mem, loc.offset, arg_size));
                }
            }
            else {
                assert(IS_LREG_IN_REG(loc.kind));
                assert(loc.reg == slot->preg);
            }
        }
    }
}

static void X64_place_struct_args_in_stack(X64_Generator* generator, u32 num_args, X64_InstrCallArg* args)
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

static void X64_place_args_in_stack(X64_Generator* generator, u32 num_args, X64_InstrCallArg* args)
{
    // 1st pass: Copy struct arguments into the stack.
    X64_place_struct_args_in_stack(generator, num_args, args);

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
            if (IS_LREG_IN_REG(loc.kind)) {
                X64_RegClass reg_class = x64_reg_classes[loc.reg];

                if (reg_class == X64_REG_CLASS_INT) {
                    X64_emit_text(generator, "  mov %s [rsp + %d], %s", x64_mem_size_label[arg_size], slot->sp_offset,
                                  x64_int_reg_names[arg_size][loc.reg]);
                }
                else {
                    assert(reg_class == X64_REG_CLASS_FLOAT);
                    const char* mov_name = arg_size == float_kind_sizes[FLOAT_F64] ? "movsd" : "movss";

                    X64_emit_text(generator, "  %s %s [rsp + %d], %s", mov_name, x64_mem_size_label[arg_size], slot->sp_offset,
                                  x64_flt_reg_names[loc.reg]);
                }
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

        if (IS_LREG_IN_STACK(loc.kind)) {
            // Move into RAX.
            X64_emit_text(generator, "  mov %s, %s", x64_int_reg_names[arg_size][X64_RAX],
                          X64_print_stack_offset(generator->tmp_mem, loc.offset, arg_size));

            // Move RAX into stack slot.
            X64_emit_text(generator, "  mov %s [rsp + %d], %s", x64_mem_size_label[arg_size], slot->sp_offset,
                          x64_int_reg_names[arg_size][X64_RAX]);
        }
    }
}

static void X64_cpy_ret_small_obj(X64_Generator* generator, Type* ret_type, X64_CallValue* dst_val)
{
    X64_RegClass reg_class = X64_obj_reg_class(ret_type);
    X64_ScratchRegs ret_regs = (*x64_target.ret_regs)[reg_class];

    // Procedure returned a small struct/union/array object in registers.
    // Copy into appropriate memory location.
    if (!X64_is_obj_retarg_large(ret_type->size)) {
        X64_SIBDAddr obj_addr = {0};
        X64_get_sibd_addr(generator, &obj_addr, &dst_val->addr);

        // Copy RAX into the first 8 bytes of struct memory.
        size_t rem_amnt = X64_cpy_reg_to_mem(generator, &obj_addr, ret_regs.regs[0], ret_type->size);

        // Copy RDX into the second 8 bytes of struct memory.
        if (rem_amnt) {
            rem_amnt = X64_cpy_reg_to_mem(generator, &obj_addr, ret_regs.regs[1], rem_amnt);
            assert(!rem_amnt);
        }
    }
}

static void X64_gen_instr(X64_Generator* generator, X64_Instr* instr, bool last_instr, long bblock_id)
{
    AllocatorState mem_state = allocator_get_state(generator->tmp_mem);

    switch (instr->kind) {
    case X64_InstrAdd_R_R_KIND: {
        X64_InstrAdd_R_R* act_instr = (X64_InstrAdd_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_rr_instr(generator, "add", true, X64_REG_CLASS_INT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrSub_R_R_KIND: {
        X64_InstrSub_R_R* act_instr = (X64_InstrSub_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_rr_instr(generator, "sub", true, X64_REG_CLASS_INT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrIMul_R_R_KIND: {
        X64_InstrIMul_R_R* act_instr = (X64_InstrIMul_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_rr_instr(generator, "imul", true, X64_REG_CLASS_INT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrAnd_R_R_KIND: {
        X64_InstrAnd_R_R* act_instr = (X64_InstrAnd_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_rr_instr(generator, "and", true, X64_REG_CLASS_INT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrOr_R_R_KIND: {
        X64_InstrOr_R_R* act_instr = (X64_InstrOr_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_rr_instr(generator, "or", true, X64_REG_CLASS_INT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrXor_R_R_KIND: {
        X64_InstrXor_R_R* act_instr = (X64_InstrXor_R_R*)instr;
        u8 size = act_instr->size;

        X64_emit_rr_instr(generator, "xor", true, X64_REG_CLASS_INT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrAdd_R_I_KIND: {
        X64_InstrAdd_R_I* act_instr = (X64_InstrAdd_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_ri_instr(generator, "add", size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrSub_R_I_KIND: {
        X64_InstrSub_R_I* act_instr = (X64_InstrSub_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_ri_instr(generator, "sub", size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrIMul_R_I_KIND: {
        X64_InstrIMul_R_I* act_instr = (X64_InstrIMul_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_ri_instr(generator, "imul", size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrAnd_R_I_KIND: {
        X64_InstrAnd_R_I* act_instr = (X64_InstrAnd_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_ri_instr(generator, "and", size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrOr_R_I_KIND: {
        X64_InstrOr_R_I* act_instr = (X64_InstrOr_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_ri_instr(generator, "or", size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrXor_R_I_KIND: {
        X64_InstrXor_R_I* act_instr = (X64_InstrXor_R_I*)instr;
        u8 size = act_instr->size;

        X64_emit_ri_instr(generator, "xor", size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrAdd_R_M_KIND: {
        X64_InstrAdd_R_M* act_instr = (X64_InstrAdd_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_rm_instr(generator, "add", true, X64_REG_CLASS_INT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrSub_R_M_KIND: {
        X64_InstrSub_R_M* act_instr = (X64_InstrSub_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_rm_instr(generator, "sub", true, X64_REG_CLASS_INT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrIMul_R_M_KIND: {
        X64_InstrIMul_R_M* act_instr = (X64_InstrIMul_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_rm_instr(generator, "imul", true, X64_REG_CLASS_INT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrAnd_R_M_KIND: {
        X64_InstrAnd_R_M* act_instr = (X64_InstrAnd_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_rm_instr(generator, "and", true, X64_REG_CLASS_INT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrOr_R_M_KIND: {
        X64_InstrOr_R_M* act_instr = (X64_InstrOr_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_rm_instr(generator, "or", true, X64_REG_CLASS_INT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrXor_R_M_KIND: {
        X64_InstrXor_R_M* act_instr = (X64_InstrXor_R_M*)instr;
        u8 size = act_instr->size;

        X64_emit_rm_instr(generator, "xor", true, X64_REG_CLASS_INT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrAddSS_R_R_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F32];
        X64_InstrAddSS_R_R* act_instr = (X64_InstrAddSS_R_R*)instr;

        X64_emit_rr_instr(generator, "addss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrAddSS_R_M_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F32];
        X64_InstrAddSS_R_M* act_instr = (X64_InstrAddSS_R_M*)instr;

        X64_emit_rm_instr(generator, "addss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrAddSD_R_R_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F64];
        X64_InstrAddSD_R_R* act_instr = (X64_InstrAddSD_R_R*)instr;

        X64_emit_rr_instr(generator, "addsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrAddSD_R_M_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F64];
        X64_InstrAddSD_R_M* act_instr = (X64_InstrAddSD_R_M*)instr;

        X64_emit_rm_instr(generator, "addsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrSubSS_R_R_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F32];
        X64_InstrSubSS_R_R* act_instr = (X64_InstrSubSS_R_R*)instr;

        X64_emit_rr_instr(generator, "subss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrSubSS_R_M_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F32];
        X64_InstrSubSS_R_M* act_instr = (X64_InstrSubSS_R_M*)instr;

        X64_emit_rm_instr(generator, "subss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrSubSD_R_R_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F64];
        X64_InstrSubSD_R_R* act_instr = (X64_InstrSubSD_R_R*)instr;

        X64_emit_rr_instr(generator, "subsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrSubSD_R_M_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F64];
        X64_InstrSubSD_R_M* act_instr = (X64_InstrSubSD_R_M*)instr;

        X64_emit_rm_instr(generator, "subsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrMulSS_R_R_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F32];
        X64_InstrMulSS_R_R* act_instr = (X64_InstrMulSS_R_R*)instr;

        X64_emit_rr_instr(generator, "mulss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrMulSS_R_M_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F32];
        X64_InstrMulSS_R_M* act_instr = (X64_InstrMulSS_R_M*)instr;

        X64_emit_rm_instr(generator, "mulss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrMulSD_R_R_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F64];
        X64_InstrMulSD_R_R* act_instr = (X64_InstrMulSD_R_R*)instr;

        X64_emit_rr_instr(generator, "mulsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrMulSD_R_M_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F64];
        X64_InstrMulSD_R_M* act_instr = (X64_InstrMulSD_R_M*)instr;

        X64_emit_rm_instr(generator, "mulsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrDivSS_R_R_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F32];
        X64_InstrDivSS_R_R* act_instr = (X64_InstrDivSS_R_R*)instr;

        X64_emit_rr_instr(generator, "divss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrDivSS_R_M_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F32];
        X64_InstrDivSS_R_M* act_instr = (X64_InstrDivSS_R_M*)instr;

        X64_emit_rm_instr(generator, "divss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrDivSD_R_R_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F64];
        X64_InstrDivSD_R_R* act_instr = (X64_InstrDivSD_R_R*)instr;

        X64_emit_rr_instr(generator, "divsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrDivSD_R_M_KIND: {
        const u8 size = float_kind_sizes[FLOAT_F64];
        X64_InstrDivSD_R_M* act_instr = (X64_InstrDivSD_R_M*)instr;

        X64_emit_rm_instr(generator, "divsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrDiv_R_KIND: {
        X64_InstrDiv_R* act_instr = (X64_InstrDiv_R*)instr;
        const u8 size = act_instr->size;

        X64_LRegLoc src_loc = X64_lreg_loc(generator, act_instr->src);
        bool src_is_reg = IS_LREG_IN_REG(src_loc.kind);
        const char* src_op_str =
            src_is_reg ? x64_int_reg_names[size][src_loc.reg] : X64_print_stack_offset(generator->tmp_mem, src_loc.offset, size);

        X64_emit_text(generator, "  div %s", src_op_str);

        break;
    }
    case X64_InstrIDiv_R_KIND: {
        X64_InstrIDiv_R* act_instr = (X64_InstrIDiv_R*)instr;
        const u8 size = act_instr->size;

        X64_LRegLoc src_loc = X64_lreg_loc(generator, act_instr->src);
        bool src_is_reg = IS_LREG_IN_REG(src_loc.kind);
        const char* src_op_str =
            src_is_reg ? x64_int_reg_names[size][src_loc.reg] : X64_print_stack_offset(generator->tmp_mem, src_loc.offset, size);

        X64_emit_text(generator, "  idiv %s", src_op_str);

        break;
    }
    case X64_InstrDiv_M_KIND: {
        X64_InstrDiv_M* act_instr = (X64_InstrDiv_M*)instr;
        const u8 size = act_instr->size;
        X64_SIBDAddr op_addr = {0};

        X64_get_sibd_addr(generator, &op_addr, &act_instr->src);
        X64_emit_text(generator, "  div %s", X64_print_sibd_addr(generator->tmp_mem, &op_addr, size));

        break;
    }
    case X64_InstrIDiv_M_KIND: {
        X64_InstrIDiv_M* act_instr = (X64_InstrIDiv_M*)instr;
        const u8 size = act_instr->size;
        X64_SIBDAddr op_addr = {0};

        X64_get_sibd_addr(generator, &op_addr, &act_instr->src);
        X64_emit_text(generator, "  idiv %s", X64_print_sibd_addr(generator->tmp_mem, &op_addr, size));

        break;
    }
    case X64_InstrSExtAxToDx_KIND: {
        X64_InstrSExtAxToDx* act_instr = (X64_InstrSExtAxToDx*)instr;
        X64_emit_text(generator, "  %s", x64_sext_ax_into_dx[act_instr->size]);
        break;
    }
    case X64_InstrSar_R_R_KIND: {
        X64_InstrSar_R_R* act_instr = (X64_InstrSar_R_R*)instr;
        const u8 dst_size = act_instr->size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, act_instr->src);
        bool dst_in_reg = IS_LREG_IN_REG(dst_loc.kind);

        assert(IS_LREG_IN_REG(src_loc.kind) && src_loc.reg == X64_RCX);

        const char* dst_op_str = dst_in_reg ? x64_int_reg_names[dst_size][dst_loc.reg] :
                                              X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, dst_size);

        X64_emit_text(generator, "  sar %s, %s", dst_op_str, x64_int_reg_names[1][X64_RCX]);
        break;
    }
    case X64_InstrShl_R_R_KIND: {
        X64_InstrShl_R_R* act_instr = (X64_InstrShl_R_R*)instr;
        const u8 dst_size = act_instr->size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, act_instr->src);
        bool dst_in_reg = IS_LREG_IN_REG(dst_loc.kind);

        assert(IS_LREG_IN_REG(src_loc.kind) && src_loc.reg == X64_RCX);

        const char* dst_op_str = dst_in_reg ? x64_int_reg_names[dst_size][dst_loc.reg] :
                                              X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, dst_size);

        X64_emit_text(generator, "  shl %s, %s", dst_op_str, x64_int_reg_names[1][X64_RCX]);
        break;
    }
    case X64_InstrSar_R_I_KIND: {
        X64_InstrSar_R_I* act_instr = (X64_InstrSar_R_I*)instr;
        const u8 dst_size = act_instr->size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        bool dst_in_reg = IS_LREG_IN_REG(dst_loc.kind);
        const char* dst_op_str = dst_in_reg ? x64_int_reg_names[dst_size][dst_loc.reg] :
                                              X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, dst_size);

        X64_emit_text(generator, "  sar %s, %d", dst_op_str, act_instr->src.as_int._u8);
        break;
    }
    case X64_InstrShl_R_I_KIND: {
        X64_InstrShl_R_I* act_instr = (X64_InstrShl_R_I*)instr;
        const u8 dst_size = act_instr->size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        bool dst_in_reg = IS_LREG_IN_REG(dst_loc.kind);
        const char* dst_op_str = dst_in_reg ? x64_int_reg_names[dst_size][dst_loc.reg] :
                                              X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, dst_size);

        X64_emit_text(generator, "  shl %s, %d", dst_op_str, act_instr->src.as_int._u8);
        break;
    }
    case X64_InstrNeg_KIND: {
        X64_InstrNeg* act_instr = (X64_InstrNeg*)instr;
        const u8 size = act_instr->size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        bool dst_in_reg = IS_LREG_IN_REG(dst_loc.kind);
        const char* dst_op_str =
            dst_in_reg ? x64_int_reg_names[size][dst_loc.reg] : X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, size);

        X64_emit_text(generator, "  neg %s", dst_op_str);
        break;
    }
    case X64_InstrNot_KIND: {
        X64_InstrNot* act_instr = (X64_InstrNot*)instr;
        const u8 size = act_instr->size;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        bool dst_in_reg = IS_LREG_IN_REG(dst_loc.kind);
        const char* dst_op_str =
            dst_in_reg ? x64_int_reg_names[size][dst_loc.reg] : X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, size);

        X64_emit_text(generator, "  not %s", dst_op_str);
        break;
    }
    case X64_InstrRepMovsb_KIND: {
        X64_emit_text(generator, "  rep movsb");
        break;
    }
    case X64_InstrRepStosb_KIND: {
        X64_emit_text(generator, "  rep stosb");
        break;
    }
    case X64_InstrSyscall_KIND: {
        X64_emit_text(generator, "  syscall");
        break;
    }
    case X64_InstrMov_R_RH_KIND: {
        X64_InstrMov_R_RH* act_instr = (X64_InstrMov_R_RH*)instr;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, act_instr->src);

        assert(IS_LREG_IN_REG(src_loc.kind));

        const char* src_h_name = x64_reg_h_names[src_loc.reg];

        assert(src_h_name);

        switch (dst_loc.kind) {
        case X64_LREG_LOC_REG: {
            X64_emit_text(generator, "  mov %s, %s", x64_int_reg_names[1][dst_loc.reg], src_h_name);
            break;
        }
        case X64_LREG_LOC_STACK: {
            X64_emit_text(generator, "  mov %s, %s", X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, 1), src_h_name);
            break;
        }
        default:
            NIBBLE_FATAL_EXIT("Invalid dst_loc.kind in X64_InstrMov_R_RH_KIND generation.");
            break;
        }
        break;
    }
    case X64_InstrMov_R_R_KIND: {
        X64_InstrMov_R_R* act_instr = (X64_InstrMov_R_R*)instr;
        const u8 size = act_instr->size;

        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, act_instr->src);

        bool same_ops = (dst_loc.kind == src_loc.kind) && ((IS_LREG_IN_REG(dst_loc.kind) && (dst_loc.reg == src_loc.reg)) ||
                                                           (IS_LREG_IN_STACK(dst_loc.kind) && (dst_loc.offset == src_loc.offset)));

        if (!same_ops) {
            X64_emit_rr_instr(generator, "mov", true, X64_REG_CLASS_INT, size, act_instr->dst, size, act_instr->src);
        }
        break;
    }
    case X64_InstrMov_R_I_KIND: {
        X64_InstrMov_R_I* act_instr = (X64_InstrMov_R_I*)instr;
        const u8 size = act_instr->size;

        X64_emit_ri_instr(generator, "mov", size, act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrMov_R_M_KIND: {
        X64_InstrMov_R_M* act_instr = (X64_InstrMov_R_M*)instr;
        const u8 size = act_instr->size;

        X64_emit_rm_instr(generator, "mov", true, X64_REG_CLASS_INT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrMov_M_R_KIND: {
        X64_InstrMov_M_R* act_instr = (X64_InstrMov_M_R*)instr;
        const u8 size = act_instr->size;

        X64_emit_mr_instr(generator, "mov", size, &act_instr->dst, X64_REG_CLASS_INT, size, act_instr->src);
        break;
    }
    case X64_InstrMov_M_I_KIND: {
        X64_InstrMov_M_I* act_instr = (X64_InstrMov_M_I*)instr;
        const u8 size = act_instr->size;

        X64_emit_mi_instr(generator, "mov", size, &act_instr->dst, size, act_instr->src);
        break;
    }
    case X64_InstrMovZX_R_R_KIND: {
        X64_InstrMovZX_R_R* act_instr = (X64_InstrMovZX_R_R*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;

        // There is no encoding for an instruction that zero-extends a 4-byte source to an 8-byte destination! Also, note that
        // an instruction like mov eax, __ clears the upper 4 bytes of eax.
        // See: https://stackoverflow.com/a/51394642
        if (src_size != 4) {
            X64_emit_rr_instr(generator, "movzx", true, X64_REG_CLASS_INT, dst_size, act_instr->dst, src_size, act_instr->src);
        }
        // EX: Instead of movzx rax, edi (invalid), use mov eax, edi to zero-extend edi into rax.
        else {
            assert(dst_size == X64_MAX_INT_REG_SIZE);

            // NOTE: Not necessary if a previous instruction already cleared the upper 4-bytes of the dest reg with a mov instruction.
            // We would need to track the "zxt" state of all registers: if mov rx, _ => rx is "zxt", otherwise if <not_mov> rx, _ =>
            // rx is NOT "zxt".
            X64_emit_rr_instr(generator, "mov", true, X64_REG_CLASS_INT, 4, act_instr->dst, 4, act_instr->src);
        }
        break;
    }
    case X64_InstrMovZX_R_M_KIND: {
        X64_InstrMovZX_R_M* act_instr = (X64_InstrMovZX_R_M*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;

        // There is no encoding for an instruction that zero-extends a 4-byte source to an 8-byte destination! Also, note that
        // an instruction like mov eax, __ clears the upper 4 bytes of eax.
        // See: https://stackoverflow.com/a/51394642
        if (src_size != 4) {
            X64_emit_rm_instr(generator, "movzx", true, X64_REG_CLASS_INT, dst_size, act_instr->dst, src_size, &act_instr->src);
        }
        // EX: Instead of movzx rax, edi (invalid), use mov eax, edi to zero-extend edi into rax.
        else {
            assert(dst_size == X64_MAX_INT_REG_SIZE);

            // NOTE: Not necessary if a previous instruction already cleared the upper 4-bytes of the dest reg with a mov instruction.
            // We would need to track the "zxt" state of all registers: if mov rx, _ => rx is "zxt", otherwise if <not_mov> rx, _ =>
            // rx is NOT "zxt".
            X64_emit_rm_instr(generator, "mov", true, X64_REG_CLASS_INT, 4, act_instr->dst, 4, &act_instr->src);
        }
        break;
    }
    case X64_InstrMovSX_R_R_KIND: {
        X64_InstrMovSX_R_R* act_instr = (X64_InstrMovSX_R_R*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;
        const char* movsx = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? "movsxd" : "movsx";

        X64_emit_rr_instr(generator, movsx, true, X64_REG_CLASS_INT, dst_size, act_instr->dst, src_size, act_instr->src);
        break;
    }
    case X64_InstrMovSX_R_M_KIND: {
        X64_InstrMovSX_R_M* act_instr = (X64_InstrMovSX_R_M*)instr;
        const u8 dst_size = act_instr->dst_size;
        const u8 src_size = act_instr->src_size;
        const char* movsx = src_size >= builtin_types[BUILTIN_TYPE_U32].type->size ? "movsxd" : "movsx";

        X64_emit_rm_instr(generator, movsx, true, X64_REG_CLASS_INT, dst_size, act_instr->dst, src_size, &act_instr->src);
        break;
    }
    case X64_InstrMovSS_R_R_KIND: {
        X64_InstrMovSS_R_R* act_instr = (X64_InstrMovSS_R_R*)instr;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, act_instr->src);

        bool same_ops = (dst_loc.kind == src_loc.kind) && ((IS_LREG_IN_REG(dst_loc.kind) && (dst_loc.reg == src_loc.reg)) ||
                                                           (IS_LREG_IN_STACK(dst_loc.kind) && (dst_loc.offset == src_loc.offset)));

        if (!same_ops) {
            const u8 size = float_kind_sizes[FLOAT_F32];
            X64_emit_rr_instr(generator, "movss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        }
        break;
    }
    case X64_InstrMovSS_R_M_KIND: {
        X64_InstrMovSS_R_M* act_instr = (X64_InstrMovSS_R_M*)instr;
        const u8 size = float_kind_sizes[FLOAT_F32];

        X64_emit_rm_instr(generator, "movss", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrMovSS_M_R_KIND: {
        X64_InstrMovSS_M_R* act_instr = (X64_InstrMovSS_M_R*)instr;
        const u8 size = float_kind_sizes[FLOAT_F32];

        X64_emit_mr_instr(generator, "movss", size, &act_instr->dst, X64_REG_CLASS_FLOAT, size, act_instr->src);
        break;
    }
    case X64_InstrMovSD_R_R_KIND: {
        X64_InstrMovSD_R_R* act_instr = (X64_InstrMovSD_R_R*)instr;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);
        X64_LRegLoc src_loc = X64_lreg_loc(generator, act_instr->src);

        bool same_ops = (dst_loc.kind == src_loc.kind) && ((IS_LREG_IN_REG(dst_loc.kind) && (dst_loc.reg == src_loc.reg)) ||
                                                           (IS_LREG_IN_STACK(dst_loc.kind) && (dst_loc.offset == src_loc.offset)));

        if (!same_ops) {
            const u8 size = float_kind_sizes[FLOAT_F64];
            X64_emit_rr_instr(generator, "movsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, act_instr->src);
        }
        break;
    }
    case X64_InstrMovSD_R_M_KIND: {
        X64_InstrMovSD_R_M* act_instr = (X64_InstrMovSD_R_M*)instr;
        const u8 size = float_kind_sizes[FLOAT_F64];

        X64_emit_rm_instr(generator, "movsd", true, X64_REG_CLASS_FLOAT, size, act_instr->dst, size, &act_instr->src);
        break;
    }
    case X64_InstrMovSD_M_R_KIND: {
        X64_InstrMovSD_M_R* act_instr = (X64_InstrMovSD_M_R*)instr;
        u32 size = float_kind_sizes[FLOAT_F64];

        X64_emit_mr_instr(generator, "movsd", size, &act_instr->dst, X64_REG_CLASS_FLOAT, size, act_instr->src);
        break;
    }
    case X64_InstrCvtSS2SD_R_R_KIND: { // f32 to f64
        X64_InstrCvtSS2SD_R_R* act_instr = (X64_InstrCvtSS2SD_R_R*)instr;

        X64_emit_flt2flt_rr_instr(generator, "cvtss2sd", float_kind_sizes[FLOAT_F32], float_kind_sizes[FLOAT_F64], act_instr->src,
                                  act_instr->dst);
        break;
    }
    case X64_InstrCvtSD2SS_R_R_KIND: { // f64 to f32
        X64_InstrCvtSD2SS_R_R* act_instr = (X64_InstrCvtSD2SS_R_R*)instr;

        X64_emit_flt2flt_rr_instr(generator, "cvtsd2ss", float_kind_sizes[FLOAT_F64], float_kind_sizes[FLOAT_F32], act_instr->src,
                                  act_instr->dst);
        break;
    }
    case X64_InstrCvtSS2SD_R_M_KIND: { // f32 (in memory) to f64
        X64_InstrCvtSS2SD_R_M* act_instr = (X64_InstrCvtSS2SD_R_M*)instr;
        X64_SIBDAddr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(generator, &src_addr, &act_instr->src);

        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, float_kind_sizes[FLOAT_F64], true, used_regs);
        const char* dst_name = x64_flt_reg_names[dst_reg];

        X64_emit_text(generator, "  cvtss2sd %s, %s", dst_name,
                      X64_print_sibd_addr(generator->tmp_mem, &src_addr, float_kind_sizes[FLOAT_F32]));
        X64_end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrCvtSD2SS_R_M_KIND: { // f64 (in memory) to f32
        X64_InstrCvtSD2SS_R_M* act_instr = (X64_InstrCvtSD2SS_R_M*)instr;
        X64_SIBDAddr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(generator, &src_addr, &act_instr->src);

        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, float_kind_sizes[FLOAT_F32], true, used_regs);
        const char* dst_name = x64_flt_reg_names[dst_reg];

        X64_emit_text(generator, "  cvtsd2ss %s, %s", dst_name,
                      X64_print_sibd_addr(generator->tmp_mem, &src_addr, float_kind_sizes[FLOAT_F64]));
        X64_end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrCvtSS2SI_R_R_KIND: { // f32 to integer
        X64_InstrCvtSS2SI_R_R* act_instr = (X64_InstrCvtSS2SI_R_R*)instr;

        X64_emit_flt2int_rr_instr(generator, "cvttss2si", float_kind_sizes[FLOAT_F32], act_instr->dst_size, act_instr->src,
                                  act_instr->dst);
        break;
    }
    case X64_InstrCvtSD2SI_R_R_KIND: { // f64 to integer
        X64_InstrCvtSD2SI_R_R* act_instr = (X64_InstrCvtSD2SI_R_R*)instr;

        X64_emit_flt2int_rr_instr(generator, "cvttsd2si", float_kind_sizes[FLOAT_F64], act_instr->dst_size, act_instr->src,
                                  act_instr->dst);
        break;
    }
    case X64_InstrCvtSS2SI_R_M_KIND: { // f32 (in memory) to integer
        X64_InstrCvtSS2SI_R_M* act_instr = (X64_InstrCvtSS2SI_R_M*)instr;
        X64_SIBDAddr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(generator, &src_addr, &act_instr->src);

        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, act_instr->dst_size, true, used_regs);

        u32 dst_size = act_instr->dst_size <= 4 ? 4 : 8; // TODO: No magic allowed.
        const char* dst_name = x64_int_reg_names[dst_size][dst_reg];

        X64_emit_text(generator, "  cvttss2si %s, %s", dst_name,
                      X64_print_sibd_addr(generator->tmp_mem, &src_addr, float_kind_sizes[FLOAT_F32]));
        X64_end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrCvtSD2SI_R_M_KIND: { // f64 (in memory) to integer
        X64_InstrCvtSD2SI_R_M* act_instr = (X64_InstrCvtSD2SI_R_M*)instr;
        X64_SIBDAddr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(generator, &src_addr, &act_instr->src);

        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_INT, act_instr->dst, act_instr->dst_size, true, used_regs);

        u32 dst_size = act_instr->dst_size <= 4 ? 4 : 8; // TODO: No magic allowed.
        const char* dst_name = x64_int_reg_names[dst_size][dst_reg];

        X64_emit_text(generator, "  cvttsd2si %s, %s", dst_name,
                      X64_print_sibd_addr(generator->tmp_mem, &src_addr, float_kind_sizes[FLOAT_F64]));
        X64_end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrCvtSI2SS_R_R_KIND: { // integer -> f32
        X64_InstrCvtSI2SS_R_R* act_instr = (X64_InstrCvtSI2SS_R_R*)instr;

        X64_emit_int2flt_rr_instr(generator, "cvtsi2ss", act_instr->src_size, float_kind_sizes[FLOAT_F32], act_instr->src,
                                  act_instr->dst);
        break;
    }
    case X64_InstrCvtSI2SD_R_R_KIND: { // integer -> f64
        X64_InstrCvtSI2SD_R_R* act_instr = (X64_InstrCvtSI2SD_R_R*)instr;

        X64_emit_int2flt_rr_instr(generator, "cvtsi2sd", act_instr->src_size, float_kind_sizes[FLOAT_F64], act_instr->src,
                                  act_instr->dst);
        break;
    }
    case X64_InstrCvtSI2SS_R_M_KIND: { // integer (in memory) -> f32
        X64_InstrCvtSI2SS_R_M* act_instr = (X64_InstrCvtSI2SS_R_M*)instr;
        X64_SIBDAddr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(generator, &src_addr, &act_instr->src);
        u8 dst_size = float_kind_sizes[FLOAT_F32];

        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, dst_size, true, used_regs);
        const char* dst_name = x64_flt_reg_names[dst_reg];

        u8 src_size = act_instr->src_size;

        assert(src_size >= 4);
        X64_emit_text(generator, "  cvtsi2ss %s, %s", dst_name, X64_print_sibd_addr(generator->tmp_mem, &src_addr, src_size));
        X64_end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrCvtSI2SD_R_M_KIND: { // integer (in memory) -> f64
        X64_InstrCvtSI2SD_R_M* act_instr = (X64_InstrCvtSI2SD_R_M*)instr;
        X64_SIBDAddr src_addr = {0};
        u32 used_regs = X64_get_sibd_addr(generator, &src_addr, &act_instr->src);
        u8 dst_size = float_kind_sizes[FLOAT_F64];

        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg dst_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->dst, dst_size, true, used_regs);
        const char* dst_name = x64_flt_reg_names[dst_reg];

        u8 src_size = act_instr->src_size;

        assert(src_size >= 4);
        X64_emit_text(generator, "  cvtsi2sd %s, %s", dst_name, X64_print_sibd_addr(generator->tmp_mem, &src_addr, src_size));
        X64_end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrLEA_KIND: {
        X64_InstrLEA* act_instr = (X64_InstrLEA*)instr;

        X64_emit_rm_instr(generator, "lea", true, X64_REG_CLASS_INT, X64_MAX_INT_REG_SIZE, act_instr->dst, 0, &act_instr->mem);
        break;
    }
    case X64_InstrCmp_R_R_KIND: {
        X64_InstrCmp_R_R* act_instr = (X64_InstrCmp_R_R*)instr;
        const u8 size = act_instr->size;

        X64_emit_rr_instr(generator, "cmp", false, X64_REG_CLASS_INT, size, act_instr->op1, size, act_instr->op2);
        break;
    }
    case X64_InstrCmp_R_I_KIND: {
        X64_InstrCmp_R_I* act_instr = (X64_InstrCmp_R_I*)instr;
        const u8 size = act_instr->size;

        X64_emit_ri_instr(generator, "cmp", size, act_instr->op1, size, act_instr->op2);
        break;
    }
    case X64_InstrCmp_R_M_KIND: {
        X64_InstrCmp_R_M* act_instr = (X64_InstrCmp_R_M*)instr;
        const u8 size = act_instr->size;

        X64_emit_rm_instr(generator, "cmp", false, X64_REG_CLASS_INT, size, act_instr->op1, size, &act_instr->op2);
        break;
    }
    case X64_InstrCmp_M_R_KIND: {
        X64_InstrCmp_M_R* act_instr = (X64_InstrCmp_M_R*)instr;
        const u8 size = act_instr->size;

        X64_emit_mr_instr(generator, "cmp", size, &act_instr->op1, X64_REG_CLASS_INT, size, act_instr->op2);
        break;
    }
    case X64_InstrCmp_M_I_KIND: {
        X64_InstrCmp_M_I* act_instr = (X64_InstrCmp_M_I*)instr;
        const u8 size = act_instr->size;

        X64_emit_mi_instr(generator, "cmp", size, &act_instr->op1, size, act_instr->op2);
        break;
    }
    case X64_InstrUComiSS_R_R_KIND: {
        X64_InstrUComiSS_R_R* act_instr = (X64_InstrUComiSS_R_R*)instr;

        X64_emit_flt_cmp_rr_instr(generator, "ucomiss", FLOAT_F32, act_instr->op1, act_instr->op2);
        break;
    }
    case X64_InstrUComiSD_R_R_KIND: {
        X64_InstrUComiSD_R_R* act_instr = (X64_InstrUComiSD_R_R*)instr;

        X64_emit_flt_cmp_rr_instr(generator, "ucomisd", FLOAT_F64, act_instr->op1, act_instr->op2);
        break;
    }
    case X64_InstrUComiSS_R_M_KIND: {
        X64_InstrUComiSS_R_M* act_instr = (X64_InstrUComiSS_R_M*)instr;
        X64_SIBDAddr op2_addr = {0};
        u32 used_regs = X64_get_sibd_addr(generator, &op2_addr, &act_instr->op2);

        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->op1, float_kind_sizes[FLOAT_F32], false, used_regs);
        const char* op1_name = x64_flt_reg_names[op1_reg];

        X64_emit_text(generator, "  ucomiss %s, %s", op1_name,
                      X64_print_sibd_addr(generator->tmp_mem, &op2_addr, float_kind_sizes[FLOAT_F32]));
        X64_end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrUComiSD_R_M_KIND: {
        X64_InstrUComiSD_R_M* act_instr = (X64_InstrUComiSD_R_M*)instr;
        X64_SIBDAddr op2_addr = {0};
        u32 used_regs = X64_get_sibd_addr(generator, &op2_addr, &act_instr->op2);

        X64_RegGroup tmp_group = X64_begin_reg_group(generator);
        X64_Reg op1_reg = X64_get_reg(&tmp_group, X64_REG_CLASS_FLOAT, act_instr->op1, float_kind_sizes[FLOAT_F64], false, used_regs);
        const char* op1_name = x64_flt_reg_names[op1_reg];

        X64_emit_text(generator, "  ucomisd %s, %s", op1_name,
                      X64_print_sibd_addr(generator->tmp_mem, &op2_addr, float_kind_sizes[FLOAT_F64]));
        X64_end_reg_group(&tmp_group);
        break;
    }
    case X64_InstrJmp_KIND: {
        X64_InstrJmp* act_instr = (X64_InstrJmp*)instr;
        long target_id = act_instr->target->id;

        if (target_id != bblock_id + 1) {
            X64_emit_text(generator, "  jmp %s", X64_get_label(generator, target_id));
        }
        break;
    }
    case X64_InstrJmpCC_KIND: {
        X64_InstrJmpCC* act_instr = (X64_InstrJmpCC*)instr;
        X64_emit_text(generator, "  j%s %s", x64_condition_codes[act_instr->cond], X64_get_label(generator, act_instr->true_bb->id));
        break;
    }
    case X64_InstrSetCC_KIND: {
        X64_InstrSetCC* act_instr = (X64_InstrSetCC*)instr;
        X64_LRegLoc dst_loc = X64_lreg_loc(generator, act_instr->dst);

        if (IS_LREG_IN_REG(dst_loc.kind)) {
            X64_emit_text(generator, "  set%s %s", x64_condition_codes[act_instr->cond], x64_int_reg_names[1][dst_loc.reg]);
        }
        else {
            assert(IS_LREG_IN_STACK(dst_loc.kind));
            X64_emit_text(generator, "  set%s %s", x64_condition_codes[act_instr->cond],
                          X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, 1));
        }
        break;
    }
    case X64_InstrRet_KIND: {
        if (!last_instr) { // Not the last instruction
            X64_emit_text(generator, "  jmp end.%s", symbol_mangled_name(generator->tmp_mem, generator->curr_proc.sym));
        }

        break;
    }
    case X64_InstrCall_KIND:
    case X64_InstrCall_R_KIND: {
        Type* proc_type;
        X64_CallValue dst_val;
        u32 num_args;
        X64_InstrCallArg* args;
        X64_StackArgsInfo stack_args_info;
        unsigned save_reg_mask;

        if (instr->kind == X64_InstrCall_KIND) {
            X64_InstrCall* instr_call = (X64_InstrCall*)instr;

            proc_type = instr_call->sym->type;
            dst_val = instr_call->dst;
            num_args = instr_call->num_args;
            args = instr_call->args;
            stack_args_info = instr_call->stack_info;
            save_reg_mask = instr_call->save_reg_mask;
        }
        else {
            assert(instr->kind == X64_InstrCall_R_KIND);
            X64_InstrCall_R* instr_call_r = (X64_InstrCall_R*)instr;

            proc_type = instr_call_r->proc_type;
            dst_val = instr_call_r->dst;
            num_args = instr_call_r->num_args;
            args = instr_call_r->args;
            stack_args_info = instr_call_r->stack_info;
            save_reg_mask = instr_call_r->save_reg_mask;
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
            X64_Reg dst_reg = (*x64_target.arg_regs)[X64_REG_CLASS_INT].regs[0];
            X64_SIBDAddr obj_addr = {0};
            X64_get_sibd_addr(generator, &obj_addr, &dst_val.addr);

            X64_emit_text(generator, "  lea %s, %s", x64_int_reg_names[X64_MAX_INT_REG_SIZE][dst_reg],
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

        if (instr->kind == X64_InstrCall_KIND) {
            X64_InstrCall* instr_call = (X64_InstrCall*)instr;

            X64_emit_text(generator, "  call %s", symbol_mangled_name(generator->tmp_mem, instr_call->sym));
        }
        else {
            X64_InstrCall_R* instr_call_r = (X64_InstrCall_R*)instr;
            X64_LRegLoc proc_reg_loc = X64_lreg_loc(generator, instr_call_r->proc_loc);
            const char* call_op_str = IS_LREG_IN_REG(proc_reg_loc.kind) ?
                                          x64_int_reg_names[PTR_SIZE][proc_reg_loc.reg] :
                                          X64_print_stack_offset(generator->tmp_mem, proc_reg_loc.offset, PTR_SIZE);

            X64_emit_text(generator, "  call %s", call_op_str);
        }

        // Move return value (if any) to appropriate register.
        if (ret_type != builtin_types[BUILTIN_TYPE_VOID].type) {
            if (!type_is_obj_like(ret_type)) {
                // Returns a primitive type.

                X64_LRegLoc dst_loc = X64_lreg_loc(generator, dst_val.reg);

                X64_RegClass ret_class;
                X64_Reg ret_reg;
                const char* ret_reg_name;
                const char* mov_instr_name;

                if (ret_type->kind == TYPE_FLOAT) {
                    ret_class = X64_REG_CLASS_FLOAT;
                    ret_reg = (*x64_target.ret_regs)[ret_class].regs[0];
                    ret_reg_name = x64_flt_reg_names[ret_reg];
                    mov_instr_name = ret_type->as_float.kind == FLOAT_F64 ? "movsd" : "movss";
                }
                else {
                    ret_class = X64_REG_CLASS_INT;
                    ret_reg = (*x64_target.ret_regs)[ret_class].regs[0];
                    ret_reg_name = x64_int_reg_names[ret_type->size][ret_reg];
                    mov_instr_name = "mov";
                }

                if (IS_LREG_IN_STACK(dst_loc.kind)) {
                    // Move result (in RAX/XMM0) to stack offset.
                    // Ex: mov qword [rbp + x], rax
                    X64_emit_text(generator, "  %s %s, %s", mov_instr_name,
                                  X64_print_stack_offset(generator->tmp_mem, dst_loc.offset, ret_type->size), ret_reg_name);
                }
                else {
                    assert(IS_LREG_IN_REG(dst_loc.kind));

                    if (dst_loc.reg != ret_reg) {
                        const char* dst_reg_name = X64_reg_name(dst_loc.reg, ret_type->size);

                        // Move result (in RAX/XMMO) to allocated result register.
                        X64_emit_text(generator, "  %s %s, %s", mov_instr_name, dst_reg_name, ret_reg_name);
                    }
                }
            }
            else {
                X64_cpy_ret_small_obj(generator, ret_type, &dst_val);
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
    X64_LIRBuilder builder = {.arena = generator->gen_mem};

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
    for (u32 i = 0; i < num_lreg_ranges; i += 1) {
        if (X64_find_alias_reg(&builder, i) != i) continue;

        X64_LRegRange* rng = builder.lreg_ranges + i;
        X64_LRegLoc* loc = &rng->loc;

        if (IS_LREG_IN_REG(loc->kind)) {
            ftprint_out("\tr%u -> %s", i, x64_int_reg_names[8][loc->reg]);
        }
        else {
            assert(IS_LREG_IN_STACK(loc->kind));
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
    Allocator* tmp_mem = generator->tmp_mem;
    AllocatorState mem_state = allocator_get_state(tmp_mem);

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

        Type* ftype = float_lit->kind == FLOAT_F64 ? builtin_types[BUILTIN_TYPE_F64].type : builtin_types[BUILTIN_TYPE_F32].type;
        ConstExpr const_expr = {.kind = CONST_EXPR_FLOAT_LIT, .type = ftype, .float_lit = float_lit};

        X64_emit_global_data(generator, X64_float_lit_mangled_name(tmp_mem, float_lit), &const_expr);
    }

    X64_emit_data(generator, "\nSECTION .data\n");

    size_t num_vars = vars->num_elems;

    for (u32 i = 0; i < num_vars; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(vars, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);

        X64_emit_global_data(generator, symbol_mangled_name(tmp_mem, sym), &sym->as_var.const_expr);
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
