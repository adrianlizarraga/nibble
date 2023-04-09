#include "allocator.h"
#include "array.h"
#include "cstring.h"
#include "nibble.h"
#include "x64_gen/lir_to_x64.h"
#include "x64_gen/regs.h"

#define X64_NASM_STR_LIT_PRE "__nibble_str_lit_"
#define X64_NASM_FLOAT_LIT_PRE "__nibble_float_lit_"

// Print formatted text (no newline)
#define X64_NASM_PRINT_F(str_builder, fmt, ...) ftprint_char_array(&(str_builder), false, fmt, __VA_ARGS__)

// Print line of text
#define X64_NASM_PRINT_L(str_builder, text) ftprint_char_array(&(str_builder), false, text "\n")

// Print tabbed line of text
#define X64_NASM_PRINT_TL(str_builder, text) ftprint_char_array(&(str_builder), false, "  " text "\n")

// Print formatted line
#define X64_NASM_PRINT_FL(str_builder, fmt, ...) ftprint_char_array(&(str_builder), false, fmt "\n", __VA_ARGS__)

// Print formatted tabbed line
#define X64_NASM_PRINT_FTL(str_builder, fmt, ...) ftprint_char_array(&(str_builder), false, "  " fmt "\n", __VA_ARGS__)

static const char* x64_nasm_int_reg_names[X64_MAX_INT_REG_SIZE + 1][X64_REG_COUNT] = {
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

static const char* X64_nasm_float_lit_mangled_name(Allocator* alloc, FloatLit* float_lit)
{
    char* dstr = array_create(alloc, char, 16);
    ftprint_char_array(&dstr, true, "%s_%llu", X64_NASM_FLOAT_LIT_PRE, float_lit->id);

    return dstr;
}

static char* X64_nasm_get_label(Allocator* mem, size_t proc_id, size_t instr_num)
{
    char* dstr = array_create(mem, char, 8);
    ftprint_char_array(&dstr, true, "L.%llu.%llu", proc_id, instr_num);

    return dstr;
}

static char* X64_nasm_print_sibd_addr(Allocator* allocator, X64_SIBD_Addr* addr, u32 mem_label_size)
{
    assert(mem_label_size <= X64_MAX_INT_REG_SIZE);
    char* dstr = array_create(allocator, char, 16);
    const char* mem_label = mem_label_size ? x64_mem_size_label[mem_label_size] : "";

    if (addr->kind == X64__SIBD_ADDR_STR_LIT) {
        ftprint_char_array(&dstr, true, "[rel %s_%llu]", X64_NASM_STR_LIT_PRE, addr->str_lit->id);
    }
    else if (addr->kind == X64__SIBD_ADDR_FLOAT_LIT) {
        ftprint_char_array(&dstr, true, "[rel %s_%llu]", X64_NASM_FLOAT_LIT_PRE, addr->float_lit->id);
    }
    else if (addr->kind == X64__SIBD_ADDR_GLOBAL) {
        ftprint_char_array(&dstr, true, "%s [rel %s]", mem_label, symbol_mangled_name(allocator, addr->global));
    }
    else {
        assert(addr->kind == X64__SIBD_ADDR_LOCAL);
        bool has_base = addr->local.base_reg < X64_REG_COUNT;
        bool has_index = addr->local.scale && (addr->local.index_reg < X64_REG_COUNT);
        bool has_disp = addr->local.disp != 0;

        if (has_base) {
            const char* base_reg_name = x64_nasm_int_reg_names[X64_MAX_INT_REG_SIZE][addr->local.base_reg];

            if (has_index) {
                const char* index_reg_name = x64_nasm_int_reg_names[X64_MAX_INT_REG_SIZE][addr->local.index_reg];

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
            const char* index_reg_name = x64_nasm_int_reg_names[X64_MAX_INT_REG_SIZE][addr->local.index_reg];

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

static char* X64_nasm_print_imm(Allocator* arena, u64 imm, u8 size)
{
    char* dstr = array_create(arena, char, 8);

    switch (size) {
    case 1:
        ftprint_char_array(&dstr, false, "0x%X", (u8)imm);
        break;
    case 2:
        ftprint_char_array(&dstr, false, "0x%X", (u16)imm);
        break;
    case 4:
        ftprint_char_array(&dstr, false, "0x%X", (u32)imm);
        break;
    case 8:
        ftprint_char_array(&dstr, false, "0x%lX", imm);
        break;
    default:
        assert(0);
        break;
    }

    array_push(dstr, '\0');

    return dstr;
}

static void X64_nasm_print_global_val(Allocator* allocator, const ConstExpr* const_expr, char** line);

static void X64_nasm_print_global_zero_bytes(char** line, size_t size)
{
    ftprint_char_array(line, false, "%s ", x64_data_size_label[1]);

    for (size_t j = 0; j < size; j += 1) {
        char inner_sep = (j == size - 1) ? '\n' : ',';
        ftprint_char_array(line, false, "0x%.2X%c", 0, inner_sep);
    }
}

static void X64_nasm_print_global_arr_init(Allocator* allocator, const ConstExpr* const_expr, char** line)
{
    Type* type = const_expr->type;
    assert(type->kind == TYPE_ARRAY);

    Type* elem_type = type->as_array.base;
    size_t num_elems = type->as_array.len;
    ConstExpr** init_vals = alloc_array(allocator, ConstExpr*, num_elems, true); // Initialized to NULL

    // Iterate through initializers and overwrite appropriate elements in init_vals array with
    // the specified initializer value.
    const ConstArrayInitzer* init = &const_expr->array_initzer;

    for (size_t i = 0; i < init->num_initzers; i += 1) {
        ConstArrayMemberInitzer* initzer = init->initzers + i;

        init_vals[initzer->index] = &initzer->const_expr;
    }

    // Print an initial value for each element.
    for (u64 i = 0; i < num_elems; i += 1) {
        if (init_vals[i]) {
            X64_nasm_print_global_val(allocator, init_vals[i], line);
        }
        else {
            X64_nasm_print_global_zero_bytes(line, elem_type->size);
        }
    }
}

static void X64_nasm_print_global_struct_init(Allocator* allocator, const ConstExpr* const_expr, char** line)
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
            X64_nasm_print_global_zero_bytes(line, padding);
            offset = field->offset;
        }

        // Init field with specified value or zero.
        if (field_exprs[i]) {
            X64_nasm_print_global_val(allocator, field_exprs[i], line);
        }
        else {
            X64_nasm_print_global_zero_bytes(line, field_size);
        }

        offset += field_size;
    }

    // Clear padding after last field.
    size_t padding = type->size - offset;

    if (padding) {
        X64_nasm_print_global_zero_bytes(line, padding);
    }
}

static void X64_nasm_print_global_union_init(Allocator* allocator, const ConstExpr* const_expr, char** line)
{
    Type* type = const_expr->type;
    assert(type->kind == TYPE_UNION);

    TypeAggregateField* field = &type->as_union.body.fields[const_expr->union_initzer.field_index];
    ConstExpr* field_expr = const_expr->union_initzer.field_expr;

    if (field_expr) {
        X64_nasm_print_global_val(allocator, field_expr, line);

        size_t padding = type->size - field->type->size;

        if (padding) {
            X64_nasm_print_global_zero_bytes(line, padding);
        }
    }
    else {
        X64_nasm_print_global_zero_bytes(line, type->size);
    }
}

static void X64_nasm_print_global_int_bytes(Scalar imm, size_t size, char** line)
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
static void X64_nasm_print_global_val(Allocator* allocator, const ConstExpr* const_expr, char** line)
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
        X64_nasm_print_global_int_bytes(const_expr->imm, const_expr->type->size, line);
        break;
    }
    case CONST_EXPR_MEM_ADDR: {
        const ConstAddr* addr = &const_expr->addr;

        if (addr->kind == CONST_ADDR_SYM) {
            ftprint_char_array(line, false, "%s %s", x64_data_size_label[const_expr->type->size],
                               symbol_mangled_name(allocator, addr->sym));
        }
        else {
            assert(addr->kind == CONST_ADDR_STR_LIT);
            ftprint_char_array(line, false, "%s %s_%llu", x64_data_size_label[const_expr->type->size], X64_NASM_STR_LIT_PRE,
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

        X64_nasm_print_global_int_bytes(imm, size, line);
        break;
    }
    case CONST_EXPR_PROC: {
        ftprint_char_array(line, false, "%s %s", x64_data_size_label[const_expr->type->size],
                           symbol_mangled_name(allocator, const_expr->sym));
        break;
    }
    case CONST_EXPR_ARRAY_INIT: {
        X64_nasm_print_global_arr_init(allocator, const_expr, line);
        break;
    }
    case CONST_EXPR_STRUCT_INIT: {
        X64_nasm_print_global_struct_init(allocator, const_expr, line);
        break;
    }
    case CONST_EXPR_UNION_INIT: {
        X64_nasm_print_global_union_init(allocator, const_expr, line);
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void X64_nasm_emit_global_data(Allocator* tmp_mem, Array(char)* str_builder, const char* name, const ConstExpr* const_expr)
{
    Type* type = const_expr->type;

    X64_NASM_PRINT_FL(*str_builder, "ALIGN %d", type->align);
    X64_NASM_PRINT_FL(*str_builder, "%s: ", name);

    AllocatorState mem_state = allocator_get_state(tmp_mem);
    X64_nasm_print_global_val(tmp_mem, const_expr, str_builder);
    allocator_restore_state(mem_state);

    X64_NASM_PRINT_L(*str_builder, "");
}

static Array(char) X64_nasm_gen_rodata(Allocator* gen_mem, Allocator* tmp_mem, const GlobalData* str_lits, const GlobalData* float_lits)
{
    Array(char) str_builder = array_create(gen_mem, char, 256);

    const size_t num_str_lits = str_lits->list.num_elems;
    const size_t num_float_lits = float_lits->list.num_elems;

    // Exit if nothing to do.
    if (!num_str_lits && !num_float_lits) {
        return str_builder;
    }

    AllocatorState mem_state = allocator_get_state(tmp_mem);
    {
        X64_NASM_PRINT_L(str_builder, "SECTION .rodata\n");

        // Emit static/const string literals
        for (size_t i = 0; i < num_str_lits; i++) {
            void** str_lit_ptr = bucket_list_get_elem_packed(&str_lits->list, i);
            assert(str_lit_ptr);

            StrLit* str_lit = (StrLit*)(*str_lit_ptr);

            assert(str_lit->used);

            const char* escaped_str = cstr_escape(tmp_mem, str_lit->str, str_lit->len, '`'); // TODO: Just print bytes.

            X64_NASM_PRINT_FL(str_builder, "%s_%llu: ", X64_NASM_STR_LIT_PRE, str_lit->id);
            X64_NASM_PRINT_FL(str_builder, "db `%s\\0`", escaped_str);
        }

        // Emit static/const float literals
        for (size_t i = 0; i < num_float_lits; i++) {
            void** float_lit_ptr = bucket_list_get_elem_packed(&float_lits->list, i);
            assert(float_lit_ptr);

            FloatLit* float_lit = (FloatLit*)(*float_lit_ptr);

            assert(float_lit->used);

            Type* ftype = float_lit->kind == FLOAT_F64 ? builtin_types[BUILTIN_TYPE_F64].type : builtin_types[BUILTIN_TYPE_F32].type;
            ConstExpr const_expr = {.kind = CONST_EXPR_FLOAT_LIT, .type = ftype, .float_lit = float_lit};

            X64_nasm_emit_global_data(tmp_mem, &str_builder, X64_nasm_float_lit_mangled_name(tmp_mem, float_lit), &const_expr);
        }
    }
    allocator_restore_state(mem_state);

    array_push(str_builder, '\0');

    return str_builder;
}

static Array(char) X64_nasm_gen_data(Allocator* gen_mem, Allocator* tmp_mem, const GlobalData* vars)
{
    Array(char) str_builder = array_create(gen_mem, char, 256);

    const size_t num_vars = vars->list.num_elems;

    if (!num_vars) {
        return str_builder;
    }

    AllocatorState mem_state = allocator_get_state(tmp_mem);
    {
        X64_NASM_PRINT_L(str_builder, "\nSECTION .data\n");

        for (u32 i = 0; i < num_vars; i += 1) {
            void** sym_ptr = bucket_list_get_elem_packed(&vars->list, i);
            assert(sym_ptr);
            Symbol* sym = (Symbol*)(*sym_ptr);

            X64_nasm_emit_global_data(tmp_mem, &str_builder, symbol_mangled_name(tmp_mem, sym), &sym->as_var.const_expr);
        }
    }
    allocator_restore_state(mem_state);

    array_push(str_builder, '\0');

    return str_builder;
}

static Array(char) X64_nasm_gen_proc(Allocator* gen_mem, Allocator* tmp_mem, size_t proc_id, Symbol* proc_sym)
{
    Array(char) proc_str = array_create(gen_mem, char, 256);

    AllocatorState tmp_mem_state = allocator_get_state(tmp_mem);

    // Print procedure name
    const char* proc_mangled = symbol_mangled_name(tmp_mem, proc_sym);
    X64_NASM_PRINT_FL(proc_str, "%s:", proc_mangled);

    Array(X64__Instr) instrs = X64__gen_proc_instrs(gen_mem, tmp_mem, proc_sym);
    const size_t num_instrs = array_len(instrs);

    // Print instructions.
    for (size_t i = 0; i < num_instrs; i += 1) {
        X64__Instr* instr = &instrs[i];

        // Print label for instruction (needed for jumps)
        X64_NASM_PRINT_F(proc_str, "  %s:", X64_nasm_get_label(tmp_mem, proc_id, i));

        switch (instr->kind) {
        case X64_Instr_Kind_NOOP: {
            // No-Op. Do nothing.
        } break;
        case X64_Instr_Kind_RET: {
            X64_NASM_PRINT_TL(proc_str, "ret");
        } break;
        case X64_Instr_Kind_JMP_TO_RET:
        case X64_Instr_Kind_JMP: {
            X64_NASM_PRINT_FTL(proc_str, "jmp %s", X64_nasm_get_label(tmp_mem, proc_id, instr->jmp.target));
        } break;
        case X64_Instr_Kind_PUSH: {
            X64_NASM_PRINT_FTL(proc_str, "push %s", x64_nasm_int_reg_names[X64_MAX_INT_REG_SIZE][instr->push.reg]);
        } break;
        case X64_Instr_Kind_POP: {
            X64_NASM_PRINT_FTL(proc_str, "pop %s", x64_nasm_int_reg_names[X64_MAX_INT_REG_SIZE][instr->pop.reg]);
        } break;
        // ADD
        case X64_Instr_Kind_ADD_RR: {
            const char* r1 = x64_nasm_int_reg_names[instr->add_rr.size][instr->add_rr.dst];
            const char* r2 = x64_nasm_int_reg_names[instr->add_rr.size][instr->add_rr.src];
            X64_NASM_PRINT_FTL(proc_str, "add %s, %s", r1, r2);
        } break;
        case X64_Instr_Kind_ADD_RM: {
            const char* reg_op = x64_nasm_int_reg_names[instr->add_rm.size][instr->add_rm.dst];
            const char* mem_op = X64_nasm_print_sibd_addr(tmp_mem, &instr->add_rm.src, instr->add_rm.size);
            X64_NASM_PRINT_FTL(proc_str, "add %s, %s", reg_op, mem_op);
        } break;
        case X64_Instr_Kind_ADD_MR: {
            const char* mem_op = X64_nasm_print_sibd_addr(tmp_mem, &instr->add_mr.dst, instr->add_mr.size);
            const char* reg_op = x64_nasm_int_reg_names[instr->add_mr.size][instr->add_mr.src];
            X64_NASM_PRINT_FTL(proc_str, "add %s, %s", mem_op, reg_op);
        } break;
        case X64_Instr_Kind_ADD_RI: {
            const char* dst_reg = x64_nasm_int_reg_names[instr->add_ri.size][instr->add_ri.dst];
            const char* src_imm = X64_nasm_print_imm(tmp_mem, instr->add_ri.imm, instr->add_ri.size);
            X64_NASM_PRINT_FTL(proc_str, "add %s, %s", dst_reg, src_imm);
        } break;
        // SUB
        case X64_Instr_Kind_SUB_RR: {
            const char* r1 = x64_nasm_int_reg_names[instr->sub_rr.size][instr->sub_rr.dst];
            const char* r2 = x64_nasm_int_reg_names[instr->sub_rr.size][instr->sub_rr.src];
            X64_NASM_PRINT_FTL(proc_str, "sub %s, %s", r1, r2);
        } break;
        case X64_Instr_Kind_SUB_RM: {
            const char* reg_op = x64_nasm_int_reg_names[instr->sub_rm.size][instr->sub_rm.dst];
            const char* mem_op = X64_nasm_print_sibd_addr(tmp_mem, &instr->sub_rm.src, instr->sub_rm.size);
            X64_NASM_PRINT_FTL(proc_str, "sub %s, %s", reg_op, mem_op);
        } break;
        case X64_Instr_Kind_SUB_MR: {
            const char* mem_op = X64_nasm_print_sibd_addr(tmp_mem, &instr->sub_mr.dst, instr->sub_mr.size);
            const char* reg_op = x64_nasm_int_reg_names[instr->sub_mr.size][instr->sub_mr.src];
            X64_NASM_PRINT_FTL(proc_str, "sub %s, %s", mem_op, reg_op);
        } break;
        case X64_Instr_Kind_SUB_RI: {
            const char* dst_reg = x64_nasm_int_reg_names[instr->sub_ri.size][instr->sub_ri.dst];
            const char* src_imm = X64_nasm_print_imm(tmp_mem, instr->sub_ri.imm, instr->sub_ri.size);
            X64_NASM_PRINT_FTL(proc_str, "sub %s, %s", dst_reg, src_imm);
        } break;
        // IMUL
        case X64_Instr_Kind_IMUL_RR: {
            const char* r1 = x64_nasm_int_reg_names[instr->imul_rr.size][instr->imul_rr.dst];
            const char* r2 = x64_nasm_int_reg_names[instr->imul_rr.size][instr->imul_rr.src];
            X64_NASM_PRINT_FTL(proc_str, "imul %s, %s", r1, r2);
        } break;
        case X64_Instr_Kind_IMUL_RM: {
            const char* reg_op = x64_nasm_int_reg_names[instr->imul_rm.size][instr->imul_rm.dst];
            const char* mem_op = X64_nasm_print_sibd_addr(tmp_mem, &instr->imul_rm.src, instr->imul_rm.size);
            X64_NASM_PRINT_FTL(proc_str, "imul %s, %s", reg_op, mem_op);
        } break;
        case X64_Instr_Kind_IMUL_MR: {
            const char* mem_op = X64_nasm_print_sibd_addr(tmp_mem, &instr->imul_mr.dst, instr->imul_mr.size);
            const char* reg_op = x64_nasm_int_reg_names[instr->imul_mr.size][instr->imul_mr.src];
            X64_NASM_PRINT_FTL(proc_str, "imul %s, %s", mem_op, reg_op);
        } break;
        case X64_Instr_Kind_IMUL_RI: {
            const char* dst_reg = x64_nasm_int_reg_names[instr->imul_ri.size][instr->imul_ri.dst];
            const char* src_imm = X64_nasm_print_imm(tmp_mem, instr->imul_ri.imm, instr->imul_ri.size);
            X64_NASM_PRINT_FTL(proc_str, "imul %s, %s", dst_reg, src_imm);
        } break;
        // MOV
        case X64_Instr_Kind_MOV_RR: {
            const char* r1 = x64_nasm_int_reg_names[instr->mov_rr.size][instr->mov_rr.dst];
            const char* r2 = x64_nasm_int_reg_names[instr->mov_rr.size][instr->mov_rr.src];
            X64_NASM_PRINT_FTL(proc_str, "mov %s, %s", r1, r2);
        } break;
        case X64_Instr_Kind_MOV_RM: {
            const char* reg_op = x64_nasm_int_reg_names[instr->mov_rm.size][instr->mov_rm.dst];
            const char* mem_op = X64_nasm_print_sibd_addr(tmp_mem, &instr->mov_rm.src, instr->mov_rm.size);
            X64_NASM_PRINT_FTL(proc_str, "mov %s, %s", reg_op, mem_op);
        } break;
        case X64_Instr_Kind_MOV_MR: {
            const char* mem_op = X64_nasm_print_sibd_addr(tmp_mem, &instr->mov_mr.dst, instr->mov_mr.size);
            const char* reg_op = x64_nasm_int_reg_names[instr->mov_mr.size][instr->mov_mr.src];
            X64_NASM_PRINT_FTL(proc_str, "mov %s, %s", mem_op, reg_op);
        } break;
        case X64_Instr_Kind_MOV_RI: {
            const char* dst_reg = x64_nasm_int_reg_names[instr->mov_ri.size][instr->mov_ri.dst];
            const char* src_imm = X64_nasm_print_imm(tmp_mem, instr->mov_ri.imm, instr->mov_ri.size);
            X64_NASM_PRINT_FTL(proc_str, "mov %s, %s", dst_reg, src_imm);
        } break;
        case X64_Instr_Kind_MOV_MI: {
            const char* dst_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->mov_mi.dst, instr->mov_mi.size);
            const char* src_imm = X64_nasm_print_imm(tmp_mem, instr->mov_mi.imm, instr->mov_mi.size);
            X64_NASM_PRINT_FTL(proc_str, "mov %s, %s", dst_mem, src_imm);
        } break;
        // MOVSX
        case X64_Instr_Kind_MOVSX_RR: {
            const char* dst_reg = x64_nasm_int_reg_names[instr->movsx_rr.dst_size][instr->movsx_rr.dst];
            const char* src_reg = x64_nasm_int_reg_names[instr->movsx_rr.src_size][instr->movsx_rr.src];
            X64_NASM_PRINT_FTL(proc_str, "movsx %s, %s", dst_reg, src_reg);
        } break;
        case X64_Instr_Kind_MOVSX_RM: {
            const char* dst_reg = x64_nasm_int_reg_names[instr->movsx_rm.dst_size][instr->movsx_rm.dst];
            const char* src_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->movsx_rm.src, instr->movsx_rm.src_size);
            X64_NASM_PRINT_FTL(proc_str, "movsx %s, %s", dst_reg, src_mem);
        } break;
        // MOVSXD
        case X64_Instr_Kind_MOVSXD_RR: {
            const char* dst_reg = x64_nasm_int_reg_names[instr->movsxd_rr.dst_size][instr->movsxd_rr.dst];
            const char* src_reg = x64_nasm_int_reg_names[instr->movsxd_rr.src_size][instr->movsxd_rr.src];
            X64_NASM_PRINT_FTL(proc_str, "movsxd %s, %s", dst_reg, src_reg);
        } break;
        case X64_Instr_Kind_MOVSXD_RM: {
            const char* dst_reg = x64_nasm_int_reg_names[instr->movsxd_rm.dst_size][instr->movsxd_rm.dst];
            const char* src_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->movsxd_rm.src, instr->movsxd_rm.src_size);
            X64_NASM_PRINT_FTL(proc_str, "movsxd %s, %s", dst_reg, src_mem);
        } break;
        // MOVSS
        case X64_Instr_Kind_MOVSS_MR: {
            const u8 size = float_kind_sizes[FLOAT_F32];
            const char* dst_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->movss_mr.dst, size);
            const char* src_reg = x64_flt_reg_names[instr->movss_mr.src];
            X64_NASM_PRINT_FTL(proc_str, "movss %s, %s", dst_mem, src_reg);
        } break;
        case X64_Instr_Kind_MOVSS_RM: {
            const u8 size = float_kind_sizes[FLOAT_F32];
            const char* dst_reg = x64_flt_reg_names[instr->movss_rm.dst];
            const char* src_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->movss_rm.src, size);
            X64_NASM_PRINT_FTL(proc_str, "movss %s, %s", dst_reg, src_mem);
        } break;
        case X64_Instr_Kind_MOVSD_MR: {
            const u8 size = float_kind_sizes[FLOAT_F64];
            const char* dst_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->movsd_mr.dst, size);
            const char* src_reg = x64_flt_reg_names[instr->movsd_mr.src];
            X64_NASM_PRINT_FTL(proc_str, "movsd %s, %s", dst_mem, src_reg);
        } break;
        case X64_Instr_Kind_MOVSD_RM: {
            const u8 size = float_kind_sizes[FLOAT_F64];
            const char* dst_reg = x64_flt_reg_names[instr->movsd_rm.dst];
            const char* src_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->movsd_rm.src, size);
            X64_NASM_PRINT_FTL(proc_str, "movsd %s, %s", dst_reg, src_mem);
        } break;
        case X64_Instr_Kind_MOVDQU_MR: {
            const u8 size = 16; // xmm 128 bit = 16 bytes
            const char* dst_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->movdqu_mr.dst, size);
            const char* src_reg = x64_flt_reg_names[instr->movdqu_mr.src];
            X64_NASM_PRINT_FTL(proc_str, "movdqu %s, %s", dst_mem, src_reg);
        } break;
        case X64_Instr_Kind_MOVDQU_RM: {
            const u8 size = 16; // xmm 128 bit = 16 bytes
            const char* dst_reg = x64_flt_reg_names[instr->movdqu_rm.dst];
            const char* src_mem = X64_nasm_print_sibd_addr(tmp_mem, &instr->movdqu_rm.src, size);
            X64_NASM_PRINT_FTL(proc_str, "movdqu %s, %s", dst_reg, src_mem);
        } break;
        default:
            NIBBLE_FATAL_EXIT("Unknown X64 instruction kind %d\n", instr->kind);
            break;
        }
    }

    array_push(proc_str, '\0');

    allocator_restore_state(tmp_mem_state);
    return proc_str;
}

bool X64_nasm_gen_module(Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars, BucketList* procs, GlobalData* str_lits,
                         GlobalData* float_lits, BucketList* foreign_procs, const char* output_file)
{
    (void)output_file;
    AllocatorState gen_mem_state = allocator_get_state(gen_mem);
    AllocatorState tmp_mem_state = allocator_get_state(tmp_mem);

    Array(char) rodata_str = X64_nasm_gen_rodata(gen_mem, tmp_mem, str_lits, float_lits);
    Array(char) data_str = X64_nasm_gen_data(gen_mem, tmp_mem, vars);

    // Generate instructions for each procedure.
    const size_t num_procs = procs->num_elems;
    Array(char)* proc_strs = alloc_array(gen_mem, Array(char), num_procs, true);

    for (size_t i = 0; i < num_procs; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);

        proc_strs[i] = X64_nasm_gen_proc(gen_mem, tmp_mem, i, sym);
    }

    //
    // Write out file.
    //

    FILE* out_fd = fopen("_nasm_out.s", "w");
    if (!out_fd) {
        ftprint_err("Failed to open output file `_nasm_out.s`\n");
        return false;
    }

    // Write top comment to file and use rip-relative addressing of globals by default.
    ftprint_file(out_fd, false, "; Generated by the Nibble compiler.\ndefault rel\n");

    // Write extern procs.
    u32 num_foreign_procs = foreign_procs->num_elems;

    for (u32 i = 0; i < num_foreign_procs; i += 1) {
        Symbol* proc_sym = (Symbol*)(*bucket_list_get_elem_packed(foreign_procs, i));
        ftprint_file(out_fd, false, "extern %s\n", proc_sym->as_proc.foreign_name->str);
    }

    // Write out .rodata section.
    ftprint_file(out_fd, false, "%.*s", array_len(rodata_str), rodata_str);

    // Write out .data section.
    ftprint_file(out_fd, false, "%.*s", array_len(data_str), data_str);

    // Write out startup/builtin code.
    ftprint_file(out_fd, false, "%s\n\n", x64_target.startup_code);

    // Write out procedures.
    for (size_t i = 0; i < num_procs; i += 1) {
        Array(char) proc_str = proc_strs[i];
        ftprint_file(out_fd, false, "%.*s", array_len(proc_str), proc_str);
    }

    fclose(out_fd);
    allocator_restore_state(tmp_mem_state);
    allocator_restore_state(gen_mem_state);

    return true;
}

