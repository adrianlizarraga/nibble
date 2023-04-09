#include "allocator.h"
#include "array.h"
#include "x64_gen/lir_to_x64.h"
#include "x64_gen/regs.h"

#define X64_NASM_STR_LIT_PRE "__nibble_str_lit_"
#define X64_NASM_FLOAT_LIT_PRE "__nibble_float_lit_"

static const char* X64_nasm_float_lit_mangled_name(Allocator* alloc, FloatLit* float_lit)
{
    char* dstr = array_create(alloc, char, 16);
    ftprint_char_array(&dstr, true, "%s_%llu", X64_NASM_FLOAT_LIT_PRE, float_lit->id);

    return dstr;
}

// Print line of text
#define X64_NASM_PRINT_L(str_builder, text) ftprint_char_array(&(str_builder), false, text "\n")

// Print tabbed line of text
#define X64_NASM_PRINT_TL(str_builder, text) ftprint_char_array(&(str_builder), false, "  " text "\n")

// Print formatted line
#define X64_NASM_PRINT_FL(str_builder, fmt, ...) ftprint_char_array(&(str_builder), false, fmt "\n", __VA_ARGS__)

// Print formatted tabbed line
#define X64_NASM_PRINT_FTL(str_builder, fmt, ...) ftprint_char_array(&(str_builder), false, "  " fmt "\n", __VA_ARGS__)

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

    return str_builder;
}

void X64_nasm_gen_module(Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars, BucketList* procs, GlobalData* str_lits,
                         GlobalData* float_lits, BucketList* foreign_procs, const char* output_file)
{
    Array(char) rodata_str = X64_nasm_gen_rodata(gen_mem, tmp_mem, str_lits, float_lits);
    Array(char) data_str = X64_nasm_gen_data(gen_mem, tmp_mem, vars);
}

