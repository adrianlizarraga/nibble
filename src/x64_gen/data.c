#include "x64_gen/data.h"

static inline void X64_add_reloc_use(Array(X64_DataReloc) * relocs, u64 usage_off, ConstAddr ref_addr)
{
    X64_DataReloc r = {.ref_addr = ref_addr, .usage_off = usage_off};
    array_push(*relocs, r);
}

// Forward declaration.
static void X64_data_serialize(Array(char) * buf, Allocator* tmp_mem, ConstExpr* const_expr, Array(X64_DataReloc) * relocs);

static inline void X64_data_fill_zeros(Array(char) * buf, size_t size)
{
    for (size_t i = 0; i < size; i += 1) {
        array_push(*buf, 0);
    }
}

static inline void X64_data_serialize_int(Array(char) * buf, Scalar imm, size_t size)
{
    u64 elem_val = imm.as_int._u64;

    // Write each byte of the value
    for (size_t i = 0; i < size; i += 1) {
        array_push(*buf, elem_val & 0xFFLL);
        elem_val = elem_val >> 8;
    }
}

static void X64_data_serialize_array_init(Array(char) * buf, Allocator* tmp_mem, ConstExpr* const_expr, Array(X64_DataReloc) * relocs)
{
    AllocatorState mem_state = allocator_get_state(tmp_mem);

    Type* type = const_expr->type;
    assert(type->kind == TYPE_ARRAY);

    Type* elem_type = type->as_array.base;
    size_t num_elems = type->as_array.len;
    ConstExpr** init_vals = alloc_array(tmp_mem, ConstExpr*, num_elems, true); // Initialized to NULL

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
            X64_data_serialize(buf, tmp_mem, init_vals[i], relocs);
        }
        else {
            X64_data_fill_zeros(buf, elem_type->size);
        }
    }

    allocator_restore_state(mem_state);
}

static void X64_data_serialize_struct_init(Array(char) * buf, Allocator* tmp_mem, ConstExpr* const_expr, Array(X64_DataReloc) * relocs)
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
        X64_data_fill_zeros(buf, padding);
        offset += padding;

        // Init field with specified value or zero.
        if (field_exprs[i]) {
            X64_data_serialize(buf, tmp_mem, field_exprs[i], relocs);
        }
        else {
            X64_data_fill_zeros(buf, field_size);
        }

        offset += field_size;
    }

    // Clear padding after last field.
    X64_data_fill_zeros(buf, type->size - offset);
}

static void X64_data_serialize_union_init(Array(char) * buf, Allocator* tmp_mem, ConstExpr* const_expr, Array(X64_DataReloc) * relocs)
{
    Type* type = const_expr->type;
    assert(type->kind == TYPE_UNION);

    TypeAggregateField* field = &type->as_union.body.fields[const_expr->union_initzer.field_index];
    ConstExpr* field_expr = const_expr->union_initzer.field_expr;

    if (field_expr) {
        X64_data_serialize(buf, tmp_mem, field_expr, relocs);
        X64_data_fill_zeros(buf, type->size - field->type->size);
    }
    else {
        X64_data_fill_zeros(buf, type->size);
    }
}

static void X64_data_serialize(Array(char) * buf, Allocator* tmp_mem, ConstExpr* const_expr, Array(X64_DataReloc) * relocs)
{
    switch (const_expr->kind) {
    case CONST_EXPR_NONE: {
        X64_data_fill_zeros(buf, const_expr->type->size);
        break;
    }
    case CONST_EXPR_IMM: {
        X64_data_serialize_int(buf, const_expr->imm, const_expr->type->size);
        break;
    }
    case CONST_EXPR_MEM_ADDR: {
        assert(relocs != NULL);

        // Handles relocations for address of symbols and string literals.
        X64_add_reloc_use(relocs, array_len(*buf), const_expr->addr);

        // Fill with zeros. Linker will fill-in the actual address
        // using the relacation information we generate.
        X64_data_fill_zeros(buf, const_expr->type->size);
        break;
    }
    case CONST_EXPR_STR_LIT: {
        StrLit* str_lit = const_expr->str_lit;
        size_t len = str_lit->len;
        const char* str = str_lit->str;

        for (size_t i = 0; i < len; i += 1) {
            array_push(*buf, str[i]);
        }

        array_push(*buf, '\0');
        break;
    }
    case CONST_EXPR_FLOAT_LIT: {
        FloatLit* float_lit = const_expr->float_lit;
        Scalar imm = {.as_float = float_lit->value};
        size_t size = float_kind_sizes[float_lit->kind];

        X64_data_serialize_int(buf, imm, size);
        break;
    }
    case CONST_EXPR_PROC: {
        assert(relocs != NULL);

        X64_add_reloc_use(relocs, array_len(*buf), (ConstAddr){.kind = CONST_ADDR_SYM, .sym = const_expr->sym});

        // Fill with zeros. Linker will fill-in the actual address
        // using the relocation information we generate.
        X64_data_fill_zeros(buf, const_expr->type->size);
        break;
    }
    case CONST_EXPR_ARRAY_INIT: {
        X64_data_serialize_array_init(buf, tmp_mem, const_expr, relocs);
        break;
    }
    case CONST_EXPR_STRUCT_INIT: {
        X64_data_serialize_struct_init(buf, tmp_mem, const_expr, relocs);
        break;
    }
    case CONST_EXPR_UNION_INIT: {
        X64_data_serialize_union_init(buf, tmp_mem, const_expr, relocs);
        break;
    }
    default:
        NIBBLE_FATAL_EXIT("Unhandled ConstExprKind `%d`\n", const_expr->kind);
        break;
    }
}

static size_t X64_add_data_item(Array(char) * buf, Allocator* tmp_mem, ConstExpr* const_expr, Array(X64_DataReloc) * relocs)
{
    Type* type = const_expr->type;
    size_t offset = array_len(*buf);
    size_t align_pad = ALIGN_UP(offset, type->align) - offset;

    X64_data_fill_zeros(buf, align_pad);
    X64_data_serialize(buf, tmp_mem, const_expr, relocs);

    return offset + align_pad;
}

void X64_init_data_section(X64_DataSection* data_sec, Allocator* gen_mem, Allocator* tmp_mem, GlobalData* vars)
{
    size_t num_vars = vars->list.num_elems;

    if (!num_vars) {
        return;
    }

    // Set the initial capacity to twice the combined size of all variables to avoid reallocation
    // due to additional alignment buf.
    data_sec->buf = array_create(gen_mem, char, vars->size << 1);
    data_sec->var_offs = hmap(clp2(num_vars), gen_mem);
    data_sec->relocs = array_create(gen_mem, X64_DataReloc, vars->size);

    // Serialize the first variable separately to get its required alignment (w/o a branch in the loop).
    {
        Symbol* sym = (Symbol*)(*bucket_list_get_elem_packed(&vars->list, 0));
        size_t offset = X64_add_data_item(&data_sec->buf, tmp_mem, &sym->as_var.const_expr, &data_sec->relocs);

        hmap_put(&data_sec->var_offs, PTR_UINT(sym), offset); // Record offset.

        data_sec->align = sym->type->align;
    }

    // Serialize all other variables.
    for (size_t i = 1; i < num_vars; i += 1) {
        Symbol* sym = (Symbol*)(*bucket_list_get_elem_packed(&vars->list, i));
        size_t offset = X64_add_data_item(&data_sec->buf, tmp_mem, &sym->as_var.const_expr, &data_sec->relocs);

        hmap_put(&data_sec->var_offs, PTR_UINT(sym), offset); // Record offset.
    }
}

void X64_init_rodata_section(X64_RODataSection* rodata, Allocator* gen_mem, Allocator* tmp_mem, GlobalData* floats,
                             GlobalData* strs)
{
    rodata->align = 0x10;

    size_t num_floats = floats->list.num_elems;
    size_t num_strs = strs->list.num_elems;

    if (!num_floats && !num_strs) {
        return;
    }

    rodata->buf = array_create(gen_mem, char, floats->size + strs->size);

    // Serialize all floats.
    if (num_floats) {
        rodata->float_offs = hmap(clp2(num_floats), gen_mem);

        for (size_t i = 0; i < num_floats; i += 1) {
            FloatLit* float_lit = (FloatLit*)(*bucket_list_get_elem_packed(&floats->list, i));
            Type* type = float_lit->kind == FLOAT_F64 ? builtin_types[BUILTIN_TYPE_F64].type : builtin_types[BUILTIN_TYPE_F32].type;
            ConstExpr const_expr = {.kind = CONST_EXPR_FLOAT_LIT, .type = type, .float_lit = float_lit};
            size_t offset = X64_add_data_item(&rodata->buf, tmp_mem, &const_expr, NULL);

            hmap_put(&rodata->float_offs, PTR_UINT(float_lit), offset);
        }
    }

    // Serialize all strings.
    if (num_strs) {
        rodata->str_offs = hmap(clp2(num_strs), gen_mem);

        for (size_t i = 0; i < num_strs; i += 1) {
            StrLit* str_lit = (StrLit*)(*bucket_list_get_elem_packed(&strs->list, i));
            size_t len = str_lit->len;
            const char* str = str_lit->str;
            size_t offset = array_len(rodata->buf);

            for (size_t i = 0; i < len; i += 1) {
                array_push(rodata->buf, str[i]);
            }

            array_push(rodata->buf, '\0');
            hmap_put(&rodata->str_offs, PTR_UINT(str_lit), offset);
        }
    }

    return;
}

