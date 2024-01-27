#include <assert.h>
#include "ast/module.h"
#include "resolver/internal.h"

bool resolve_decl_var(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_VAR);

    DeclVar* decl = (DeclVar*)sym->decl;
    bool global = !sym->is_local;
    TypeSpec* typespec = decl->typespec;
    Expr* expr = decl->init;
    Type* type = NULL;

    if (typespec) {
        Type* declared_type = resolve_typespec(resolver, typespec);

        if (!declared_type)
            return false;

        if (declared_type == builtin_types[BUILTIN_TYPE_VOID].type) {
            resolver_on_error(resolver, typespec->range, "Cannot declare a variable of type `%s`.", type_name(declared_type));
            return false;
        }

        if (expr) {
            if (!resolve_expr(resolver, expr, declared_type))
                return false;

            assert(!type_has_incomplete_array(expr->type));

            ExprOperand right_eop = OP_FROM_EXPR(expr);

            // If the declared type contains an incomplete array type, try to use the
            // rhs type if the types are compatible.
            if (type_has_incomplete_array(declared_type)) {
                if (!types_are_compatible(declared_type, right_eop.type)) {
                    resolver_on_error(resolver, expr->range,
                                      "Incomplete variable type `%s` is not compatible with expression of type `%s`.",
                                      type_name(declared_type), type_name(right_eop.type));
                    return false;
                }

                declared_type = right_eop.type;
            }
            else {
                CastResult r = convert_eop(&right_eop, declared_type, true);

                if (!r.success) {
                    resolver_cast_error(resolver, r, sym->decl->range, "Invalid variable declaration", right_eop.type, declared_type);
                    return false;
                }
            }

            if (global && !right_eop.is_constexpr) {
                resolver_on_error(resolver, expr->range, "Global variables must be initialized with a constant expression");
                return false;
            }

            decl->init = try_wrap_cast_expr(resolver, &right_eop, decl->init);
            type = declared_type;
        }
        else {
            if (type_has_incomplete_array(declared_type)) {
                resolver_on_error(resolver, typespec->range, "Cannot infer the number of elements in array type specification");
                return false;
            }

            type = declared_type;
        }
    }
    else {
        assert(expr);

        if (!resolve_expr(resolver, expr, NULL))
            return false;

        if (global && !expr->is_constexpr) {
            resolver_on_error(resolver, expr->range, "Global variables must be initialized with a constant expression");
            return false;
        }

        if (type_has_incomplete_array(expr->type)) {
            resolver_on_error(resolver, expr->range, "Expression type `%s` contains array of unknown size.", type_name(expr->type));
            return false;
        }

        type = expr->type;
    }

    if (!try_complete_aggregate_type(resolver, type)) {
        return false;
    }

    if (type->size == 0) {
        resolver_on_error(resolver, expr->range, "Cannot declare a variable of zero size.");
        return false;
    }

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

bool resolve_decl_typedef(Resolver* resolver, Symbol* sym)
{
    DeclTypedef* decl = (DeclTypedef*)sym->decl;
    Type* type = resolve_typespec(resolver, decl->typespec);

    if (!type) {
        return false;
    }

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static inline void swap_syms(Symbol** syms, s64 i, s64 j)
{
    Symbol* tmp = syms[i];
    syms[i] = syms[j];
    syms[j] = tmp;
}

static inline bool cmp_lteq_enum_syms(Symbol* a, Symbol* b)
{
    assert(a->type == b->type);

    if (type_is_signed(a->type)) {
        return a->as_const.imm.as_int._s64 <= b->as_const.imm.as_int._s64;
    }

    return a->as_const.imm.as_int._u64 <= b->as_const.imm.as_int._u64;
}

static s64 qsort_partition_enum_syms(Symbol** syms, s64 lo, s64 hi)
{
    // Pick middle as pivot since this is already likely sorted.
    // Afterwards, move pivot element to the end of the array.
    s64 mid = lo + ((hi - lo) / 2);
    swap_syms(syms, mid, hi);

    s64 i = lo;
    for (s64 j = lo; j < hi; j++) {
        if (cmp_lteq_enum_syms(syms[j], syms[hi])) {
            swap_syms(syms, i, j);
            i++;
        }
    }

    swap_syms(syms, i, hi);
    return i;
}

static void qsort_enum_syms(Symbol** syms, s64 lo, s64 hi)
{
    if (lo < 0 || lo >= hi) {
        return;
    }

    s64 pivot_index = qsort_partition_enum_syms(syms, lo, hi);

    // Recursively sort the two partitions.
    qsort_enum_syms(syms, lo, pivot_index - 1);
    qsort_enum_syms(syms, pivot_index + 1, hi);
}

bool resolve_decl_enum(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_TYPE);
    DeclEnum* decl_enum = (DeclEnum*)sym->decl;
    Type* base_type;

    if (decl_enum->typespec) {
        base_type = resolve_typespec(resolver, decl_enum->typespec);

        if (!base_type) {
            return false;
        }
    }
    else {
        base_type = builtin_types[BUILTIN_TYPE_INT].type;
    }

    if (base_type->kind != TYPE_INTEGER) {
        assert(decl_enum->typespec);
        resolver_on_error(resolver, decl_enum->typespec->range, "Enum must be an integer type, but found `%s`.", type_name(base_type));
        return false;
    }

    Type* enum_type = type_enum(&resolver->ctx->ast_mem, base_type, sym);

    // Resolve enum items.
    Symbol** item_syms = alloc_array(&resolver->ctx->ast_mem, Symbol*, decl_enum->num_items, false);
    Scalar prev_enum_val = {0};

    List* head = &decl_enum->items;
    List* it = head->next;
    size_t i = 0;

    // TODO: This is really sloppy. Need to validate that enum values fit in the base type.
    // Also need to account for overflow when the enum value is auto-incremented.
    while (it != head) {
        DeclEnumItem* enum_item = list_entry(it, DeclEnumItem, lnode);
        Scalar enum_val = {0};

        if (enum_item->value) {
            if (!resolve_expr(resolver, enum_item->value, enum_type)) {
                return false;
            }

            ExprOperand value_eop = OP_FROM_EXPR(enum_item->value);

            if (!value_eop.is_constexpr) {
                resolver_on_error(resolver, enum_item->value->range, "Value for enum item `%s` must be a constant expression",
                                  enum_item->super.name->str);
                return false;
            }

            if (!type_is_integer_like(value_eop.type)) {
                resolver_on_error(resolver, enum_item->value->range, "Enum item's value must be of an integer type");
                return false;
            }

            CastResult r = convert_eop(&value_eop, enum_type, true);

            if (!r.success) {
                resolver_cast_error(resolver, r, enum_item->super.range, "Invalid enum item declaration", value_eop.type, enum_type);
                return false;
            }

            enum_item->value = try_wrap_cast_expr(resolver, &value_eop, enum_item->value);
            enum_val = value_eop.imm;
        }
        else if (i > 0) { // Has a previous value
            Scalar one_imm = {.as_int._u64 = 1};
            ExprOperand item_op = {0};

            eval_binary_op(TKN_PLUS, &item_op, enum_type, prev_enum_val, one_imm);
            enum_val = item_op.imm;
        }

        Symbol* enum_sym = new_symbol_decl(&resolver->ctx->ast_mem, (Decl*)enum_item, sym->home);
        enum_sym->type = enum_type;
        enum_sym->status = SYMBOL_STATUS_RESOLVED;
        enum_sym->as_const.imm = enum_val;
        item_syms[i] = enum_sym;

        prev_enum_val = enum_val;
        it = it->next;
        i += 1;
    }

    // Sort enum syms by value.
    qsort_enum_syms(item_syms, 0, decl_enum->num_items - 1);

    // Check for any repeated enum values.
    if (decl_enum->num_items > 0) {
        for (size_t i = 0; i < decl_enum->num_items - 1; i++) {
            u64 v0 = item_syms[i]->as_const.imm.as_int._u64;
            u64 v1 = item_syms[i + 1]->as_const.imm.as_int._u64;
            if (v0 == v1) {
                resolver_on_error(resolver, item_syms[i]->decl->range, "Duplicate enum value");
                return false;
            }
        }
    }

    sym->type = enum_type;
    sym->status = SYMBOL_STATUS_RESOLVED;
    sym->as_enum.items = item_syms;
    sym->as_enum.num_items = decl_enum->num_items;

    return true;
}

bool resolve_decl_const(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_CONST);

    DeclConst* decl = (DeclConst*)sym->decl;
    TypeSpec* typespec = decl->typespec;
    Expr* init = decl->init;

    if (!resolve_expr(resolver, init, NULL))
        return false;

    if (!init->is_constexpr) {
        resolver_on_error(resolver, init->range, "Value for const decl `%s` must be a constant expression", decl->super.name->str);
        return false;
    }

    if (!type_is_scalar(init->type)) {
        resolver_on_error(resolver, init->range, "Constant expression must be of a scalar type");
        return false;
    }

    Type* type = NULL;

    if (typespec) {
        Type* declared_type = resolve_typespec(resolver, typespec);

        if (!declared_type)
            return false;

        ExprOperand init_eop = OP_FROM_EXPR(init);

        CastResult r = convert_eop(&init_eop, declared_type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, typespec->range, "Invalid const declaration", init_eop.type, declared_type);
            return false;
        }

        decl->init = try_wrap_cast_expr(resolver, &init_eop, decl->init);

        type = declared_type;
    }
    else {
        type = init->type;
    }

    assert(type);

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;
    sym->as_const.imm = decl->init->imm;

    return true;
}

static bool resolve_proc_param(Resolver* resolver, Symbol* sym)
{
    DeclVar* decl = (DeclVar*)sym->decl;
    TypeSpec* typespec = decl->typespec;

    assert(typespec);

    Type* type = resolve_typespec(resolver, typespec);

    if (!type) {
        return false;
    }

    if (type_is_incomplete_array(type)) {
        resolver_on_error(resolver, decl->super.range, "Procedure parameter cannot be an array with an inferred length.");
        return false;
    }

    if (!try_complete_aggregate_type(resolver, type)) {
        return false;
    }

    if (type->size == 0) {
        resolver_on_error(resolver, decl->super.range, "Cannot declare a parameter of zero size.");
        return false;
    }

    if (decl->flags & DECL_VAR_IS_VARIADIC) {
        type = type_slice(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.slices, &resolver->ctx->type_cache.ptrs, type);
    }

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static DeclAnnotation* find_annotation(List* annotations, Annotation which)
{
    List* head = annotations;

    for (List* it = head->next; it != head; it = it->next) {
        DeclAnnotation* annotation = list_entry(it, DeclAnnotation, lnode);
        const char* name_cstr = annotation->ident->str;

        if (name_cstr == annotation_names[which]) {
            return annotation;
        }
    }

    return NULL;
}

static bool check_foreign_ident_name(const char* str, size_t len)
{
    if (!len) {
        return false;
    }

    // Cannot just be '_'
    if (str[0] == '_' && len == 1) {
        return false;
    }

    // Characters must be alphanumeric (or '_')
    for (size_t i = 0; i < len; i += 1) {
        if (!is_alphanum(str[i])) {
            return false;
        }
    }

    return true;
}

bool resolve_decl_proc(Resolver* resolver, Symbol* sym)
{
    DeclProc* decl = (DeclProc*)sym->decl;

    bool is_variadic = decl->is_variadic;
    bool is_incomplete = decl->is_incomplete;
    bool is_foreign = decl->super.flags & DECL_IS_FOREIGN;
    bool is_intrinsic = decl->super.name->kind == IDENTIFIER_INTRINSIC;

    if (is_foreign && !is_incomplete) {
        // TODO: Need a ProgRange for just the procedure header.
        resolver_on_error(resolver, decl->super.range, "Foreign declaration cannot have a body");
        return false;
    }

    if (is_foreign && decl->is_variadic) {
        resolver_on_error(resolver, decl->super.range, "Foreign procedures cannot use nibble-style variadic parameters");
        return false;
    }

    // Get the library for the foreign procedure.
    if (is_foreign) {
        DeclAnnotation* foreign_anno = find_annotation(&decl->super.annotations, ANNOTATION_FOREIGN);
        assert(foreign_anno);

        List* args = &foreign_anno->args;
        u32 num_args = foreign_anno->num_args;

        if (!num_args || (num_args > 2)) {
            resolver_on_error(resolver, foreign_anno->range,
                              "Foreign declaration must have 1 or 2 arguments: <lib_name> [, <foreign_func_name>].");
            return false;
        }

        ExprStr* foreign_lib_arg = NULL;
        ExprStr* foreign_name_arg = NULL;
        u32 arg_index = 0;

        for (List* it = args->next; it != args; it = it->next) {
            ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);

            if (!resolve_expr(resolver, arg->expr, NULL)) {
                return false;
            }

            // TODO: Proper typechecking (is_constrexpr && is_string_type)
            if (arg->expr->kind != CST_ExprStr) {
                resolver_on_error(resolver, arg->expr->range, "Arguments to foreign annotation must be string literals, but got %s.",
                                  type_name(arg->expr->type));
                return false;
            }

            // TODO: Allow named args
            if (arg_index == 0) {
                foreign_lib_arg = (ExprStr*)(arg->expr);
            }
            else if (arg_index == 1) {
                foreign_name_arg = (ExprStr*)(arg->expr);
            }

            arg_index += 1;
        }

        if (!nibble_add_foreign_lib(resolver->ctx, foreign_lib_arg->str_lit)) {
            resolver_on_error(resolver, foreign_lib_arg->super.range, "Unsupported library type for `%s`",
                              foreign_lib_arg->str_lit->str);
            return false;
        }

        StrLit* foreign_name = NULL;

        // Set symbol's foreign name. If not provided as an annotation arg, copy the symbol's current name.
        // Otherwise, we need to check that the provided foreign name is a valid 'C' identifier.
        if (!foreign_name_arg) {
            foreign_name = intern_str_lit(&resolver->ctx->str_lit_map, sym->name->str, sym->name->len);
        }
        else {
            foreign_name = foreign_name_arg->str_lit;

            if (!check_foreign_ident_name(foreign_name->str, foreign_name->len)) {
                resolver_on_error(resolver, foreign_name_arg->super.range, "Invalid identifer for foreign procedure `%.*s`.",
                                  foreign_name->len, foreign_name->str);
                return false;
            }
        }

        sym->as_proc.foreign_name = foreign_name;
    }

    if (is_incomplete && !(is_foreign || is_intrinsic)) {
        resolver_on_error(resolver, decl->super.range, "Procedure `%s` must have a body", decl->super.name->str);
        return false;
    }

    decl->scope = push_scope(resolver, decl->num_params + decl->num_decls);

    AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);
    Type** params = array_create(&resolver->ctx->tmp_mem, Type*, 16);
    List* head = &decl->params;

    for (List* it = head->next; it != head; it = it->next) {
        Decl* proc_param = list_entry(it, Decl, lnode);
        Symbol* param_sym = add_unresolved_symbol(&resolver->ctx->ast_mem, decl->scope, sym->home, proc_param);

        assert(param_sym);

        if (!resolve_proc_param(resolver, param_sym)) {
            allocator_restore_state(mem_state);
            return false;
        }

        Type* param_type = param_sym->type;

        if (is_foreign && ((param_type->kind == TYPE_ARRAY) || type_is_slice(param_type))) {
            resolver_on_error(resolver, proc_param->range,
                              "Foreign procedures cannot have array type arguments. Use pointers instead.");
            allocator_restore_state(mem_state);
            return false;
        }

        array_push(params, param_type);
    }

    pop_scope(resolver);
    assert(array_len(params) == decl->num_params);

    Type* ret_type = builtin_types[BUILTIN_TYPE_VOID].type;

    if (decl->ret) {
        ret_type = resolve_typespec(resolver, decl->ret);

        if (!ret_type) {
            return false;
        }

        if (!try_complete_aggregate_type(resolver, ret_type)) {
            return false;
        }

        if (type_is_incomplete_array(ret_type)) {
            resolver_on_error(resolver, decl->ret->range, "Procedure return type cannot be an array with an inferred length.");
            return false;
        }

        if (ret_type->size == 0) {
            resolver_on_error(resolver, decl->super.range, "Invalid procedure return type `%s` of zero size.", type_name(ret_type));
            return false;
        }
    }

    sym->type = type_proc(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.procs, array_len(params), params, ret_type, is_variadic);
    sym->status = SYMBOL_STATUS_RESOLVED;

    allocator_restore_state(mem_state);
    return true;
}
