static bool try_complete_aggregate_type(Resolver* resolver, Type* type);

typedef struct SeenField {
    Identifier* name;
    ProgRange range;
} SeenField;

// NOTE: Returned object is a stretchy buffer allocated with temporary memory.
// On error, NULL is returned.
static TypeAggregateField* resolve_aggregate_fields(Resolver* resolver, List* in_fields)
{
    TypeAggregateField* out_fields = array_create(&resolver->ctx->tmp_mem, TypeAggregateField, 16);
    HMap seen_fields = hmap(4, &resolver->ctx->tmp_mem); // Used to check for duplicate field names

    List* head = in_fields;

    for (List* it = head->next; it != head; it = it->next) {
        AggregateField* field = list_entry(it, AggregateField, lnode);

        // Check for duplicate field names.
        if (field->name) {
            SeenField* seen_field = hmap_get_obj(&seen_fields, PTR_UINT(field->name));

            if (seen_field) {
                assert(seen_field->name == field->name);
                resolver_on_error(resolver, seen_field->range, "Duplicate member field name `%s`.", field->name->str);
                return NULL;
            }

            seen_field = alloc_type(&resolver->ctx->tmp_mem, SeenField, false);
            seen_field->name = field->name;
            seen_field->range = field->range;

            hmap_put(&seen_fields, PTR_UINT(field->name), PTR_UINT(seen_field));
        }

        // Resolve field's type.
        Type* field_type = resolve_typespec(resolver, field->typespec);
        assert(field_type);

        if (!try_complete_aggregate_type(resolver, field_type)) {
            return NULL;
        }

        if (type_has_incomplete_array(field_type)) {
            resolver_on_error(resolver, field->range, "Member field has an array type of unknown size");
            return NULL;
        }

        if (field_type->size == 0) {
            resolver_on_error(resolver, field->range, "Member field has zero size");
            return NULL;
        }

        TypeAggregateField field_type_info = {.type = field_type, .name = field->name};

        array_push(out_fields, field_type_info);
    }

    return out_fields;
}

static bool complete_aggregate_type(Resolver* resolver, Type* type, DeclAggregate* decl_aggregate)
{
    AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);

    // Resolve each parsed aggregate field into a field type.
    TypeAggregateField* field_types = resolve_aggregate_fields(resolver, &decl_aggregate->fields);

    if (!field_types) {
        return false;
    }

    // Call the appropriate procedure to fill in the aggregate type's size and alignment.
    if (decl_aggregate->super.kind == CST_DeclStruct) {
        complete_struct_type(&resolver->ctx->ast_mem, type, array_len(field_types), field_types);
    }
    else if (decl_aggregate->super.kind == CST_DeclUnion) {
        complete_union_type(&resolver->ctx->ast_mem, type, array_len(field_types), field_types);
    }

    assert(type->kind != TYPE_INCOMPLETE_AGGREGATE);

    allocator_restore_state(mem_state);

    return true;
}

static bool try_complete_aggregate_type(Resolver* resolver, Type* type)
{
    if (type->kind != TYPE_INCOMPLETE_AGGREGATE) {
        return true;
    }

    Symbol* sym = type->as_incomplete.sym;

    if (type->as_incomplete.is_completing) {
        resolver_on_error(resolver, sym->decl->range, "Cannot resolve type `%s` due to cyclic dependency", sym->name->str);
        return false;
    }

    assert(sym->decl->kind == CST_DeclStruct || sym->decl->kind == CST_DeclUnion);

    bool success;
    DeclAggregate* decl_aggregate = (DeclAggregate*)sym->decl;

    ModuleState mod_state = enter_module(resolver, sym->home);
    {
        type->as_incomplete.is_completing = true; // Mark as `completing` to detect dependency cycles.
        success = complete_aggregate_type(resolver, type, decl_aggregate);
    }
    exit_module(resolver, mod_state);

    return success;
}

static Type* resolve_typespec(Resolver* resolver, TypeSpec* typespec)
{
    if (!typespec)
        return builtin_types[BUILTIN_TYPE_VOID].type;

    switch (typespec->kind) {
    case CST_TypeSpecIdent: {
        TypeSpecIdent* ts = (TypeSpecIdent*)typespec;
        Symbol* ident_sym = lookup_ident(resolver, &ts->ns_ident);

        if (!ident_sym) {
            resolver_on_error(resolver, typespec->range, "Undefined type `%s`",
                              ftprint_ns_ident(&resolver->ctx->tmp_mem, &ts->ns_ident));
            return NULL;
        }

        if (ident_sym->kind != SYMBOL_TYPE) {
            resolver_on_error(resolver, typespec->range, "Identifier `%s` is not a type",
                              ftprint_ns_ident(&resolver->ctx->tmp_mem, &ts->ns_ident));
            return NULL;
        }

        return ident_sym->type;
    }
    case CST_TypeSpecTypeof: {
        TypeSpecTypeof* ts = (TypeSpecTypeof*)typespec;

        if (!resolve_expr(resolver, ts->expr, NULL))
            return NULL;

        return ts->expr->type;
    }
    case CST_TypeSpecRetType: {
        TypeSpecRetType* ts = (TypeSpecRetType*)typespec;

        Type* proc_type = NULL;

        // Get proc type from expression.
        if (ts->proc_expr) {
            if (!resolve_expr(resolver, ts->proc_expr, NULL)) {
                return NULL;
            }

            proc_type = ts->proc_expr->type;

            if (proc_type->kind != TYPE_PROC) {
                resolver_on_error(resolver, ts->proc_expr->range, "Expected expression of procedure type, but found type `%s`.",
                                  type_name(proc_type));
                return NULL;
            }
        }
        // Get proc type from current procedure
        else {
            Symbol* curr_proc = resolver->state.proc;

            if (!curr_proc) {
                resolver_on_error(resolver, typespec->range, "Cannot use #ret_type (without an argument) outside of a procedure.");
                return NULL;
            }

            assert(curr_proc->kind == SYMBOL_PROC);

            proc_type = curr_proc->type;
        }

        return proc_type->as_proc.ret;
    }
    case CST_TypeSpecPtr: {
        TypeSpecPtr* ts = (TypeSpecPtr*)typespec;
        TypeSpec* base_ts = ts->base;
        Type* base_type = resolve_typespec(resolver, base_ts);

        if (!base_type)
            return NULL;

        return type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, base_type);
    }
    case CST_TypeSpecArray: {
        TypeSpecArray* ts = (TypeSpecArray*)typespec;
        Type* base_type = resolve_typespec(resolver, ts->base);

        if (!base_type) {
            return NULL;
        }

        if (!try_complete_aggregate_type(resolver, base_type)) {
            return NULL;
        }

        if (base_type->size == 0) {
            resolver_on_error(resolver, ts->base->range, "Array element type must have non-zero size");
            return NULL;
        }

        // Array slice type.
        if (!ts->len && !ts->infer_len) {
            return type_slice(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.slices, &resolver->ctx->type_cache.ptrs, base_type);
        }

        // Actual array type.
        size_t len = 0;

        if (ts->len) {
            assert(!ts->infer_len);

            if (!resolve_expr(resolver, ts->len, NULL))
                return NULL;

            if (!(ts->len->is_constexpr && ts->len->is_imm)) {
                resolver_on_error(resolver, ts->len->range, "Array length must be a compile-time constant");
                return NULL;
            }

            if (ts->len->type->kind != TYPE_INTEGER) {
                resolver_on_error(resolver, ts->len->range, "Array length must be an integer");
                return NULL;
            }

            len = (size_t)(ts->len->imm.as_int._u64);

            if (len == 0) {
                resolver_on_error(resolver, ts->len->range, "Array length must be a positive, non-zero integer");
                return NULL;
            }
        }

        return type_array(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.arrays, base_type, len);
    }
    case CST_TypeSpecProc: {
        TypeSpecProc* ts = (TypeSpecProc*)typespec;

        AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);
        Type** params = array_create(&resolver->ctx->tmp_mem, Type*, ts->num_params);
        List* head = &ts->params;

        for (List* it = head->next; it != head; it = it->next) {
            ProcParam* proc_param = list_entry(it, ProcParam, lnode);
            Type* param = resolve_typespec(resolver, proc_param->typespec);

            if (!param) {
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (type_is_incomplete_array(param)) {
                resolver_on_error(resolver, proc_param->range, "Procedure parameter cannot be an array with an inferred length.");
                return false;
            }

            if (!try_complete_aggregate_type(resolver, param)) {
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (param->size == 0) {
                resolver_on_error(resolver, proc_param->range, "Invalid procedure paramater type `%s` of zero size.",
                                  type_name(param));
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (proc_param->is_variadic) {
                param = type_slice(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.slices, &resolver->ctx->type_cache.ptrs, param);
            }

            array_push(params, param);
        }
        assert(array_len(params) == ts->num_params);

        Type* ret = builtin_types[BUILTIN_TYPE_VOID].type;

        if (ts->ret) {
            ret = resolve_typespec(resolver, ts->ret);

            if (!ret) {
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (type_is_incomplete_array(ret)) {
                assert(ts->ret);
                resolver_on_error(resolver, ts->ret->range, "Procedure return type cannot be an array with an inferred length.");
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (ret->size == 0) {
                resolver_on_error(resolver, ts->ret->range, "Invalid procedure return type `%s` of zero size.", type_name(ret));
                return NULL;
            }
        }

        Type* type =
            type_proc(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.procs, array_len(params), params, ret, ts->is_variadic);
        allocator_restore_state(mem_state);

        return type;
    }
    case CST_TypeSpecStruct: { // Anonymous struct (aka tuples)
        TypeSpecStruct* ts = (TypeSpecStruct*)typespec;

        AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);
        TypeAggregateField* fields = resolve_aggregate_fields(resolver, &ts->fields);

        if (!fields) {
            return NULL;
        }

        Type* type =
            type_anon_aggregate(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.structs, TYPE_STRUCT, array_len(fields), fields);

        assert(type->kind == TYPE_STRUCT);
        allocator_restore_state(mem_state);

        return type;
    }
    case CST_TypeSpecUnion: { // Anonymous union
        TypeSpecUnion* ts = (TypeSpecUnion*)typespec;

        AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);
        TypeAggregateField* fields = resolve_aggregate_fields(resolver, &ts->fields);

        if (!fields) {
            return NULL;
        }

        Type* type =
            type_anon_aggregate(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.unions, TYPE_UNION, array_len(fields), fields);

        assert(type->kind == TYPE_UNION);
        allocator_restore_state(mem_state);

        return type;
    }
    default:
        ftprint_err("Unsupported typespec kind `%d` in resolution\n", typespec->kind);
        assert(0);
        break;
    }

    return NULL;
}

