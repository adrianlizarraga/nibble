static bool resolve_decl_var(Resolver* resolver, Symbol* sym)
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

static bool resolve_decl_typedef(Resolver* resolver, Symbol* sym)
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

static bool resolve_decl_enum(Resolver* resolver, Symbol* sym)
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

    Type* enum_type = type_enum(&resolver->ctx->ast_mem, base_type, decl_enum);

    // Resolve enum items.
    Symbol** item_syms = alloc_array(&resolver->ctx->ast_mem, Symbol*, decl_enum->num_items, false);
    Scalar prev_enum_val = {0};

    List* head = &decl_enum->items;
    List* it = head->next;
    size_t i = 0;

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

    sym->type = enum_type;
    sym->status = SYMBOL_STATUS_RESOLVED;
    sym->as_enum.items = item_syms;
    sym->as_enum.num_items = decl_enum->num_items;

    return true;
}

static bool resolve_decl_const(Resolver* resolver, Symbol* sym)
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

static bool resolve_decl_proc(Resolver* resolver, Symbol* sym)
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

        // TODO: Handle 2nd argument that denotes the foreign proc's original name.
        if (num_args != 1) {
            resolver_on_error(resolver, foreign_anno->range, "Foreign declaration must have 1 argument: lib_name.");
            return false;
        }

        ProcCallArg* lib_arg = list_entry(args->next, ProcCallArg, lnode);

        if (!resolve_expr(resolver, lib_arg->expr, NULL)) {
            return false;
        }

        // TODO: Proper typechecking (is_constrexpr && is_string_type)
        if (lib_arg->expr->kind != CST_ExprStr) {
            resolver_on_error(resolver, lib_arg->expr->range, "Argument to foreign annotation must be a string literal, but got %s.",
                              type_name(lib_arg->expr->type));
            return false;
        }

        // Record name of foreign lib.
        bucket_list_add_elem(&resolver->ctx->foreign_libs, ((ExprStr*)(lib_arg->expr))->str_lit);
        bucket_list_add_elem(&resolver->ctx->foreign_procs, sym);
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
            resolver_on_error(resolver, proc_param->range, "Foreign procedures cannot have array type arguments. Use pointers instead.");
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

