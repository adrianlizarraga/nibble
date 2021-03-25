#include "ast.h"

TypeSpec* typespec_alloc(Allocator* allocator, TypeSpecKind kind, ProgRange range)
{
    TypeSpec* type = mem_allocate(allocator, sizeof(TypeSpec), DEFAULT_ALIGN, true);
    type->kind = kind;
    type->range = range;

    return type;
}

TypeSpec* typespec_ident(Allocator* allocator, const char* name, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_IDENT, range);
    type->ident.name = name;

    return type;
}

TypeSpec* typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_PTR, range);
    type->ptr.base = base;

    return type;
}

TypeSpec* typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_ARRAY, range);
    type->array.base = base;
    type->array.len = len;

    return type;
}

TypeSpec* typespec_func(Allocator* allocator, size_t num_params, DLList* params, TypeSpec* ret, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_FUNC, range);
    type->func.num_params = num_params;
    type->func.ret = ret;

    dllist_replace(params, &type->func.params);

    return type;
}

TypeSpecParam* typespec_func_param(Allocator* allocator, TypeSpec* type, const char* name)
{
    TypeSpecParam* param = mem_allocate(allocator, sizeof(TypeSpecParam), DEFAULT_ALIGN, true);
    param->type = type;
    param->name = name;

    return param;
}
