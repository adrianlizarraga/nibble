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


