#include "ast.h"

TypeSpec* typespec_alloc(Allocator* allocator, TypeSpecKind kind, ProgRange range)
{
    TypeSpec* type = mem_allocate(allocator, sizeof(TypeSpec), DEFAULT_ALIGN, true); 
    type->kind = kind;
    type->range = range;

    return type;
}

TypeSpec* typespec_identifier(Allocator* allocator, const char* name, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_IDENTIFIER, pos); 
    type->tsidentifier.name = mem_dup(allocator, name, strlen(name) + 1, DEFAULT_ALIGN);

    return type;
}

TypeSpec* typespec_func(Allocator* allocator, size_t num_params, TypeSpec** params, TypeSpec* ret, ProgRange range)
{
    
}
