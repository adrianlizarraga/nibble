#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "nibble.h"
#include "allocator.h"
#include "stream.h"
#include "llist.h"
#include "ast.h"

#define MAX_LOCAL_SYMS 512

typedef struct Resolver Resolver;

// Exprs have a Type*
// Create hmap for const exprs

struct Resolver {
    Allocator* ast_mem;
    Allocator* tmp_mem;
    ByteStream* errors;
    TypeCache* type_cache;
    Scope* global_scope;

    Scope* curr_scope;
};

void init_resolver(Resolver* resolver, Allocator* ast_mem, Allocator* tmp_mem, ByteStream* errors,
                   TypeCache* type_cache, Scope* global_scope);
bool resolve_global_decls(Resolver* resolver, List* decls);
#endif
