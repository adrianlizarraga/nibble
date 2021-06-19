#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "nibble.h"
#include "allocator.h"
#include "stream.h"
#include "llist.h"
#include "ast.h"
#include "bytecode.h"

#define MAX_LOCAL_SYMS 512

typedef struct Resolver Resolver;

// Exprs have a Type*
// Create hmap for const exprs

struct Resolver {
    Allocator* ast_mem;
    Allocator* tmp_mem;
    ByteStream* errors;
    TypeCache* type_cache;

    // TODO: Only need one reusable scope tree per procedure. Can use tmp memory state restoration
    // to reuse mem.
    Scope* global_scope;
    Scope* curr_scope;
    Symbol** incomplete_syms;
    BucketList* curr_instrs_bucket;
    BucketList procs;
};

void init_resolver(Resolver* resolver, Allocator* ast_mem, Allocator* tmp_mem,
                   ByteStream* errors, TypeCache* type_cache, Scope* global_scope);
bool resolve_global_decls(Resolver* resolver, List* decls);
#endif
