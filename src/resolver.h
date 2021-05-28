#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "nibble.h"
#include "allocator.h"
#include "stream.h"
#include "llist.h"
#include "cst.h"

#define MAX_LOCAL_SYMS 512

typedef struct Resolver Resolver;

struct Resolver {
    Allocator* ast_mem;
    Allocator* tmp_mem;
    ByteStream* errors;
    TypeCache* type_cache;

    HMap global_syms;
    List syms_list;

    // TODO: Create an expandable stack similar to the arena allocator
    Symbol local_syms[MAX_LOCAL_SYMS];
    Symbol* local_syms_at;
};

void init_resolver(Resolver* resolver, Allocator* ast_mem, Allocator* tmp_mem, ByteStream* errors,
                   TypeCache* type_cache);
void free_resolver(Resolver* resolver);
bool resolve_global_decls(Resolver* resolver, List* decls);
#endif
