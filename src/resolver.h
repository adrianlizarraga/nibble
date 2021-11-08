#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "compiler.h"

#define MAX_LOCAL_SYMS 512

typedef struct Resolver Resolver;

struct Resolver {
    NibbleCtx* ctx;
    Module* curr_mod;
    Scope* curr_scope;
};

void init_resolver(Resolver* resolver, NibbleCtx* ctx, Module* mod);
bool resolve_module(Resolver* resolver, Module* mod);
bool resolve_reachable_sym_defs(Resolver* resolver);
#endif
