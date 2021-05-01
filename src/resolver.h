#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "nibble.h"
#include "allocator.h"
#include "stream.h"
#include "ast.h"
#include "types.h"

typedef struct Module Module;

typedef enum SymbolKind {
    SYMBOL_NONE,
    SYMBOL_VAR,
    SYMBOL_CONST,
    SYMBOL_PROC,
    SYMBOL_TYPE,
    SYMBOL_MODULE,
} SymbolKind;

typedef enum SymbolStatus {
    SYMBOL_STATUS_UNRESOLVED,
    SYMBOL_STATUS_RESOLVING,
    SYMBOL_STATUS_RESOLVED,
} SymbolStatus;

typedef struct SymbolTyped {
    Type* type;
    Integer value;
} SymbolTyped;

typedef struct SymbolModule {
    Module* module;
} SymbolModule;

typedef struct Symbol {
    SymbolKind kind;
    SymbolStatus status;
    const char* name;
    Module* module;
    Decl* decl;

    union {
        SymbolTyped t;
        SymbolModule m;
    };
} Symbol;

struct Module {
    Allocator allocator;
    Allocator ast_arena;
    char* name;

    ByteStream errors;

    size_t num_decls;
    Decl** decls;

    HashMap syms;
};

typedef struct Resolver {
    HashMap modules;
    Module* curr_module;

    HashMap local_syms;
} Resolver;

Module* compile_module(const char* filename, ProgPos pos);
void free_module(Module* module);
#endif
