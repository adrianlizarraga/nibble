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
    char* name;
    size_t num_decls;
    Decl** decls;
    HashMap syms;
};

typedef struct Program {
    Allocator gen_mem;
    Allocator ast_mem;
    ByteStream errors;

    HashMap modules;
    Module* curr_module;

    HashMap local_syms;
} Program;

Program* compile_program(const char* path);
void free_program(Program* program);
#endif
