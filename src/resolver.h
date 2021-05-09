#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "nibble.h"
#include "allocator.h"
#include "stream.h"
#include "llist.h"
#include "ast.h"
#include "types.h"

typedef struct Module Module;
typedef struct SymbolTyped SymbolTyped;
typedef struct SymbolModule SymbolModule;
typedef struct Symbol Symbol;
typedef struct Program Program;
typedef struct ResolvedExpr ResolvedExpr;

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

struct SymbolTyped {
    Type* type;
    Scalar const_val;
};

struct SymbolModule {
    Module* module;
};

struct Symbol {
    SymbolKind kind;
    SymbolStatus status;
    bool is_local;
    const char* name;
    Module* module;
    Decl* decl;

    union {
        SymbolTyped t;
        SymbolModule m;
    };
};

struct ResolvedExpr {
    Type* type;
    bool is_lvalue;
    bool is_const;
    ScalarKind scalar_kind;
    Scalar value;
};

struct Module {
    const char* path;
    ProgRange range;
    size_t num_decls;
    Decl** decls;
    HMap syms;
};

struct Program {
    Allocator gen_mem;
    Allocator tmp_mem;
    Allocator ast_mem;
    ByteStream errors;

    HMap modules;
    Module* curr_module;
    ProgPos curr_pos;

    List local_syms;
};

Program* compile_program(const char* path);
void free_program(Program* program);
#endif
