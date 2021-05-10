#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "nibble.h"
#include "allocator.h"
#include "stream.h"
#include "llist.h"
#include "ast.h"
#include "types.h"

typedef struct Symbol Symbol;
typedef struct Program Program;
typedef struct ResolvedExpr ResolvedExpr;

typedef enum SymbolKind {
    SYMBOL_NONE,
    SYMBOL_VAR,
    SYMBOL_CONST,
    SYMBOL_PROC,
    SYMBOL_TYPE,
} SymbolKind;

typedef enum SymbolStatus {
    SYMBOL_STATUS_UNRESOLVED,
    SYMBOL_STATUS_RESOLVING,
    SYMBOL_STATUS_RESOLVED,
} SymbolStatus;

struct Symbol {
    SymbolKind kind;
    SymbolStatus status;
    bool is_local;
    const char* name;
    Decl* decl;
    Type* type;
    Scalar const_val;
};

struct ResolvedExpr {
    Type* type;
    bool is_lvalue;
    bool is_const;
    ScalarKind scalar_kind;
    Scalar value;
};

struct Program {
    Allocator gen_mem;
    Allocator tmp_mem;
    Allocator ast_mem;
    ByteStream errors;

    //Scope* global_scope;
    //Scope* curr_scope;
};

Program* compile_program(const char* path);
void free_program(Program* program);
#endif
