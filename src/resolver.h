#ifndef NIBBLE_RESOLVER_H
#define NIBBLE_RESOLVER_H
#include "nibble.h"
#include "allocator.h"
#include "stream.h"
#include "llist.h"
#include "ast.h"
#include "types.h"

typedef struct Symbol Symbol;
typedef struct SymbolVar SymbolVar;
typedef struct SymbolConst SymbolConst;
typedef struct SymbolProc SymbolProc;
typedef struct SymbolType SymbolType;
typedef struct Program Program;
typedef struct ResolvedExpr ResolvedExpr;
typedef struct Scope Scope;

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

typedef enum SymbolTypeKind {
    SYMBOL_TYPE_STRUCT,
    SYMBOL_TYPE_UNION,
    SYMBOL_TYPE_ENUM,
    SYMBOL_TYPE_TYPEDEF,
    SYMBOL_TYPE_BASIC,
} SymbolTypeKind;

enum SymbolFlags {
    SYMBOL_IS_LOCAL   = 0x1,
    SYMBOL_IS_BUILTIN = 0x2,
};

struct SymbolVar {
    Decl* decl;
    Type* type;
};

struct SymbolConst {
    Decl* decl;
    Type* type;
    Scalar value;
};

struct SymbolProc {
    Decl* decl;
    Type* type;
};

struct SymbolType {
    SymbolTypeKind kind;
    Decl* decl;
    Type* type;
};

struct Symbol {
    SymbolKind kind;
    SymbolStatus status;
    uint64_t flags;
    const char* name;

    union {
        SymbolVar as_var;
        SymbolConst as_const;
        SymbolProc as_proc;
        SymbolType as_type;
    };

    ListNode lnode;
};

struct ResolvedExpr {
    Type* type;
    bool is_lvalue;
    bool is_const;
    ScalarKind scalar_kind;
    Scalar value;
};

struct Scope {
    List decls;
    HMap syms_map;

    List children;
    Scope* parent;

    ListNode lnode;
};

struct Program {
    Allocator gen_mem;
    Allocator tmp_mem;
    Allocator ast_mem;
    ByteStream errors;

    const char* path;
    const char* code;

    // NOTE: Maps an Expr* to a ResolvedExpr*
    HMap exprs_map;

    Scope global_scope;
    Scope* curr_scope;
};

Program* compile_program(const char* path);
void free_program(Program* program);
#endif
