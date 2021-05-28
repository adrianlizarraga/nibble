#ifndef NIBBLE_AST_H
#define NIBBLE_AST_H

#include <stddef.h>
#include <stdint.h>

#include "ast.h"

typedef struct AST_Program AST_Program;
typedef struct AST_Scope AST_Scope;
typedef struct AST_Var AST_Var;
typedef struct AST_Proc AST_Proc;
typedef struct AST_Stmt AST_Stmt;
typedef struct AST_Expr AST_Expr;

struct AST_Scope {
    size_t num_vars;
    AST_Var** vars;

    size_t num_stmts;
    AST_Stmt** stmts;
};

typedef enum AST_StmtKind {
    AST_STMT_NONE,
    AST_StmtNoOp,
    AST_StmtBlock,
    AST_StmtIf, // Only need nested if/else to support elif
    AST_StmtWhile, // For-loop is a while-loop nested in a parent scope
    AST_StmtDoWhile,
    AST_StmtReturn,
    AST_StmtBreak,
    AST_StmtContinue,
    AST_StmtExpr,
    AST_StmtExprAssign,
} AST_StmtKind;

struct AST_Stmt {
    AST_StmtKind kind;
    
    union {
        AST_Scope as_block;

        struct {
            AST_Expr* if_cond;
            AST_Stmt* if_body;
            AST_Stmt* else_body;
        } as_if;

        struct {
            AST_Expr* cond;
            AST_Stmt* body;
        } as_while; // NOTE: Handles both while and do-while

        struct {
            AST_Expr* expr;
        } as_return;

        struct {
            AST_Expr* expr;
        } as_expr;

        struct {
            AST_Expr* left;
            AST_Expr* right;
            TokenKind op;
        } as_expr_assign;
    };
};

typedef enum AST_ExprKind {
    AST_EXPR_NONE,
    AST_ExprTernary,
    AST_ExprBinary,
    AST_ExprUnary,
    AST_ExprCall,
    AST_ExprIndex,
    AST_ExprField,
    AST_ExprInt,
    AST_ExprFloat,
    AST_ExprStr,
    AST_ExprVar,
    AST_ExprProc,
} AST_ExprKind;

struct AST_Expr { // NOTE: identifier references should only variables at this point?
    AST_ExprKind kind;
    Type* type;

    union {
        struct {
            AST_Expr* cond;
            AST_Expr* then_expr;
            AST_Expr* else_expr;
        } as_ternary;

        struct {
            TokenKind op;
            AST_Expr* left;
            AST_Expr* right;
        } as_binary;

        struct {
            TokenKind op;
            AST_Expr* expr;
        } as_unary;

        struct {
            AST_Expr* proc;
            size_t num_args;
            AST_Expr** args;
        } as_call;

        struct {
            AST_Expr* array;
            AST_Expr* index;
        } as_index;

        struct {
            AST_Expr* object;
            const char* name;
        } as_field;

        struct {
            uint64_t value;
        } as_int;

        struct {
            Float value;
        } as_float;

        struct {
            const char* value;
        } as_str;

        struct {
            AST_Var* var;
        } as_var;

        struct {
            AST_Proc* proc;
        } as_proc;
    };
};

struct AST_Var {
    const char* name;
    Type* type;

    // NOTE: Only for global variables.
    // Local variables are initialized via a separate assignment statement.
    AST_Expr* init;
};

struct AST_Proc {
    const char* name;
    Type* type;

    // NOTE: This scope contains parameter variables and a single body statement.
    AST_Scope scope;
};

struct AST_Program {
    Allocator mem;

    size_t num_vars;
    AST_Var** vars;

    size_t num_procs;
    AST_Proc** procs;

    HMap type_ptr_cache;
    HMap type_proc_cache;
};

AST_Program* compile_program(const char* path);
void free_program(AST_Program* program);
#endif
