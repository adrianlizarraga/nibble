#ifndef NIBBLE_IR_H
#define NIBBLE_IR_H

#include <stddef.h>
#include <stdint.h>

#include "ast.h"

typedef struct IR_Program IR_Program;
typedef struct IR_Scope IR_Scope;
typedef struct IR_Var IR_Var;
typedef struct IR_Proc IR_Proc;
typedef struct IR_Stmt IR_Stmt;
typedef struct IR_Expr IR_Expr;

struct IR_Scope {
    size_t num_vars;
    IR_Var** vars;

    size_t num_stmts;
    IR_Stmt** stmts;
};

typedef enum IR_StmtKind {
    IR_STMT_NONE,
    IR_STMT_NOOP,
    IR_STMT_BLOCK,
    IR_STMT_IF, // Only need nested if/else to support elif
    IR_STMT_WHILE, // For-loop is a while-loop nested in a parent scope
    IR_STMT_DO_WHILE,
    IR_STMT_RETURN,
    IR_STMT_BREAK,
    IR_STMT_CONTINUE,
    IR_STMT_EXPR,
    IR_STMT_EXPR_ASSIGN,
} IR_StmtKind;

struct IR_Stmt {
    IR_StmtKind kind;
    
    union {
        IR_Scope as_block;

        struct {
            IR_Expr* if_cond;
            IR_Stmt* if_body;
            IR_Stmt* else_body;
        } as_if;

        struct {
            IR_Expr* cond;
            IR_Stmt* body;
        } as_while; // NOTE: Handles both while and do-while

        struct {
            IR_Expr* expr;
        } as_return;

        struct {
            IR_Expr* expr;
        } as_expr;

        struct {
            IR_Expr* left;
            IR_Expr* right;
            TokenKind op;
        } as_expr_assign;
    };
};

typedef enum IR_ExprKind {
    IR_EXPR_NONE,
    IR_EXPR_TERNARY,
    IR_EXPR_BINARY,
    IR_EXPR_UNARY,
    IR_EXPR_CALL,
    IR_EXPR_INDEX,
    IR_EXPR_FIELD,
    IR_EXPR_INT,
    IR_EXPR_FLOAT,
    IR_EXPR_STR,
    IR_EXPR_VAR,
    IR_EXPR_PROC,
} IR_ExprKind;

struct IR_Expr { // NOTE: identifier references should only variables at this point?
    IR_ExprKind kind;
    Type* type;

    union {
        struct {
            IR_Expr* cond;
            IR_Expr* then_expr;
            IR_Expr* else_expr;
        } as_ternary;

        struct {
            TokenKind op;
            IR_Expr* left;
            IR_Expr* right;
        } as_binary;

        struct {
            TokenKind op;
            IR_Expr* expr;
        } as_unary;

        struct {
            IR_Expr* proc;
            size_t num_args;
            IR_Expr** args;
        } as_call;

        struct {
            IR_Expr* array;
            IR_Expr* index;
        } as_index;

        struct {
            IR_Expr* object;
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
            IR_Var* var;
        } as_var;

        struct {
            IR_Proc* proc;
        } as_proc;
    };
};

struct IR_Var {
    const char* name;
    Type* type;

    // NOTE: Only for global variables.
    // Local variables are initialized via a separate assignment statement.
    IR_Expr* init;
};

struct IR_Proc {
    const char* name;
    Type* type;

    // NOTE: This scope contains parameter variables and a single body statement.
    IR_Scope scope;
};

struct IR_Program {
    Allocator mem;

    size_t num_vars;
    IR_Var** vars;

    size_t num_procs;
    IR_Proc** procs;

    HMap type_ptr_cache;
    HMap type_proc_cache;
};

IR_Program* compile_program(const char* path);
void free_program(IR_Program* program);
#endif
