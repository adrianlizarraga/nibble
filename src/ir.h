#ifndef NIBBLE_IR_H
#define NIBBLE_IR_H

#include <stddef.h>
#include <stdint.h>

#include "ast.h"

typedef struct Scope Scope;
typedef struct IR_VarDecl IR_VarDecl;
typedef struct IR_Stmt IR_Stmt;
typedef struct IR_Expr IR_Expr;

struct IR_VarDecl {
    const char* name;
    Type* type;
};

struct IR_Stmt {
    IR_StmtKind kind;
    
    union {

    };
};

struct IR_Expr { // NOTE: indetifier references should only variables at this point?
    IR_ExprKind kind;
    Type* type;
};

struct Scope {
    size_t num_var_decls;
    IR_VarDecl** var_decls;

    size_t num_stmts;
    IR_Stmt** stmts;
};
#endif
