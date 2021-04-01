#ifndef NIBBLE_AST_H
#define NIBBLE_AST_H
#include <stddef.h>
#include <stdint.h>

#include "nibble.h"
#include "allocator.h"
#include "llist.h"
#include "lexer.h"

typedef struct Expr Expr;
typedef struct TypeSpec TypeSpec;
typedef struct Decl Decl;
typedef struct Stmt Stmt;

///////////////////////////////
//       Type Specifiers
//////////////////////////////
typedef struct TypeSpecParam {
    TypeSpec* type;
    const char* name;
    DLList list;
} TypeSpecParam;

typedef struct TypeSpecIdent {
    const char* name;
} TypeSpecIdent;

typedef struct TypeSpecFunc {
    size_t num_params;
    DLList params;
    TypeSpec* ret;
} TypeSpecFunc;

typedef struct TypeSpecPtr {
    TypeSpec* base;
} TypeSpecPtr;

typedef struct TypeSpecArray {
    TypeSpec* base;
    Expr* len;
} TypeSpecArray;

typedef struct TypeSpecConst {
    TypeSpec* base;
} TypeSpecConst;

typedef enum TypeSpecKind {
    TYPE_SPEC_NONE,
    TYPE_SPEC_IDENT,
    TYPE_SPEC_FUNC,
    TYPE_SPEC_PTR,
    TYPE_SPEC_ARRAY,
    TYPE_SPEC_CONST,
} TypeSpecKind;

struct TypeSpec {
    TypeSpecKind kind;
    ProgRange range;

    union {
        TypeSpecIdent ident;
        TypeSpecFunc func;
        TypeSpecPtr ptr;
        TypeSpecArray array;
        TypeSpecConst const_;
    };
};

TypeSpec* typespec_alloc(Allocator* allocator, TypeSpecKind kind, ProgRange range);
TypeSpec* typespec_ident(Allocator* allocator, const char* name, ProgRange range);
TypeSpec* typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range);
TypeSpec* typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range);
TypeSpec* typespec_const(Allocator* allocator, TypeSpec* base, ProgRange range);
TypeSpecParam* typespec_func_param(Allocator* allocator, TypeSpec* type, const char* name);
TypeSpec* typespec_func(Allocator* allocator, size_t num_params, DLList* params, TypeSpec* ret, ProgRange range);

char* ftprint_typespec(Allocator* allocator, TypeSpec* type);
///////////////////////////////
//       Expressions
//////////////////////////////
typedef struct ExprTernary {
    Expr* cond;
    Expr* then_expr;
    Expr* else_expr;
} ExprTernary;

typedef struct ExprBinary {
    TokenKind op;
    Expr* left;
    Expr* right;
} ExprBinary;

typedef struct ExprUnary {
    TokenKind op;
    Expr* expr;
} ExprUnary;

typedef struct ExprCallArg {
    Expr* expr;
    const char* name;
    DLList list;
} ExprCallArg;

typedef struct ExprCall {
    Expr* func;
    size_t num_args;
    DLList args;
} ExprCall;

typedef struct ExprIndex {
    Expr* array;
    Expr* index;
} ExprIndex;

typedef struct ExprField {
    Expr* object;
    const char* field;
} ExprField;

typedef struct ExprInt {
    uint64_t value;
} ExprInt;

typedef struct ExprFloat {
    double value;
} ExprFloat;

typedef struct ExprStr {
    const char* value;
} ExprStr;

typedef struct ExprIdent {
    const char* name;
} ExprIdent;

typedef struct ExprCast {
    TypeSpec* type;
    Expr* expr;
} ExprCast;

typedef enum ExprSizeofKind {
    EXPR_SIZEOF_ARG_TYPE,
    EXPR_SIZEOF_ARG_EXPR,
} ExprSizeofKind;

typedef struct ExprSizeof {
    ExprSizeofKind kind;
    union {
        TypeSpec* type;
        Expr* expr;
    };
} ExprSizeof;

typedef enum ExprInitializerKind {
    EXPR_INITIALIZER_POS,
    EXPR_INITIALIZER_NAME,
    EXPR_INITIALIZER_INDEX,
} ExprInitializerKind;

typedef struct ExprInitializer {
    ExprInitializerKind kind;
    ProgRange range;
    Expr* init;
    union {
        const char* name;
        Expr* index;
    };

    DLList list;
} ExprInitializer;

typedef struct ExprCompoundLit {
    TypeSpec* type;
    size_t num_initzers;
    DLList initzers;
} ExprCompoundLit;

typedef enum ExprKind {
    EXPR_NONE,
    EXPR_TERNARY,
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_CALL,
    EXPR_INDEX,
    EXPR_FIELD,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STR,
    EXPR_IDENT,
    EXPR_CAST,
    EXPR_SIZEOF,
    EXPR_COMPOUND_LIT,
} ExprKind;

struct Expr {
    ExprKind kind;
    ProgRange range;

    union {
        ExprTernary eternary;
        ExprBinary ebinary;
        ExprUnary eunary;
        ExprCall ecall;
        ExprIndex eindex;
        ExprField efield;
        ExprInt eint;
        ExprFloat efloat;
        ExprStr estr;
        ExprIdent eident;
        ExprCast ecast;
        ExprSizeof esizeof;
        ExprCompoundLit ecompound;
    };
};

Expr* expr_ternary(Allocator* allocator, Expr* cond, Expr* then_expr, Expr* else_expr);
Expr* expr_binary(Allocator* allocator, TokenKind op, Expr* left, Expr* right);
Expr* expr_unary(Allocator* allocator, TokenKind op, Expr* expr, ProgRange range);
Expr* expr_field(Allocator* allocator, Expr* object, const char* field, ProgRange range);
Expr* expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range);
Expr* expr_call(Allocator* allocator, Expr* func, size_t num_args, DLList* args, ProgRange range);
ExprCallArg* expr_call_arg(Allocator* allocator, Expr* expr, const char* name);
Expr* expr_int(Allocator* allocator, uint64_t value, ProgRange range);
Expr* expr_float(Allocator* allocator, double value, ProgRange range);
Expr* expr_str(Allocator* allocator, const char* value, ProgRange range);
Expr* expr_ident(Allocator* allocator, const char* name, ProgRange range);
Expr* expr_cast(Allocator* allocator, TypeSpec* type, Expr* unary, ProgRange range);
Expr* expr_sizeof_type(Allocator* allocator, TypeSpec* type, ProgRange range);
Expr* expr_sizeof_expr(Allocator* allocator, Expr* arg, ProgRange range);
ExprInitializer* expr_pos_initializer(Allocator* allocator, Expr* init, ProgRange range);
ExprInitializer* expr_name_initializer(Allocator* allocator, const char* name, Expr* init, ProgRange range);
ExprInitializer* expr_index_initializer(Allocator* allocator, Expr* index, Expr* init, ProgRange range);
Expr* expr_compound_lit(Allocator* allocator, TypeSpec* type, size_t num_initzers, DLList* initzers, ProgRange range);

char* ftprint_expr(Allocator* allocator, Expr* expr);
/////////////////////////////
//        Statements
/////////////////////////////
typedef struct StmtBlock {
    size_t num_stmts;
    Stmt** stmts;
} StmtBlock;

typedef struct StmtCondBlock {
    Expr* cond;
    StmtBlock block;
} StmtCondBlock;

typedef struct StmtIf {
    StmtCondBlock if_;

    size_t num_elifs;
    StmtCondBlock* elifs;

    StmtBlock else_;
} StmtIf;

typedef struct StmtFor {
    Stmt* init;
    Expr* cond;
    Stmt* next;
    StmtBlock block;
} StmtFor;

typedef struct StmtSwitchCase {
    Expr* start; // NOTE: Both start and end are null for default case.
    Expr* end;

    size_t num_stmts;
    Stmt** stmts;
} StmtSwitchCase;

typedef struct StmtSwitch {
    Expr* expr;
    size_t num_cases;
    StmtSwitchCase* cases;
} StmtSwitch;

typedef struct StmtReturn {
    Expr* expr;
} StmtReturn;

typedef struct StmtBreak {
    const char* label;
} StmtBreak;

typedef struct StmtContinue {
    const char* label;
} StmtContinue;

typedef struct StmtLabel {
    const char* label;
} StmtLabel;

typedef struct StmtGoto {
    const char* label;
} StmtGoto;

typedef struct StmtExpr {
    Expr* expr;
} StmtExpr;

typedef struct StmtExprAssign {
    Expr* left;
    TokenKind op;
    Expr* right;
} StmtExprAssign;

typedef struct StmtDecl {
    Decl* decl;
} StmtDecl;

typedef enum StmtKind {
    STMT_IF,
    STMT_WHILE,
    STMT_DO_WHILE,
    STMT_FOR,
    STMT_SWITCH,
    STMT_RETURN,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_GOTO,
    STMT_LABEL,
    STMT_EXPR,
    STMT_EXPR_ASSIGN,
    STMT_DECL,
    STMT_BLOCK,
} StmtKind;

struct Stmt {
    StmtKind kind;

    union {
        StmtIf sif;
        StmtCondBlock swhile;
        StmtCondBlock sdo_while;
        StmtFor sfor;
        StmtSwitch sswitch;
        StmtReturn sreturn;
        StmtBreak sbreak;
        StmtContinue scontinue;
        StmtLabel slabel;
        StmtGoto sgoto;
        StmtExpr sexpr;
        StmtExprAssign sassign;
        StmtDecl sdecl;
        StmtBlock sblock;
    };
};

///////////////////////////////
//       Declarations
//////////////////////////////
typedef struct DeclVar {
    TypeSpec* type;
    Expr* expr;
} DeclVar;

typedef struct DeclEnumItem {
    const char* name;
    Expr* value;
} DeclEnumItem;

typedef struct DeclEnum {
    size_t num_items;
    DeclEnumItem* items;
} DeclEnum;

typedef struct DeclAggregateFieldItem {
    size_t num_names;
    const char* names;
    TypeSpec* type;
} DeclAggregateFieldItem;

typedef enum DeclAggregateItemKind {
    DECL_AGGREGATE_ITEM_NONE,
    DECL_AGGREGATE_ITEM_FIELD,
    DECL_AGGREGATE_ITEM_SUBAGGREGATE,
} DeclAggregateItemKind;

typedef struct DeclAggregateItem {
    DeclAggregateItemKind kind;

    union {
        DeclAggregateFieldItem field;
        Decl* subaggregate;
    };
} DeclAggregateItem;

typedef struct DeclAggregate {
    size_t num_items;
    DeclAggregateItem* items;
} DeclAggregate;

typedef struct DeclFuncParam {
    const char* name;
    TypeSpec* type;
} DeclFuncParam;

typedef struct DeclFunc {
    size_t num_params;
    DeclFuncParam* params;
    TypeSpec* ret;
    StmtBlock block;
} DeclFunc;

typedef struct DeclTypedef {
    TypeSpec* type;
} DeclTypedef;

typedef enum DeclKind {
    DECL_NONE,
    DECL_VAR,
    DECL_ENUM,
    DECL_UNION,
    DECL_STRUCT,
    DECL_FUNC,
    DECL_TYPEDEF,
} DeclKind;

struct Decl {
    DeclKind kind;
    const char* name;

    union {
        DeclVar dvar;
        DeclEnum denum;
        DeclAggregate daggregate;
        DeclFunc dfunc;
        DeclTypedef dtypedef;
    };
};

#endif
