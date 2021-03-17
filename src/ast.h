#ifndef NIBBLE_AST_H
#define NIBBLE_AST_H
#include <stdint.h>
#include <stddef.h>

typedef struct Expr Expr;
typedef struct TypeSpec TypeSpec;
typedef struct Decl Decl;

///////////////////////////////
//       Type Specifiers 
//////////////////////////////
typedef struct TypeSpecIdentifier {
    const char* name;
} TypeSpecIdentifier;

typedef struct TypeSpecFunc {
    size_t num_params;
    TypeSpec** params;
    TypeSpec* ret;
} TypeSpecFunc;

typedef struct TypeSpecPtr {
    TypeSpec* base;
} TypeSpecPtr;

typedef struct TypeSpecArray {
    TypeSpec* base;
    Expr* len;
} TypeSpecArray;

typedef enum TypeSpecKind {
    TYPE_SPEC_NONE,
    TYPE_SPEC_IDENTIFIER,
    TYPE_SPEC_FUNC,
    TYPE_SPEC_PTR,
    TYPE_SPEC_ARRAY,
} TypeSpecKind;

struct TypeSpec {
    TypeSpecKind kind;

    union {
        TypeSpecIdentifier tsidentifier;
        TypeSpecFunc tsfunc;
        TypeSpecPtr tsptr;
        TypeSpecArray tsarray;
    };
};

///////////////////////////////
//       Expressions 
//////////////////////////////
typedef struct ExprTernary {
    Expr* condition;
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

typedef struct ExprCall {
    Expr* func;
    size_t num_args;
    Expr** args;     // TODO: Named arguments
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

typedef struct ExprIdentifier {
    const char* name;
} ExprIdentifier;

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

typedef enum ExprCompoundLitInitKind {
    EXPR_COMPOUND_LIT_INIT_NAMED,
    EXPR_COMPOUND_LIT_INIT_INDEXED,
} ExprCompoundLitInitKind;

typedef struct ExprCompoundLitInit {
    ExprCompoundLitInitKind kind;
    Expr* init;
    union {
        const char* name;    
        Expr* index;
    };
} ExprCompoundLitInit;

typedef struct ExprCompoundLit {
    TypeSpec* type;
    size_t num_initzers;
    ExprCompoundLitInit* initzers;
} ExprCompoundLit;

typedef struct ExprParent {
    Expr* expr;
} ExprParent;

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
    EXPR_IDENTIFIER,
    EXPR_CAST,
    EXPR_SIZEOF,
    EXPR_COMPOUND,
    EXPR_PARENT,
} ExprKind;

struct Expr {
    ExprKind kind;

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
        ExprIdentifier eidentifier;
        ExprCast ecast;
        ExprSizeof esizeof;
        ExprCompoundLit ecompound;
        ExprParent eparent;
    };
};

///////////////////////////////
//       Declarations 
//////////////////////////////
typedef struct DeclVar {
    TypeSpec* type;
    Expr* expr;
} DeclVar;

typedef struct DeclConst {
    TypeSpec* type;
    Expr* expr;
} DeclConst;

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
    //StmtBlock stmt_block;
} DeclFunc;

typedef struct DeclTypedef {
    TypeSpec* type;
} DeclTypedef;

typedef enum DeclKind {
    DECL_NONE,
    DECL_VAR,
    DECL_CONST,
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
        DeclConst dconst;
        DeclEnum denum;
        DeclAggregate daggregate;
        DeclFunc dfunc;
        DeclTypedef dtypedef;
    };
};
#endif
