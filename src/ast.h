#ifndef NIBBLE_AST_H
#define NIBBLE_AST_H
#include <stdint.h>
#include <stddef.h>

#include "allocator.h"
#include "lexer.h"

typedef struct Parser {
    Allocator* allocator;
    ByteStream* errors;
    ProgPos start;
    Lexer lexer;
    Token token;
} Parser;

Parser parser_create(Allocator* allocator, const char* str, ProgPos pos, ByteStream* errors);
void parser_destroy(Parser* parser);

bool next_token(Parser* parser);
bool is_token(Parser* parser, TokenKind kind);
bool match_token(Parser* parser, TokenKind kind);
bool expect_token(Parser* parser, TokenKind kind);

typedef struct Expr Expr;
typedef struct TypeSpec TypeSpec;
typedef struct Decl Decl;
typedef struct Stmt Stmt;

///////////////////////////////
//       Type Specifiers 
//////////////////////////////
typedef struct TypeSpecIdent {
    const char* name;
} TypeSpecIdent;

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
    TYPE_SPEC_IDENT,
    TYPE_SPEC_FUNC,
    TYPE_SPEC_PTR,
    TYPE_SPEC_ARRAY,
} TypeSpecKind;

struct TypeSpec {
    TypeSpecKind kind;
    ProgRange range;

    union {
        TypeSpecIdent ident;
        TypeSpecFunc func;
        TypeSpecPtr ptr;
        TypeSpecArray array;
    };
};

TypeSpec* typespec_create(Allocator* allocator, TypeSpecKind kind, ProgRange range);
TypeSpec* typespec_ident(Allocator* allocator, const char* name, ProgRange range);
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
    StmtBlock block;
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
