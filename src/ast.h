#ifndef NIBBLE_AST_H
#define NIBBLE_AST_H
#include <stddef.h>
#include <stdint.h>

#include "allocator.h"
#include "lexer.h"
#include "llist.h"
#include "nibble.h"

typedef struct Expr Expr;
typedef struct TypeSpec TypeSpec;
typedef struct Decl Decl;
typedef struct Stmt Stmt;

///////////////////////////////
//       Type Specifiers
//////////////////////////////
typedef struct AggregateField {
    const char* name;
    TypeSpec* type;
    ProgRange range;
    DLList list;
} AggregateField;

typedef struct AggregateBody {
    size_t num_fields;
    DLList fields;
} AggregateBody;

typedef struct TypeSpecParam {
    TypeSpec* type;
    DLList list;
} TypeSpecParam;

typedef struct TypeSpecIdent {
    const char* name;
} TypeSpecIdent;

typedef struct TypeSpecProc {
    size_t num_params;
    DLList params;
    TypeSpec* ret;
} TypeSpecProc;

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
    TYPE_SPEC_PROC,
    TYPE_SPEC_ANON_STRUCT,
    TYPE_SPEC_ANON_UNION,
    TYPE_SPEC_PTR,
    TYPE_SPEC_ARRAY,
    TYPE_SPEC_CONST,
} TypeSpecKind;

struct TypeSpec {
    TypeSpecKind kind;
    ProgRange range;

    union {
        TypeSpecIdent as_ident;
        TypeSpecProc as_proc;
        AggregateBody as_struct;
        AggregateBody as_union;
        TypeSpecPtr as_ptr;
        TypeSpecArray as_array;
        TypeSpecConst as_const;
    };
};

AggregateField* aggregate_field(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);

TypeSpec* typespec_ident(Allocator* allocator, const char* name, ProgRange range);
TypeSpec* typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range);
TypeSpec* typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range);
TypeSpec* typespec_const(Allocator* allocator, TypeSpec* base, ProgRange range);
TypeSpecParam* typespec_proc_param(Allocator* allocator, TypeSpec* type);
TypeSpec* typespec_proc(Allocator* allocator, size_t num_params, DLList* params, TypeSpec* ret, ProgRange range);

typedef TypeSpec* TypeSpecAggregateProc(Allocator* alloc, size_t num_fields, DLList* fields, ProgRange range);
TypeSpec* typespec_anon_struct(Allocator* allocator, size_t num_fields, DLList* fields, ProgRange range);
TypeSpec* typespec_anon_union(Allocator* allocator, size_t num_fields, DLList* fields, ProgRange range);

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
    Expr* proc;
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

typedef struct ExprSizeof {
    TypeSpec* type;
} ExprSizeof;

typedef struct ExprTypeof {
    Expr* expr;
} ExprTypeof;

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
    EXPR_TYPEOF,
    EXPR_COMPOUND_LIT,
} ExprKind;

struct Expr {
    ExprKind kind;
    ProgRange range;

    union {
        ExprTernary as_ternary;
        ExprBinary as_binary;
        ExprUnary as_unary;
        ExprCall as_call;
        ExprIndex as_index;
        ExprField as_field;
        ExprInt as_int;
        ExprFloat as_float;
        ExprStr as_str;
        ExprIdent as_ident;
        ExprCast as_cast;
        ExprSizeof as_sizeof;
        ExprTypeof as_typeof;
        ExprCompoundLit as_compound;
    };
};

Expr* expr_ternary(Allocator* allocator, Expr* cond, Expr* then_expr, Expr* else_expr);
Expr* expr_binary(Allocator* allocator, TokenKind op, Expr* left, Expr* right);
Expr* expr_unary(Allocator* allocator, TokenKind op, Expr* expr, ProgRange range);
Expr* expr_field(Allocator* allocator, Expr* object, const char* field, ProgRange range);
Expr* expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range);
Expr* expr_call(Allocator* allocator, Expr* proc, size_t num_args, DLList* args, ProgRange range);
ExprCallArg* expr_call_arg(Allocator* allocator, Expr* expr, const char* name);
Expr* expr_int(Allocator* allocator, uint64_t value, ProgRange range);
Expr* expr_float(Allocator* allocator, double value, ProgRange range);
Expr* expr_str(Allocator* allocator, const char* value, ProgRange range);
Expr* expr_ident(Allocator* allocator, const char* name, ProgRange range);
Expr* expr_cast(Allocator* allocator, TypeSpec* type, Expr* arg, ProgRange range);
Expr* expr_sizeof(Allocator* allocator, TypeSpec* type, ProgRange range);
Expr* expr_typeof(Allocator* allocator, Expr* arg, ProgRange range);
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
    DLList stmts;
} StmtBlock;

typedef struct StmtCondBlock {
    Expr* cond;
    StmtBlock block;
} StmtCondBlock;

typedef struct StmtIf {
    StmtCondBlock if_;

    size_t num_elifs;
    DLList elifs; // StmtCondBlocks

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
    DLList stmts;
    DLList list;
} StmtSwitchCase;

typedef struct StmtSwitch {
    Expr* expr;
    size_t num_cases;
    DLList cases;
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
    ProgRange range;
    DLList list;

    union {
        StmtIf as_if;
        StmtCondBlock as_while;
        StmtCondBlock as_do_while;
        StmtFor as_for;
        StmtSwitch as_switch;
        StmtReturn as_return;
        StmtBreak as_break;
        StmtContinue as_continue;
        StmtLabel as_label;
        StmtGoto as_goto;
        StmtExpr as_expr;
        StmtExprAssign as_assign;
        StmtDecl as_decl;
        StmtBlock as_block;
    };
};

///////////////////////////////
//       Declarations
//////////////////////////////
typedef struct DeclVar {
    TypeSpec* type;
    Expr* init;
} DeclVar;

typedef struct DeclConst {
    TypeSpec* type;
    Expr* init;
} DeclConst;

typedef struct DeclEnumItem {
    const char* name;
    Expr* value;
    DLList list;
} DeclEnumItem;

typedef struct DeclEnum {
    TypeSpec* type;
    size_t num_items;
    DLList items;
} DeclEnum;

typedef struct DeclProcParam {
    ProgRange range;
    const char* name;
    TypeSpec* type;
    DLList list;
} DeclProcParam;

typedef struct DeclProc {
    size_t num_params;
    DLList params;
    TypeSpec* ret;
    StmtBlock block;
} DeclProc;

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
    DECL_PROC,
    DECL_TYPEDEF,
} DeclKind;

struct Decl {
    DeclKind kind;
    ProgRange range;
    const char* name;

    union {
        DeclVar as_var;
        DeclConst as_const;
        DeclEnum as_enum;
        AggregateBody as_struct;
        AggregateBody as_union;
        DeclProc as_proc;
        DeclTypedef as_typedef;
    };
};

Decl* decl_var(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);
Decl* decl_enum(Allocator* allocator, const char* name, TypeSpec* type, size_t num_items, DLList* items,
                ProgRange range);
DeclEnumItem* decl_enum_item(Allocator* allocator, const char* name, Expr* value);

typedef Decl* DeclAggregateProc(Allocator* alloc, const char* name, size_t num_fields, DLList* fields, ProgRange range);
Decl* decl_struct(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range);
Decl* decl_union(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range);
#endif
