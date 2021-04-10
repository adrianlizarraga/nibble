#ifndef NIBBLE_AST_H
#define NIBBLE_AST_H
#include <stddef.h>
#include <stdint.h>

#include "nibble.h"
#include "llist.h"
#include "allocator.h"
#include "lexer.h"

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
typedef enum StmtKind {
    STMT_NONE,
    STMT_If,
    STMT_While,
    STMT_DoWhile,
    STMT_For,
    STMT_Switch,
    STMT_Return,
    STMT_Break,
    STMT_Continue,
    STMT_Expr,
    STMT_ExprAssign,
    STMT_Decl,
    STMT_Block,
} StmtKind;

struct Stmt {
    StmtKind kind;
    ProgRange range;
    DLList list;
};

typedef struct StmtBlock {
    Stmt base;
    size_t num_stmts;
    DLList stmts;
} StmtBlock;

typedef struct IfCondBlock {
    ProgRange range;
    Expr* cond;
    size_t num_stmts;
    DLList stmts;
} IfCondBlock;

typedef struct ElifBlock {
    IfCondBlock block;
    DLList list;
} ElifBlock;

typedef struct ElseBlock {
    ProgRange range;
    size_t num_stmts;
    DLList stmts;
} ElseBlock;

typedef struct StmtIf {
    Stmt base;
    IfCondBlock if_blk;

    size_t num_elif_blks;
    DLList elif_blks;

    ElseBlock else_blk;
} StmtIf;

typedef struct StmtWhile {
    Stmt base;
    Expr* cond;
    size_t num_stmts;
    DLList stmts;
} StmtWhile;

typedef struct StmtDoWhile {
    Stmt base;
    Expr* cond;
    size_t num_stmts;
    DLList stmts;
} StmtDoWhile;

typedef struct StmtFor {
    Stmt base;
    Stmt* init;
    Expr* cond;
    Stmt* next;
    size_t num_stmts;
    DLList stmts;
} StmtFor;

typedef struct SwitchCase {
    Expr* start; // NOTE: Both start and end are null for default case.
    Expr* end;

    size_t num_stmts;
    DLList stmts;
    DLList list;
} SwitchCase;

typedef struct StmtSwitch {
    Stmt base;
    Expr* expr;
    size_t num_cases;
    DLList cases;
} StmtSwitch;

typedef struct StmtReturn {
    Stmt base;
    Expr* expr;
} StmtReturn;

typedef struct StmtExpr {
    Stmt base;
    Expr* expr;
} StmtExpr;

typedef struct StmtExprAssign {
    Stmt base;
    Expr* left;
    TokenKind op_assign;
    Expr* right;
} StmtExprAssign;

typedef struct StmtDecl {
    Stmt base;
    Decl* decl;
} StmtDecl;

typedef struct StmtBreak {
    Stmt base;
    const char* label;
} StmtBreak;

typedef struct StmtContinue {
    Stmt base;
    const char* label;
} StmtContinue;

Stmt* stmt_block(Allocator* allocator, size_t num_stmts, DLList* stmts, ProgRange range);
Stmt* stmt_decl(Allocator* allocator, Decl* decl);
Stmt* stmt_expr(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range);
Stmt* stmt_while(Allocator* allocator, Expr* cond, size_t num_stmts, DLList* stmts, ProgRange range);
Stmt* stmt_do_while(Allocator* allocator, Expr* cond, size_t num_stmts, DLList* stmts, ProgRange range);
Stmt* stmt_if(Allocator* allocator, IfCondBlock* if_blk, size_t num_elif_blks, DLList* elif_blks, ElseBlock* else_blk,
              ProgRange range);
ElifBlock* elif_block(Allocator* allocator, Expr* cond, size_t num_stmts, DLList* stmts, ProgRange range);

char* ftprint_stmt(Allocator* allocator, Stmt* stmt);
///////////////////////////////
//       Declarations
//////////////////////////////

typedef enum DeclKind {
    DECL_NONE,
    DECL_Var,
    DECL_Const,
    DECL_Enum,
    DECL_Union,
    DECL_Struct,
    DECL_Proc,
    DECL_Typedef,
} DeclKind;

struct Decl {
    DeclKind kind;
    ProgRange range;
    const char* name;
};

typedef struct DeclVar {
    Decl base;
    TypeSpec* type;
    Expr* init;
} DeclVar;

typedef struct DeclConst {
    Decl base;
    TypeSpec* type;
    Expr* init;
} DeclConst;

typedef struct EnumItem {
    const char* name;
    Expr* value;
    DLList list;
} EnumItem;

typedef struct DeclEnum {
    Decl base;
    TypeSpec* type;
    size_t num_items;
    DLList items;
} DeclEnum;

typedef struct DeclAggregate {
    Decl base; 
    size_t num_fields;
    DLList fields;
} DeclAggregate;

typedef DeclAggregate DeclUnion;
typedef DeclAggregate DeclStruct;

typedef struct ProcParam {
    ProgRange range;
    const char* name;
    TypeSpec* type;
    DLList list;
} ProcParam;

typedef struct DeclProc {
    Decl base;
    size_t num_params;
    DLList params;
    TypeSpec* ret;
    size_t num_stmts;
    DLList stmts;
} DeclProc;

typedef struct DeclTypedef {
    Decl base;
    TypeSpec* type;
} DeclTypedef;

Decl* decl_var(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);
Decl* decl_enum(Allocator* allocator, const char* name, TypeSpec* type, size_t num_items, DLList* items,
                ProgRange range);
EnumItem* enum_item(Allocator* allocator, const char* name, Expr* value);

typedef Decl* DeclAggregateProc(Allocator* alloc, const char* name, size_t num_fields, DLList* fields, ProgRange range);
Decl* decl_struct(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range);
Decl* decl_union(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range);
Decl* decl_proc(Allocator* allocator, const char* name, size_t num_params, DLList* params, TypeSpec* ret,
                size_t num_stmts, DLList* stmts, ProgRange range);
ProcParam* proc_param(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);

char* ftprint_decl(Allocator* allocator, Decl* decl);
#endif
