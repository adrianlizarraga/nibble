#ifndef NIBBLE_AST_H
#define NIBBLE_AST_H
#include <stddef.h>
#include <stdint.h>

#include "nibble.h"
#include "allocator.h"
#include "lexer.h"

typedef struct Expr Expr;
typedef struct TypeSpec TypeSpec;
typedef struct Decl Decl;
typedef struct Stmt Stmt;

typedef struct Ast {
    size_t num_decls;
    Decl** decls;
} Ast;

///////////////////////////////
//       Type Specifiers
//////////////////////////////

typedef enum TypeSpecKind {
    AST_TYPE_SPEC_NONE,
    AST_TypeSpecIdent,
    AST_TypeSpecProc,
    AST_TypeSpecStruct,
    AST_TypeSpecUnion,
    AST_TypeSpecPtr,
    AST_TypeSpecArray,
    AST_TypeSpecConst,
} TypeSpecKind;

struct TypeSpec {
    TypeSpecKind kind;
    ProgRange range;
};

typedef struct TypeSpecIdent {
    TypeSpec super;
    size_t path_size;
    const char** path;
} TypeSpecIdent;

typedef struct AggregateField {
    const char* name;
    TypeSpec* type;
    ProgRange range;
} AggregateField;

typedef struct TypeSpecAggregate {
    TypeSpec super;
    size_t num_fields;
    AggregateField** fields;
} TypeSpecAggregate;

typedef TypeSpecAggregate TypeSpecStruct;
typedef TypeSpecAggregate TypeSpecUnion;

typedef struct ProcParam {
    ProgRange range;
    const char* name;
    TypeSpec* type;
} ProcParam;

typedef struct TypeSpecProc {
    TypeSpec super;
    size_t num_params;
    ProcParam** params;
    TypeSpec* ret;
} TypeSpecProc;

typedef struct TypeSpecPtr {
    TypeSpec super;
    TypeSpec* base;
} TypeSpecPtr;

typedef struct TypeSpecArray {
    TypeSpec super;
    TypeSpec* base;
    Expr* len;
} TypeSpecArray;

typedef struct TypeSpecConst {
    TypeSpec super;
    TypeSpec* base;
} TypeSpecConst;

AggregateField* aggregate_field(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);

TypeSpec* typespec_ident(Allocator* allocator, size_t path_size, const char** path, ProgRange range);
TypeSpec* typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range);
TypeSpec* typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range);
TypeSpec* typespec_const(Allocator* allocator, TypeSpec* base, ProgRange range);
ProcParam* proc_param(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);
TypeSpec* typespec_proc(Allocator* allocator, size_t num_params, ProcParam** params, TypeSpec* ret, ProgRange range);

typedef TypeSpec* TypeSpecAggregateProc(Allocator* alloc, size_t num_fields, AggregateField** fields, ProgRange range);
TypeSpec* typespec_struct(Allocator* allocator, size_t num_fields, AggregateField** fields, ProgRange range);
TypeSpec* typespec_union(Allocator* allocator, size_t num_fields, AggregateField** fields, ProgRange range);

char* ftprint_typespec(Allocator* allocator, TypeSpec* type);
///////////////////////////////
//       Expressions
//////////////////////////////

typedef enum ExprKind {
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
    AST_ExprIdent,
    AST_ExprCast,
    AST_ExprSizeof,
    AST_ExprTypeof,
    AST_ExprCompoundLit,
} ExprKind;

struct Expr {
    ExprKind kind;
    ProgRange range;
};

typedef struct ExprTernary {
    Expr super;
    Expr* cond;
    Expr* then_expr;
    Expr* else_expr;
} ExprTernary;

typedef struct ExprBinary {
    Expr super;
    TokenKind op;
    Expr* left;
    Expr* right;
} ExprBinary;

typedef struct ExprUnary {
    Expr super;
    TokenKind op;
    Expr* expr;
} ExprUnary;

typedef struct ProcCallArg {
    ProgRange range;
    Expr* expr;
    const char* name;
} ProcCallArg;

typedef struct ExprCall {
    Expr super;
    Expr* proc;
    size_t num_args;
    ProcCallArg** args;
} ExprCall;

typedef struct ExprIndex {
    Expr super;
    Expr* array;
    Expr* index;
} ExprIndex;

typedef struct ExprField {
    Expr super;
    Expr* object;
    const char* field;
} ExprField;

typedef struct ExprInt {
    Expr super;
    uint64_t value;
} ExprInt;

typedef struct ExprFloat {
    Expr super;
    FloatKind fkind;
    Float value;
} ExprFloat;

typedef struct ExprStr {
    Expr super;
    const char* value;
} ExprStr;

typedef struct ExprIdent {
    Expr super;
    const char* name;
} ExprIdent;

typedef struct ExprCast {
    Expr super;
    TypeSpec* type;
    Expr* expr;
} ExprCast;

typedef struct ExprSizeof {
    Expr super;
    TypeSpec* type;
} ExprSizeof;

typedef struct ExprTypeof {
    Expr super;
    Expr* expr;
} ExprTypeof;

typedef enum DesignatorKind {
    DESIGNATOR_NONE,
    DESIGNATOR_NAME,
    DESIGNATOR_INDEX,
} DesignatorKind;

typedef struct Designator {
    DesignatorKind kind;

    union {
        const char* name;
        Expr* index;
    };
} Designator;

typedef struct MemberInitializer {
    ProgRange range;
    Designator designator;
    Expr* init;
} MemberInitializer;

typedef struct ExprCompoundLit {
    Expr super;
    TypeSpec* type;
    size_t num_initzers;
    MemberInitializer** initzers;
} ExprCompoundLit;

Expr* expr_ternary(Allocator* allocator, Expr* cond, Expr* then_expr, Expr* else_expr);
Expr* expr_binary(Allocator* allocator, TokenKind op, Expr* left, Expr* right);
Expr* expr_unary(Allocator* allocator, TokenKind op, Expr* expr, ProgRange range);
Expr* expr_field(Allocator* allocator, Expr* object, const char* field, ProgRange range);
Expr* expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range);
Expr* expr_call(Allocator* allocator, Expr* proc, size_t num_args, ProcCallArg** args, ProgRange range);
ProcCallArg* proc_call_arg(Allocator* allocator, Expr* expr, const char* name);
Expr* expr_int(Allocator* allocator, uint64_t value, ProgRange range);
Expr* expr_float(Allocator* allocator, Float value, FloatKind fkind, ProgRange range);
Expr* expr_str(Allocator* allocator, const char* value, ProgRange range);
Expr* expr_ident(Allocator* allocator, const char* name, ProgRange range);
Expr* expr_cast(Allocator* allocator, TypeSpec* type, Expr* arg, ProgRange range);
Expr* expr_sizeof(Allocator* allocator, TypeSpec* type, ProgRange range);
Expr* expr_typeof(Allocator* allocator, Expr* arg, ProgRange range);
MemberInitializer* member_initializer(Allocator* allocator, Expr* init, Designator designator, ProgRange range);
Expr* expr_compound_lit(Allocator* allocator, TypeSpec* type, size_t num_initzers, MemberInitializer** initzers,
                        ProgRange range);

char* ftprint_expr(Allocator* allocator, Expr* expr);
/////////////////////////////
//        Statements
/////////////////////////////
typedef enum StmtKind {
    AST_STMT_NONE,
    AST_StmtNoOp,
    AST_StmtIf,
    AST_StmtWhile,
    AST_StmtDoWhile,
    AST_StmtFor,
    AST_StmtSwitch,
    AST_StmtReturn,
    AST_StmtBreak,
    AST_StmtContinue,
    AST_StmtGoto,
    AST_StmtLabel,
    AST_StmtExpr,
    AST_StmtExprAssign,
    AST_StmtDecl,
    AST_StmtBlock,
} StmtKind;

struct Stmt {
    StmtKind kind;
    ProgRange range;
};

typedef struct StmtNoOp {
    Stmt super;
} StmtNoOp;

typedef struct StmtBlock {
    Stmt super;
    size_t num_stmts;
    Stmt** stmts;
} StmtBlock;

typedef struct IfCondBlock {
    ProgRange range;
    Expr* cond;
    Stmt* body;
} IfCondBlock;

typedef struct ElseBlock {
    ProgRange range;
    Stmt* body;
} ElseBlock;

typedef struct StmtIf {
    Stmt super;
    IfCondBlock if_blk;

    size_t num_elif_blks;
    IfCondBlock** elif_blks;

    ElseBlock else_blk;
} StmtIf;

typedef struct StmtWhile {
    Stmt super;
    Expr* cond;
    Stmt* body;
} StmtWhile;

typedef struct StmtDoWhile {
    Stmt super;
    Expr* cond;
    Stmt* body;
} StmtDoWhile;

typedef struct StmtFor {
    Stmt super;
    Stmt* init;
    Expr* cond;
    Stmt* next;
    Stmt* body;
} StmtFor;

typedef struct SwitchCase {
    Expr* start; // NOTE: Both start and end are null for default case.
    Expr* end;
    ProgRange range;
    size_t num_stmts;
    Stmt** stmts;
} SwitchCase;

typedef struct StmtSwitch {
    Stmt super;
    Expr* expr;
    size_t num_cases;
    SwitchCase** cases;
} StmtSwitch;

typedef struct StmtReturn {
    Stmt super;
    Expr* expr;
} StmtReturn;

typedef struct StmtExpr {
    Stmt super;
    Expr* expr;
} StmtExpr;

typedef struct StmtExprAssign {
    Stmt super;
    Expr* left;
    TokenKind op_assign;
    Expr* right;
} StmtExprAssign;

typedef struct StmtDecl {
    Stmt super;
    Decl* decl;
} StmtDecl;

typedef struct StmtBreak {
    Stmt super;
    const char* label;
} StmtBreak;

typedef struct StmtContinue {
    Stmt super;
    const char* label;
} StmtContinue;

typedef struct StmtGoto {
    Stmt super;
    const char* label;
} StmtGoto;

typedef struct StmtLabel {
    Stmt super;
    const char* label;
    Stmt* target;
} StmtLabel;

Stmt* stmt_noop(Allocator* allocator, ProgRange range);
Stmt* stmt_block(Allocator* allocator, size_t num_stmts, Stmt** stmts, ProgRange range);
Stmt* stmt_decl(Allocator* allocator, Decl* decl);
Stmt* stmt_expr(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range);
Stmt* stmt_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* stmt_do_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* stmt_if(Allocator* allocator, IfCondBlock* if_blk, size_t num_elif_blks, IfCondBlock** elif_blks,
              ElseBlock* else_blk, ProgRange range);
IfCondBlock* if_cond_block(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* stmt_for(Allocator* allocator, Stmt* init, Expr* cond, Stmt* next, Stmt* body, ProgRange range);
Stmt* stmt_return(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* stmt_break(Allocator* allocator, const char* label, ProgRange range);
Stmt* stmt_continue(Allocator* allocator, const char* label, ProgRange range);
Stmt* stmt_goto(Allocator* allocator, const char* label, ProgRange range);
Stmt* stmt_label(Allocator* allocator, const char* label, Stmt* target, ProgRange range);
SwitchCase* switch_case(Allocator* allocator, Expr* start, Expr* end, size_t num_stmts, Stmt** stmts, ProgRange range);
Stmt* stmt_switch(Allocator* allocator, Expr* expr, size_t num_cases, SwitchCase** cases, ProgRange range);

char* ftprint_stmt(Allocator* allocator, Stmt* stmt);
///////////////////////////////
//       Declarations
//////////////////////////////

typedef enum DeclKind {
    AST_DECL_NONE,
    AST_DeclVar,
    AST_DeclConst,
    AST_DeclEnum,
    AST_DeclUnion,
    AST_DeclStruct,
    AST_DeclProc,
    AST_DeclTypedef,
} DeclKind;

struct Decl {
    DeclKind kind;
    ProgRange range;
    const char* name;
};

typedef struct DeclVar {
    Decl super;
    TypeSpec* type;
    Expr* init;
} DeclVar;

typedef struct DeclConst {
    Decl super;
    TypeSpec* type;
    Expr* init;
} DeclConst;

typedef struct EnumItem {
    const char* name;
    Expr* value;
} EnumItem;

typedef struct DeclEnum {
    Decl super;
    TypeSpec* type;
    size_t num_items;
    EnumItem** items;
} DeclEnum;

typedef struct DeclAggregate {
    Decl super;
    size_t num_fields;
    AggregateField** fields;
} DeclAggregate;

typedef DeclAggregate DeclUnion;
typedef DeclAggregate DeclStruct;

typedef struct DeclProc {
    Decl super;
    size_t num_params;
    ProcParam** params;
    TypeSpec* ret;
    size_t num_stmts;
    Stmt** stmts;
} DeclProc;

typedef struct DeclTypedef {
    Decl super;
    TypeSpec* type;
} DeclTypedef;

Decl* decl_var(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);
Decl* decl_enum(Allocator* allocator, const char* name, TypeSpec* type, size_t num_items, EnumItem** items,
                ProgRange range);
EnumItem* enum_item(Allocator* allocator, const char* name, Expr* value);

typedef Decl* DeclAggregateProc(Allocator* alloc, const char* name, size_t num_fields, AggregateField** fields,
                                ProgRange range);
Decl* decl_struct(Allocator* allocator, const char* name, size_t num_fields, AggregateField** fields, ProgRange range);
Decl* decl_union(Allocator* allocator, const char* name, size_t num_fields, AggregateField** fields, ProgRange range);
Decl* decl_proc(Allocator* allocator, const char* name, size_t num_params, ProcParam** params, TypeSpec* ret,
                size_t num_stmts, Stmt** stmts, ProgRange range);

char* ftprint_decl(Allocator* allocator, Decl* decl);

#endif
