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
typedef struct Scope Scope;

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
    const char* name;
} TypeSpecIdent;

typedef struct AggregateField {
    const char* name;
    TypeSpec* typespec;
    ProgRange range;
    ListNode lnode;
} AggregateField;

typedef struct TypeSpecAggregate {
    TypeSpec super;
    size_t num_fields;
    List fields;
} TypeSpecAggregate;

typedef TypeSpecAggregate TypeSpecStruct;
typedef TypeSpecAggregate TypeSpecUnion;

typedef struct ProcParam {
    ProgRange range;
    const char* name;
    TypeSpec* typespec;
    ListNode lnode;
} ProcParam;

typedef struct TypeSpecProc {
    TypeSpec super;
    size_t num_params;
    List params;
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

AggregateField* new_aggregate_field(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);

TypeSpec* new_typespec_ident(Allocator* allocator, const char* name, ProgRange range);
TypeSpec* new_typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range);
TypeSpec* new_typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range);
TypeSpec* new_typespec_const(Allocator* allocator, TypeSpec* base, ProgRange range);
ProcParam* new_proc_param(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);
TypeSpec* new_typespec_proc(Allocator* allocator, size_t num_params, List* params, TypeSpec* ret, ProgRange range);

typedef TypeSpec* NewTypeSpecAggregateProc(Allocator* alloc, size_t num_fields, List* fields, ProgRange range);
TypeSpec* new_typespec_struct(Allocator* allocator, size_t num_fields, List* fields, ProgRange range);
TypeSpec* new_typespec_union(Allocator* allocator, size_t num_fields, List* fields, ProgRange range);

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
    ListNode lnode;
} ProcCallArg;

typedef struct ExprCall {
    Expr super;
    Expr* proc;
    size_t num_args;
    List args;
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
    TypeSpec* typespec;
    Expr* expr;
} ExprCast;

typedef struct ExprSizeof {
    Expr super;
    TypeSpec* typespec;
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
    ListNode lnode;
} MemberInitializer;

typedef struct ExprCompoundLit {
    Expr super;
    TypeSpec* typespec;
    size_t num_initzers;
    List initzers;
} ExprCompoundLit;

Expr* new_expr_ternary(Allocator* allocator, Expr* cond, Expr* then_expr, Expr* else_expr);
Expr* new_expr_binary(Allocator* allocator, TokenKind op, Expr* left, Expr* right);
Expr* new_expr_unary(Allocator* allocator, TokenKind op, Expr* expr, ProgRange range);
Expr* new_expr_field(Allocator* allocator, Expr* object, const char* field, ProgRange range);
Expr* new_expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range);
Expr* new_expr_call(Allocator* allocator, Expr* proc, size_t num_args, List* args, ProgRange range);
ProcCallArg* new_proc_call_arg(Allocator* allocator, Expr* expr, const char* name);
Expr* new_expr_int(Allocator* allocator, uint64_t value, ProgRange range);
Expr* new_expr_float(Allocator* allocator, Float value, ProgRange range);
Expr* new_expr_str(Allocator* allocator, const char* value, ProgRange range);
Expr* new_expr_ident(Allocator* allocator, const char* name, ProgRange range);
Expr* new_expr_cast(Allocator* allocator, TypeSpec* type, Expr* arg, ProgRange range);
Expr* new_expr_sizeof(Allocator* allocator, TypeSpec* type, ProgRange range);
Expr* new_expr_typeof(Allocator* allocator, Expr* arg, ProgRange range);
MemberInitializer* new_member_initializer(Allocator* allocator, Expr* init, Designator designator, ProgRange range);
Expr* new_expr_compound_lit(Allocator* allocator, TypeSpec* type, size_t num_initzers, List* initzers, ProgRange range);

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
    AST_StmtBlock,
} StmtKind;

struct Stmt {
    StmtKind kind;
    ProgRange range;
    ListNode lnode;
};

typedef struct StmtNoOp {
    Stmt super;
} StmtNoOp;

struct Scope {
    size_t num_decls;
    List decls;

    ListNode lnode;
    Scope* parent;
    List children; // Scopes
};

typedef enum BlockItemKind {
    BLOCK_ITEM_NONE,
    BLOCK_ITEM_DECL,
    BLOCK_ITEM_STMT,
} BlockItemKind;

typedef struct BlockItem {
    BlockItemKind kind;

    union {
        Decl* decl;
        Stmt* stmt;
    };
} BlockItem;

typedef struct StmtBlock {
    Stmt super;
    size_t num_stmts;
    List stmts;
    Scope* scope;
} StmtBlock;

typedef struct IfCondBlock {
    ProgRange range;
    Expr* cond;
    Stmt* body;
    ListNode lnode;
} IfCondBlock;

typedef struct ElseBlock {
    ProgRange range;
    Stmt* body;
} ElseBlock;

typedef struct StmtIf {
    Stmt super;
    IfCondBlock if_blk;

    size_t num_elif_blks;
    List elif_blks;

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
    Scope* scope;
    Stmt* init;
    Expr* cond;
    Stmt* next;
    Stmt* body;
} StmtFor;

typedef struct SwitchCase {
    ProgRange range;

    Expr* start; // NOTE: Both start and end are null for default case.
    Expr* end;

    size_t num_stmts; // TODO: Make this just a Stmt* body??
    List stmts;

    ListNode lnode;
} SwitchCase;

typedef struct StmtSwitch {
    Stmt super;
    Expr* expr;

    size_t num_cases;
    List cases;
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

Stmt* new_stmt_noop(Allocator* allocator, ProgRange range);
Scope* new_scope(Allocator* allocator, Scope* parent);
Stmt* new_stmt_block(Allocator* allocator, size_t num_stmts, List* stmts, Scope* scope, ProgRange range);
Stmt* new_stmt_expr(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* new_stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range);
Stmt* new_stmt_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* new_stmt_do_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* new_stmt_if(Allocator* allocator, IfCondBlock* if_blk, size_t num_elif_blks, List* elif_blks, ElseBlock* else_blk,
                  ProgRange range);
IfCondBlock* new_if_cond_block(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* new_stmt_for(Allocator* allocator, Scope* scope, Stmt* init, Expr* cond, Stmt* next, Stmt* body, ProgRange range);
Stmt* new_stmt_return(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* new_stmt_break(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_continue(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_goto(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_label(Allocator* allocator, const char* label, Stmt* target, ProgRange range);
SwitchCase* new_switch_case(Allocator* allocator, Expr* start, Expr* end, size_t num_stmts, List* stmts,
                            ProgRange range);
Stmt* new_stmt_switch(Allocator* allocator, Expr* expr, size_t num_cases, List* cases, ProgRange range);

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
    ListNode lnode;
};

typedef struct DeclVar {
    Decl super;
    TypeSpec* typespec;
    Expr* init;
} DeclVar;

typedef struct DeclConst {
    Decl super;
    TypeSpec* typespec;
    Expr* init;
} DeclConst;

typedef struct EnumItem {
    const char* name;
    Expr* value;
    ListNode lnode;
} EnumItem;

typedef struct DeclEnum {
    Decl super;
    TypeSpec* typespec;

    size_t num_items;
    List items;
} DeclEnum;

typedef struct DeclAggregate {
    Decl super;
    size_t num_fields;
    List fields;
} DeclAggregate;

typedef DeclAggregate DeclUnion;
typedef DeclAggregate DeclStruct;

typedef struct DeclProc {
    Decl super;
    TypeSpec* ret;
    Scope* param_scope;
    Stmt* body;
} DeclProc;

typedef struct DeclTypedef {
    Decl super;
    TypeSpec* typespec;
} DeclTypedef;

Decl* new_decl_var(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* new_decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* new_decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);
Decl* new_decl_enum(Allocator* allocator, const char* name, TypeSpec* type, size_t num_items, List* items,
                    ProgRange range);
EnumItem* new_enum_item(Allocator* allocator, const char* name, Expr* value);

typedef Decl* NewDeclAggregateProc(Allocator* alloc, const char* name, size_t num_fields, List* fields,
                                   ProgRange range);
Decl* new_decl_struct(Allocator* allocator, const char* name, size_t num_fields, List* fields, ProgRange range);
Decl* new_decl_union(Allocator* allocator, const char* name, size_t num_fields, List* fields, ProgRange range);
Decl* new_decl_proc(Allocator* allocator, const char* name, Scope* param_scope, TypeSpec* ret, Stmt* body,
                    ProgRange range);

char* ftprint_decl(Allocator* allocator, Decl* decl);

#endif
