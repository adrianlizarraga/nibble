#ifndef NIBBLE_CST_H
#define NIBBLE_CST_H
#include <stddef.h>
#include <stdint.h>

#include "allocator.h"
#include "lexer.h"
#include "llist.h"
#include "nibble.h"
#include "hash_map.h"

typedef struct Expr Expr;
typedef struct TypeSpec TypeSpec;
typedef struct Decl Decl;
typedef struct Stmt Stmt;

typedef struct Type Type;
typedef struct Symbol Symbol;
typedef struct Scope Scope;

///////////////////////////////
//       Type Specifiers
//////////////////////////////

typedef enum TypeSpecKind {
    CST_TYPE_SPEC_NONE,
    CST_TypeSpecIdent,
    CST_TypeSpecProc,
    CST_TypeSpecStruct,
    CST_TypeSpecUnion,
    CST_TypeSpecPtr,
    CST_TypeSpecArray,
    CST_TypeSpecConst,
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

typedef TypeSpec* NewTypeSpecAggregateProc(Allocator* alloc, List* fields, ProgRange range);
TypeSpec* new_typespec_struct(Allocator* allocator, List* fields, ProgRange range);
TypeSpec* new_typespec_union(Allocator* allocator, List* fields, ProgRange range);

char* ftprint_typespec(Allocator* allocator, TypeSpec* type);
///////////////////////////////
//       Expressions
//////////////////////////////

typedef enum ExprKind {
    CST_EXPR_NONE,
    CST_ExprTernary,
    CST_ExprBinary,
    CST_ExprUnary,
    CST_ExprCall,
    CST_ExprIndex,
    CST_ExprField,
    CST_ExprInt,
    CST_ExprFloat,
    CST_ExprStr,
    CST_ExprIdent,
    CST_ExprCast,
    CST_ExprSizeof,
    CST_ExprTypeof,
    CST_ExprCompoundLit,
} ExprKind;

struct Expr {
    ExprKind kind;
    ProgRange range;
    Type* type;
    //bool is_const;
    //bool is_lvalue;
    //Scalar const_val;
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
    CST_STMT_NONE,
    CST_StmtNoOp,
    CST_StmtIf,
    CST_StmtWhile,
    CST_StmtDoWhile,
    CST_StmtFor,
    CST_StmtSwitch,
    CST_StmtReturn,
    CST_StmtBreak,
    CST_StmtContinue,
    CST_StmtGoto,
    CST_StmtLabel,
    CST_StmtExpr,
    CST_StmtExprAssign,
    CST_StmtDecl,
    CST_StmtBlock,
} StmtKind;

struct Stmt {
    StmtKind kind;
    ProgRange range;
    ListNode lnode;
};

typedef struct StmtNoOp {
    Stmt super;
} StmtNoOp;

typedef struct StmtBlock {
    Stmt super;
    List stmts;
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
    Stmt* init;
    Expr* cond;
    Stmt* next;
    Stmt* body;
} StmtFor;

typedef struct SwitchCase {
    ProgRange range;

    Expr* start; // NOTE: Both start and end are null for default case.
    Expr* end;

    List stmts;

    ListNode lnode;
} SwitchCase;

typedef struct StmtSwitch {
    Stmt super;
    Expr* expr;
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

typedef struct StmtDecl {
    Stmt super;
    Decl* decl;
} StmtDecl;

Stmt* new_stmt_noop(Allocator* allocator, ProgRange range);
Stmt* new_stmt_decl(Allocator* allocator, Decl* decl);
Stmt* new_stmt_block(Allocator* allocator, List* stmts, ProgRange range);
Stmt* new_stmt_expr(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* new_stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range);
Stmt* new_stmt_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* new_stmt_do_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* new_stmt_if(Allocator* allocator, IfCondBlock* if_blk, List* elif_blks, ElseBlock* else_blk, ProgRange range);
IfCondBlock* new_if_cond_block(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* new_stmt_for(Allocator* allocator, Stmt* init, Expr* cond, Stmt* next, Stmt* body, ProgRange range);
Stmt* new_stmt_return(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* new_stmt_break(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_continue(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_goto(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_label(Allocator* allocator, const char* label, Stmt* target, ProgRange range);
SwitchCase* new_switch_case(Allocator* allocator, Expr* start, Expr* end, List* stmts, ProgRange range);
Stmt* new_stmt_switch(Allocator* allocator, Expr* expr, List* cases, ProgRange range);

char* ftprint_stmt(Allocator* allocator, Stmt* stmt);
///////////////////////////////
//       Declarations
//////////////////////////////

typedef enum DeclKind {
    CST_DECL_NONE,
    CST_DeclVar,
    CST_DeclConst,
    CST_DeclEnum,
    CST_DeclUnion,
    CST_DeclStruct,
    CST_DeclProc,
    CST_DeclTypedef,
} DeclKind;

struct Decl {
    DeclKind kind;
    ProgRange range;
    //Type* type;
    ListNode lnode;
};

typedef struct DeclVar {
    Decl super;
    const char* name;
    TypeSpec* typespec;
    Expr* init;
} DeclVar;

typedef struct DeclConst {
    Decl super;
    const char* name;
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
    const char* name;
    TypeSpec* typespec;
    List items;
} DeclEnum;

typedef struct DeclAggregate {
    Decl super;
    const char* name;
    List fields;
} DeclAggregate;

typedef DeclAggregate DeclUnion;
typedef DeclAggregate DeclStruct;

typedef struct DeclProc {
    Decl super;
    const char* name;
    TypeSpec* ret;
    size_t num_params;
    List params;
    Stmt* body;
} DeclProc;

typedef struct DeclTypedef {
    Decl super;
    const char* name;
    TypeSpec* typespec;
} DeclTypedef;

Decl* new_decl_var(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* new_decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* new_decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range);
Decl* new_decl_enum(Allocator* allocator, const char* name, TypeSpec* type, List* items, ProgRange range);
EnumItem* new_enum_item(Allocator* allocator, const char* name, Expr* value);

typedef Decl* NewDeclAggregateProc(Allocator* alloc, const char* name, List* fields, ProgRange range);
Decl* new_decl_struct(Allocator* allocator, const char* name, List* fields, ProgRange range);
Decl* new_decl_union(Allocator* allocator, const char* name, List* fields, ProgRange range);
Decl* new_decl_proc(Allocator* allocator, const char* name, size_t num_params, List* params, TypeSpec* ret, Stmt* body,
                    ProgRange range);

char* ftprint_decl(Allocator* allocator, Decl* decl);

///////////////////////////////
//       Types
//////////////////////////////

typedef enum TypeKind {
    TYPE_VOID,
    TYPE_INTEGER,
    TYPE_FLOAT,
    TYPE_ENUM,
    TYPE_PTR,
    TYPE_PROC,
    TYPE_ARRAY,
    TYPE_STRUCT,
    TYPE_UNION,
} TypeKind;

typedef enum TypeStatus {
    TYPE_STATUS_COMPLETE,
    TYPE_STATUS_INCOMPLETE,
    TYPE_STATUS_COMPLETING,
} TypeStatus;

typedef struct Type Type;

typedef struct TypeInteger {
    IntegerKind kind;
    bool is_signed;
    unsigned long long max;
} TypeInteger;

typedef struct TypeFloat {
    FloatKind kind;
} TypeFloat;

typedef struct TypeProc {
    size_t num_params;
    Type** params;
    Type* ret;
} TypeProc;

typedef struct TypeArray {
    Type* base;
    size_t len;
} TypeArray;

typedef struct TypePtr {
    Type* base;
} TypePtr;

typedef struct TypeAggregateField {
    Type* type;
    size_t offset;
    const char* name;
} TypeAggregateField;

typedef struct TypeAggregate {
    size_t num_fields;
    TypeAggregateField* fields;
} TypeAggregate;

struct Type {
    TypeKind kind;
    TypeStatus status;
    int id;
    size_t size;
    size_t align;

    union {
        TypeInteger as_integer;
        TypeFloat as_float;
        TypePtr as_ptr;
        TypeProc as_proc;
        TypeArray as_array;
        TypeAggregate as_struct;
        TypeAggregate as_union;
    };
};


extern Type* type_void;
extern Type* type_bool;
extern Type* type_char;
extern Type* type_schar;
extern Type* type_uchar;
extern Type* type_short;
extern Type* type_ushort;
extern Type* type_int;
extern Type* type_uint;
extern Type* type_long;
extern Type* type_ulong;
extern Type* type_llong;
extern Type* type_ullong;
extern Type* type_ssize;
extern Type* type_usize;
extern Type* type_f32;
extern Type* type_f64;

extern size_t PTR_SIZE;
extern size_t PTR_ALIGN;

void init_builtin_types(OS target_os, Arch target_arch);
const char* type_name(Type* type);
bool type_is_arithmetic(Type* type);
bool type_is_scalar(Type* type);

Type* type_ptr(Allocator* allocator, HMap* type_ptr_cache, Type* base);
Type* type_decay(Allocator* allocator, HMap* type_ptr_cache, Type* type);
Type* type_proc(Allocator* allocator, HMap* type_proc_cache, size_t num_params, Type** params, Type* ret);

///////////////////////////////
//       Symbols
//////////////////////////////

typedef enum SymbolKind {
    SYMBOL_NONE,
    SYMBOL_VAR,
    SYMBOL_CONST,
    SYMBOL_PROC,
    SYMBOL_TYPE,
    SYMBOL_PACKAGE,
} SymbolKind;

typedef enum SymbolStatus {
    SYMBOL_STATUS_UNRESOLVED,
    SYMBOL_STATUS_RESOLVING,
    SYMBOL_STATUS_RESOLVED,
} SymbolStatus;

struct Symbol {
    SymbolKind kind;
    SymbolStatus status;
    const char* name;
    Decl* decl;
    Type* type;
    List lnode;
};

Symbol* new_symbol_decl(Allocator* allocator, SymbolKind kind, const char* name, Decl* decl);
Symbol* new_symbol_type(Allocator* allocator, const char* name, Type* type);

///////////////////////////////
//       Scope
//////////////////////////////

struct Scope {
    struct Scope* parent;
    List children;

    HMap sym_table;
    List sym_list;

    ListNode lnode;
};

void init_scope(Scope* scope, Scope* parent, size_t log2_num_syms);
void free_scope(Scope* scope);
#endif
