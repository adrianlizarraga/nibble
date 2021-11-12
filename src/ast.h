#ifndef NIBBLE_CST_H
#define NIBBLE_CST_H
#include <stddef.h>
#include <stdint.h>

#include "allocator.h"
#include "lexer.h"
#include "llist.h"
#include "nibble.h"
#include "hash_map.h"
#include "stream.h"

typedef struct Expr Expr;
typedef struct TypeSpec TypeSpec;
typedef struct Decl Decl;
typedef struct Stmt Stmt;

typedef struct Type Type;
typedef struct Symbol Symbol;
typedef struct SymbolVar SymbolVar;
typedef struct SymbolProc SymbolProc;
typedef struct SymbolModule SymbolModule;
typedef struct Scope Scope;
typedef struct Module Module;

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
    CST_TypeSpecTypeof,
} TypeSpecKind;

struct TypeSpec {
    TypeSpecKind kind;
    ProgRange range;
};

typedef struct TypeSpecIdent {
    TypeSpec super;
    Identifier* name;
} TypeSpecIdent;

typedef struct TypeSpecTypeof {
    TypeSpec super;
    Expr* expr;
} TypeSpecTypeof;

typedef struct AggregateField {
    Identifier* name;
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
    Identifier* name;
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

AggregateField* new_aggregate_field(Allocator* allocator, Identifier* name, TypeSpec* type, ProgRange range);

TypeSpec* new_typespec_ident(Allocator* allocator, Identifier* name, ProgRange range);
TypeSpec* new_typespec_typeof(Allocator* allocator, Expr* expr, ProgRange range);
TypeSpec* new_typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range);
TypeSpec* new_typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range);
TypeSpec* new_typespec_const(Allocator* allocator, TypeSpec* base, ProgRange range);
ProcParam* new_proc_param(Allocator* allocator, Identifier* name, TypeSpec* type, ProgRange range);
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
    CST_ExprCompoundLit,
} ExprKind;

struct Expr {
    ExprKind kind;
    ProgRange range;

    // TODO: Composition of ExprOperand?
    // This would save a lot of unnecessary copying of ExprOperands.
    Type* type;
    bool is_constexpr;
    bool is_lvalue;
    Scalar const_val;
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
    Identifier* name;
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
    Identifier* field;
} ExprField;

typedef struct ExprInt {
    Expr super;
    TokenInt token;
} ExprInt;

typedef struct ExprFloat {
    Expr super;
    FloatKind fkind;
    Float value;
} ExprFloat;

typedef struct ExprStr {
    Expr super;
    StrLit* str_lit;
} ExprStr;

typedef struct ExprIdent {
    Expr super;
    Identifier* mod_ns;
    Identifier* name;
} ExprIdent;

typedef struct ExprCast {
    Expr super;
    TypeSpec* typespec;
    Expr* expr;
    bool implicit;
} ExprCast;

typedef struct ExprSizeof {
    Expr super;
    TypeSpec* typespec;
} ExprSizeof;

typedef enum DesignatorKind {
    DESIGNATOR_NONE,
    DESIGNATOR_NAME,
    DESIGNATOR_INDEX,
} DesignatorKind;

typedef struct Designator {
    DesignatorKind kind;

    union {
        Identifier* name;
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
Expr* new_expr_field(Allocator* allocator, Expr* object, Identifier* field, ProgRange range);
Expr* new_expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range);
Expr* new_expr_call(Allocator* allocator, Expr* proc, size_t num_args, List* args, ProgRange range);
ProcCallArg* new_proc_call_arg(Allocator* allocator, Expr* expr, Identifier* name);
Expr* new_expr_int(Allocator* allocator, TokenInt token, ProgRange range);
Expr* new_expr_float(Allocator* allocator, FloatKind fkind, Float value, ProgRange range);
Expr* new_expr_str(Allocator* allocator, StrLit* str_lit, ProgRange range);
Expr* new_expr_ident(Allocator* allocator, Identifier* mod_ns, Identifier* name, ProgRange range);
Expr* new_expr_cast(Allocator* allocator, TypeSpec* type, Expr* arg, bool implicit, ProgRange range);
Expr* new_expr_sizeof(Allocator* allocator, TypeSpec* type, ProgRange range);
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
    CST_StmtStaticAssert,
    CST_StmtImport,
} StmtKind;

struct Stmt {
    StmtKind kind;
    ProgRange range;
    bool returns; // True if all control paths within this block return from proc.
    ListNode lnode;
};

typedef struct StmtStaticAssert {
    Stmt super;
    Expr* cond;
    StrLit* msg;
} StmtStaticAssert;

typedef struct ImportSymbol {
    ProgRange range;
    Identifier* name;
    Identifier* rename;
    ListNode lnode;
} ImportSymbol;

typedef struct StmtImport {
    Stmt super;
    List import_syms;
    StrLit* mod_pathname;
    Identifier* mod_namespace;
} StmtImport;

typedef struct StmtNoOp {
    Stmt super;
} StmtNoOp;

typedef struct StmtBlock {
    Stmt super;
    List stmts;
    u32 num_decls;
    Scope* scope;
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
Stmt* new_stmt_block(Allocator* allocator, List* stmts, u32 num_decls, ProgRange range);
Stmt* new_stmt_expr(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* new_stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range);
Stmt* new_stmt_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* new_stmt_do_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range);
Stmt* new_stmt_if(Allocator* allocator, IfCondBlock* if_blk, ElseBlock* else_blk, ProgRange range);
Stmt* new_stmt_for(Allocator* allocator, Stmt* init, Expr* cond, Stmt* next, Stmt* body, ProgRange range);
Stmt* new_stmt_return(Allocator* allocator, Expr* expr, ProgRange range);
Stmt* new_stmt_break(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_continue(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_goto(Allocator* allocator, const char* label, ProgRange range);
Stmt* new_stmt_label(Allocator* allocator, const char* label, Stmt* target, ProgRange range);
SwitchCase* new_switch_case(Allocator* allocator, Expr* start, Expr* end, List* stmts, ProgRange range);
Stmt* new_stmt_switch(Allocator* allocator, Expr* expr, List* cases, ProgRange range);
Stmt* new_stmt_static_assert(Allocator* allocator, Expr* cond, StrLit* msg, ProgRange range);
ImportSymbol* new_import_symbol(Allocator* allocator, Identifier* name, Identifier* rename, ProgRange range);
Stmt* new_stmt_import(Allocator* allocator, List* import_entities, StrLit* mod_pathname, Identifier* mod_namespace, ProgRange range);

char* ftprint_stmt(Allocator* allocator, Stmt* stmt);
Identifier* get_import_sym_name(StmtImport* stmt, Identifier* name);
///////////////////////////////
//       Declarations
//////////////////////////////

typedef struct DeclAnnotation {
    Identifier* ident;
    ProgRange range;
    ListNode lnode;
} DeclAnnotation;

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

enum DeclFlags {
    DECL_IS_EXPORTED = 0x1,
    DECL_IS_INCOMPLETE = 0x2,
    DECL_IS_FOREIGN = 0x4,
};

struct Decl {
    DeclKind kind;
    ProgRange range;
    unsigned flags;
    List annotations;
    ListNode lnode;
};

typedef struct DeclVar {
    Decl super;
    Identifier* name;
    TypeSpec* typespec;
    Expr* init;
} DeclVar;

typedef struct DeclConst {
    Decl super;
    Identifier* name;
    TypeSpec* typespec;
    Expr* init;
} DeclConst;

typedef struct EnumItem {
    Identifier* name;
    Expr* value;
    ListNode lnode;
} EnumItem;

typedef struct DeclEnum {
    Decl super;
    Identifier* name;
    TypeSpec* typespec;
    List items;
} DeclEnum;

typedef struct DeclAggregate {
    Decl super;
    Identifier* name;
    List fields;
} DeclAggregate;

typedef DeclAggregate DeclUnion;
typedef DeclAggregate DeclStruct;

typedef struct DeclProc {
    Decl super;
    Identifier* name;
    TypeSpec* ret;

    u32 num_params;
    u32 num_decls;
    bool returns;

    List params;
    List stmts;

    Scope* scope;
} DeclProc;

typedef struct DeclTypedef {
    Decl super;
    Identifier* name;
    TypeSpec* typespec;
} DeclTypedef;

DeclAnnotation* new_annotation(Allocator* allocator, Identifier* ident, ProgRange range);
Decl* new_decl_var(Allocator* allocator, Identifier* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* new_decl_const(Allocator* allocator, Identifier* name, TypeSpec* type, Expr* init, ProgRange range);
Decl* new_decl_typedef(Allocator* allocator, Identifier* name, TypeSpec* type, ProgRange range);
Decl* new_decl_enum(Allocator* allocator, Identifier* name, TypeSpec* type, List* items, ProgRange range);
EnumItem* new_enum_item(Allocator* allocator, Identifier* name, Expr* value);

typedef Decl* NewDeclAggregateProc(Allocator* alloc, Identifier* name, List* fields, ProgRange range);
Decl* new_decl_struct(Allocator* allocator, Identifier* name, List* fields, ProgRange range);
Decl* new_decl_union(Allocator* allocator, Identifier* name, List* fields, ProgRange range);
Decl* new_decl_proc(Allocator* allocator, Identifier* name, u32 num_params, List* params, TypeSpec* ret, List* stmts,
                    u32 num_decls, u32 flags, ProgRange range);

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
    u64 max;
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

typedef struct TypeEnum {
    Type* base;
} TypeEnum;

typedef struct TypeAggregateField {
    Type* type;
    size_t offset;
    Identifier* name;
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
        TypeEnum as_enum;
        TypeProc as_proc;
        TypeArray as_array;
        TypeAggregate as_aggregate;
    };
};

enum BuiltinTypeKind {
    // Basic primitive types
    BUILTIN_TYPE_VOID = 0,
    BUILTIN_TYPE_U8,
    BUILTIN_TYPE_S8,
    BUILTIN_TYPE_U16,
    BUILTIN_TYPE_S16,
    BUILTIN_TYPE_U32,
    BUILTIN_TYPE_S32,
    BUILTIN_TYPE_U64,
    BUILTIN_TYPE_S64,
    BUILTIN_TYPE_F32,
    BUILTIN_TYPE_F64,

    // Aliases for primitive types
    BUILTIN_TYPE_BOOL,
    BUILTIN_TYPE_CHAR,
    BUILTIN_TYPE_SCHAR,
    BUILTIN_TYPE_UCHAR,
    BUILTIN_TYPE_SHORT,
    BUILTIN_TYPE_USHORT,
    BUILTIN_TYPE_INT,
    BUILTIN_TYPE_UINT,
    BUILTIN_TYPE_LONG,
    BUILTIN_TYPE_ULONG,
    BUILTIN_TYPE_LLONG,
    BUILTIN_TYPE_ULLONG,
    BUILTIN_TYPE_SSIZE,
    BUILTIN_TYPE_USIZE,

    NUM_BUILTIN_TYPES,
};

typedef struct BuiltinType {
    const char* name;
    Type* type;
} BuiltinType;

extern BuiltinType builtin_types[NUM_BUILTIN_TYPES];

// Common types used in the compiler for type-checking
extern Type* type_ptr_void;
extern Type* type_ptr_char;
extern Type* type_ptr_ptr_char;

extern size_t PTR_SIZE;
extern size_t PTR_ALIGN;

extern int type_integer_ranks[];

void init_builtin_types(OS target_os, Arch target_arch, Allocator* ast_mem, TypeCache* type_cache);
const char* type_name(Type* type);
bool type_is_integer_like(Type* type);
bool type_is_arithmetic(Type* type);
bool type_is_scalar(Type* type);
bool type_is_ptr_like(Type* type);
bool type_is_aggregate(Type* type);

Type* type_ptr(Allocator* allocator, HMap* type_ptr_cache, Type* base);
Type* type_array(Allocator* allocator, HMap* type_array_cache, Type* base, size_t len);
Type* type_decay(Allocator* allocator, HMap* type_ptr_cache, Type* type);
Type* type_proc(Allocator* allocator, HMap* type_proc_cache, size_t num_params, Type** params, Type* ret);
Type* type_unsigned_int(Type* type_int);

///////////////////////////////
//       Symbols
//////////////////////////////

typedef enum SymbolKind {
    SYMBOL_NONE,
    SYMBOL_VAR,
    SYMBOL_CONST,
    SYMBOL_PROC,
    SYMBOL_TYPE,
    SYMBOL_MODULE,
    SYMBOL_KIND_COUNT,
} SymbolKind;

typedef enum SymbolStatus {
    SYMBOL_STATUS_UNRESOLVED,
    SYMBOL_STATUS_RESOLVING,
    SYMBOL_STATUS_RESOLVED,
} SymbolStatus;

struct SymbolVar {
    // Used by backends to store this var's
    // location in the stack.
    s32 offset;
};

typedef struct LifetimeInterval {
    u32 start;
    u32 end;
    bool is_ret;
    bool is_arg;
    u32 arg_index;
} LifetimeInterval;

struct SymbolProc {
    struct IR_Instr** instrs; // NOTE: stretchy buf
    LifetimeInterval* reg_intervals;
    bool is_nonleaf;
};

struct SymbolModule {
    Module* mod;
    Stmt* stmt;
};

struct Symbol {
    SymbolKind kind;
    SymbolStatus status;

    bool is_local;
    Identifier* name;
    Decl* decl;
    Type* type;
    Module* home;
    List lnode;

    union {
        SymbolVar as_var;
        SymbolProc as_proc;
        SymbolModule as_mod;
    };
};

Symbol* new_symbol(Allocator* allocator, SymbolKind kind, SymbolStatus status, Identifier* name, Module* home_mod);
Symbol* new_symbol_decl(Allocator* allocator, SymbolKind kind, Identifier* name, Decl* decl, Module* home_mod);
Symbol* new_symbol_builtin_type(Allocator* allocator, Identifier* name, Type* type, Module* home_mod);
Symbol* new_symbol_mod(Allocator* alloc, StmtImport* stmt, Module* import_mod, Module* home_mod);
char* symbol_mangled_name(Allocator* allocator, Symbol* sym);

///////////////////////////////
//       Scope
//////////////////////////////

struct Scope {
    struct Scope* parent;
    List children;

    HMap sym_table;

    // TODO: Make this a vanilla array with a size equal to number of parsed decls.
    // Why? Because we are only store symbols native to the corresponding module.
    List sym_list;
    size_t num_syms;

    ListNode lnode;
};

void scope_init(Scope* scope);

Scope* new_scope(Allocator* allocator, u32 num_syms);
void init_scope_sym_table(Scope* scope, Allocator* allocator, u32 num_syms);

Symbol* lookup_symbol(Scope* curr_scope, Identifier* name);
Symbol* lookup_scope_symbol(Scope* scope, Identifier* name);

void add_scope_symbol(Scope* scope, Identifier* name, Symbol* sym, bool add_list);
Symbol* add_unresolved_symbol(Allocator* allocator, Scope* scope, Module* mod, SymbolKind kind, Identifier* name, Decl* decl);
bool install_module_decls(Allocator* allocator, Module* mod);
bool module_add_global_sym(Module* mod, Identifier* name, Symbol* sym);
bool import_all_mod_syms(Module* dst_mod, Module* src_mod, bool ignore_exported);

///////////////////////////////
//      Module
///////////////////////////////

struct Module {
    StrLit* mod_path;
    List stmts;
    Scope scope;
};

// TODO: REMOVE THIS ONCE Decls have name in the base struct
void fill_decl_symbol_info(Decl* decl, SymbolKind* kind, Identifier** name);

#endif
