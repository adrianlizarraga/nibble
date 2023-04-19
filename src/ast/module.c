#include <assert.h>
#include <stdlib.h>
#include <stddef.h>
#include "ast/module.h"
#include "array.h"
#include "cstring.h"

#define new_typespec(a, k, r) (k*)new_typespec_((a), sizeof(k), alignof(k), CST_##k, (r))
static TypeSpec* new_typespec_(Allocator* allocator, size_t size, size_t align, TypeSpecKind kind, ProgRange range)
{
    TypeSpec* typespec = mem_allocate(allocator, size, align, true);
    typespec->kind = kind;
    typespec->range = range;

    return typespec;
}

TypeSpec* new_typespec_ident(Allocator* allocator, NSIdent* ns_ident)
{
    TypeSpecIdent* typespec = new_typespec(allocator, TypeSpecIdent, ns_ident->range);
    typespec->ns_ident.range = ns_ident->range;
    typespec->ns_ident.num_idents = ns_ident->num_idents;

    list_replace(&ns_ident->idents, &typespec->ns_ident.idents);

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_typeof(Allocator* allocator, Expr* expr, ProgRange range)
{
    TypeSpecTypeof* typespec = new_typespec(allocator, TypeSpecTypeof, range);
    typespec->expr = expr;

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_ret_type(Allocator* allocator, Expr* proc_expr, ProgRange range)
{
    TypeSpecRetType* ts = new_typespec(allocator, TypeSpecRetType, range);
    ts->proc_expr = proc_expr;

    return (TypeSpec*)ts;
}

TypeSpec* new_typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpecPtr* typespec = new_typespec(allocator, TypeSpecPtr, range);
    typespec->base = base;

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, bool infer_len, ProgRange range)
{
    TypeSpecArray* typespec = new_typespec(allocator, TypeSpecArray, range);
    typespec->base = base;
    typespec->len = len;
    typespec->infer_len = infer_len;

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_const(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpecConst* typespec = new_typespec(allocator, TypeSpecConst, range);
    typespec->base = base;

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_proc(Allocator* allocator, size_t num_params, List* params, TypeSpec* ret, bool is_variadic, ProgRange range)
{
    TypeSpecProc* typespec = new_typespec(allocator, TypeSpecProc, range);
    typespec->is_variadic = is_variadic;
    typespec->num_params = num_params;
    typespec->ret = ret;

    list_replace(params, &typespec->params);

    return (TypeSpec*)typespec;
}

ProcParam* new_proc_param(Allocator* allocator, Identifier* name, TypeSpec* typespec, bool is_variadic, ProgRange range)
{
    ProcParam* param = alloc_type(allocator, ProcParam, true);
    param->name = name;
    param->typespec = typespec;
    param->is_variadic = is_variadic;
    param->range = range;

    return param;
}

TypeSpec* new_typespec_struct(Allocator* allocator, List* fields, ProgRange range)
{
    TypeSpecStruct* typespec = new_typespec(allocator, TypeSpecStruct, range);

    list_replace(fields, &typespec->fields);

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_union(Allocator* allocator, List* fields, ProgRange range)
{
    TypeSpecUnion* typespec = new_typespec(allocator, TypeSpecUnion, range);

    list_replace(fields, &typespec->fields);

    return (TypeSpec*)typespec;
}

#define new_expr(a, k, r) (k*)new_expr_((a), sizeof(k), alignof(k), CST_##k, (r))
static Expr* new_expr_(Allocator* allocator, size_t size, size_t align, ExprKind kind, ProgRange range)
{
    Expr* expr = mem_allocate(allocator, size, align, true);
    expr->kind = kind;
    expr->range = range;

    return expr;
}

Expr* new_expr_ternary(Allocator* allocator, Expr* cond, Expr* then_expr, Expr* else_expr)
{
    ProgRange range = {.start = cond->range.start, .end = else_expr->range.end};
    ExprTernary* expr = new_expr(allocator, ExprTernary, range);
    expr->cond = cond;
    expr->then_expr = then_expr;
    expr->else_expr = else_expr;

    return (Expr*)expr;
}

Expr* new_expr_binary(Allocator* allocator, TokenKind op, Expr* left, Expr* right)
{
    ProgRange range = {.start = left->range.start, .end = right->range.end};
    ExprBinary* expr = new_expr(allocator, ExprBinary, range);
    expr->op = op;
    expr->left = left;
    expr->right = right;

    return (Expr*)expr;
}

Expr* new_expr_unary(Allocator* allocator, TokenKind op, Expr* unary_expr, ProgRange range)
{
    ExprUnary* expr = new_expr(allocator, ExprUnary, range);
    expr->op = op;
    expr->expr = unary_expr;

    return (Expr*)expr;
}

Expr* new_expr_field(Allocator* allocator, Expr* object, Identifier* field, ProgRange range)
{
    ExprField* expr = new_expr(allocator, ExprField, range);
    expr->object = object;
    expr->field = field;

    return (Expr*)expr;
}

Expr* new_expr_field_index(Allocator* allocator, Expr* object, Expr* index, ProgRange range)
{
    ExprFieldIndex* expr = new_expr(allocator, ExprFieldIndex, range);
    expr->object = object;
    expr->index = index;

    return (Expr*)expr;
}

Expr* new_expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range)
{
    ExprIndex* expr = new_expr(allocator, ExprIndex, range);
    expr->array = array;
    expr->index = index;

    return (Expr*)expr;
}

Expr* new_expr_call(Allocator* allocator, Expr* proc, size_t num_args, List* args, ProgRange range)
{
    ExprCall* expr = new_expr(allocator, ExprCall, range);
    expr->proc = proc;
    expr->num_args = num_args;

    list_replace(args, &expr->args);

    return (Expr*)expr;
}

ProcCallArg* new_proc_call_arg(Allocator* allocator, Expr* expr, Identifier* name)
{
    ProcCallArg* arg = alloc_type(allocator, ProcCallArg, true);
    arg->expr = expr;
    arg->name = name;

    return arg;
}

Expr* new_expr_int(Allocator* allocator, TokenInt token, ProgRange range)
{
    ExprInt* expr = new_expr(allocator, ExprInt, range);
    expr->token = token;

    return (Expr*)expr;
}

Expr* new_expr_float(Allocator* allocator, FloatKind fkind, Float value, ProgRange range)
{
    ExprFloat* expr = new_expr(allocator, ExprFloat, range);
    expr->fkind = fkind;
    expr->value = value;

    return (Expr*)expr;
}

Expr* new_expr_str(Allocator* allocator, StrLit* str_lit, ProgRange range)
{
    ExprStr* expr = new_expr(allocator, ExprStr, range);
    expr->str_lit = str_lit;

    return (Expr*)expr;
}

Expr* new_expr_ident(Allocator* allocator, NSIdent* ns_ident)
{
    ExprIdent* expr = new_expr(allocator, ExprIdent, ns_ident->range);
    expr->ns_ident.range = ns_ident->range;
    expr->ns_ident.num_idents = ns_ident->num_idents;

    list_replace(&ns_ident->idents, &expr->ns_ident.idents);

    return (Expr*)expr;
}

Expr* new_expr_cast(Allocator* allocator, TypeSpec* typespec, Expr* arg, bool implicit, ProgRange range)
{
    ExprCast* expr = new_expr(allocator, ExprCast, range);
    expr->typespec = typespec;
    expr->expr = arg;
    expr->implicit = implicit;

    return (Expr*)expr;
}

Expr* new_expr_bit_cast(Allocator* allocator, TypeSpec* typespec, Expr* arg, ProgRange range)
{
    ExprBitCast* expr = new_expr(allocator, ExprBitCast, range);
    expr->typespec = typespec;
    expr->expr = arg;

    return (Expr*)expr;
}

Expr* new_expr_sizeof(Allocator* allocator, TypeSpec* typespec, ProgRange range)
{
    ExprSizeof* expr = new_expr(allocator, ExprSizeof, range);
    expr->typespec = typespec;

    return (Expr*)expr;
}

Expr* new_expr_typeid(Allocator* allocator, TypeSpec* typespec, ProgRange range)
{
    ExprTypeid* expr = new_expr(allocator, ExprTypeid, range);
    expr->typespec = typespec;

    return (Expr*)expr;
}

Expr* new_expr_offsetof(Allocator* allocator, TypeSpec* obj_ts, Identifier* field_ident, ProgRange range)
{
    ExprOffsetof* expr = new_expr(allocator, ExprOffsetof, range);
    expr->obj_ts = obj_ts;
    expr->field_ident = field_ident;

    return (Expr*)expr;
}

Expr* new_expr_indexof(Allocator* allocator, TypeSpec* obj_ts, Identifier* field_ident, ProgRange range)
{
    ExprIndexof* expr = new_expr(allocator, ExprIndexof, range);
    expr->obj_ts = obj_ts;
    expr->field_ident = field_ident;

    return (Expr*)expr;
}

Expr* new_expr_length(Allocator* allocator, Expr* arg, ProgRange range)
{
    ExprLength* expr = new_expr(allocator, ExprLength, range);
    expr->arg = arg;

    return (Expr*)expr;
}

MemberInitializer* new_member_initializer(Allocator* allocator, Expr* init, Designator designator, ProgRange range)
{
    MemberInitializer* initzer = alloc_type(allocator, MemberInitializer, true);
    initzer->range = range;
    initzer->init = init;
    initzer->designator = designator;

    return initzer;
}

Expr* new_expr_compound_lit(Allocator* allocator, TypeSpec* typespec, size_t num_initzers, List* initzers, ProgRange range)
{
    ExprCompoundLit* expr = new_expr(allocator, ExprCompoundLit, range);
    expr->typespec = typespec;
    expr->num_initzers = num_initzers;

    list_replace(initzers, &expr->initzers);

    return (Expr*)expr;
}

Expr* new_expr_bool_lit(Allocator* allocator, bool val, ProgRange range)
{
    ExprBoolLit* expr = new_expr(allocator, ExprBoolLit, range);
    expr->val = val;

    return (Expr*)expr;
}

Expr* new_expr_null_lit(Allocator* allocator, ProgRange range)
{
    ExprNullLit* expr = new_expr(allocator, ExprNullLit, range);

    return (Expr*)expr;
}

DeclAnnotation* new_annotation(Allocator* allocator, Identifier* ident, u32 num_args, List* args, ProgRange range)
{
    DeclAnnotation* annotation = alloc_type(allocator, DeclAnnotation, true);
    annotation->ident = ident;
    annotation->range = range;
    annotation->num_args = num_args;

    list_replace(args, &annotation->args);

    return annotation;
}

#define new_decl(a, k, n, r) (k*)new_decl_((a), sizeof(k), alignof(k), CST_##k, (n), (r))
static Decl* new_decl_(Allocator* allocator, size_t size, size_t align, DeclKind kind, Identifier* name, ProgRange range)
{
    Decl* decl = mem_allocate(allocator, size, align, true);
    decl->kind = kind;
    decl->name = name;
    decl->range = range;

    list_head_init(&decl->annotations);

    return decl;
}

Decl* new_decl_var(Allocator* allocator, Identifier* name, TypeSpec* typespec, Expr* init, unsigned flags, ProgRange range)
{
    DeclVar* decl = new_decl(allocator, DeclVar, name, range);
    decl->typespec = typespec;
    decl->flags = flags;
    decl->init = init;

    return (Decl*)decl;
}

Decl* new_decl_const(Allocator* allocator, Identifier* name, TypeSpec* typespec, Expr* init, ProgRange range)
{
    DeclConst* decl = new_decl(allocator, DeclConst, name, range);
    decl->typespec = typespec;
    decl->init = init;

    return (Decl*)decl;
}

Decl* new_decl_typedef(Allocator* allocator, Identifier* name, TypeSpec* typespec, ProgRange range)
{
    DeclTypedef* decl = new_decl(allocator, DeclTypedef, name, range);
    decl->typespec = typespec;

    return (Decl*)decl;
}

Decl* new_decl_enum(Allocator* allocator, Identifier* name, TypeSpec* typespec, size_t num_items, List* items, ProgRange range)
{
    DeclEnum* decl = new_decl(allocator, DeclEnum, name, range);
    decl->typespec = typespec;
    decl->num_items = num_items;

    list_replace(items, &decl->items);

    return (Decl*)decl;
}

DeclEnumItem* new_decl_enum_item(Allocator* allocator, Identifier* name, Expr* value, ProgRange range)
{
    DeclEnumItem* item = new_decl(allocator, DeclEnumItem, name, range);
    item->value = value;

    return item;
}

Decl* new_decl_struct(Allocator* allocator, Identifier* name, List* fields, ProgRange range)
{
    DeclStruct* decl = new_decl(allocator, DeclStruct, name, range);

    list_replace(fields, &decl->fields);

    return (Decl*)decl;
}

Decl* new_decl_union(Allocator* allocator, Identifier* name, List* fields, ProgRange range)
{
    DeclUnion* decl = new_decl(allocator, DeclUnion, name, range);

    list_replace(fields, &decl->fields);

    return (Decl*)decl;
}

AggregateField* new_aggregate_field(Allocator* allocator, Identifier* name, TypeSpec* typespec, ProgRange range)
{
    AggregateField* field = alloc_type(allocator, AggregateField, true);
    field->name = name;
    field->typespec = typespec;
    field->range = range;

    return field;
}

Decl* new_decl_proc(Allocator* allocator, Identifier* name, u32 num_params, List* params, TypeSpec* ret, List* stmts, u32 num_decls,
                    bool is_incomplete, bool is_variadic, ProgRange range)
{
    DeclProc* decl = new_decl(allocator, DeclProc, name, range);
    decl->ret = ret;
    decl->num_params = num_params;
    decl->is_incomplete = is_incomplete;
    decl->is_variadic = is_variadic;
    decl->num_decls = num_decls;

    list_replace(params, &decl->params);
    list_replace(stmts, &decl->stmts);

    return (Decl*)decl;
}

#define new_stmt(a, k, r) (k*)new_stmt_((a), sizeof(k), alignof(k), CST_##k, (r))
static Stmt* new_stmt_(Allocator* allocator, size_t size, size_t align, StmtKind kind, ProgRange range)
{
    Stmt* stmt = mem_allocate(allocator, size, align, true);
    stmt->kind = kind;
    stmt->range = range;

    return stmt;
}

Stmt* new_stmt_noop(Allocator* allocator, ProgRange range)
{
    StmtNoOp* stmt = new_stmt(allocator, StmtNoOp, range);

    return (Stmt*)stmt;
}

Stmt* new_stmt_static_assert(Allocator* allocator, Expr* cond, StrLit* msg, ProgRange range)
{
    StmtStaticAssert* stmt = new_stmt(allocator, StmtStaticAssert, range);
    stmt->cond = cond;
    stmt->msg = msg;

    return (Stmt*)stmt;
}

ImportSymbol* new_import_symbol(Allocator* allocator, Identifier* name, Identifier* rename, ProgRange range)
{
    ImportSymbol* isym = alloc_type(allocator, ImportSymbol, true);
    isym->name = name;
    isym->rename = rename;
    isym->range = range;

    return isym;
}

Stmt* new_stmt_import(Allocator* allocator, size_t num_imports, List* import_syms, StrLit* mod_pathname, Identifier* mod_namespace,
                      ProgRange range)
{
    StmtImport* stmt = new_stmt(allocator, StmtImport, range);
    stmt->mod_pathname = mod_pathname;
    stmt->mod_namespace = mod_namespace;
    stmt->num_imports = num_imports;

    list_replace(import_syms, &stmt->import_syms);

    return (Stmt*)stmt;
}

ExportSymbol* new_export_symbol(Allocator* allocator, NSIdent* ns_ident, Identifier* rename, ProgRange range)
{
    ExportSymbol* esym = alloc_type(allocator, ExportSymbol, true);
    esym->rename = rename;
    esym->range = range;
    esym->ns_ident.range = ns_ident->range;
    esym->ns_ident.num_idents = ns_ident->num_idents;

    list_replace(&ns_ident->idents, &esym->ns_ident.idents);

    return esym;
}

Stmt* new_stmt_export(Allocator* allocator, size_t num_exports, List* export_syms, ProgRange range)
{
    StmtExport* stmt = new_stmt(allocator, StmtExport, range);
    stmt->num_exports = num_exports;

    list_replace(export_syms, &stmt->export_syms);

    return (Stmt*)stmt;
}

Stmt* new_stmt_include(Allocator* allocator, StrLit* file_pathname, ProgRange range)
{
    StmtInclude* stmt = new_stmt(allocator, StmtInclude, range);
    stmt->file_pathname = file_pathname;

    return (Stmt*)stmt;
}

Identifier* get_import_sym_name(StmtImport* stmt, Identifier* name)
{
    //
    // Import statements support symbol renaming. This procedure takes in an identifier used
    // in an expression context and returns the original imported symbol's name.
    //
    // For example, assume the following import statement:
    //
    //   import { len, cpy as copy } from "./cstring.nib" as CString;
    //
    // The following are true:
    //
    //   'len' returns 'len'
    //   'copy' returns 'cpy'
    //   'malloc' returns 'malloc'
    //   'unknown' returns NULL
    //
    // Now, assume the following greedy import statement.
    //
    //   import "./memory" as Memory; // Assume this module exports "malloc"
    //
    // For this kind of import statement, the input name is always returned as the output.
    //
    //   'malloc' returns 'malloc'
    //   'unknown' returns 'unknown'
    //
    // This is only called for import statements with a module namespace.
    //

    Identifier* sym_name = NULL;
    List* import_syms = &stmt->import_syms;

    // If the import statement explicitly imports certain symbols, make sure that we're not trying
    // to access a symbol that was not imported.
    if (!list_empty(import_syms)) {
        List* it = import_syms->next;

        // Look to see if the expression's identifier name is among the imported symbols.
        // If so, set sym_name to the expected native symbol name.
        while (it != import_syms) {
            ImportSymbol* isym = list_entry(it, ImportSymbol, lnode);
            Identifier* isym_name = isym->rename != NULL ? isym->rename : isym->name;

            if (isym_name == name) {
                sym_name = isym->name;
                break;
            }

            it = it->next;
        }
    }
    else {
        sym_name = name;
    }

    return sym_name;
}

Stmt* new_stmt_decl(Allocator* allocator, Decl* decl)
{
    StmtDecl* stmt = new_stmt(allocator, StmtDecl, decl->range);
    stmt->decl = decl;

    return (Stmt*)stmt;
}

Stmt* new_stmt_block(Allocator* allocator, List* stmts, u32 num_decls, ProgRange range)
{
    StmtBlock* stmt = new_stmt(allocator, StmtBlock, range);
    stmt->num_decls = num_decls;

    list_replace(stmts, &stmt->stmts);

    return (Stmt*)stmt;
}

Stmt* new_stmt_expr(Allocator* allocator, Expr* expr, ProgRange range)
{
    StmtExpr* stmt = new_stmt(allocator, StmtExpr, range);
    stmt->expr = expr;

    return (Stmt*)stmt;
}

Stmt* new_stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range)
{
    StmtExprAssign* stmt = new_stmt(allocator, StmtExprAssign, range);
    stmt->left = lexpr;
    stmt->op_assign = op_assign;
    stmt->right = rexpr;

    return (Stmt*)stmt;
}

Stmt* new_stmt_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range)
{
    StmtWhile* stmt = new_stmt(allocator, StmtWhile, range);
    stmt->cond = cond;
    stmt->body = body;

    return (Stmt*)stmt;
}

Stmt* new_stmt_do_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range)
{
    StmtDoWhile* stmt = new_stmt(allocator, StmtDoWhile, range);
    stmt->cond = cond;
    stmt->body = body;

    return (Stmt*)stmt;
}

Stmt* new_stmt_if(Allocator* allocator, IfCondBlock* if_blk, ElseBlock* else_blk, ProgRange range)
{
    StmtIf* stmt = new_stmt(allocator, StmtIf, range);

    stmt->if_blk.range = if_blk->range;
    stmt->if_blk.cond = if_blk->cond;
    stmt->if_blk.body = if_blk->body;

    stmt->else_blk.range = else_blk->range;
    stmt->else_blk.body = else_blk->body;

    return (Stmt*)stmt;
}

Stmt* new_stmt_for(Allocator* allocator, Stmt* init, Expr* cond, Stmt* next, Stmt* body, ProgRange range)
{
    StmtFor* stmt = new_stmt(allocator, StmtFor, range);
    stmt->init = init;
    stmt->cond = cond;
    stmt->next = next;
    stmt->body = body;

    return (Stmt*)stmt;
}

Stmt* new_stmt_return(Allocator* allocator, Expr* expr, ProgRange range)
{
    StmtReturn* stmt = new_stmt(allocator, StmtReturn, range);
    stmt->expr = expr;

    return (Stmt*)stmt;
}

Stmt* new_stmt_break(Allocator* allocator, const char* label, ProgRange range)
{
    StmtBreak* stmt = new_stmt(allocator, StmtBreak, range);
    stmt->label = label;

    return (Stmt*)stmt;
}

Stmt* new_stmt_continue(Allocator* allocator, const char* label, ProgRange range)
{
    StmtContinue* stmt = new_stmt(allocator, StmtContinue, range);
    stmt->label = label;

    return (Stmt*)stmt;
}

Stmt* new_stmt_goto(Allocator* allocator, const char* label, ProgRange range)
{
    StmtGoto* stmt = new_stmt(allocator, StmtGoto, range);
    stmt->label = label;

    return (Stmt*)stmt;
}

Stmt* new_stmt_label(Allocator* allocator, const char* label, Stmt* target, ProgRange range)
{
    StmtLabel* stmt = new_stmt(allocator, StmtLabel, range);
    stmt->label = label;
    stmt->target = target;

    return (Stmt*)stmt;
}

SwitchCase* new_switch_case(Allocator* allocator, Expr* start, Expr* end, List* stmts, ProgRange range)
{
    SwitchCase* swcase = alloc_type(allocator, SwitchCase, true);
    swcase->start = start;
    swcase->end = end;
    swcase->range = range;

    list_replace(stmts, &swcase->stmts);

    return swcase;
}

Stmt* new_stmt_switch(Allocator* allocator, Expr* expr, List* cases, ProgRange range)
{
    StmtSwitch* stmt = new_stmt(allocator, StmtSwitch, range);
    stmt->expr = expr;

    list_replace(cases, &stmt->cases);

    return (Stmt*)stmt;
}

//////////////////////////////
//     Types
//////////////////////////////

BuiltinType builtin_types[NUM_BUILTIN_TYPES];

Type* type_ptr_void;
Type* type_ptr_char;
Type* type_ptr_ptr_char;

size_t PTR_SIZE = 8;
size_t PTR_ALIGN = 8;

int type_integer_ranks[] = {
    [INTEGER_U8] = 1,  [INTEGER_S8] = 1,  [INTEGER_U16] = 2, [INTEGER_S16] = 2,
    [INTEGER_U32] = 3, [INTEGER_S32] = 3, [INTEGER_U64] = 4, [INTEGER_S64] = 4,
};

static const char* type_names[] = {
    [TYPE_VOID] = "void",     [TYPE_INTEGER] = "_integer_",
    [TYPE_FLOAT] = "_float_", [TYPE_ENUM] = "_enum_",
    [TYPE_PTR] = "_ptr_",     [TYPE_PROC] = "_proc_",
    [TYPE_ARRAY] = "_array_", [TYPE_STRUCT] = "_struct_",
    [TYPE_UNION] = "_union_", [TYPE_INCOMPLETE_AGGREGATE] = "_incomplete_aggregate_",
};

static const char* type_integer_names[] = {
    [INTEGER_BOOL] = "bool", [INTEGER_U8] = "u8",   [INTEGER_S8] = "s8",   [INTEGER_U16] = "u16", [INTEGER_S16] = "s16",
    [INTEGER_U32] = "u32",   [INTEGER_S32] = "s32", [INTEGER_U64] = "u64", [INTEGER_S64] = "s64",
};

static const char* type_float_names[] = {
    [FLOAT_F64] = "f64",
    [FLOAT_F32] = "f32",
};

static size_t next_type_id = 1;

const char* type_name(const Type* type)
{
    if (!type)
        return "null";

    switch (type->kind) {
    case TYPE_INTEGER:
        return type_integer_names[type->as_integer.kind];
    case TYPE_FLOAT:
        return type_float_names[type->as_float.kind];
    default:
        return type_names[type->kind];
    }
}

bool type_is_integer_like(const Type* type)
{
    TypeKind kind = type->kind;

    return (kind == TYPE_INTEGER) || (kind == TYPE_ENUM);
}

bool type_is_bool(const Type* type)
{
    return (type->kind == TYPE_INTEGER) && (type->as_integer.kind == INTEGER_BOOL);
}

bool type_is_signed(const Type* type)
{
    IntegerKind ikind;

    if (type->kind == TYPE_INTEGER) {
        ikind = type->as_integer.kind;
    }
    else if (type->kind == TYPE_ENUM) {
        ikind = type->as_enum.base->as_integer.kind;
    }
    else {
        assert(type->kind == TYPE_PTR);
        ikind = INTEGER_U64;
    }

    return int_kind_signed[ikind];
}

bool type_is_arithmetic(const Type* type)
{
    TypeKind kind = type->kind;

    return (kind == TYPE_INTEGER) || (kind == TYPE_FLOAT) || (kind == TYPE_ENUM);
}

bool type_is_scalar(const Type* type)
{
    TypeKind kind = type->kind;

    return type_is_arithmetic(type) || (kind == TYPE_PTR) || (kind == TYPE_PROC);
}

bool type_is_int_scalar(const Type* type)
{
    return type_is_integer_like(type) || type_is_ptr_like(type);
}

bool type_is_ptr_like(const Type* type)
{
    TypeKind kind = type->kind;

    return (kind == TYPE_PTR) || (kind == TYPE_PROC);
}

bool type_is_aggregate(const Type* type)
{
    TypeKind kind = type->kind;

    return (kind == TYPE_STRUCT) || (kind == TYPE_UNION);
}

bool type_is_obj_like(const Type* type)
{
    return (type->kind == TYPE_ARRAY) || type_is_aggregate(type);
}

bool type_is_slice(const Type* type)
{
    return (type->kind == TYPE_STRUCT) && (type->as_struct.wrapper_kind == TYPE_STRUCT_IS_SLICE_WRAPPER);
}

bool slice_and_array_compatible(Type* array_type, Type* slice_type)
{
    assert(array_type->kind == TYPE_ARRAY);
    assert(type_is_slice(slice_type));

    TypeAggregateField* data_field = get_type_struct_field(slice_type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA]);
    Type* slice_elem_type = data_field->type->as_ptr.base;
    Type* array_elem_type = array_type->as_array.base;

    return slice_elem_type == array_elem_type;
}

bool type_is_incomplete_array(Type* type)
{
    return (type->kind == TYPE_ARRAY) && (type->as_array.len == 0);
}

bool type_has_incomplete_array(Type* type)
{
    Type* t = type;

    while (t->kind == TYPE_ARRAY || t->kind == TYPE_PTR) {
        if (t->kind == TYPE_ARRAY) {
            if (t->as_array.len == 0) {
                return true;
            }

            t = t->as_array.base;
        }
        else {
            t = t->as_ptr.base;
        }
    }

    return false;
}

bool ptr_types_are_derived(Type* base, Type* derived)
{
    assert(base->kind == TYPE_PTR && derived->kind == TYPE_PTR);

    Type* base_pointed_type = base->as_ptr.base;
    Type* derived_pointed_type = derived->as_ptr.base;

    // A type is "derived" if its first field is of type "base".
    // Ex: struct Base { ... };  struct Derived { Base base;};
    // Derived* d = malloc(...);
    // Base* b = d;
    if (type_is_aggregate(base_pointed_type) && (derived_pointed_type->kind == TYPE_STRUCT) &&
        (base_pointed_type == derived_pointed_type->as_struct.body.fields[0].type)) {
        return true;
    }

    return false;
}

Type* common_ptr_type(Type* t, Type* u)
{
    assert(t->kind == TYPE_PTR && u->kind == TYPE_PTR);

    Type* common = NULL;

    if (types_are_compatible(t, u)) {
        common = t;
    }
    else if (ptr_types_are_derived(t, u)) {
        common = t;
    }
    else if (ptr_types_are_derived(u, t)) {
        common = u;
    }
    else if (t == type_ptr_void) {
        common = u;
    }
    else if (u == type_ptr_void) {
        common = t;
    }

    return common;
}

bool types_are_compatible(Type* t, Type* u)
{
    if (t == u) {
        return true;
    }

    if (t->kind == TYPE_PTR && u->kind == TYPE_PTR) {
        return types_are_compatible(t->as_ptr.base, u->as_ptr.base);
    }

    if (t->kind == TYPE_ARRAY && u->kind == TYPE_ARRAY) {
        TypeArray* t_arr = &t->as_array;
        TypeArray* u_arr = &u->as_array;

        bool elems_compat = types_are_compatible(t_arr->base, u_arr->base);
        bool sizes_compat = (!t_arr->len || !u_arr->len || t_arr->len == u_arr->len);

        return elems_compat && sizes_compat;
    }

    if (t->kind == TYPE_ENUM && u->kind == TYPE_INTEGER) {
        return t->as_enum.base == u;
    }

    if (t->kind == TYPE_INTEGER && u->kind == TYPE_ENUM) {
        return t == u->as_enum.base;
    }

    if (t->kind == TYPE_PROC && u->kind == TYPE_PROC) {
        TypeProc* t_proc = &t->as_proc;
        TypeProc* u_proc = &u->as_proc;

        if (!types_are_compatible(t_proc->ret, u_proc->ret)) {
            return false;
        }

        if (t_proc->num_params != u_proc->num_params) {
            return false;
        }

        for (size_t i = 0; i < t_proc->num_params; i += 1) {
            if (!types_are_compatible(t_proc->params[i], u_proc->params[i])) {
                return false;
            }
        }

        return true;
    }

    return false;
}

bool type_agg_has_non_float(const Type* type)
{
    if (type->kind == TYPE_FLOAT) {
        return false;
    }

    if (type->kind == TYPE_ARRAY) {
        return type_agg_has_non_float(type->as_array.base);
    }

    const TypeAggregateBody* agg_body = NULL;

    if (type->kind == TYPE_STRUCT) {
        agg_body = &type->as_struct.body;
    }
    else if (type->kind == TYPE_UNION) {
        agg_body = &type->as_union.body;
    }

    if (agg_body) {
        for (size_t i = 0; i < agg_body->num_fields; i++) {
            const TypeAggregateField* field = agg_body->fields + i;

            if (type_agg_has_non_float(field->type)) {
                return true;
            }
        }

        return false;
    }

    return true;
}

Type* type_unsigned_int(Type* type_int)
{
    assert(type_int->kind == TYPE_INTEGER);

    switch (type_int->as_integer.kind) {
    case INTEGER_BOOL:
        return type_int;
    case INTEGER_U8:
    case INTEGER_S8:
        return builtin_types[BUILTIN_TYPE_U8].type;
    case INTEGER_U16:
    case INTEGER_S16:
        return builtin_types[BUILTIN_TYPE_U16].type;
    case INTEGER_U32:
    case INTEGER_S32:
        return builtin_types[BUILTIN_TYPE_U32].type;
    case INTEGER_U64:
    case INTEGER_S64:
        return builtin_types[BUILTIN_TYPE_U64].type;
    default:
        NIBBLE_FATAL_EXIT("type_unsigned_int() - unexpected IntegerKind %d.", type_int->as_integer.kind);
    }

    return NULL;
}

Type* try_array_decay(Allocator* allocator, HMap* type_ptr_cache, Type* type)
{
    if (type->kind == TYPE_ARRAY)
        return type_ptr(allocator, type_ptr_cache, type->as_array.base);

    return type;
}

static Type* type_alloc(Allocator* allocator, TypeKind kind)
{
    Type* type = alloc_type(allocator, Type, true);
    type->kind = kind;
    type->id = next_type_id++;

    return type;
}

static Type* type_int_alloc(Allocator* allocator, IntegerKind kind)
{
    const size_t size = int_kind_sizes[kind];

    Type* type = type_alloc(allocator, TYPE_INTEGER);
    type->size = size;
    type->align = size;
    type->as_integer.kind = kind;

    return type;
}

static Type* type_float_alloc(Allocator* allocator, FloatKind kind)
{
    const size_t size = float_kind_sizes[kind];
    Type* type = type_alloc(allocator, TYPE_FLOAT);
    type->size = size;
    type->align = size;
    type->as_float.kind = kind;

    return type;
}

Type* type_enum(Allocator* allocator, Type* base, DeclEnum* decl)
{
    assert(base);
    Type* type = type_alloc(allocator, TYPE_ENUM);
    type->size = base->size;
    type->align = base->align;
    type->as_enum.base = base;
    type->as_enum.decl = decl;

    return type;
}

Type* type_incomplete_aggregate(Allocator* allocator, Symbol* sym)
{
    Type* type = type_alloc(allocator, TYPE_INCOMPLETE_AGGREGATE);
    type->as_incomplete.sym = sym;

    return type;
}

TypeAggregateField* get_type_struct_field(Type* type, Identifier* name)
{
    assert(type->kind == TYPE_STRUCT);
    TypeAggregateBody* body = &type->as_struct.body;
    size_t num_fields = body->num_fields;
    TypeAggregateField* fields = body->fields;

    for (size_t i = 0; i < num_fields; i += 1) {
        TypeAggregateField* field = fields + i;

        if (field->name == name) {
            return field;
        }
    }

    return NULL;
}

TypeAggregateField* get_type_union_field(Type* type, Identifier* name)
{
    assert(type->kind == TYPE_UNION);
    TypeAggregateBody* body = &type->as_union.body;
    size_t num_fields = body->num_fields;
    TypeAggregateField* fields = body->fields;

    for (size_t i = 0; i < num_fields; i += 1) {
        TypeAggregateField* field = fields + i;

        if (field->name == name) {
            return field;
        }
    }

    return NULL;
}

TypeAggregateField* get_type_aggregate_field(Type* type, Identifier* name)
{
    if (type->kind == TYPE_STRUCT) {
        return get_type_struct_field(type, name);
    }
    else {
        assert(type->kind == TYPE_UNION);
        return get_type_union_field(type, name);
    }
}

Type* type_slice(Allocator* allocator, HMap* type_slice_cache, HMap* type_ptr_cache, Type* elem_type)
{
    uint64_t* pval = hmap_get(type_slice_cache, PTR_UINT(elem_type));
    Type* type = pval ? (void*)*pval : NULL;

    if (!type) {
        type = type_alloc(allocator, TYPE_INCOMPLETE_AGGREGATE);

        TypeAggregateField fields[2] = {0};
        fields[0].type = builtin_types[BUILTIN_TYPE_USIZE].type;
        fields[0].name = builtin_struct_fields[BUILTIN_STRUCT_FIELD_LENGTH];

        fields[1].type = type_ptr(allocator, type_ptr_cache, elem_type);
        fields[1].name = builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA];

        complete_struct_type(allocator, type, ARRAY_LEN(fields), fields);

        type->as_struct.wrapper_kind = TYPE_STRUCT_IS_SLICE_WRAPPER;

        hmap_put(type_slice_cache, PTR_UINT(elem_type), PTR_UINT(type));
    }

    return type;
}

static u64 hash_aggregate_fields(size_t num_fields, const TypeAggregateField* fields)
{
    u64 h = FNV_INIT;
    size_t i = 0;

    do {
        const TypeAggregateField* f = fields + i;

        u64 t_h = hash_bytes(&f->type, sizeof(Type*), h);
        h = hash_bytes(&f->name, sizeof(Identifier*), t_h);

        i += 1;
    } while (i < num_fields);

    return h;
}

typedef struct CachedType {
    Type* type;
    struct CachedType* next;
} CachedType;

Type* type_anon_aggregate(Allocator* allocator, HMap* type_cache, TypeKind kind, size_t num_fields, const TypeAggregateField* fields)
{
    u64 key = hash_aggregate_fields(num_fields, fields);
    u64* pval = hmap_get(type_cache, key);
    CachedType* cached = pval ? (void*)*pval : NULL;

    // Returned cached type if it exists.
    for (CachedType* it = cached; it != NULL; it = it->next) {
        Type* type = it->type;
        assert(type->kind == TYPE_STRUCT || type->kind == TYPE_UNION);
        TypeAggregateBody* body = type->kind == TYPE_STRUCT ? &type->as_struct.body : &type->as_union.body;

        if (body->num_fields == num_fields) {
            bool equal = true;

            for (size_t i = 0; i < num_fields; i++) {
                TypeAggregateField* f_other = body->fields + i;
                const TypeAggregateField* f_this = fields + i;

                if ((f_this->type != f_other->type) || (f_this->name != f_other->name)) {
                    equal = false;
                    break;
                }
            }

            if (equal) {
                return type;
            }
        }
    }

    // Create a new type, cache it, and return it.
    Type* type = type_alloc(allocator, TYPE_INCOMPLETE_AGGREGATE);

    if (kind == TYPE_STRUCT) {
        complete_struct_type(allocator, type, num_fields, fields);
    }
    else {
        assert(kind == TYPE_UNION);
        complete_union_type(allocator, type, num_fields, fields);
    }

    CachedType* new_cached = alloc_type(allocator, CachedType, true);
    new_cached->type = type;
    new_cached->next = cached;

    hmap_put(type_cache, key, PTR_UINT(new_cached));

    return type;
}

void complete_struct_type(Allocator* allocator, Type* type, size_t num_fields, const TypeAggregateField* fields)
{
    size_t size = 0;
    size_t align = 0;

    TypeAggregateField* fields_cpy = mem_dup_array(allocator, TypeAggregateField, fields, num_fields);

    for (size_t i = 0; i < num_fields; i += 1) {
        TypeAggregateField* field = fields_cpy + i;
        size_t field_align = field->type->align;
        size_t field_size = field->type->size;

        // Make sure the field is placed at an offset that is divisible by its alignment.
        size = ALIGN_UP(size, field_align);
        field->offset = size;
        field->index = i;

        // Increase the struct's size by the field's size.
        size += field_size;

        // Update the struct to use the maximum field alignment.
        if (field_align > align) {
            align = field_align;
        }
    }

    // Align-up struct size by the largest field alignment (to ensure an array of struct elems is correctly aligned).
    size = ALIGN_UP(size, align);

    // Update the previously incomplete type into a proper struct type.
    type->kind = TYPE_STRUCT;
    type->size = size;
    type->align = align;

    type->as_struct.wrapper_kind = TYPE_STRUCT_IS_NOT_WRAPPER; // Default
    type->as_struct.body.num_fields = num_fields;
    type->as_struct.body.fields = fields_cpy;
}

void complete_union_type(Allocator* allocator, Type* type, size_t num_fields, const TypeAggregateField* fields)
{
    size_t size = 0;
    size_t align = 0;

    TypeAggregateField* fields_cpy = mem_dup_array(allocator, TypeAggregateField, fields, num_fields);

    for (size_t i = 0; i < num_fields; i += 1) {
        TypeAggregateField* field = fields_cpy + i;
        size_t field_align = field->type->align;
        size_t field_size = field->type->size;

        // All union fields start at offset 0 (share the same memory region).
        field->offset = 0;
        field->index = i;

        // Update the union to use the maximum field alignment and size.
        if (field_align > align) {
            align = field_align;
        }

        if (field_size > size) {
            size = field_size;
        }
    }

    // Align-up union size by the largest field alignment (to ensure an array of union elems is correctly aligned).
    size = ALIGN_UP(size, align);

    // Update the previously incomplete type into a proper union type.
    type->kind = TYPE_UNION;
    type->size = size;
    type->align = align;

    type->as_union.body.num_fields = num_fields;
    type->as_union.body.fields = fields_cpy;
}

Type* type_ptr(Allocator* allocator, HMap* type_ptr_cache, Type* base)
{
    uint64_t* pval = hmap_get(type_ptr_cache, PTR_UINT(base));
    Type* type = pval ? (void*)*pval : NULL;

    if (!type) {
        type = type_alloc(allocator, TYPE_PTR);
        type->size = PTR_SIZE;
        type->align = PTR_ALIGN;
        type->as_ptr.base = base;

        hmap_put(type_ptr_cache, PTR_UINT(base), PTR_UINT(type));
    }

    return type;
}

Type* type_array(Allocator* allocator, HMap* type_array_cache, Type* base, size_t len)
{
    u64 key = hash_mix_uint64(hash_ptr(base), len);
    u64* pval = hmap_get(type_array_cache, key);
    CachedType* cached = pval ? (void*)*pval : NULL;

    // Return cached type if it exists.
    for (CachedType* it = cached; it != NULL; it = it->next) {
        Type* type = it->type;

        if ((type->as_array.len == len) && (type->as_array.base == base))
            return type;
    }

    // Create a new type, cache it, and return it.
    Type* type = type_alloc(allocator, TYPE_ARRAY);
    type->size = base->size * len;
    type->align = base->align;
    type->as_array.base = base;
    type->as_array.len = len;

    CachedType* new_cached = alloc_type(allocator, CachedType, true);
    new_cached->type = type;
    new_cached->next = cached;

    hmap_put(type_array_cache, key, PTR_UINT(new_cached));

    return type;
}

Type* type_proc(Allocator* allocator, HMap* type_proc_cache, size_t num_params, Type** params, Type* ret, bool is_variadic)
{
    size_t params_size = num_params * sizeof(Type*);
    uint64_t key = hash_mix_uint64(hash_mix_uint64(hash_bytes(params, params_size, FNV_INIT), hash_ptr(ret)), is_variadic);
    uint64_t* pval = hmap_get(type_proc_cache, key);
    CachedType* cached = pval ? (void*)*pval : NULL;

    // Return cached type if it exists.
    for (CachedType* it = cached; it != NULL; it = it->next) {
        Type* type = it->type;

        if ((type->as_proc.is_variadic == is_variadic) && (type->as_proc.num_params == num_params) && (type->as_proc.ret == ret)) {
            bool params_equal = true;

            for (size_t i = 0; i < num_params; i += 1) {
                if (type->as_proc.params[i] != params[i]) {
                    params_equal = false;
                    break;
                }
            }

            if (params_equal) {
                return it->type;
            }
        }
    }

    // Create a new type, cache it, and return it.
    Type* type = type_alloc(allocator, TYPE_PROC);
    type->size = PTR_SIZE;
    type->align = PTR_ALIGN;
    type->as_proc.num_params = num_params;
    type->as_proc.params = mem_dup_array(allocator, Type*, params, num_params);
    type->as_proc.ret = ret;
    type->as_proc.is_variadic = is_variadic;

    CachedType* new_cached = alloc_type(allocator, CachedType, true);
    new_cached->type = type;
    new_cached->next = cached;

    hmap_put(type_proc_cache, key, PTR_UINT(new_cached));

    return type;
}

void init_builtin_types(OS target_os, Arch target_arch, Allocator* ast_mem, TypeCache* type_cache)
{
    bool invalid_os_arch = false;

    builtin_types[BUILTIN_TYPE_VOID] = (BuiltinType){.name = "void", .type = type_alloc(ast_mem, TYPE_VOID)};
    builtin_types[BUILTIN_TYPE_BOOL] = (BuiltinType){.name = "bool", .type = type_int_alloc(ast_mem, INTEGER_BOOL)};
    builtin_types[BUILTIN_TYPE_U8] = (BuiltinType){.name = "u8", .type = type_int_alloc(ast_mem, INTEGER_U8)};
    builtin_types[BUILTIN_TYPE_S8] = (BuiltinType){.name = "s8", .type = type_int_alloc(ast_mem, INTEGER_S8)};
    builtin_types[BUILTIN_TYPE_U16] = (BuiltinType){.name = "u16", .type = type_int_alloc(ast_mem, INTEGER_U16)};
    builtin_types[BUILTIN_TYPE_S16] = (BuiltinType){.name = "s16", .type = type_int_alloc(ast_mem, INTEGER_S16)};
    builtin_types[BUILTIN_TYPE_U32] = (BuiltinType){.name = "u32", .type = type_int_alloc(ast_mem, INTEGER_U32)};
    builtin_types[BUILTIN_TYPE_S32] = (BuiltinType){.name = "s32", .type = type_int_alloc(ast_mem, INTEGER_S32)};
    builtin_types[BUILTIN_TYPE_U64] = (BuiltinType){.name = "u64", .type = type_int_alloc(ast_mem, INTEGER_U64)};
    builtin_types[BUILTIN_TYPE_S64] = (BuiltinType){.name = "s64", .type = type_int_alloc(ast_mem, INTEGER_S64)};
    builtin_types[BUILTIN_TYPE_F32] = (BuiltinType){.name = "f32", .type = type_float_alloc(ast_mem, FLOAT_F32)};
    builtin_types[BUILTIN_TYPE_F64] = (BuiltinType){.name = "f64", .type = type_float_alloc(ast_mem, FLOAT_F64)};

    builtin_types[BUILTIN_TYPE_CHAR] = (BuiltinType){.name = "char", .type = builtin_types[BUILTIN_TYPE_S8].type};
    builtin_types[BUILTIN_TYPE_SCHAR] = (BuiltinType){.name = "schar", .type = builtin_types[BUILTIN_TYPE_S8].type};
    builtin_types[BUILTIN_TYPE_UCHAR] = (BuiltinType){.name = "uchar", .type = builtin_types[BUILTIN_TYPE_U8].type};
    builtin_types[BUILTIN_TYPE_SHORT] = (BuiltinType){.name = "short", .type = builtin_types[BUILTIN_TYPE_S16].type};
    builtin_types[BUILTIN_TYPE_USHORT] = (BuiltinType){.name = "ushort", .type = builtin_types[BUILTIN_TYPE_U16].type};
    builtin_types[BUILTIN_TYPE_INT] = (BuiltinType){.name = "int", .type = builtin_types[BUILTIN_TYPE_S32].type};
    builtin_types[BUILTIN_TYPE_UINT] = (BuiltinType){.name = "uint", .type = builtin_types[BUILTIN_TYPE_U32].type};
    builtin_types[BUILTIN_TYPE_LONG] = (BuiltinType){.name = "long"};
    builtin_types[BUILTIN_TYPE_ULONG] = (BuiltinType){.name = "ulong"};
    builtin_types[BUILTIN_TYPE_LLONG] = (BuiltinType){.name = "llong", .type = builtin_types[BUILTIN_TYPE_S64].type};
    builtin_types[BUILTIN_TYPE_ULLONG] = (BuiltinType){.name = "ullong", .type = builtin_types[BUILTIN_TYPE_U64].type};
    builtin_types[BUILTIN_TYPE_SSIZE] = (BuiltinType){.name = "ssize"};
    builtin_types[BUILTIN_TYPE_USIZE] = (BuiltinType){.name = "usize"};

    switch (target_os) {
    case OS_LINUX:
        switch (target_arch) {
        case ARCH_X64:
            builtin_types[BUILTIN_TYPE_LONG].type = builtin_types[BUILTIN_TYPE_S64].type;
            builtin_types[BUILTIN_TYPE_ULONG].type = builtin_types[BUILTIN_TYPE_U64].type;

            PTR_SIZE = 8;
            PTR_ALIGN = 8;
            break;
        default:
            invalid_os_arch = true;
            break;
        }
        break;
    default:
        invalid_os_arch = true;
        break;
    }

    if (invalid_os_arch) {
        ftprint_err("Unsupported OS architecture: %s %s\n", os_names[target_os], arch_names[target_arch]);
        exit(1);
    }

    if (PTR_SIZE == 4) {
        builtin_types[BUILTIN_TYPE_SSIZE].type = builtin_types[BUILTIN_TYPE_INT].type;
        builtin_types[BUILTIN_TYPE_USIZE].type = builtin_types[BUILTIN_TYPE_UINT].type;
    }
    else {
        assert(PTR_SIZE == 8);
        builtin_types[BUILTIN_TYPE_SSIZE].type = builtin_types[BUILTIN_TYPE_LLONG].type;
        builtin_types[BUILTIN_TYPE_USIZE].type = builtin_types[BUILTIN_TYPE_ULLONG].type;
    }

    type_ptr_void = type_ptr(ast_mem, &type_cache->ptrs, builtin_types[BUILTIN_TYPE_VOID].type);
    type_ptr_char = type_ptr(ast_mem, &type_cache->ptrs, builtin_types[BUILTIN_TYPE_CHAR].type);
    type_ptr_ptr_char = type_ptr(ast_mem, &type_cache->ptrs, type_ptr_char);

    // Create the _any_ type.
    // TODO: Specify this in a dedicated builtin.nib file.
    Type* type_any = type_alloc(ast_mem, TYPE_INCOMPLETE_AGGREGATE);

    TypeAggregateField fields[2] = {0};
    fields[0].type = builtin_types[BUILTIN_TYPE_USIZE].type;
    fields[0].name = builtin_struct_fields[BUILTIN_STRUCT_FIELD_TYPE];

    fields[1].type = type_ptr_void;
    fields[1].name = builtin_struct_fields[BUILTIN_STRUCT_FIELD_PTR];

    complete_struct_type(ast_mem, type_any, ARRAY_LEN(fields), fields);

    builtin_types[BUILTIN_TYPE_ANY].name = "any";
    builtin_types[BUILTIN_TYPE_ANY].type = type_any;
}

//////////////////////////////
//     Symbols
//////////////////////////////

const SymbolKind decl_sym_kind[CST_DECL_KIND_COUNT] = {
    [CST_DECL_NONE] = SYMBOL_NONE,  [CST_DeclVar] = SYMBOL_VAR,        [CST_DeclConst] = SYMBOL_CONST,
    [CST_DeclEnum] = SYMBOL_TYPE,   [CST_DeclEnumItem] = SYMBOL_CONST, [CST_DeclUnion] = SYMBOL_TYPE,
    [CST_DeclStruct] = SYMBOL_TYPE, [CST_DeclProc] = SYMBOL_PROC,      [CST_DeclTypedef] = SYMBOL_TYPE,
};

const char* sym_kind_names[SYMBOL_KIND_COUNT] = {
    [SYMBOL_VAR] = "variable", [SYMBOL_CONST] = "constant", [SYMBOL_PROC] = "procedure",
    [SYMBOL_TYPE] = "type",    [SYMBOL_MODULE] = "module",
};

Symbol* new_symbol(Allocator* allocator, SymbolKind kind, SymbolStatus status, Identifier* name, Module* home_mod)
{
    Symbol* sym = alloc_type(allocator, Symbol, true);

    sym->kind = kind;
    sym->status = status;
    sym->name = name;
    sym->home = home_mod;

    return sym;
}

Symbol* new_symbol_decl(Allocator* allocator, Decl* decl, Module* home_mod)
{
    Symbol* sym = new_symbol(allocator, decl_sym_kind[decl->kind], SYMBOL_STATUS_UNRESOLVED, decl->name, home_mod);
    sym->decl = decl;

    return sym;
}

Symbol* new_symbol_builtin_type(Allocator* allocator, Identifier* name, Type* type, Module* home_mod)
{
    Symbol* sym = new_symbol(allocator, SYMBOL_TYPE, SYMBOL_STATUS_RESOLVED, name, home_mod);
    sym->type = type;

    return sym;
}

Symbol* new_symbol_mod(Allocator* alloc, StmtImport* stmt, Module* import_mod, Module* home_mod)
{
    Symbol* sym = new_symbol(alloc, SYMBOL_MODULE, SYMBOL_STATUS_RESOLVED, stmt->mod_namespace, home_mod);
    sym->as_mod.mod = import_mod;
    sym->as_mod.stmt = (Stmt*)stmt;
    sym->is_local = false;

    return sym;
}

char* symbol_mangled_name(Allocator* allocator, const Symbol* sym)
{
    char* dstr = array_create(allocator, char, 32);

    if ((sym->kind == SYMBOL_PROC) && (sym->decl->flags & DECL_IS_FOREIGN)) {
        ftprint_char_array(&dstr, true, "%s wrt ..plt", sym->as_proc.foreign_name->str);
    }
    else if (sym->name == main_proc_ident) {
        ftprint_char_array(&dstr, true, "%s", sym->name->str);
    }
    else {
        assert(!sym->is_local);
        Module* mod = sym->home;
        ftprint_char_array(&dstr, true, "module%u_%s", mod->id, sym->name->str);
    }

    size_t len = array_len(dstr);

    for (size_t i = 0; i < len; i += 1) {
        if (dstr[i] == NIBBLE_PATH_SEP) {
            dstr[i] = '_';
        }
    }

    return dstr;
}

//////////////////////////////
//     Scope
//////////////////////////////

void scope_init(Scope* scope)
{
    memset(scope, 0, sizeof(Scope));

    list_head_init(&scope->children);
    list_head_init(&scope->sym_list);
    list_head_init(&scope->obj_list);
}

Scope* new_scope(Allocator* allocator, u32 num_syms)
{
    Scope* scope = alloc_type(allocator, Scope, true);

    list_head_init(&scope->children);
    list_head_init(&scope->sym_list);
    list_head_init(&scope->obj_list);

    if (num_syms) {
        size_t log2_cap = calc_hmap_size((size_t)num_syms);

        scope->sym_table = hmap(log2_cap, allocator);
    }

    return scope;
}

Symbol* lookup_scope_symbol(Scope* scope, Identifier* name)
{
    u64* pval = hmap_get(&scope->sym_table, PTR_UINT(name));

    return pval ? (void*)*pval : NULL;
}

Symbol* lookup_symbol(Scope* curr_scope, Identifier* name)
{
    for (Scope* scope = curr_scope; scope != NULL; scope = scope->parent) {
        Symbol* sym = lookup_scope_symbol(scope, name);

        if (sym)
            return sym;
    }

    return NULL;
}

void add_scope_symbol(Scope* scope, Identifier* name, Symbol* sym, bool add_list)
{
    hmap_put(&scope->sym_table, PTR_UINT(name), PTR_UINT(sym));

    if (add_list) {
        list_add_last(&scope->sym_list, &sym->lnode);
        scope->num_syms += 1;
    }
}

Symbol* add_unresolved_symbol(Allocator* allocator, Scope* scope, Module* mod, Decl* decl)
{
    Symbol* old_sym = lookup_symbol(scope, decl->name);

    if (old_sym) {
        return NULL; // Shadows a symbol in the current scope or a parent scope.
    }

    Symbol* sym = new_symbol_decl(allocator, decl, mod);
    sym->status = SYMBOL_STATUS_UNRESOLVED;
    sym->is_local = (scope != &mod->scope);

    add_scope_symbol(scope, sym->name, sym, true);

    return sym;
}

AnonObj* add_anon_obj(Allocator* allocator, List* objs, s32 id, size_t size, size_t align)
{
    AnonObj* obj = alloc_type(allocator, AnonObj, true);
    obj->size = size;
    obj->align = align;
    obj->id = id;

    list_add_last(objs, &obj->lnode);

    return obj;
}

static bool install_module_decl(Allocator* ast_mem, Module* mod, Decl* decl, ErrorStream* errors)
{
    Symbol* sym = add_unresolved_symbol(ast_mem, &mod->scope, mod, decl);

    if (!sym) {
        report_error(errors, decl->range, "Duplicate definition of symbol `%s`", decl->name->str);
        return false;
    }

    // Add to export table if decl is exported.
    if (decl->flags & DECL_IS_EXPORTED) {
        if (!module_add_export_sym(mod, sym->name, sym)) {
            report_error(errors, decl->range, "Conflicting export symbol name `%s`", sym->name->str);
            return false;
        }
    }

    return true;
}

bool install_module_decls(Allocator* ast_mem, Module* mod, ErrorStream* errors)
{
    List* head = &mod->stmts;

    // Install decls in global symbol table.
    for (List* it = head->next; it != head; it = it->next) {
        Stmt* stmt = list_entry(it, Stmt, lnode);

        if (stmt->kind == CST_StmtDecl) {
            Decl* decl = ((StmtDecl*)stmt)->decl;

            if (!install_module_decl(ast_mem, mod, decl, errors)) {
                return false;
            }
        }
    }

    return true;
}

bool module_add_global_sym(Module* mod, Identifier* name, Symbol* sym, ErrorStream* errors)
{
    Symbol* old_sym = lookup_scope_symbol(&mod->scope, name);

    if (sym == old_sym) {
        return true;
    }

    bool is_imported = (sym->home != mod);

    // If replacing an existing symbol AND the new symbol is imported OR the old symbol
    // is native to this module... check for errors.
    if (old_sym && (is_imported || (old_sym->home == mod))) {
        // TODO: Handle module symbols?

        ProgRange range = sym->decl ? sym->decl->range : mod->range;

        if (sym->home == mod) {
            report_error(errors, range, "Duplicate definition of symbol `%s`", name->str);
        }
        else {
            report_error(errors, range, "Conflicting import name `%s`", name->str);
        }

        return false;
    }

    assert(!sym->is_local);

    add_scope_symbol(&mod->scope, name, sym, !is_imported);

    return true;
}

bool import_all_mod_syms(Module* dst_mod, Module* src_mod, ErrorStream* errors)
{
    HMap* export_table = &src_mod->export_table;
    size_t cap = export_table->cap;

    // TODO: Iterating through all empty slots in the hash table is slow....
    for (size_t i = 0; i < cap; i += 1) {
        HMapEntry* entry = export_table->entries + i;

        if (entry->key != HASH_MAP_NULL_KEY) {
            Symbol* sym = UINT_PTR(entry->value, Symbol);

            if (!module_add_global_sym(dst_mod, sym->name, sym, errors)) {
                return false;
            }
        }
    }

    return true;
}

bool import_mod_syms(Module* dst_mod, Module* src_mod, StmtImport* stmt, ErrorStream* errors)
{
    List* head = &stmt->import_syms;
    List* it = head->next;

    while (it != head) {
        ImportSymbol* isym = list_entry(it, ImportSymbol, lnode);
        Symbol* sym = module_get_export_sym(src_mod, isym->name);

        if (!sym) {
            report_error(errors, stmt->super.range, "Importing unknown or private symbol `%s` from module `%s`", isym->name->str,
                         src_mod->abs_path->str);
            return false;
        }

        Identifier* name = isym->rename ? isym->rename : isym->name;

        if (!module_add_global_sym(dst_mod, name, sym, errors)) {
            return false;
        }

        it = it->next;
    }

    return true;
}

void module_init(Module* mod, u64 id, StrLit* abs_path)
{
    mod->id = id;
    mod->abs_path = abs_path;
    mod->num_decls = 0;

    memset(&mod->export_table, 0, sizeof(HMap));
    scope_init(&mod->scope);
    list_head_init(&mod->stmts);
    list_head_init(&mod->import_stmts);
    list_head_init(&mod->export_stmts);
}

void module_init_tables(Module* mod, Allocator* allocator, size_t num_builtins)
{
    size_t syms_cap = (mod->num_decls + mod->num_imports + num_builtins) << 1;
    size_t exports_cap = (mod->num_exports) << 1;

    if (syms_cap) {
        size_t log2_cap = calc_hmap_size(syms_cap);

        mod->scope.sym_table = hmap(log2_cap, allocator);
    }

    if (exports_cap) {
        size_t log2_cap = calc_hmap_size(exports_cap);

        mod->export_table = hmap(log2_cap, allocator);
    }
}

Symbol* module_get_export_sym(Module* mod, Identifier* name)
{
    u64* pval = hmap_get(&mod->export_table, PTR_UINT(name));
    return pval ? (void*)*pval : NULL;
}

bool module_add_export_sym(Module* mod, Identifier* name, Symbol* sym)
{
    Symbol* old_sym = module_get_export_sym(mod, name);

    if (old_sym) {
        return sym == old_sym;
    }

    hmap_put(&mod->export_table, PTR_UINT(name), PTR_UINT(sym));

    return true;
}
