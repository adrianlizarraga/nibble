#include "ast.h"
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

TypeSpec* new_typespec_ident(Allocator* allocator, Identifier* mod_ns, Identifier* name, ProgRange range)
{
    TypeSpecIdent* typespec = new_typespec(allocator, TypeSpecIdent, range);
    typespec->mod_ns = mod_ns;
    typespec->name = name;

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_typeof(Allocator* allocator, Expr* expr, ProgRange range)
{
    TypeSpecTypeof* typespec = new_typespec(allocator, TypeSpecTypeof, range);
    typespec->expr = expr;

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpecPtr* typespec = new_typespec(allocator, TypeSpecPtr, range);
    typespec->base = base;

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range)
{
    TypeSpecArray* typespec = new_typespec(allocator, TypeSpecArray, range);
    typespec->base = base;
    typespec->len = len;

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

Expr* new_expr_ident(Allocator* allocator, Identifier* mod_ns, Identifier* name, ProgRange range)
{
    ExprIdent* expr = new_expr(allocator, ExprIdent, range);
    expr->mod_ns = mod_ns;
    expr->name = name;

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

Expr* new_expr_sizeof(Allocator* allocator, TypeSpec* typespec, ProgRange range)
{
    ExprSizeof* expr = new_expr(allocator, ExprSizeof, range);
    expr->typespec = typespec;

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

DeclAnnotation* new_annotation(Allocator* allocator, Identifier* ident, ProgRange range)
{
    DeclAnnotation* annotation = alloc_type(allocator, DeclAnnotation, true);
    annotation->ident = ident;
    annotation->range = range;

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

Decl* new_decl_var(Allocator* allocator, Identifier* name, TypeSpec* typespec, Expr* init, bool is_variadic, ProgRange range)
{
    DeclVar* decl = new_decl(allocator, DeclVar, name, range);
    decl->typespec = typespec;
    decl->is_variadic = is_variadic;
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

EnumItem* new_enum_item(Allocator* allocator, Identifier* name, Expr* value, ProgRange range)
{
    EnumItem* item = alloc_type(allocator, EnumItem, true);
    item->range = range;
    item->name = name;
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

PortSymbol* new_port_symbol(Allocator* allocator, Identifier* name, Identifier* rename, ProgRange range)
{
    PortSymbol* psym = alloc_type(allocator, PortSymbol, true);
    psym->name = name;
    psym->rename = rename;
    psym->range = range;

    return psym;
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
            PortSymbol* isym = list_entry(it, PortSymbol, lnode);
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
    [TYPE_VOID] = "void",   [TYPE_INTEGER] = "_integer_", [TYPE_FLOAT] = "_float_",   [TYPE_ENUM] = "_enum_",   [TYPE_PTR] = "_ptr_",
    [TYPE_PROC] = "_proc_", [TYPE_ARRAY] = "_array_",     [TYPE_STRUCT] = "_struct_", [TYPE_UNION] = "_union_",
    [TYPE_INCOMPLETE_AGGREGATE] = "_incomplete_aggregate_",
};

static const char* type_integer_names[] = {
    [INTEGER_U8] = "u8",   [INTEGER_S8] = "s8",   [INTEGER_U16] = "u16", [INTEGER_S16] = "s16",
    [INTEGER_U32] = "u32", [INTEGER_S32] = "s32", [INTEGER_U64] = "u64", [INTEGER_S64] = "s64",
};

static const char* type_float_names[] = {
    [FLOAT_F64] = "f64",
    [FLOAT_F32] = "f32",
};

static size_t next_type_id = 1;

const char* type_name(Type* type)
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

bool type_is_integer_like(Type* type)
{
    TypeKind kind = type->kind;

    return (kind == TYPE_INTEGER) || (kind == TYPE_ENUM);
}

bool type_is_arithmetic(Type* type)
{
    TypeKind kind = type->kind;

    return (kind == TYPE_INTEGER) || (kind == TYPE_FLOAT) || (kind == TYPE_ENUM);
}

bool type_is_scalar(Type* type)
{
    TypeKind kind = type->kind;

    return type_is_arithmetic(type) || (kind == TYPE_PTR) || (kind == TYPE_PROC);
}

bool type_is_ptr_like(Type* type)
{
    TypeKind kind = type->kind;

    return (kind == TYPE_PTR) || (kind == TYPE_PROC);
}

bool type_is_aggregate(Type* type)
{
    TypeKind kind = type->kind;

    return (kind == TYPE_STRUCT) || (kind == TYPE_UNION);
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

Type* type_unsigned_int(Type* type_int)
{
    assert(type_int->kind == TYPE_INTEGER);

    switch (type_int->as_integer.kind) {
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
    }

    return NULL;
}

Type* try_array_decay(Allocator* allocator, HMap* type_ptr_cache, Type* type)
{
    if (type->kind == TYPE_ARRAY)
        return type_ptr(allocator, type_ptr_cache, type->as_array.base);

    return type;
}

// Recursively decay incomplete array types into pointers. Other types are returned unchanged.
// Examples:
//     []char => ^char
//     [][]char => ^^char
//     ^[]char => ^^char
//     ^^[]char => ^^^char
Type* try_incomplete_array_decay(Allocator* alloc, HMap* type_ptr_cache, Type* type)
{
    Type* result = type;

    if (type_is_incomplete_array(type)) {
        result = type_ptr(alloc, type_ptr_cache, try_incomplete_array_decay(alloc, type_ptr_cache, type->as_array.base));
    }
    else if (type->kind == TYPE_PTR) {
        result = type_ptr(alloc, type_ptr_cache, try_incomplete_array_decay(alloc, type_ptr_cache, type->as_ptr.base));
    }

    return result;
}

static Type* type_alloc(Allocator* allocator, TypeKind kind)
{
    Type* type = alloc_type(allocator, Type, true);
    type->kind = kind;
    type->id = next_type_id++;

    return type;
}

static Type* type_int_alloc(Allocator* allocator, IntegerKind kind, size_t size, bool is_signed, u64 max)
{
    Type* type = type_alloc(allocator, TYPE_INTEGER);
    type->size = size;
    type->align = size;
    type->as_integer.kind = kind;
    type->as_integer.is_signed = is_signed;
    type->as_integer.max = max;

    return type;
}

static Type* type_float_alloc(Allocator* allocator, FloatKind kind, size_t size)
{
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

TypeAggregateField* get_type_aggregate_field(Type* type, Identifier* name)
{
    assert(type->kind == TYPE_STRUCT || type->kind == TYPE_UNION);
    TypeAggregate* type_aggregate = &type->as_aggregate;
    size_t num_fields = type_aggregate->num_fields;
    TypeAggregateField* fields = type_aggregate->fields;

    for (size_t i = 0; i < num_fields; i += 1) {
        TypeAggregateField* field = fields + i;

        if (field->name == name) {
            return field;
        }
    }
    
    return NULL;
}

Type* type_variadic_struct(Allocator* allocator, HMap* type_variadic_cache, HMap* type_ptr_cache, Type* elem_type)
{
    uint64_t* pval = hmap_get(type_variadic_cache, PTR_UINT(elem_type));
    Type* type = pval ? (void*)*pval : NULL;

    if (!type) {
        type = type_alloc(allocator, TYPE_INCOMPLETE_AGGREGATE);

        TypeAggregateField fields[2] = {0};
        fields[0].type = builtin_types[BUILTIN_TYPE_USIZE].type;
        fields[0].name = builtin_struct_fields[BUILTIN_STRUCT_FIELD_SIZE];

        fields[1].type = type_ptr(allocator, type_ptr_cache, elem_type);
        fields[1].name = builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA];

        complete_struct_type(allocator, type, ARRAY_LEN(fields), fields);

        hmap_put(type_variadic_cache, PTR_UINT(elem_type), PTR_UINT(type));
    }

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

    type->as_aggregate.num_fields = num_fields;
    type->as_aggregate.fields = fields_cpy;
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

    type->as_aggregate.num_fields = num_fields;
    type->as_aggregate.fields = fields_cpy;
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

typedef struct CachedType {
    Type* type;
    struct CachedType* next;
} CachedType;

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
    size_t params_size = num_params * sizeof(params[0]);
    uint64_t key = hash_mix_uint64(hash_bytes(params, params_size), hash_ptr(ret));
    uint64_t* pval = hmap_get(type_proc_cache, key);
    CachedType* cached = pval ? (void*)*pval : NULL;

    // Return cached type if it exists.
    for (CachedType* it = cached; it != NULL; it = it->next) {
        Type* type = it->type;

        if ((type->as_proc.num_params == num_params) && (type->as_proc.ret == ret)) {
            bool params_equal = true;

            for (size_t i = 0; i < num_params; i += 1) {
                if (type->as_proc.params[i] != params[i]) {
                    params_equal = false;
                    break;
                }
            }

            if (params_equal)
                return it->type;
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
    builtin_types[BUILTIN_TYPE_U8] = (BuiltinType){.name = "u8", .type = type_int_alloc(ast_mem, INTEGER_U8, 1, false, 0xFF)};
    builtin_types[BUILTIN_TYPE_S8] = (BuiltinType){.name = "s8", .type = type_int_alloc(ast_mem, INTEGER_S8, 1, true, 0x7F)};
    builtin_types[BUILTIN_TYPE_U16] = (BuiltinType){.name = "u16", .type = type_int_alloc(ast_mem, INTEGER_U16, 2, false, 0xFFFF)};
    builtin_types[BUILTIN_TYPE_S16] = (BuiltinType){.name = "s16", .type = type_int_alloc(ast_mem, INTEGER_S16, 2, true, 0x7FFF)};
    builtin_types[BUILTIN_TYPE_U32] = (BuiltinType){.name = "u32", .type = type_int_alloc(ast_mem, INTEGER_U32, 4, false, 0xFFFFFFFF)};
    builtin_types[BUILTIN_TYPE_S32] = (BuiltinType){.name = "s32", .type = type_int_alloc(ast_mem, INTEGER_S32, 4, true, 0x7FFFFFFF)};
    builtin_types[BUILTIN_TYPE_U64] =
        (BuiltinType){.name = "u64", .type = type_int_alloc(ast_mem, INTEGER_U64, 8, false, 0xFFFFFFFFFFFFFFFF)};
    builtin_types[BUILTIN_TYPE_S64] =
        (BuiltinType){.name = "s64", .type = type_int_alloc(ast_mem, INTEGER_S64, 8, true, 0x7FFFFFFFFFFFFFFF)};
    builtin_types[BUILTIN_TYPE_F32] = (BuiltinType){.name = "f32", .type = type_float_alloc(ast_mem, FLOAT_F32, 4)};
    builtin_types[BUILTIN_TYPE_F64] = (BuiltinType){.name = "f64", .type = type_float_alloc(ast_mem, FLOAT_F64, 8)};

    builtin_types[BUILTIN_TYPE_BOOL] = (BuiltinType){.name = "bool", .type = builtin_types[BUILTIN_TYPE_S8].type};
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
        case ARCH_X86:
            builtin_types[BUILTIN_TYPE_LONG].type = builtin_types[BUILTIN_TYPE_S32].type;
            builtin_types[BUILTIN_TYPE_ULONG].type = builtin_types[BUILTIN_TYPE_U32].type;

            PTR_SIZE = 4;
            PTR_ALIGN = 4;
            break;
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
    case OS_WIN32:
        switch (target_arch) {
        case ARCH_X86:
            builtin_types[BUILTIN_TYPE_LONG].type = builtin_types[BUILTIN_TYPE_S32].type;
            builtin_types[BUILTIN_TYPE_ULONG].type = builtin_types[BUILTIN_TYPE_U32].type;

            PTR_SIZE = 4;
            PTR_ALIGN = 4;
            break;
        case ARCH_X64:
            builtin_types[BUILTIN_TYPE_LONG].type = builtin_types[BUILTIN_TYPE_S32].type;
            builtin_types[BUILTIN_TYPE_ULONG].type = builtin_types[BUILTIN_TYPE_U32].type;

            PTR_SIZE = 8;
            PTR_ALIGN = 8;
            break;
        default:
            invalid_os_arch = true;
            break;
        }
        break;
    case OS_OSX:
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
}

//////////////////////////////
//     Symbols
//////////////////////////////

const SymbolKind decl_sym_kind[CST_DECL_KIND_COUNT] = {
    [CST_DECL_NONE] = SYMBOL_NONE, [CST_DeclVar] = SYMBOL_VAR,     [CST_DeclConst] = SYMBOL_CONST, [CST_DeclEnum] = SYMBOL_TYPE,
    [CST_DeclUnion] = SYMBOL_TYPE, [CST_DeclStruct] = SYMBOL_TYPE, [CST_DeclProc] = SYMBOL_PROC,   [CST_DeclTypedef] = SYMBOL_TYPE,
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

char* symbol_mangled_name(Allocator* allocator, Symbol* sym)
{
    // TODO: Change to something stronger. Will require regenerating builtin code though.
    static const char intrin_pre[] = "_nibble";

    char* dstr = NULL;
    size_t len = sym->name->len;

    if (sym->name == main_proc_ident) {
        dstr = array_create(allocator, char, len + 1);
        ftprint_char_array(&dstr, true, "%s", sym->name->str);
    }
    else if (sym->name->kind == IDENTIFIER_INTRINSIC) {
        len += sizeof(intrin_pre); // Counts the +1 for the joining `_`
        dstr = array_create(allocator, char, len + 1); // TODO: Print to a fixed buffer

        ftprint_char_array(&dstr, true, "%s_%s", intrin_pre, sym->name->str);
    }
    else if (!sym->is_local) {
        len += sym->home->cpath_lit->len + 1;
        dstr = array_create(allocator, char, len + 1);

        ftprint_char_array(&dstr, true, "%s_%s", sym->home->cpath_lit->str, sym->name->str);
    }
    else {
        dstr = array_create(allocator, char, len + 1);
        ftprint_char_array(&dstr, true, "%s", sym->name->str);
    }

    for (size_t i = 0; i < len; i += 1) {
        if (dstr[i] == NIBBLE_PATH_SEP || dstr[i] == '.') {
            dstr[i] = '_';
        }
    }

    assert(len + 1 == array_len(dstr));

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

static bool install_module_decl(Allocator* allocator, Module* mod, Decl* decl)
{
    Symbol* sym = add_unresolved_symbol(allocator, &mod->scope, mod, decl);

    if (!sym) {
        report_error(decl->range, "Duplicate definition of symbol `%s`", decl->name->str);
        return false;
    }

    // Add to export table if decl is exported.
    if (decl->flags & DECL_IS_EXPORTED) {
        if (!module_add_export_sym(mod, sym->name, sym)) {
            report_error(decl->range, "Conflicting export symbol name `%s`", sym->name->str);
            return false;
        }
    }

    // If this is an enum, create const decls for each enum item.
    if (decl->kind == CST_DeclEnum) {
        DeclEnum* decl_enum = (DeclEnum*)decl;

        TypeSpec* enum_item_typespec = new_typespec_ident(allocator, NULL, decl->name, decl->range); // TODO: Range is wrong

        List* head = &decl_enum->items;
        List* it = head->next;
        EnumItem* prev_enum_item = NULL;

        while (it != head) {
            EnumItem* enum_item = list_entry(it, EnumItem, lnode);
            Expr* enum_item_val;

            // TODO: This range is wrong! Consider using a custom expr_enum_inc that does not require dummy ranges.
            ProgRange dummy_range = enum_item->range;

            // Use the explicit enum item initialization value.
            if (enum_item->value) {
                enum_item_val = enum_item->value;
            }
            // Add one to the previous enum item value.
            else if (prev_enum_item) {
                Expr* prev_enum_val = new_expr_ident(allocator, NULL, prev_enum_item->name, dummy_range);
                TokenInt token_one = {.value = 1, .rep = TKN_INT_DEC, .suffix = TKN_INT_SUFFIX_NONE};
                Expr* expr_one = new_expr_int(allocator, token_one, dummy_range);

                enum_item_val = new_expr_binary(allocator, TKN_PLUS, prev_enum_val, expr_one);
            }
            // Initialize to zero.
            else {
                TokenInt token_zero = {.value = 0, .rep = TKN_INT_DEC, .suffix = TKN_INT_SUFFIX_NONE};
                enum_item_val = new_expr_int(allocator, token_zero, dummy_range);
            }

            Decl* enum_item_const = new_decl_const(allocator, enum_item->name, enum_item_typespec, enum_item_val, enum_item->range);

            if (!install_module_decl(allocator, mod, enum_item_const)) {
                return false;
            }

            // Track the previous enum item so that we know what value to assign the next enum item.
            prev_enum_item = enum_item;

            it = it->next;
        }
    }

    return true;
}

bool install_module_decls(Allocator* allocator, Module* mod)
{
    List* head = &mod->stmts;

    // Install decls in global symbol table.
    for (List* it = head->next; it != head; it = it->next) {
        Stmt* stmt = list_entry(it, Stmt, lnode);

        if (stmt->kind == CST_StmtDecl) {
            Decl* decl = ((StmtDecl*)stmt)->decl;

            if (!install_module_decl(allocator, mod, decl)) {
                return false;
            }
        }
    }

    return true;
}

bool module_add_global_sym(Module* mod, Identifier* name, Symbol* sym)
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
            report_error(range, "Duplicate definition of symbol `%s`", name->str);
        }
        else {
            report_error(range, "Conflicting import name `%s`", name->str);
        }

        return false;
    }

    assert(!sym->is_local);

    add_scope_symbol(&mod->scope, name, sym, !is_imported);

    return true;
}

bool import_all_mod_syms(Module* dst_mod, Module* src_mod)
{
    HMap* export_table = &src_mod->export_table;
    size_t cap = export_table->cap;

    // TODO: Iterating through all empty slots in the hash table is slow....
    for (size_t i = 0; i < cap; i += 1) {
        HMapEntry* entry = export_table->entries + i;

        if (entry->key != HASH_MAP_NULL_KEY) {
            Symbol* sym = UINT_PTR(entry->value, Symbol);

            if (!module_add_global_sym(dst_mod, sym->name, sym)) {
                return false;
            }
        }
    }

    return true;
}

bool import_mod_syms(Module* dst_mod, Module* src_mod, StmtImport* stmt)
{
    List* head = &stmt->import_syms;
    List* it = head->next;

    while (it != head) {
        PortSymbol* isym = list_entry(it, PortSymbol, lnode);
        Symbol* sym = module_get_export_sym(src_mod, isym->name);

        if (!sym) {
            report_error(stmt->super.range, "Importing unknown or private symbol `%s` from module `%s`", isym->name->str,
                         src_mod->cpath_lit->str); // TODO: mod_path is not an OS path
            return false;
        }

        Identifier* name = isym->rename ? isym->rename : isym->name;

        if (!module_add_global_sym(dst_mod, name, sym)) {
            return false;
        }

        it = it->next;
    }

    return true;
}

void module_init(Module* mod, StrLit* cpath_lit)
{
    mod->cpath_lit = cpath_lit;
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

//////////////////////////////
//     CST Printing
//////////////////////////////

char* ftprint_typespec(Allocator* allocator, TypeSpec* typespec)
{
    char* dstr = NULL;

    if (typespec) {
        switch (typespec->kind) {
        case CST_TYPE_SPEC_NONE: {
            assert(0);
        } break;
        case CST_TypeSpecIdent: {
            TypeSpecIdent* t = (TypeSpecIdent*)typespec;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(:ident ");

            if (t->mod_ns) {
                ftprint_char_array(&dstr, false, "%s::", t->mod_ns->str);
            }

            ftprint_char_array(&dstr, false, "%s", t->name->str);
        } break;
        case CST_TypeSpecTypeof: {
            TypeSpecTypeof* t = (TypeSpecTypeof*)typespec;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(:typeof %s)", ftprint_expr(allocator, t->expr));
        } break;
        case CST_TypeSpecProc: {
            TypeSpecProc* t = (TypeSpecProc*)typespec;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:proc =>%s", ftprint_typespec(allocator, t->ret));

            size_t num_params = t->num_params;

            if (num_params) {
                ftprint_char_array(&dstr, false, " ");

                ListNode* head = &t->params;

                for (ListNode* it = head->next; it != head; it = it->next) {
                    ProcParam* param = list_entry(it, ProcParam, lnode);

                    if (param->name)
                        ftprint_char_array(&dstr, false, "(%s %s)", param->name->str, ftprint_typespec(allocator, param->typespec));
                    else
                        ftprint_char_array(&dstr, false, "%s", ftprint_typespec(allocator, param->typespec));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_TypeSpecStruct:
        case CST_TypeSpecUnion: {
            dstr = array_create(allocator, char, 32);
            bool is_struct = typespec->kind == CST_TypeSpecStruct;

            ftprint_char_array(&dstr, false, "(:%s", (is_struct ? "struct" : "union"));

            TypeSpecAggregate* aggregate = (TypeSpecAggregate*)typespec;

            if (!list_empty(&aggregate->fields)) {
                ftprint_char_array(&dstr, false, " ");

                ListNode* head = &aggregate->fields;

                for (ListNode* it = head->next; it != head; it = it->next) {
                    AggregateField* field = list_entry(it, AggregateField, lnode);

                    ftprint_char_array(&dstr, false, "(%s %s)", field->name->str, ftprint_typespec(allocator, field->typespec));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_TypeSpecPtr: {
            TypeSpecPtr* t = (TypeSpecPtr*)typespec;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:ptr %s)", ftprint_typespec(allocator, t->base));
        } break;
        case CST_TypeSpecConst: {
            TypeSpecConst* t = (TypeSpecConst*)typespec;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:const %s)", ftprint_typespec(allocator, t->base));
        } break;
        case CST_TypeSpecArray: {
            TypeSpecArray* t = (TypeSpecArray*)typespec;
            dstr = array_create(allocator, char, 32);

            if (t->len) {
                ftprint_char_array(&dstr, false, "(:arr %s %s)", ftprint_expr(allocator, t->len),
                                   ftprint_typespec(allocator, t->base));
            }
            else {
                ftprint_char_array(&dstr, false, "(:arr %s)", ftprint_typespec(allocator, t->base));
            }
        } break;
        default: {
            ftprint_err("Unknown typespec kind: %d\n", typespec->kind);
            assert(0);
        } break;
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_expr(Allocator* allocator, Expr* expr)
{
    char* dstr = NULL;

    if (expr) {
        switch (expr->kind) {
        case CST_EXPR_NONE: {
            assert(0);
        } break;
        case CST_ExprTernary: {
            ExprTernary* e = (ExprTernary*)expr;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(? %s %s %s)", ftprint_expr(allocator, e->cond), ftprint_expr(allocator, e->then_expr),
                               ftprint_expr(allocator, e->else_expr));
        } break;
        case CST_ExprBinary: {
            ExprBinary* e = (ExprBinary*)expr;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(%s %s %s)", token_kind_names[e->op], ftprint_expr(allocator, e->left),
                               ftprint_expr(allocator, e->right));
        } break;
        case CST_ExprUnary: {
            ExprUnary* e = (ExprUnary*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(%s %s)", token_kind_names[e->op], ftprint_expr(allocator, e->expr));
        } break;
        case CST_ExprCall: {
            ExprCall* e = (ExprCall*)expr;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(call %s", ftprint_expr(allocator, e->proc));

            size_t num_args = e->num_args;

            if (num_args) {
                ftprint_char_array(&dstr, false, " ");

                List* head = &e->args;

                for (List* it = head->next; it != head; it = it->next) {
                    ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);

                    if (arg->name)
                        ftprint_char_array(&dstr, false, "%s=", arg->name->str);

                    ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, arg->expr));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_ExprIndex: {
            ExprIndex* e = (ExprIndex*)expr;
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "(index %s %s)", ftprint_expr(allocator, e->array), ftprint_expr(allocator, e->index));
        } break;
        case CST_ExprField: {
            ExprField* e = (ExprField*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(field %s %s)", ftprint_expr(allocator, e->object), e->field->str);
        } break;
        case CST_ExprInt: {
            ExprInt* e = (ExprInt*)expr;
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "%lu", e->token.value);
        } break;
        case CST_ExprFloat: {
            ExprFloat* e = (ExprFloat*)expr;
            dstr = array_create(allocator, char, 8);

            if (e->fkind == FLOAT_F64)
                ftprint_char_array(&dstr, false, "%lf", e->value._f64);
            else
                ftprint_char_array(&dstr, false, "%lf", e->value._f32);
        } break;
        case CST_ExprStr: {
            ExprStr* e = (ExprStr*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "\"%s\"", e->str_lit->str);
        } break;
        case CST_ExprIdent: {
            ExprIdent* e = (ExprIdent*)expr;
            dstr = array_create(allocator, char, 16);

            if (e->mod_ns) {
                ftprint_char_array(&dstr, false, "%s::", e->mod_ns->str);
            }

            ftprint_char_array(&dstr, false, "%s", e->name->str);
        } break;
        case CST_ExprCast: {
            ExprCast* e = (ExprCast*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(cast %s %s)", ftprint_typespec(allocator, e->typespec),
                               ftprint_expr(allocator, e->expr));
        } break;
        case CST_ExprSizeof: {
            ExprSizeof* e = (ExprSizeof*)expr;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(sizeof %s)", ftprint_typespec(allocator, e->typespec));
        } break;
        case CST_ExprCompoundLit: {
            ExprCompoundLit* e = (ExprCompoundLit*)expr;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(compound ");

            if (e->typespec)
                ftprint_char_array(&dstr, false, "%s ", ftprint_typespec(allocator, e->typespec));

            ftprint_char_array(&dstr, false, "{");

            if (e->num_initzers) {
                List* head = &e->initzers;

                for (List* it = head->next; it != head; it = it->next) {
                    MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);

                    if (initzer->designator.kind == DESIGNATOR_NAME)
                        ftprint_char_array(&dstr, false, "%s = ", initzer->designator.name);
                    else if (initzer->designator.kind == DESIGNATOR_INDEX)
                        ftprint_char_array(&dstr, false, "[%s] = ", ftprint_expr(allocator, initzer->designator.index));

                    ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, initzer->init));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, "})");
        } break;
        default: {
            ftprint_err("Unknown expr kind: %d\n", expr->kind);
            assert(0);
        } break;
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

static char* ftprint_stmt_list(Allocator* allocator, List* stmts)
{
    char* dstr = NULL;

    if (!list_empty(stmts)) {
        dstr = array_create(allocator, char, 32);
        List* head = stmts;

        for (List* it = head->next; it != head; it = it->next) {
            Stmt* s = list_entry(it, Stmt, lnode);

            ftprint_char_array(&dstr, false, "%s", ftprint_stmt(allocator, s));

            if (it->next != head)
                ftprint_char_array(&dstr, false, " ");
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_stmt(Allocator* allocator, Stmt* stmt)
{
    char* dstr = NULL;

    if (stmt) {
        switch (stmt->kind) {
        case CST_STMT_NONE: {
            assert(0);
        } break;
        case CST_StmtNoOp: {
            dstr = array_create(allocator, char, 6);
            ftprint_char_array(&dstr, false, "no-op");
        } break;
        case CST_StmtDecl: {
            StmtDecl* s = (StmtDecl*)stmt;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, s->decl));
        } break;
        case CST_StmtBlock: {
            StmtBlock* s = (StmtBlock*)stmt;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(stmt-block");

            if (!list_empty(&s->stmts))
                ftprint_char_array(&dstr, false, " %s)", ftprint_stmt_list(allocator, &s->stmts));
            else
                ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_StmtExpr: {
            StmtExpr* s = (StmtExpr*)stmt;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, s->expr));
        } break;
        case CST_StmtExprAssign: {
            StmtExprAssign* s = (StmtExprAssign*)stmt;
            dstr = array_create(allocator, char, 32);
            const char* op = token_kind_names[s->op_assign];

            ftprint_char_array(&dstr, false, "(%s %s %s)", op, ftprint_expr(allocator, s->left), ftprint_expr(allocator, s->right));
        } break;
        case CST_StmtWhile: {
            StmtWhile* s = (StmtWhile*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(while %s %s)", ftprint_expr(allocator, s->cond), ftprint_stmt(allocator, s->body));
        } break;
        case CST_StmtDoWhile: {
            StmtDoWhile* s = (StmtDoWhile*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(do-while %s %s)", ftprint_expr(allocator, s->cond), ftprint_stmt(allocator, s->body));
        } break;
        case CST_StmtFor: {
            StmtFor* s = (StmtFor*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(for ");

            if (s->init)
                ftprint_char_array(&dstr, false, "%s; ", ftprint_stmt(allocator, s->init));
            else
                ftprint_char_array(&dstr, false, "; ");

            if (s->cond)
                ftprint_char_array(&dstr, false, "%s; ", ftprint_expr(allocator, s->cond));
            else
                ftprint_char_array(&dstr, false, "; ");

            if (s->next)
                ftprint_char_array(&dstr, false, "%s ", ftprint_stmt(allocator, s->next));
            else
                ftprint_char_array(&dstr, false, " ");

            ftprint_char_array(&dstr, false, "%s)", ftprint_stmt(allocator, s->body));
        } break;
        case CST_StmtIf: {
            StmtIf* s = (StmtIf*)stmt;
            dstr = array_create(allocator, char, 64);

            ftprint_char_array(&dstr, false, "(if %s %s", ftprint_expr(allocator, s->if_blk.cond),
                               ftprint_stmt(allocator, s->if_blk.body));

            if (s->else_blk.body)
                ftprint_char_array(&dstr, false, " (else %s)", ftprint_stmt(allocator, s->else_blk.body));

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_StmtSwitch: {
            StmtSwitch* s = (StmtSwitch*)stmt;
            dstr = array_create(allocator, char, 64);

            ftprint_char_array(&dstr, false, "(switch %s ", ftprint_expr(allocator, s->expr));

            List* head = &s->cases;

            for (List* it = head->next; it != head; it = it->next) {
                SwitchCase* swcase = list_entry(it, SwitchCase, lnode);

                ftprint_char_array(&dstr, false, "(case");

                if (swcase->start) {
                    ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, swcase->start));

                    if (swcase->end)
                        ftprint_char_array(&dstr, false, "..%s", ftprint_expr(allocator, swcase->end));
                }

                if (!list_empty(&swcase->stmts))
                    ftprint_char_array(&dstr, false, " (stmt-list %s))", ftprint_stmt_list(allocator, &swcase->stmts));
                else
                    ftprint_char_array(&dstr, false, " (stmt-list))");

                if (it->next != head)
                    ftprint_char_array(&dstr, false, " ");
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_StmtReturn: {
            StmtReturn* s = (StmtReturn*)stmt;
            dstr = array_create(allocator, char, 16);

            ftprint_char_array(&dstr, false, "(return");

            if (s->expr) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, s->expr));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_StmtBreak: {
            StmtBreak* s = (StmtBreak*)stmt;
            dstr = array_create(allocator, char, 16);

            ftprint_char_array(&dstr, false, "(break");

            if (s->label) {
                ftprint_char_array(&dstr, false, " %s)", s->label);
            }
            else {
                ftprint_char_array(&dstr, false, ")");
            }
        } break;
        case CST_StmtContinue: {
            StmtContinue* s = (StmtContinue*)stmt;
            dstr = array_create(allocator, char, 16);

            ftprint_char_array(&dstr, false, "(continue");

            if (s->label) {
                ftprint_char_array(&dstr, false, " %s)", s->label);
            }
            else {
                ftprint_char_array(&dstr, false, ")");
            }
        } break;
        case CST_StmtGoto: {
            StmtGoto* s = (StmtGoto*)stmt;
            dstr = array_create(allocator, char, 16);

            ftprint_char_array(&dstr, false, "(goto %s)", s->label);
        } break;
        case CST_StmtLabel: {
            StmtLabel* s = (StmtLabel*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(label %s %s)", s->label, ftprint_stmt(allocator, s->target));
        } break;
        case CST_StmtStaticAssert: {
            StmtStaticAssert* s = (StmtStaticAssert*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(#static_assert %s", ftprint_expr(allocator, s->cond));

            if (s->msg) {
                ftprint_char_array(&dstr, false, " %s)", s->msg->str);
            }
            else {
                ftprint_char_array(&dstr, false, ")");
            }
        } break;
        case CST_StmtInclude: {
            StmtInclude* s = (StmtInclude*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(include \"%s\")", s->file_pathname->str);
        } break;
        case CST_StmtImport: {
            StmtImport* s = (StmtImport*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(import ");

            // Print imported syms
            if (!list_empty(&s->import_syms)) {
                ftprint_char_array(&dstr, false, "{");

                List* head = &s->import_syms;
                List* it = head->next;

                while (it != head) {
                    PortSymbol* entity = list_entry(it, PortSymbol, lnode);
                    const char* suffix = (it->next == head) ? "} from " : ", ";

                    ftprint_char_array(&dstr, false, "%s%s", entity->name->str, suffix);
                    it = it->next;
                }
            }

            ftprint_char_array(&dstr, false, "\"%s\"", s->mod_pathname->str);

            if (s->mod_namespace) {
                ftprint_char_array(&dstr, false, " as %s)", s->mod_namespace->str);
            }
            else {
                ftprint_char_array(&dstr, false, ")");
            }

        } break;
        case CST_StmtExport: {
            StmtExport* s = (StmtExport*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(export ");

            // Print exported syms
            if (!list_empty(&s->export_syms)) {
                ftprint_char_array(&dstr, false, "{");

                List* head = &s->export_syms;
                List* it = head->next;

                while (it != head) {
                    PortSymbol* entity = list_entry(it, PortSymbol, lnode);
                    const char* suffix = (it->next == head) ? "}" : ", ";

                    ftprint_char_array(&dstr, false, "%s%s", entity->name->str, suffix);
                    it = it->next;
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        default: {
            ftprint_err("Unknown stmt kind: %d\n", stmt->kind);
            assert(0);
        } break;
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_decl(Allocator* allocator, Decl* decl)
{
    char* dstr = NULL;

    if (decl) {
        switch (decl->kind) {
        case CST_DECL_NONE: {
            assert(0);
        } break;
        case CST_DeclVar: {
            DeclVar* d = (DeclVar*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(var %s", decl->name->str);

            if (d->typespec) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));
            }

            if (d->init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, d->init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_DeclConst: {
            DeclConst* d = (DeclConst*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(const %s", decl->name->str);

            if (d->typespec) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));
            }

            if (d->init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, d->init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_DeclTypedef: {
            DeclTypedef* d = (DeclTypedef*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(typedef %s %s)", decl->name->str, ftprint_typespec(allocator, d->typespec));
        } break;
        case CST_DeclEnum: {
            DeclEnum* d = (DeclEnum*)decl;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(enum %s", decl->name->str);

            if (d->typespec)
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));

            if (!list_empty(&d->items)) {
                ftprint_char_array(&dstr, false, " ");

                List* head = &d->items;

                for (List* it = head->next; it != head; it = it->next) {
                    EnumItem* item = list_entry(it, EnumItem, lnode);

                    ftprint_char_array(&dstr, false, "%s", item->name->str);

                    if (item->value)
                        ftprint_char_array(&dstr, false, "=%s", ftprint_expr(allocator, item->value));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_DeclStruct:
        case CST_DeclUnion: {
            DeclAggregate* d = (DeclAggregate*)decl;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(%s %s", (decl->kind == CST_DeclStruct ? "struct" : "union"), decl->name->str);

            if (!list_empty(&d->fields)) {
                ftprint_char_array(&dstr, false, " ");

                List* head = &d->fields;

                for (List* it = head->next; it != head; it = it->next) {
                    AggregateField* field = list_entry(it, AggregateField, lnode);

                    ftprint_char_array(&dstr, false, "(%s %s)", field->name->str, ftprint_typespec(allocator, field->typespec));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case CST_DeclProc: {
            DeclProc* proc = (DeclProc*)decl;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(proc %s (", decl->name->str);

            if (!list_empty(&proc->params)) {
                List* head = &proc->params;

                for (List* it = head->next; it != head; it = it->next) {
                    Decl* param = list_entry(it, Decl, lnode);

                    ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, param));

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }
            }

            ftprint_char_array(&dstr, false, ") =>%s ", ftprint_typespec(allocator, proc->ret));

            if (list_empty(&proc->stmts))
                ftprint_char_array(&dstr, false, "(stmt-block))");
            else
                ftprint_char_array(&dstr, false, "(stmt-block %s))", ftprint_stmt_list(allocator, &proc->stmts));
        } break;
        default: {
            ftprint_err("Unknown decl kind: %d\n", decl->kind);
            assert(0);
        } break;
        }
    }
    else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_decls(Allocator* allocator, size_t num_decls, Decl** decls)
{
    assert(decls);

    char* dstr = array_create(allocator, char, 64);

    for (size_t i = 0; i < num_decls; i += 1) {
        ftprint_char_array(&dstr, false, "%s\n", ftprint_decl(allocator, decls[i]));
    }

    array_push(dstr, '\0');

    return dstr;
}
