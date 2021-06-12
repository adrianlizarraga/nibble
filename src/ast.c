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

TypeSpec* new_typespec_ident(Allocator* allocator, const char* name, ProgRange range)
{
    TypeSpecIdent* typespec = new_typespec(allocator, TypeSpecIdent, range);
    typespec->name = name;

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

TypeSpec* new_typespec_proc(Allocator* allocator, size_t num_params, List* params, TypeSpec* ret, ProgRange range)
{
    TypeSpecProc* typespec = new_typespec(allocator, TypeSpecProc, range);
    typespec->num_params = num_params;
    typespec->ret = ret;

    list_replace(params, &typespec->params);

    return (TypeSpec*)typespec;
}

ProcParam* new_proc_param(Allocator* allocator, const char* name, TypeSpec* typespec, ProgRange range)
{
    ProcParam* param = alloc_type(allocator, ProcParam, true);
    param->name = name;
    param->typespec = typespec;
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

Expr* new_expr_field(Allocator* allocator, Expr* object, const char* field, ProgRange range)
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

ProcCallArg* new_proc_call_arg(Allocator* allocator, Expr* expr, const char* name)
{
    ProcCallArg* arg = alloc_type(allocator, ProcCallArg, true);
    arg->expr = expr;
    arg->name = name;

    return arg;
}

Expr* new_expr_int(Allocator* allocator, uint64_t value, ProgRange range)
{
    ExprInt* expr = new_expr(allocator, ExprInt, range);
    expr->value = value;

    return (Expr*)expr;
}

Expr* new_expr_float(Allocator* allocator, FloatKind fkind, Float value, ProgRange range)
{
    ExprFloat* expr = new_expr(allocator, ExprFloat, range);
    expr->fkind = fkind;
    expr->value = value;

    return (Expr*)expr;
}

Expr* new_expr_str(Allocator* allocator, const char* value, ProgRange range)
{
    ExprStr* expr = new_expr(allocator, ExprStr, range);
    expr->value = value;

    return (Expr*)expr;
}

Expr* new_expr_ident(Allocator* allocator, const char* name, ProgRange range)
{
    ExprIdent* expr = new_expr(allocator, ExprIdent, range);
    expr->name = name;

    return (Expr*)expr;
}

Expr* new_expr_cast(Allocator* allocator, TypeSpec* typespec, Expr* arg, ProgRange range)
{
    ExprCast* expr = new_expr(allocator, ExprCast, range);
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

Expr* new_expr_typeof(Allocator* allocator, Expr* arg, ProgRange range)
{
    ExprTypeof* expr = new_expr(allocator, ExprTypeof, range);
    expr->expr = arg;

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

Expr* new_expr_compound_lit(Allocator* allocator, TypeSpec* typespec, size_t num_initzers, List* initzers,
                            ProgRange range)
{
    ExprCompoundLit* expr = new_expr(allocator, ExprCompoundLit, range);
    expr->typespec = typespec;
    expr->num_initzers = num_initzers;

    list_replace(initzers, &expr->initzers);

    return (Expr*)expr;
}

#define new_decl(a, k, r) (k*)new_decl_((a), sizeof(k), alignof(k), CST_##k, (r))
static Decl* new_decl_(Allocator* allocator, size_t size, size_t align, DeclKind kind, ProgRange range)
{
    Decl* decl = mem_allocate(allocator, size, align, true);
    decl->kind = kind;
    decl->range = range;

    return (Decl*)decl;
}

Decl* new_decl_var(Allocator* allocator, const char* name, TypeSpec* typespec, Expr* init, ProgRange range)
{
    DeclVar* decl = new_decl(allocator, DeclVar, range);
    decl->name = name;
    decl->typespec = typespec;
    decl->init = init;

    return (Decl*)decl;
}

Decl* new_decl_const(Allocator* allocator, const char* name, TypeSpec* typespec, Expr* init, ProgRange range)
{
    DeclConst* decl = new_decl(allocator, DeclConst, range);
    decl->name = name;
    decl->typespec = typespec;
    decl->init = init;

    return (Decl*)decl;
}

Decl* new_decl_typedef(Allocator* allocator, const char* name, TypeSpec* typespec, ProgRange range)
{
    DeclTypedef* decl = new_decl(allocator, DeclTypedef, range);
    decl->name = name;
    decl->typespec = typespec;

    return (Decl*)decl;
}

Decl* new_decl_enum(Allocator* allocator, const char* name, TypeSpec* typespec, List* items, ProgRange range)
{
    DeclEnum* decl = new_decl(allocator, DeclEnum, range);
    decl->name = name;
    decl->typespec = typespec;

    list_replace(items, &decl->items);

    return (Decl*)decl;
}

EnumItem* new_enum_item(Allocator* allocator, const char* name, Expr* value)
{
    EnumItem* item = alloc_type(allocator, EnumItem, true);
    item->name = name;
    item->value = value;

    return item;
}

Decl* new_decl_struct(Allocator* allocator, const char* name, List* fields, ProgRange range)
{
    DeclStruct* decl = new_decl(allocator, DeclStruct, range);
    decl->name = name;

    list_replace(fields, &decl->fields);

    return (Decl*)decl;
}

Decl* new_decl_union(Allocator* allocator, const char* name, List* fields, ProgRange range)
{
    DeclUnion* decl = new_decl(allocator, DeclUnion, range);
    decl->name = name;

    list_replace(fields, &decl->fields);

    return (Decl*)decl;
}

AggregateField* new_aggregate_field(Allocator* allocator, const char* name, TypeSpec* typespec, ProgRange range)
{
    AggregateField* field = alloc_type(allocator, AggregateField, true);
    field->name = name;
    field->typespec = typespec;
    field->range = range;

    return field;
}

Decl* new_decl_proc(Allocator* allocator, const char* name, size_t num_params, List* params, TypeSpec* ret, Stmt* body,
                    ProgRange range)
{
    DeclProc* decl = new_decl(allocator, DeclProc, range);
    decl->name = name;
    decl->ret = ret;
    decl->body = body;
    decl->num_params = num_params;

    list_replace(params, &decl->params);

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

Stmt* new_stmt_decl(Allocator* allocator, Decl* decl)
{
    StmtDecl* stmt = new_stmt(allocator, StmtDecl, decl->range);
    stmt->decl = decl;

    return (Stmt*)stmt;
}

Stmt* new_stmt_block(Allocator* allocator, List* stmts, size_t num_decls, ProgRange range)
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

static Type type_void_ = {.kind = TYPE_VOID};
static Type type_u8_ = {
    .kind = TYPE_INTEGER,
    .size = 1, .align = 1,
    .as_integer = {.kind = INTEGER_U8, .is_signed = false, .max = 0xFF}
};
static Type type_s8_ = {
    .kind = TYPE_INTEGER,
    .size = 1, .align = 1,
    .as_integer = {.kind = INTEGER_S8, .is_signed = true, .max = 0x7F}
};
static Type type_u16_ = {
    .kind = TYPE_INTEGER,
    .size = 2, .align = 2,
    .as_integer = {.kind = INTEGER_U16, .is_signed = false, .max = 0xFFFF}
};
static Type type_s16_ = {
    .kind = TYPE_INTEGER,
    .size = 2, .align = 2,
    .as_integer = {.kind = INTEGER_S16, .is_signed = true, .max = 0x7FFF}
};
static Type type_u32_ = {
    .kind = TYPE_INTEGER,
    .size = 4, .align = 4,
    .as_integer = {.kind = INTEGER_U32, .is_signed = false, .max = 0xFFFFFFFF}
};
static Type type_s32_ = {
    .kind = TYPE_INTEGER,
    .size = 4, .align = 4,
    .as_integer = {.kind = INTEGER_S32, .is_signed = true, .max = 0x7FFFFFFF}
};
static Type type_u64_ = {
    .kind = TYPE_INTEGER,
    .size = 8, .align = 8,
    .as_integer = {.kind = INTEGER_U64, .is_signed = false, .max = 0xFFFFFFFFFFFFFFFF}
};
static Type type_s64_ = {
    .kind = TYPE_INTEGER,
    .size = 8, .align = 8,
    .as_integer = {.kind = INTEGER_S64, .is_signed = true, .max = 0x7FFFFFFFFFFFFFFF}
};
static Type type_f32_ = {
    .kind = TYPE_FLOAT,
    .size = 4, .align = 4,
    .as_float.kind = FLOAT_F32
};
static Type type_f64_ = {
    .kind = TYPE_FLOAT,
    .size = 8, .align = 8,
    .as_float.kind = FLOAT_F64
};

Type* type_void = &type_void_;
Type* type_u8 = &type_u8_;
Type* type_s8 = &type_s8_;
Type* type_u16 = &type_u16_;
Type* type_s16 = &type_s16_;
Type* type_u32 = &type_u32_;
Type* type_s32 = &type_s32_;
Type* type_u64 = &type_u64_;
Type* type_s64 = &type_s64_;
Type* type_f32 = &type_f32_;
Type* type_f64 = &type_f64_;

// Aliases
Type* type_bool;
Type* type_char;
Type* type_schar;
Type* type_uchar;
Type* type_short;
Type* type_ushort;
Type* type_int;
Type* type_uint;
Type* type_long;
Type* type_ulong;
Type* type_llong;
Type* type_ullong;
Type* type_ssize;
Type* type_usize;

size_t PTR_SIZE = 8;
size_t PTR_ALIGN = 8;

static const char* type_names[] = {
    [TYPE_VOID] = "void",
    [TYPE_INTEGER] = "_integer_",
    [TYPE_FLOAT] = "_float_",
    [TYPE_ENUM] = "_enum_",
    [TYPE_PTR] = "_ptr_",
    [TYPE_PROC] = "_proc_",
    [TYPE_ARRAY] = "_array_",
    [TYPE_STRUCT] = "_struct_",
    [TYPE_UNION] = "_union_",
};

static const char* type_integer_names[] = {
    [INTEGER_U8] = "u8",
    [INTEGER_S8] = "s8",
    [INTEGER_U16] = "u16",
    [INTEGER_S16] = "s16",
    [INTEGER_U32] = "u32",
    [INTEGER_S32] = "s32",
    [INTEGER_U64] = "u64",
    [INTEGER_S64] = "s64",
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

    switch (type->kind)
    {
        case TYPE_INTEGER:
            return type_integer_names[type->as_integer.kind];
        case TYPE_FLOAT:
            return type_float_names[type->as_float.kind];
        default:
            return type_names[type->kind];
    }
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

static Type* type_alloc(Allocator* allocator, TypeKind kind)
{
    Type* type = alloc_type(allocator, Type, true);
    type->kind = kind;
    type->id = next_type_id++;

    return type;
}

Type* type_ptr(Allocator* allocator, HMap* type_ptr_cache, Type* base)
{
    uint64_t* pval = hmap_get(type_ptr_cache, PTR_UINT(base));
    Type* type = pval ? (void*)*pval : NULL;

    if (!type)
    {
        type = type_alloc(allocator, TYPE_PTR);
        type->size = PTR_SIZE;
        type->align = PTR_ALIGN;
        type->as_ptr.base = base;

        hmap_put(type_ptr_cache, PTR_UINT(base), PTR_UINT(type));
    }

    return type;
}

Type* type_decay(Allocator* allocator, HMap* type_ptr_cache, Type* type)
{
    if (type->kind == TYPE_ARRAY)
        return type_ptr(allocator, type_ptr_cache, type->as_array.base);

    return type;
}

typedef struct CachedType {
    Type* type;
    struct CachedType* next;
} CachedType;

Type* type_proc(Allocator* allocator, HMap* type_proc_cache, size_t num_params, Type** params, Type* ret)
{
    size_t params_size = num_params * sizeof(params[0]);
    uint64_t key = hash_mix_uint64(hash_bytes(params, params_size), hash_ptr(ret));
    uint64_t* pval = hmap_get(type_proc_cache, key);
    CachedType* cached = pval ? (void*)*pval : NULL;

    // Return cached type, if it exists.
    for (CachedType* it = cached; it != NULL; it = it->next)
    {
        Type* type = it->type;

        if ((type->as_proc.num_params == num_params) && (type->as_proc.ret == ret))
        {
            bool params_equal = true;

            for (size_t i = 0; i < num_params; i += 1)
            {
                if (type->as_proc.params[i] != params[i])
                {
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

    CachedType* new_cached = alloc_type(allocator, CachedType, true);
    new_cached->type = type;
    new_cached->next = cached;

    hmap_put(type_proc_cache, key, PTR_UINT(new_cached));

    return type;
}

void init_builtin_types(OS target_os, Arch target_arch)
{
    bool invalid_os_arch = false;

    type_void->id = next_type_id++;
    type_u8->id = next_type_id++;
    type_s8->id = next_type_id++;
    type_u16->id = next_type_id++;
    type_s16->id = next_type_id++;
    type_u32->id = next_type_id++;
    type_s32->id = next_type_id++;
    type_u64->id = next_type_id++;
    type_s64->id = next_type_id++;
    type_f32->id = next_type_id++;
    type_f64->id = next_type_id++;

    type_bool = type_s8;
    type_char = type_s8;
    type_schar = type_s8;
    type_uchar = type_u8;
    type_short = type_s16;
    type_ushort = type_u16;
    type_int = type_s32;
    type_uint = type_u32;
    type_llong = type_s64;
    type_ullong = type_u64;

    switch (target_os)
    {
        case OS_LINUX:
            switch (target_arch)
            {
                case ARCH_X86:
                    type_long = type_s32;
                    type_ulong = type_u32;

                    PTR_SIZE = 4;
                    PTR_ALIGN = 4;
                    break;
                case ARCH_X64:
                    type_long = type_s64;
                    type_ulong = type_u64;

                    PTR_SIZE = 8;
                    PTR_ALIGN = 8;
                    break;
                default:
                    invalid_os_arch = true;
                    break;
            }
            break;
        case OS_WIN32:
            switch (target_arch)
            {
                case ARCH_X86:
                    type_long = type_s32;
                    type_ulong = type_u32;

                    PTR_SIZE = 4;
                    PTR_ALIGN = 4;
                    break;
                case ARCH_X64:
                    type_long = type_s32;
                    type_ulong = type_u32;

                    PTR_SIZE = 8;
                    PTR_ALIGN = 8;
                    break;
                default:
                    invalid_os_arch = true;
                    break;
            }
            break;
        case OS_OSX:
            switch (target_arch)
            {
                case ARCH_X64:
                    type_long = type_s64;
                    type_ulong = type_u64;

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

    if (invalid_os_arch)
    {
        ftprint_err("Unsupported OS architecture: %s %s\n", os_names[target_os], arch_names[target_arch]);
        exit(1);
    }

    if (PTR_SIZE == 4)
    {
        type_ssize = type_int;
        type_usize = type_uint;

    }
    else
    {
        assert(PTR_SIZE == 8);
        type_ssize = type_llong;
        type_usize = type_ullong;
    }
}

//////////////////////////////
//     Symbols
//////////////////////////////

static Symbol* new_symbol(Allocator* allocator, SymbolKind kind, SymbolStatus status, const char* name)
{
    Symbol* sym = alloc_type(allocator, Symbol, true);

    sym->kind = kind;
    sym->status = status;
    sym->name = name;

    return sym;
}

Symbol* new_symbol_decl(Allocator* allocator, SymbolKind kind, const char* name, Decl* decl)
{
    Symbol* sym = new_symbol(allocator, kind, SYMBOL_STATUS_UNRESOLVED, name);
    sym->decl = decl;

    return sym;
}

Symbol* new_symbol_builtin_type(Allocator* allocator, const char* name, Type* type)
{
    Symbol* sym = new_symbol(allocator, SYMBOL_TYPE, SYMBOL_STATUS_RESOLVED, name);
    sym->type = type;

    return sym;
}

//////////////////////////////
//     Scope
//////////////////////////////

Scope* new_scope(Allocator* allocator, size_t num_syms)
{
    Scope* scope = alloc_type(allocator, Scope, true);

    init_scope_lists(scope);
    init_scope_sym_table(scope, allocator, num_syms);

    return scope;
}

void init_scope_sym_table(Scope* scope, Allocator* allocator, size_t num_syms)
{
    if (num_syms)
    {
        size_t log2_cap = calc_hmap_size(num_syms);

        scope->sym_table = hmap(log2_cap, allocator); // TODO: Should just be allocated by ast_mem arena.
    }
}

void init_scope_lists(Scope* scope)
{
    list_head_init(&scope->children);
    list_head_init(&scope->sym_list);
}

Symbol* lookup_scope_symbol(Scope* scope, const char* name)
{
    uint64_t* pval = hmap_get(&scope->sym_table, PTR_UINT(name));

    return pval ? (void*)*pval : NULL;
}

Symbol* lookup_symbol(Scope* curr_scope, const char* name)
{
    for (Scope* scope = curr_scope; scope != NULL; scope = scope->parent)
    {
        Symbol* sym = lookup_scope_symbol(scope, name);

        if (sym)
            return sym;
    }

    return NULL;
}

//////////////////////////////
//     CST Printing
//////////////////////////////

char* ftprint_typespec(Allocator* allocator, TypeSpec* typespec)
{
    char* dstr = NULL;

    if (typespec)
    {
        switch (typespec->kind)
        {
            case CST_TYPE_SPEC_NONE:
            {
                assert(0);
            }
            break;
            case CST_TypeSpecIdent:
            {
                TypeSpecIdent* t = (TypeSpecIdent*)typespec;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(:ident %s)", t->name);
            }
            break;
            case CST_TypeSpecProc:
            {
                TypeSpecProc* t = (TypeSpecProc*)typespec;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(:proc =>%s", ftprint_typespec(allocator, t->ret));

                size_t num_params = t->num_params;

                if (num_params)
                {
                    ftprint_char_array(&dstr, false, " ");

                    ListNode* head = &t->params;

                    for (ListNode* it = head->next; it != head; it = it->next)
                    {
                        ProcParam* param = list_entry(it, ProcParam, lnode);

                        if (param->name)
                            ftprint_char_array(&dstr, false, "(%s %s)", param->name,
                                               ftprint_typespec(allocator, param->typespec));
                        else
                            ftprint_char_array(&dstr, false, "%s", ftprint_typespec(allocator, param->typespec));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_TypeSpecStruct:
            case CST_TypeSpecUnion:
            {
                dstr = array_create(allocator, char, 32);
                bool is_struct = typespec->kind == CST_TypeSpecStruct;

                ftprint_char_array(&dstr, false, "(:%s", (is_struct ? "struct" : "union"));

                TypeSpecAggregate* aggregate = (TypeSpecAggregate*)typespec;

                if (!list_empty(&aggregate->fields))
                {
                    ftprint_char_array(&dstr, false, " ");

                    ListNode* head = &aggregate->fields;

                    for (ListNode* it = head->next; it != head; it = it->next)
                    {
                        AggregateField* field = list_entry(it, AggregateField, lnode);

                        ftprint_char_array(&dstr, false, "(%s %s)", field->name,
                                           ftprint_typespec(allocator, field->typespec));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_TypeSpecPtr:
            {
                TypeSpecPtr* t = (TypeSpecPtr*)typespec;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(:ptr %s)", ftprint_typespec(allocator, t->base));
            }
            break;
            case CST_TypeSpecConst:
            {
                TypeSpecConst* t = (TypeSpecConst*)typespec;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(:const %s)", ftprint_typespec(allocator, t->base));
            }
            break;
            case CST_TypeSpecArray:
            {
                TypeSpecArray* t = (TypeSpecArray*)typespec;
                dstr = array_create(allocator, char, 32);

                if (t->len)
                {
                    ftprint_char_array(&dstr, false, "(:arr %s %s)", ftprint_expr(allocator, t->len),
                                       ftprint_typespec(allocator, t->base));
                }
                else
                {
                    ftprint_char_array(&dstr, false, "(:arr %s)", ftprint_typespec(allocator, t->base));
                }
            }
            break;
            default:
            {
                ftprint_err("Unknown typespec kind: %d\n", typespec->kind);
                assert(0);
            }
            break;
        }
    }
    else
    {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_expr(Allocator* allocator, Expr* expr)
{
    char* dstr = NULL;

    if (expr)
    {
        switch (expr->kind)
        {
            case CST_EXPR_NONE:
            {
                assert(0);
            }
            break;
            case CST_ExprTernary:
            {
                ExprTernary* e = (ExprTernary*)expr;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(? %s %s %s)", ftprint_expr(allocator, e->cond),
                                   ftprint_expr(allocator, e->then_expr), ftprint_expr(allocator, e->else_expr));
            }
            break;
            case CST_ExprBinary:
            {
                ExprBinary* e = (ExprBinary*)expr;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(%s %s %s)", token_kind_names[e->op],
                                   ftprint_expr(allocator, e->left), ftprint_expr(allocator, e->right));
            }
            break;
            case CST_ExprUnary:
            {
                ExprUnary* e = (ExprUnary*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(%s %s)", token_kind_names[e->op], ftprint_expr(allocator, e->expr));
            }
            break;
            case CST_ExprCall:
            {
                ExprCall* e = (ExprCall*)expr;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(call %s", ftprint_expr(allocator, e->proc));

                size_t num_args = e->num_args;

                if (num_args)
                {
                    ftprint_char_array(&dstr, false, " ");

                    List* head = &e->args;

                    for (List* it = head->next; it != head; it = it->next)
                    {
                        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);

                        if (arg->name)
                            ftprint_char_array(&dstr, false, "%s=", arg->name);

                        ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, arg->expr));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_ExprIndex:
            {
                ExprIndex* e = (ExprIndex*)expr;
                dstr = array_create(allocator, char, 8);
                ftprint_char_array(&dstr, false, "(index %s %s)", ftprint_expr(allocator, e->array),
                                   ftprint_expr(allocator, e->index));
            }
            break;
            case CST_ExprField:
            {
                ExprField* e = (ExprField*)expr;
                dstr = array_create(allocator, char, 8);
                ftprint_char_array(&dstr, false, "(field %s %s)", ftprint_expr(allocator, e->object), e->field);
            }
            break;
            case CST_ExprInt:
            {
                ExprInt* e = (ExprInt*)expr;
                dstr = array_create(allocator, char, 8);
                ftprint_char_array(&dstr, false, "%lu", e->value);
            }
            break;
            case CST_ExprFloat:
            {
                ExprFloat* e = (ExprFloat*)expr;
                dstr = array_create(allocator, char, 8);

                if (e->fkind == FLOAT_F64)
                    ftprint_char_array(&dstr, false, "%lf", e->value._f64);
                else
                    ftprint_char_array(&dstr, false, "%lf", e->value._f32);
            }
            break;
            case CST_ExprStr:
            {
                ExprStr* e = (ExprStr*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "\"%s\"", e->value);
            }
            break;
            case CST_ExprIdent:
            {
                ExprIdent* e = (ExprIdent*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "%s", e->name);
            }
            break;
            case CST_ExprCast:
            {
                ExprCast* e = (ExprCast*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(cast %s %s)", ftprint_typespec(allocator, e->typespec),
                                   ftprint_expr(allocator, e->expr));
            }
            break;
            case CST_ExprSizeof:
            {
                ExprSizeof* e = (ExprSizeof*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(sizeof %s)", ftprint_typespec(allocator, e->typespec));
            }
            break;
            case CST_ExprTypeof:
            {
                ExprTypeof* e = (ExprTypeof*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(typeof %s)", ftprint_expr(allocator, e->expr));
            }
            break;
            case CST_ExprCompoundLit:
            {
                ExprCompoundLit* e = (ExprCompoundLit*)expr;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(compound ");

                if (e->typespec)
                    ftprint_char_array(&dstr, false, "%s ", ftprint_typespec(allocator, e->typespec));

                ftprint_char_array(&dstr, false, "{");

                if (e->num_initzers)
                {
                    List* head = &e->initzers;

                    for (List* it = head->next; it != head; it = it->next)
                    {
                        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);

                        if (initzer->designator.kind == DESIGNATOR_NAME)
                            ftprint_char_array(&dstr, false, "%s = ", initzer->designator.name);
                        else if (initzer->designator.kind == DESIGNATOR_INDEX)
                            ftprint_char_array(&dstr, false,
                                               "[%s] = ", ftprint_expr(allocator, initzer->designator.index));

                        ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, initzer->init));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                ftprint_char_array(&dstr, false, "})");
            }
            break;
            default:
            {
                ftprint_err("Unknown expr kind: %d\n", expr->kind);
                assert(0);
            }
            break;
        }
    }
    else
    {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

static char* ftprint_stmt_list(Allocator* allocator, List* stmts)
{
    char* dstr = NULL;

    if (!list_empty(stmts))
    {
        dstr = array_create(allocator, char, 32);
        List* head = stmts;

        for (List* it = head->next; it != head; it = it->next)
        {
            Stmt* s = list_entry(it, Stmt, lnode);

            ftprint_char_array(&dstr, false, "%s", ftprint_stmt(allocator, s));

            if (it->next != head)
                ftprint_char_array(&dstr, false, " ");
        }
    }
    else
    {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_stmt(Allocator* allocator, Stmt* stmt)
{
    char* dstr = NULL;

    if (stmt)
    {
        switch (stmt->kind)
        {
            case CST_STMT_NONE:
            {
                assert(0);
            }
            break;
            case CST_StmtNoOp:
            {
                dstr = array_create(allocator, char, 6);
                ftprint_char_array(&dstr, false, "no-op");
            }
            break;
            case CST_StmtDecl:
            {
                StmtDecl* s = (StmtDecl*)stmt;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, s->decl));
            }
            break;
            case CST_StmtBlock:
            {
                StmtBlock* s = (StmtBlock*)stmt;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(stmt-block");

                if (!list_empty(&s->stmts))
                    ftprint_char_array(&dstr, false, " %s)", ftprint_stmt_list(allocator, &s->stmts));
                else
                    ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_StmtExpr:
            {
                StmtExpr* s = (StmtExpr*)stmt;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, s->expr));
            }
            break;
            case CST_StmtExprAssign:
            {
                StmtExprAssign* s = (StmtExprAssign*)stmt;
                dstr = array_create(allocator, char, 32);
                const char* op = token_kind_names[s->op_assign];

                ftprint_char_array(&dstr, false, "(%s %s %s)", op, ftprint_expr(allocator, s->left),
                                   ftprint_expr(allocator, s->right));
            }
            break;
            case CST_StmtWhile:
            {
                StmtWhile* s = (StmtWhile*)stmt;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(while %s %s)", ftprint_expr(allocator, s->cond),
                                   ftprint_stmt(allocator, s->body));
            }
            break;
            case CST_StmtDoWhile:
            {
                StmtDoWhile* s = (StmtDoWhile*)stmt;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(do-while %s %s)", ftprint_expr(allocator, s->cond),
                                   ftprint_stmt(allocator, s->body));
            }
            break;
            case CST_StmtFor:
            {
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
            }
            break;
            case CST_StmtIf:
            {
                StmtIf* s = (StmtIf*)stmt;
                dstr = array_create(allocator, char, 64);

                ftprint_char_array(&dstr, false, "(if %s %s", ftprint_expr(allocator, s->if_blk.cond),
                                   ftprint_stmt(allocator, s->if_blk.body));

                if (s->else_blk.body)
                    ftprint_char_array(&dstr, false, " (else %s)", ftprint_stmt(allocator, s->else_blk.body));

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_StmtSwitch:
            {
                StmtSwitch* s = (StmtSwitch*)stmt;
                dstr = array_create(allocator, char, 64);

                ftprint_char_array(&dstr, false, "(switch %s ", ftprint_expr(allocator, s->expr));

                List* head = &s->cases;

                for (List* it = head->next; it != head; it = it->next)
                {
                    SwitchCase* swcase = list_entry(it, SwitchCase, lnode);

                    ftprint_char_array(&dstr, false, "(case");

                    if (swcase->start)
                    {
                        ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, swcase->start));

                        if (swcase->end)
                            ftprint_char_array(&dstr, false, "..%s", ftprint_expr(allocator, swcase->end));
                    }

                    if (!list_empty(&swcase->stmts))
                        ftprint_char_array(&dstr, false, " (stmt-list %s))",
                                           ftprint_stmt_list(allocator, &swcase->stmts));
                    else
                        ftprint_char_array(&dstr, false, " (stmt-list))");

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_StmtReturn:
            {
                StmtReturn* s = (StmtReturn*)stmt;
                dstr = array_create(allocator, char, 16);

                ftprint_char_array(&dstr, false, "(return");

                if (s->expr)
                {
                    ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, s->expr));
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_StmtBreak:
            {
                StmtBreak* s = (StmtBreak*)stmt;
                dstr = array_create(allocator, char, 16);

                ftprint_char_array(&dstr, false, "(break");

                if (s->label)
                {
                    ftprint_char_array(&dstr, false, " %s)", s->label);
                }
                else
                {
                    ftprint_char_array(&dstr, false, ")");
                }
            }
            break;
            case CST_StmtContinue:
            {
                StmtContinue* s = (StmtContinue*)stmt;
                dstr = array_create(allocator, char, 16);

                ftprint_char_array(&dstr, false, "(continue");

                if (s->label)
                {
                    ftprint_char_array(&dstr, false, " %s)", s->label);
                }
                else
                {
                    ftprint_char_array(&dstr, false, ")");
                }
            }
            break;
            case CST_StmtGoto:
            {
                StmtGoto* s = (StmtGoto*)stmt;
                dstr = array_create(allocator, char, 16);

                ftprint_char_array(&dstr, false, "(goto %s)", s->label);
            }
            break;
            case CST_StmtLabel:
            {
                StmtLabel* s = (StmtLabel*)stmt;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(label %s %s)", s->label, ftprint_stmt(allocator, s->target));
            }
            break;
            default:
            {
                ftprint_err("Unknown stmt kind: %d\n", stmt->kind);
                assert(0);
            }
            break;
        }
    }
    else
    {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_decl(Allocator* allocator, Decl* decl)
{
    char* dstr = NULL;

    if (decl)
    {
        switch (decl->kind)
        {
            case CST_DECL_NONE:
            {
                assert(0);
            }
            break;
            case CST_DeclVar:
            {
                DeclVar* d = (DeclVar*)decl;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(var %s", d->name);

                if (d->typespec)
                {
                    ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));
                }

                if (d->init)
                {
                    ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, d->init));
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_DeclConst:
            {
                DeclConst* d = (DeclConst*)decl;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(const %s", d->name);

                if (d->typespec)
                {
                    ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));
                }

                if (d->init)
                {
                    ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, d->init));
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_DeclTypedef:
            {
                DeclTypedef* d = (DeclTypedef*)decl;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(typedef %s %s)", d->name,
                                   ftprint_typespec(allocator, d->typespec));
            }
            break;
            case CST_DeclEnum:
            {
                DeclEnum* d = (DeclEnum*)decl;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(enum %s", d->name);

                if (d->typespec)
                    ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));

                if (!list_empty(&d->items))
                {
                    ftprint_char_array(&dstr, false, " ");

                    List* head = &d->items;

                    for (List* it = head->next; it != head; it = it->next)
                    {
                        EnumItem* item = list_entry(it, EnumItem, lnode);

                        ftprint_char_array(&dstr, false, "%s", item->name);

                        if (item->value)
                            ftprint_char_array(&dstr, false, "=%s", ftprint_expr(allocator, item->value));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_DeclStruct:
            case CST_DeclUnion:
            {
                DeclAggregate* d = (DeclAggregate*)decl;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(%s %s", (decl->kind == CST_DeclStruct ? "struct" : "union"),
                                   d->name);

                if (!list_empty(&d->fields))
                {
                    ftprint_char_array(&dstr, false, " ");

                    List* head = &d->fields;

                    for (List* it = head->next; it != head; it = it->next)
                    {
                        AggregateField* field = list_entry(it, AggregateField, lnode);

                        ftprint_char_array(&dstr, false, "(%s %s)", field->name,
                                           ftprint_typespec(allocator, field->typespec));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case CST_DeclProc:
            {
                DeclProc* proc = (DeclProc*)decl;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(proc %s (", proc->name);

                if (!list_empty(&proc->params))
                {
                    List* head = &proc->params;

                    for (List* it = head->next; it != head; it = it->next)
                    {
                        Decl* param = list_entry(it, Decl, lnode);

                        ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, param));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                ftprint_char_array(&dstr, false, ") =>%s %s)", ftprint_typespec(allocator, proc->ret),
                                   ftprint_stmt(allocator, proc->body));
            }
            break;
            default:
            {
                ftprint_err("Unknown decl kind: %d\n", decl->kind);
                assert(0);
            }
            break;
        }
    }
    else
    {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

char* ftprint_decls(Allocator* allocator, size_t num_decls, Decl** decls)
{
    assert(decls);

    char* dstr = array_create(allocator, char, 64);

    for (size_t i = 0; i < num_decls; i += 1)
    {
        ftprint_char_array(&dstr, false, "%s\n", ftprint_decl(allocator, decls[i]));
    }

    array_push(dstr, '\0');

    return dstr;
}
