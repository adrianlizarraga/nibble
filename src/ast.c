#include "ast.h"
#include "array.h"
#include "cstring.h"

#define new_typespec(a, k, r) (k*)new_typespec_((a), sizeof(k), alignof(k), AST_##k, (r))
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

TypeSpec* new_typespec_struct(Allocator* allocator, size_t num_fields, List* fields, ProgRange range)
{
    TypeSpecStruct* typespec = new_typespec(allocator, TypeSpecStruct, range);
    typespec->num_fields = num_fields;

    list_replace(fields, &typespec->fields);

    return (TypeSpec*)typespec;
}

TypeSpec* new_typespec_union(Allocator* allocator, size_t num_fields, List* fields, ProgRange range)
{
    TypeSpecUnion* typespec = new_typespec(allocator, TypeSpecUnion, range);
    typespec->num_fields = num_fields;

    list_replace(fields, &typespec->fields);

    return (TypeSpec*)typespec;
}

#define new_expr(a, k, r) (k*)new_expr_((a), sizeof(k), alignof(k), AST_##k, (r))
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

Expr* new_expr_float(Allocator* allocator, Float value, ProgRange range)
{
    ExprFloat* expr = new_expr(allocator, ExprFloat, range);
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

#define new_decl(a, k, n, r) (k*)new_decl_((a), sizeof(k), alignof(k), AST_##k, (n), (r))
static Decl* new_decl_(Allocator* allocator, size_t size, size_t align, DeclKind kind, const char* name,
                       ProgRange range)
{
    Decl* decl = mem_allocate(allocator, size, align, true);
    decl->kind = kind;
    decl->name = name;
    decl->range = range;

    return (Decl*)decl;
}

Decl* new_decl_var(Allocator* allocator, const char* name, TypeSpec* typespec, Expr* init, ProgRange range)
{
    DeclVar* decl = new_decl(allocator, DeclVar, name, range);
    decl->typespec = typespec;
    decl->init = init;

    return (Decl*)decl;
}

Decl* new_decl_const(Allocator* allocator, const char* name, TypeSpec* typespec, Expr* init, ProgRange range)
{
    DeclConst* decl = new_decl(allocator, DeclConst, name, range);
    decl->typespec = typespec;
    decl->init = init;

    return (Decl*)decl;
}

Decl* new_decl_typedef(Allocator* allocator, const char* name, TypeSpec* typespec, ProgRange range)
{
    DeclTypedef* decl = new_decl(allocator, DeclTypedef, name, range);
    decl->typespec = typespec;

    return (Decl*)decl;
}

Decl* new_decl_enum(Allocator* allocator, const char* name, TypeSpec* typespec, size_t num_items, List* items,
                    ProgRange range)
{
    DeclEnum* decl = new_decl(allocator, DeclEnum, name, range);
    decl->typespec = typespec;
    decl->num_items = num_items;

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

Decl* new_decl_struct(Allocator* allocator, const char* name, size_t num_fields, List* fields, ProgRange range)
{
    DeclStruct* decl = new_decl(allocator, DeclStruct, name, range);
    decl->num_fields = num_fields;

    list_replace(fields, &decl->fields);

    return (Decl*)decl;
}

Decl* new_decl_union(Allocator* allocator, const char* name, size_t num_fields, List* fields, ProgRange range)
{
    DeclUnion* decl = new_decl(allocator, DeclUnion, name, range);
    decl->num_fields = num_fields;

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

Decl* new_decl_proc(Allocator* allocator, const char* name, Scope* param_scope, TypeSpec* ret, Stmt* body,
                    ProgRange range)
{
    DeclProc* decl = new_decl(allocator, DeclProc, name, range);
    decl->param_scope = param_scope;
    decl->ret = ret;
    decl->body = body;

    return (Decl*)decl;
}

#define new_stmt(a, k, r) (k*)new_stmt_((a), sizeof(k), alignof(k), AST_##k, (r))
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

Scope* new_scope(Allocator* allocator, Scope* parent)
{
    Scope* scope = alloc_type(allocator, Scope, true);
    scope->parent = parent;

    list_head_init(&scope->decls);
    list_head_init(&scope->children);

    return scope;
}

Stmt* new_stmt_block(Allocator* allocator, size_t num_stmts, List* stmts, Scope* scope, ProgRange range)
{
    StmtBlock* stmt = new_stmt(allocator, StmtBlock, range);
    stmt->scope = scope;
    stmt->num_stmts = num_stmts;

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

Stmt* new_stmt_if(Allocator* allocator, IfCondBlock* if_blk, size_t num_elif_blks, List* elif_blks, ElseBlock* else_blk,
                  ProgRange range)
{
    StmtIf* stmt = new_stmt(allocator, StmtIf, range);

    stmt->if_blk.range = if_blk->range;
    stmt->if_blk.cond = if_blk->cond;
    stmt->if_blk.body = if_blk->body;

    stmt->num_elif_blks = num_elif_blks;
    list_replace(elif_blks, &stmt->elif_blks);

    stmt->else_blk.range = else_blk->range;
    stmt->else_blk.body = else_blk->body;

    return (Stmt*)stmt;
}

IfCondBlock* new_if_cond_block(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range)
{
    IfCondBlock* cblock = alloc_type(allocator, IfCondBlock, true);
    cblock->range = range;
    cblock->cond = cond;
    cblock->body = body;

    return cblock;
}

Stmt* new_stmt_for(Allocator* allocator, Scope* scope, Stmt* init, Expr* cond, Stmt* next, Stmt* body, ProgRange range)
{
    StmtFor* stmt = new_stmt(allocator, StmtFor, range);
    stmt->scope = scope;
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

SwitchCase* new_switch_case(Allocator* allocator, Expr* start, Expr* end, size_t num_stmts, List* stmts,
                            ProgRange range)
{
    SwitchCase* swcase = alloc_type(allocator, SwitchCase, true);
    swcase->start = start;
    swcase->end = end;
    swcase->range = range;
    swcase->num_stmts = num_stmts;

    list_replace(stmts, &swcase->stmts);

    return swcase;
}

Stmt* new_stmt_switch(Allocator* allocator, Expr* expr, size_t num_cases, List* cases, ProgRange range)
{
    StmtSwitch* stmt = new_stmt(allocator, StmtSwitch, range);
    stmt->expr = expr;
    stmt->num_cases = num_cases;

    list_replace(cases, &stmt->cases);

    return (Stmt*)stmt;
}

char* ftprint_typespec(Allocator* allocator, TypeSpec* typespec)
{
    char* dstr = NULL;

    if (typespec)
    {
        switch (typespec->kind)
        {
            case AST_TYPE_SPEC_NONE:
            {
                assert(0);
            }
            break;
            case AST_TypeSpecIdent:
            {
                TypeSpecIdent* t = (TypeSpecIdent*)typespec;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(:ident %s)", t->name);
            }
            break;
            case AST_TypeSpecProc:
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
            case AST_TypeSpecStruct:
            case AST_TypeSpecUnion:
            {
                dstr = array_create(allocator, char, 32);
                bool is_struct = typespec->kind == AST_TypeSpecStruct;

                ftprint_char_array(&dstr, false, "(:%s", (is_struct ? "struct" : "union"));

                TypeSpecAggregate* aggregate = (TypeSpecAggregate*)typespec;
                size_t num_fields = aggregate->num_fields;

                if (num_fields)
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
            case AST_TypeSpecPtr:
            {
                TypeSpecPtr* t = (TypeSpecPtr*)typespec;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(:ptr %s)", ftprint_typespec(allocator, t->base));
            }
            break;
            case AST_TypeSpecConst:
            {
                TypeSpecConst* t = (TypeSpecConst*)typespec;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(:const %s)", ftprint_typespec(allocator, t->base));
            }
            break;
            case AST_TypeSpecArray:
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
            case AST_EXPR_NONE:
            {
                assert(0);
            }
            break;
            case AST_ExprTernary:
            {
                ExprTernary* e = (ExprTernary*)expr;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(? %s %s %s)", ftprint_expr(allocator, e->cond),
                                   ftprint_expr(allocator, e->then_expr), ftprint_expr(allocator, e->else_expr));
            }
            break;
            case AST_ExprBinary:
            {
                ExprBinary* e = (ExprBinary*)expr;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(%s %s %s)", token_kind_names[e->op],
                                   ftprint_expr(allocator, e->left), ftprint_expr(allocator, e->right));
            }
            break;
            case AST_ExprUnary:
            {
                ExprUnary* e = (ExprUnary*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(%s %s)", token_kind_names[e->op], ftprint_expr(allocator, e->expr));
            }
            break;
            case AST_ExprCall:
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
            case AST_ExprIndex:
            {
                ExprIndex* e = (ExprIndex*)expr;
                dstr = array_create(allocator, char, 8);
                ftprint_char_array(&dstr, false, "(index %s %s)", ftprint_expr(allocator, e->array),
                                   ftprint_expr(allocator, e->index));
            }
            break;
            case AST_ExprField:
            {
                ExprField* e = (ExprField*)expr;
                dstr = array_create(allocator, char, 8);
                ftprint_char_array(&dstr, false, "(field %s %s)", ftprint_expr(allocator, e->object), e->field);
            }
            break;
            case AST_ExprInt:
            {
                ExprInt* e = (ExprInt*)expr;
                dstr = array_create(allocator, char, 8);
                ftprint_char_array(&dstr, false, "%lu", e->value);
            }
            break;
            case AST_ExprFloat:
            {
                ExprFloat* e = (ExprFloat*)expr;
                dstr = array_create(allocator, char, 8);

                if (e->value.kind == FLOAT_F64)
                    ftprint_char_array(&dstr, false, "%lf", e->value.f64);
                else
                    ftprint_char_array(&dstr, false, "%lf", e->value.f32);
            }
            break;
            case AST_ExprStr:
            {
                ExprStr* e = (ExprStr*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "\"%s\"", e->value);
            }
            break;
            case AST_ExprIdent:
            {
                ExprIdent* e = (ExprIdent*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "%s", e->name);
            }
            break;
            case AST_ExprCast:
            {
                ExprCast* e = (ExprCast*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(cast %s %s)", ftprint_typespec(allocator, e->typespec),
                                   ftprint_expr(allocator, e->expr));
            }
            break;
            case AST_ExprSizeof:
            {
                ExprSizeof* e = (ExprSizeof*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(sizeof %s)", ftprint_typespec(allocator, e->typespec));
            }
            break;
            case AST_ExprTypeof:
            {
                ExprTypeof* e = (ExprTypeof*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(typeof %s)", ftprint_expr(allocator, e->expr));
            }
            break;
            case AST_ExprCompoundLit:
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

static char* ftprint_stmt_list(Allocator* allocator, size_t num_stmts, List* stmts)
{
    char* dstr = NULL;

    if (num_stmts)
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
            case AST_STMT_NONE:
            {
                assert(0);
            }
            break;
            case AST_StmtNoOp:
            {
                dstr = array_create(allocator, char, 6);
                ftprint_char_array(&dstr, false, "no-op");
            }
            break;
            case AST_StmtBlock:
            {
                StmtBlock* s = (StmtBlock*)stmt;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(stmt-block");

                // Print declarations before statements.
                if (s->scope->num_decls)
                {
                    ftprint_char_array(&dstr, false, " ");

                    List* head = &s->scope->decls;

                    for (List* it = head->next; it != head; it = it->next)
                    {
                        Decl* decl = list_entry(it, Decl, lnode);

                        ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, decl));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                // Print statements.
                if (s->num_stmts)
                    ftprint_char_array(&dstr, false, " %s)", ftprint_stmt_list(allocator, s->num_stmts, &s->stmts));
                else
                    ftprint_char_array(&dstr, false, ")");
            }
            break;
            case AST_StmtExpr:
            {
                StmtExpr* s = (StmtExpr*)stmt;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, s->expr));
            }
            break;
            case AST_StmtExprAssign:
            {
                StmtExprAssign* s = (StmtExprAssign*)stmt;
                dstr = array_create(allocator, char, 32);
                const char* op = token_kind_names[s->op_assign];

                ftprint_char_array(&dstr, false, "(%s %s %s)", op, ftprint_expr(allocator, s->left),
                                   ftprint_expr(allocator, s->right));
            }
            break;
            case AST_StmtWhile:
            {
                StmtWhile* s = (StmtWhile*)stmt;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(while %s %s)", ftprint_expr(allocator, s->cond),
                                   ftprint_stmt(allocator, s->body));
            }
            break;
            case AST_StmtDoWhile:
            {
                StmtDoWhile* s = (StmtDoWhile*)stmt;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(do-while %s %s)", ftprint_expr(allocator, s->cond),
                                   ftprint_stmt(allocator, s->body));
            }
            break;
            case AST_StmtFor:
            {
                StmtFor* s = (StmtFor*)stmt;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(for ");

                if (s->scope->num_decls)
                {
                    Decl* init_decl = list_entry(s->scope->decls.next, Decl, lnode);

                    ftprint_char_array(&dstr, false, "%s; ", ftprint_decl(allocator, init_decl));
                }
                else if (s->init)
                {
                    ftprint_char_array(&dstr, false, "%s; ", ftprint_stmt(allocator, s->init));
                }
                else
                {
                    ftprint_char_array(&dstr, false, "; ");
                }

                if (s->cond)
                {
                    ftprint_char_array(&dstr, false, "%s; ", ftprint_expr(allocator, s->cond));
                }
                else
                {
                    ftprint_char_array(&dstr, false, "; ");
                }

                if (s->next)
                {
                    ftprint_char_array(&dstr, false, "%s ", ftprint_stmt(allocator, s->next));
                }
                else
                {
                    ftprint_char_array(&dstr, false, " ");
                }

                ftprint_char_array(&dstr, false, "%s)", ftprint_stmt(allocator, s->body));
            }
            break;
            case AST_StmtIf:
            {
                StmtIf* s = (StmtIf*)stmt;
                dstr = array_create(allocator, char, 64);

                ftprint_char_array(&dstr, false, "(if %s %s", ftprint_expr(allocator, s->if_blk.cond),
                                   ftprint_stmt(allocator, s->if_blk.body));

                size_t num_elif_blks = s->num_elif_blks;

                if (num_elif_blks)
                {
                    ftprint_char_array(&dstr, false, " ");

                    List* head = &s->elif_blks;

                    for (List* it = head->next; it != head; it = it->next)
                    {
                        IfCondBlock* elif = list_entry(it, IfCondBlock, lnode);

                        ftprint_char_array(&dstr, false, "(elif %s %s)", ftprint_expr(allocator, elif->cond),
                                           ftprint_stmt(allocator, elif->body));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }

                if (s->else_blk.body)
                    ftprint_char_array(&dstr, false, " (else %s)", ftprint_stmt(allocator, s->else_blk.body));

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case AST_StmtSwitch:
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

                    if (swcase->num_stmts)
                        ftprint_char_array(&dstr, false, " (stmt-list %s))",
                                           ftprint_stmt_list(allocator, swcase->num_stmts, &swcase->stmts));
                    else
                        ftprint_char_array(&dstr, false, " (stmt-list))");

                    if (it->next != head)
                        ftprint_char_array(&dstr, false, " ");
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case AST_StmtReturn:
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
            case AST_StmtBreak:
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
            case AST_StmtContinue:
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
            case AST_StmtGoto:
            {
                StmtGoto* s = (StmtGoto*)stmt;
                dstr = array_create(allocator, char, 16);

                ftprint_char_array(&dstr, false, "(goto %s)", s->label);
            }
            break;
            case AST_StmtLabel:
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
            case AST_DECL_NONE:
            {
                assert(0);
            }
            break;
            case AST_DeclVar:
            {
                DeclVar* d = (DeclVar*)decl;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(var %s", d->super.name);

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
            case AST_DeclConst:
            {
                DeclConst* d = (DeclConst*)decl;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(const %s", d->super.name);

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
            case AST_DeclTypedef:
            {
                DeclTypedef* d = (DeclTypedef*)decl;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(typedef %s %s)", d->super.name,
                                   ftprint_typespec(allocator, d->typespec));
            }
            break;
            case AST_DeclEnum:
            {
                DeclEnum* d = (DeclEnum*)decl;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(enum %s", d->super.name);

                if (d->typespec)
                    ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->typespec));

                if (d->num_items)
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
            case AST_DeclStruct:
            case AST_DeclUnion:
            {
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(%s %s", (decl->kind == AST_DeclStruct ? "struct" : "union"),
                                   decl->name);

                DeclAggregate* d = (DeclAggregate*)decl;

                if (d->num_fields)
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
            case AST_DeclProc:
            {
                DeclProc* proc = (DeclProc*)decl;
                dstr = array_create(allocator, char, 32);

                ftprint_char_array(&dstr, false, "(proc %s (", decl->name);

                if (proc->param_scope->num_decls)
                {
                    List* head = &proc->param_scope->decls;

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
