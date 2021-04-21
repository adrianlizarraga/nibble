#include "ast.h"
#include "array.h"
#include "cstring.h"

#define typespec_alloc(a, k, r) (k*)typespec_alloc_((a), sizeof(k), alignof(k), AST_##k, (r))
static TypeSpec* typespec_alloc_(Allocator* allocator, size_t size, size_t align, TypeSpecKind kind, ProgRange range)
{
    TypeSpec* type = mem_allocate(allocator, size, align, true);
    type->kind = kind;
    type->range = range;

    return type;
}

TypeSpec* typespec_ident(Allocator* allocator, const char* name, ProgRange range)
{
    TypeSpecIdent* type = typespec_alloc(allocator, TypeSpecIdent, range);
    type->name = name;

    return (TypeSpec*)type;
}

TypeSpec* typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpecPtr* type = typespec_alloc(allocator, TypeSpecPtr, range);
    type->base = base;

    return (TypeSpec*)type;
}

TypeSpec* typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range)
{
    TypeSpecArray* type = typespec_alloc(allocator, TypeSpecArray, range);
    type->base = base;
    type->len = len;

    return (TypeSpec*)type;
}

TypeSpec* typespec_const(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpecConst* type = typespec_alloc(allocator, TypeSpecConst, range);
    type->base = base;

    return (TypeSpec*)type;
}

TypeSpec* typespec_proc(Allocator* allocator, size_t num_params, DLList* params, TypeSpec* ret, ProgRange range)
{
    TypeSpecProc* type = typespec_alloc(allocator, TypeSpecProc, range);
    type->num_params = num_params;
    type->ret = ret;

    dllist_replace(params, &type->params);

    return (TypeSpec*)type;
}

ProcParam* proc_param(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    ProcParam* param = new_type(allocator, ProcParam, true);
    param->name = name;
    param->type = type;
    param->range = range;

    return param;
}

TypeSpec* typespec_struct(Allocator* allocator, size_t num_fields, DLList* fields, ProgRange range)
{
    TypeSpecStruct* type = typespec_alloc(allocator, TypeSpecStruct, range);
    type->num_fields = num_fields;

    dllist_replace(fields, &type->fields);

    return (TypeSpec*)type;
}

TypeSpec* typespec_union(Allocator* allocator, size_t num_fields, DLList* fields, ProgRange range)
{
    TypeSpecUnion* type = typespec_alloc(allocator, TypeSpecUnion, range);
    type->num_fields = num_fields;

    dllist_replace(fields, &type->fields);

    return (TypeSpec*)type;
}

#define expr_alloc(a, k, r) (k*)expr_alloc_((a), sizeof(k), alignof(k), AST_##k, (r))
static Expr* expr_alloc_(Allocator* allocator, size_t size, size_t align, ExprKind kind, ProgRange range)
{
    Expr* expr = mem_allocate(allocator, size, align, true);
    expr->kind = kind;
    expr->range = range;

    return expr;
}

Expr* expr_ternary(Allocator* allocator, Expr* cond, Expr* then_expr, Expr* else_expr)
{
    ProgRange range = {.start = cond->range.start, .end = else_expr->range.end};
    ExprTernary* expr = expr_alloc(allocator, ExprTernary, range);
    expr->cond = cond;
    expr->then_expr = then_expr;
    expr->else_expr = else_expr;

    return (Expr*)expr;
}

Expr* expr_binary(Allocator* allocator, TokenKind op, Expr* left, Expr* right)
{
    ProgRange range = {.start = left->range.start, .end = right->range.end};
    ExprBinary* expr = expr_alloc(allocator, ExprBinary, range);
    expr->op = op;
    expr->left = left;
    expr->right = right;

    return (Expr*)expr;
}

Expr* expr_unary(Allocator* allocator, TokenKind op, Expr* unary_expr, ProgRange range)
{
    ExprUnary* expr = expr_alloc(allocator, ExprUnary, range);
    expr->op = op;
    expr->expr = unary_expr;

    return (Expr*)expr;
}

Expr* expr_field(Allocator* allocator, Expr* object, const char* field, ProgRange range)
{
    ExprField* expr = expr_alloc(allocator, ExprField, range);
    expr->object = object;
    expr->field = field;

    return (Expr*)expr;
}

Expr* expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range)
{
    ExprIndex* expr = expr_alloc(allocator, ExprIndex, range);
    expr->array = array;
    expr->index = index;

    return (Expr*)expr;
}

Expr* expr_call(Allocator* allocator, Expr* proc, size_t num_args, DLList* args, ProgRange range)
{
    ExprCall* expr = expr_alloc(allocator, ExprCall, range);
    expr->proc = proc;
    expr->num_args = num_args;

    dllist_replace(args, &expr->args);

    return (Expr*)expr;
}

ProcCallArg* proc_call_arg(Allocator* allocator, Expr* expr, const char* name)
{
    ProcCallArg* arg = new_type(allocator, ProcCallArg, true);
    arg->expr = expr;
    arg->name = name;

    return arg;
}

Expr* expr_int(Allocator* allocator, uint64_t value, ProgRange range)
{
    ExprInt* expr = expr_alloc(allocator, ExprInt, range);
    expr->value = value;

    return (Expr*)expr;
}

Expr* expr_float(Allocator* allocator, Float value, FloatKind fkind, ProgRange range)
{
    ExprFloat* expr = expr_alloc(allocator, ExprFloat, range);
    expr->value = value;
    expr->fkind = fkind;

    return (Expr*)expr;
}

Expr* expr_str(Allocator* allocator, const char* value, ProgRange range)
{
    ExprStr* expr = expr_alloc(allocator, ExprStr, range);
    expr->value = value;

    return (Expr*)expr;
}

Expr* expr_ident(Allocator* allocator, const char* name, ProgRange range)
{
    ExprIdent* expr = expr_alloc(allocator, ExprIdent, range);
    expr->name = name;

    return (Expr*)expr;
}

Expr* expr_cast(Allocator* allocator, TypeSpec* type, Expr* arg, ProgRange range)
{
    ExprCast* expr = expr_alloc(allocator, ExprCast, range);
    expr->type = type;
    expr->expr = arg;

    return (Expr*)expr;
}

Expr* expr_sizeof(Allocator* allocator, TypeSpec* type, ProgRange range)
{
    ExprSizeof* expr = expr_alloc(allocator, ExprSizeof, range);
    expr->type = type;

    return (Expr*)expr;
}

Expr* expr_typeof(Allocator* allocator, Expr* arg, ProgRange range)
{
    ExprTypeof* expr = expr_alloc(allocator, ExprTypeof, range);
    expr->expr = arg;

    return (Expr*)expr;
}

MemberInitializer* member_initializer(Allocator* allocator, Expr* init, Designator designator, ProgRange range)
{
    MemberInitializer* initzer = new_type(allocator, MemberInitializer, true);
    initzer->range = range;
    initzer->init = init;
    initzer->designator = designator;

    return initzer;
}

Expr* expr_compound_lit(Allocator* allocator, TypeSpec* type, size_t num_initzers, DLList* initzers, ProgRange range)
{
    ExprCompoundLit* expr = expr_alloc(allocator, ExprCompoundLit, range);
    expr->type = type;
    expr->num_initzers = num_initzers;

    dllist_replace(initzers, &expr->initzers);

    return (Expr*)expr;
}

#define decl_alloc(a, k, n, r) (k*)decl_alloc_((a), sizeof(k), alignof(k), AST_##k, (n), (r))
static Decl* decl_alloc_(Allocator* allocator, size_t size, size_t align, DeclKind kind, const char* name,
                         ProgRange range)
{
    Decl* decl = mem_allocate(allocator, size, align, true);
    decl->kind = kind;
    decl->name = name;
    decl->range = range;

    return (Decl*)decl;
}

Decl* decl_var(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range)
{
    DeclVar* decl = decl_alloc(allocator, DeclVar, name, range);
    decl->type = type;
    decl->init = init;

    return (Decl*)decl;
}

Decl* decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range)
{
    DeclConst* decl = decl_alloc(allocator, DeclConst, name, range);
    decl->type = type;
    decl->init = init;

    return (Decl*)decl;
}

Decl* decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    DeclTypedef* decl = decl_alloc(allocator, DeclTypedef, name, range);
    decl->type = type;

    return (Decl*)decl;
}

Decl* decl_enum(Allocator* allocator, const char* name, TypeSpec* type, size_t num_items, DLList* items,
                ProgRange range)
{
    DeclEnum* decl = decl_alloc(allocator, DeclEnum, name, range);
    decl->type = type;
    decl->num_items = num_items;

    dllist_replace(items, &decl->items);

    return (Decl*)decl;
}

EnumItem* enum_item(Allocator* allocator, const char* name, Expr* value)
{
    EnumItem* item = new_type(allocator, EnumItem, true);
    item->name = name;
    item->value = value;

    return item;
}

Decl* decl_struct(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range)
{
    DeclStruct* decl = decl_alloc(allocator, DeclStruct, name, range);
    decl->num_fields = num_fields;

    dllist_replace(fields, &decl->fields);

    return (Decl*)decl;
}

Decl* decl_union(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range)
{
    DeclUnion* decl = decl_alloc(allocator, DeclUnion, name, range);
    decl->num_fields = num_fields;

    dllist_replace(fields, &decl->fields);

    return (Decl*)decl;
}

AggregateField* aggregate_field(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    AggregateField* field = new_type(allocator, AggregateField, true);
    field->name = name;
    field->type = type;
    field->range = range;

    return field;
}

Decl* decl_proc(Allocator* allocator, const char* name, size_t num_params, DLList* params, TypeSpec* ret,
                size_t num_stmts, DLList* stmts, ProgRange range)
{
    DeclProc* decl = decl_alloc(allocator, DeclProc, name, range);
    decl->num_params = num_params;
    decl->ret = ret;
    decl->num_stmts = num_stmts;

    dllist_replace(params, &decl->params);
    dllist_replace(stmts, &decl->stmts);

    return (Decl*)decl;
}

#define stmt_alloc(a, k, r) (k*)stmt_alloc_((a), sizeof(k), alignof(k), AST_##k, (r))
static Stmt* stmt_alloc_(Allocator* allocator, size_t size, size_t align, StmtKind kind, ProgRange range)
{
    Stmt* stmt = mem_allocate(allocator, size, align, true);
    stmt->kind = kind;
    stmt->range = range;

    return stmt;
}

Stmt* stmt_noop(Allocator* allocator, ProgRange range)
{
    StmtNoOp* stmt = stmt_alloc(allocator, StmtNoOp, range);

    return (Stmt*)stmt;
}

Stmt* stmt_block(Allocator* allocator, size_t num_stmts, DLList* stmts, ProgRange range)
{
    StmtBlock* stmt = stmt_alloc(allocator, StmtBlock, range);
    stmt->num_stmts = num_stmts;

    dllist_replace(stmts, &stmt->stmts);

    return (Stmt*)stmt;
}

Stmt* stmt_decl(Allocator* allocator, Decl* decl)
{
    StmtDecl* stmt = stmt_alloc(allocator, StmtDecl, decl->range);
    stmt->decl = decl;

    return (Stmt*)stmt;
}

Stmt* stmt_expr(Allocator* allocator, Expr* expr, ProgRange range)
{
    StmtExpr* stmt = stmt_alloc(allocator, StmtExpr, range);
    stmt->expr = expr;

    return (Stmt*)stmt;
}

Stmt* stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range)
{
    StmtExprAssign* stmt = stmt_alloc(allocator, StmtExprAssign, range);
    stmt->left = lexpr;
    stmt->op_assign = op_assign;
    stmt->right = rexpr;

    return (Stmt*)stmt;
}

Stmt* stmt_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range)
{
    StmtWhile* stmt = stmt_alloc(allocator, StmtWhile, range);
    stmt->cond = cond;
    stmt->body = body;

    return (Stmt*)stmt;
}

Stmt* stmt_do_while(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range)
{
    StmtDoWhile* stmt = stmt_alloc(allocator, StmtDoWhile, range);
    stmt->cond = cond;
    stmt->body = body;

    return (Stmt*)stmt;
}

Stmt* stmt_if(Allocator* allocator, IfCondBlock* if_blk, size_t num_elif_blks, DLList* elif_blks, ElseBlock* else_blk,
              ProgRange range)
{
    StmtIf* stmt = stmt_alloc(allocator, StmtIf, range);

    stmt->if_blk.range = if_blk->range;
    stmt->if_blk.cond = if_blk->cond;
    stmt->if_blk.body = if_blk->body;

    stmt->num_elif_blks = num_elif_blks;
    dllist_replace(elif_blks, &stmt->elif_blks);

    stmt->else_blk.range = else_blk->range;
    stmt->else_blk.body = else_blk->body;

    return (Stmt*)stmt;
}

ElifBlock* elif_block(Allocator* allocator, Expr* cond, Stmt* body, ProgRange range)
{
    ElifBlock* elif = new_type(allocator, ElifBlock, true);
    elif->block.range = range;
    elif->block.cond = cond;
    elif->block.body = body;

    return elif;
}

Stmt* stmt_for(Allocator* allocator, Stmt* init, Expr* cond, Stmt* next, Stmt* body, ProgRange range)
{
    StmtFor* stmt = stmt_alloc(allocator, StmtFor, range);
    stmt->init = init;
    stmt->cond = cond;
    stmt->next = next;
    stmt->body = body;

    return (Stmt*)stmt;
}

Stmt* stmt_return(Allocator* allocator, Expr* expr, ProgRange range)
{
    StmtReturn* stmt = stmt_alloc(allocator, StmtReturn, range);
    stmt->expr = expr;

    return (Stmt*)stmt;
}

Stmt* stmt_break(Allocator* allocator, const char* label, ProgRange range)
{
    StmtBreak* stmt = stmt_alloc(allocator, StmtBreak, range);
    stmt->label = label;

    return (Stmt*)stmt;
}

Stmt* stmt_continue(Allocator* allocator, const char* label, ProgRange range)
{
    StmtContinue* stmt = stmt_alloc(allocator, StmtContinue, range);
    stmt->label = label;

    return (Stmt*)stmt;
}

Stmt* stmt_goto(Allocator* allocator, const char* label, ProgRange range)
{
    StmtGoto* stmt = stmt_alloc(allocator, StmtGoto, range);
    stmt->label = label;

    return (Stmt*)stmt;
}

Stmt* stmt_label(Allocator* allocator, const char* label, Stmt* target, ProgRange range)
{
    StmtLabel* stmt = stmt_alloc(allocator, StmtLabel, range);
    stmt->label = label;
    stmt->target = target;

    return (Stmt*)stmt;
}

SwitchCase* switch_case(Allocator* allocator, Expr* start, Expr* end, size_t num_stmts, DLList* stmts, ProgRange range)
{
    SwitchCase* swcase = new_type(allocator, SwitchCase, true);
    swcase->start = start;
    swcase->end = end;
    swcase->range = range;
    swcase->num_stmts = num_stmts;

    dllist_replace(stmts, &swcase->stmts);

    return swcase;
}

Stmt* stmt_switch(Allocator* allocator, Expr* expr, size_t num_cases, DLList* cases, ProgRange range)
{
    StmtSwitch* stmt = stmt_alloc(allocator, StmtSwitch, range);
    stmt->expr = expr;
    stmt->num_cases = num_cases;

    dllist_replace(cases, &stmt->cases);

    return (Stmt*)stmt;
}

char* ftprint_typespec(Allocator* allocator, TypeSpec* type)
{
    char* dstr = NULL;

    if (type)
    {
        switch (type->kind)
        {
            case AST_TYPE_SPEC_NONE:
            {
                assert(0);
            }
            break;
            case AST_TypeSpecIdent:
            {
                TypeSpecIdent* t = (TypeSpecIdent*)type;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(:ident %s)", t->name);
            }
            break;
            case AST_TypeSpecProc:
            {
                TypeSpecProc* t = (TypeSpecProc*)type;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(:proc =>%s", ftprint_typespec(allocator, t->ret));

                if (t->num_params)
                {
                    ftprint_char_array(&dstr, false, " ");

                    DLList* head = &t->params;

                    for (DLList* it = head->next; it != head; it = it->next)
                    {
                        ProcParam* param = dllist_entry(it, ProcParam, list);

                        if (param->name)
                        {
                            ftprint_char_array(&dstr, false, "(%s %s)", param->name,
                                               ftprint_typespec(allocator, param->type));
                        }
                        else
                        {
                            ftprint_char_array(&dstr, false, "%s", ftprint_typespec(allocator, param->type));
                        }

                        if (it->next != head)
                        {
                            ftprint_char_array(&dstr, false, " ");
                        }
                    }
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case AST_TypeSpecStruct:
            case AST_TypeSpecUnion:
            {
                dstr = array_create(allocator, char, 32);
                bool is_struct = type->kind == AST_TypeSpecStruct;

                ftprint_char_array(&dstr, false, "(:%s", (is_struct ? "struct" : "union"));

                TypeSpecAggregate* aggregate = (TypeSpecAggregate*)type;

                if (aggregate->num_fields)
                {
                    ftprint_char_array(&dstr, false, " ");
                    DLList* head = &aggregate->fields;

                    for (DLList* it = head->next; it != head; it = it->next)
                    {
                        AggregateField* field = dllist_entry(it, AggregateField, list);

                        ftprint_char_array(&dstr, false, "(%s %s)", field->name,
                                           ftprint_typespec(allocator, field->type));

                        if (it->next != head)
                        {
                            ftprint_char_array(&dstr, false, " ");
                        }
                    }
                }
                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case AST_TypeSpecPtr:
            {
                TypeSpecPtr* t = (TypeSpecPtr*)type;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(:ptr %s)", ftprint_typespec(allocator, t->base));
            }
            break;
            case AST_TypeSpecConst:
            {
                TypeSpecConst* t = (TypeSpecConst*)type;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(:const %s)", ftprint_typespec(allocator, t->base));
            }
            break;
            case AST_TypeSpecArray:
            {
                TypeSpecArray* t = (TypeSpecArray*)type;
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
                ftprint_err("Unknown typespec kind: %d\n", type->kind);
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

                if (e->num_args)
                {
                    ftprint_char_array(&dstr, false, " ");

                    DLList* head = &e->args;

                    for (DLList* it = head->next; it != head; it = it->next)
                    {
                        ProcCallArg* arg = dllist_entry(it, ProcCallArg, list);

                        if (arg->name)
                        {
                            ftprint_char_array(&dstr, false, "%s=", arg->name);
                        }

                        ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, arg->expr));

                        if (it->next != head)
                        {
                            ftprint_char_array(&dstr, false, " ");
                        }
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

                if (e->fkind == FLOAT_F64)
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
                ftprint_char_array(&dstr, false, "(cast %s %s)", ftprint_typespec(allocator, e->type),
                                   ftprint_expr(allocator, e->expr));
            }
            break;
            case AST_ExprSizeof:
            {
                ExprSizeof* e = (ExprSizeof*)expr;
                dstr = array_create(allocator, char, 16);
                ftprint_char_array(&dstr, false, "(sizeof %s)", ftprint_typespec(allocator, e->type));
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

                if (e->type)
                {
                    ftprint_char_array(&dstr, false, "%s ", ftprint_typespec(allocator, e->type));
                }

                ftprint_char_array(&dstr, false, "{");
                if (e->num_initzers)
                {
                    DLList* head = &e->initzers;

                    for (DLList* it = head->next; it != head; it = it->next)
                    {
                        MemberInitializer* initzer = dllist_entry(it, MemberInitializer, list);

                        if (initzer->designator.kind == DESIGNATOR_NAME)
                        {
                            ftprint_char_array(&dstr, false, "%s = ", initzer->designator.name);
                        }
                        else if (initzer->designator.kind == DESIGNATOR_INDEX)
                        {
                            ftprint_char_array(&dstr, false,
                                               "[%s] = ", ftprint_expr(allocator, initzer->designator.index));
                        }

                        ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, initzer->init));

                        if (it->next != head)
                        {
                            ftprint_char_array(&dstr, false, " ");
                        }
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

static char* ftprint_stmt_list(Allocator* allocator, size_t num_stmts, DLList* stmts)
{
    char* dstr = NULL;

    if (num_stmts)
    {
        dstr = array_create(allocator, char, 32);
        DLList* head = stmts;

        for (DLList* it = head->next; it != head; it = it->next)
        {
            Stmt* s = dllist_entry(it, Stmt, list);

            ftprint_char_array(&dstr, false, "%s", ftprint_stmt(allocator, s));

            if (it->next != head)
            {
                ftprint_char_array(&dstr, false, " ");
            }
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

                if (s->num_stmts)
                {
                    ftprint_char_array(&dstr, false, " %s)", ftprint_stmt_list(allocator, s->num_stmts, &s->stmts));
                }
                else
                {
                    ftprint_char_array(&dstr, false, ")");
                }
            }
            break;
            case AST_StmtDecl:
            {
                StmtDecl* s = (StmtDecl*)stmt;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, s->decl));
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

                if (s->init)
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

                if (s->num_elif_blks)
                {
                    ftprint_char_array(&dstr, false, " ");

                    DLList* head = &s->elif_blks;

                    for (DLList* it = head->next; it != head; it = it->next)
                    {
                        ElifBlock* elif = dllist_entry(it, ElifBlock, list);

                        ftprint_char_array(&dstr, false, "(elif %s %s)", ftprint_expr(allocator, elif->block.cond),
                                           ftprint_stmt(allocator, elif->block.body));

                        if (it->next != head)
                        {
                            ftprint_char_array(&dstr, false, " ");
                        }
                    }
                }

                if (s->else_blk.body)
                {
                    ftprint_char_array(&dstr, false, " (else %s)", ftprint_stmt(allocator, s->else_blk.body));
                }

                ftprint_char_array(&dstr, false, ")");
            }
            break;
            case AST_StmtSwitch:
            {
                StmtSwitch* s = (StmtSwitch*)stmt;
                dstr = array_create(allocator, char, 64);

                ftprint_char_array(&dstr, false, "(switch %s ", ftprint_expr(allocator, s->expr));

                DLList* head = &s->cases;

                for (DLList* it = head->next; it != head; it = it->next)
                {
                    SwitchCase* swcase = dllist_entry(it, SwitchCase, list);

                    ftprint_char_array(&dstr, false, "(case");

                    if (swcase->start)
                    {
                        ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, swcase->start));

                        if (swcase->end)
                        {
                            ftprint_char_array(&dstr, false, "..%s", ftprint_expr(allocator, swcase->end));
                        }
                    }

                    ftprint_char_array(&dstr, false, " (stmt-list %s))",
                                       ftprint_stmt_list(allocator, swcase->num_stmts, &swcase->stmts));

                    if (it->next != head)
                    {
                        ftprint_char_array(&dstr, false, " ");
                    }
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

                if (d->type)
                {
                    ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->type));
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

                if (d->type)
                {
                    ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->type));
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
                                   ftprint_typespec(allocator, d->type));
            }
            break;
            case AST_DeclEnum:
            {
                DeclEnum* d = (DeclEnum*)decl;
                dstr = array_create(allocator, char, 32);
                ftprint_char_array(&dstr, false, "(enum %s", d->super.name);

                if (d->type)
                {
                    ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->type));
                }

                if (d->num_items)
                {
                    ftprint_char_array(&dstr, false, " ");

                    DLList* head = &d->items;

                    for (DLList* it = head->next; it != head; it = it->next)
                    {
                        EnumItem* item = dllist_entry(it, EnumItem, list);

                        ftprint_char_array(&dstr, false, "%s", item->name);
                        if (item->value)
                        {
                            ftprint_char_array(&dstr, false, "=%s", ftprint_expr(allocator, item->value));
                        }

                        if (it->next != head)
                        {
                            ftprint_char_array(&dstr, false, " ");
                        }
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
                    DLList* head = &d->fields;

                    for (DLList* it = head->next; it != head; it = it->next)
                    {
                        AggregateField* field = dllist_entry(it, AggregateField, list);

                        ftprint_char_array(&dstr, false, "(%s %s)", field->name,
                                           ftprint_typespec(allocator, field->type));

                        if (it->next != head)
                        {
                            ftprint_char_array(&dstr, false, " ");
                        }
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

                if (proc->num_params)
                {
                    DLList* head = &proc->params;

                    for (DLList* it = head->next; it != head; it = it->next)
                    {
                        ProcParam* param = dllist_entry(it, ProcParam, list);

                        ftprint_char_array(&dstr, false, "(%s %s)", param->name,
                                           ftprint_typespec(allocator, param->type));

                        if (it->next != head)
                            ftprint_char_array(&dstr, false, " ");
                    }
                }
                ftprint_char_array(&dstr, false, ") =>%s (stmt-block", ftprint_typespec(allocator, proc->ret));

                if (proc->num_stmts)
                    ftprint_char_array(&dstr, false, " %s))",
                                       ftprint_stmt_list(allocator, proc->num_stmts, &proc->stmts));
                else
                    ftprint_char_array(&dstr, false, "))");
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
