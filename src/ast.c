#include "ast.h"
#include "cstring.h"
#include "array.h"

static TypeSpec* typespec_alloc(Allocator* allocator, TypeSpecKind kind, ProgRange range)
{
    TypeSpec* type = mem_allocate(allocator, sizeof(TypeSpec), DEFAULT_ALIGN, true);
    type->kind = kind;
    type->range = range;

    return type;
}

TypeSpec* typespec_ident(Allocator* allocator, const char* name, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_IDENT, range);
    type->as_ident.name = name;

    return type;
}

TypeSpec* typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_PTR, range);
    type->as_ptr.base = base;

    return type;
}

TypeSpec* typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_ARRAY, range);
    type->as_array.base = base;
    type->as_array.len = len;

    return type;
}

TypeSpec* typespec_const(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_CONST, range);
    type->as_const.base = base;

    return type;
}

TypeSpec* typespec_proc(Allocator* allocator, size_t num_params, DLList* params, TypeSpec* ret, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_PROC, range);
    type->as_proc.num_params = num_params;
    type->as_proc.ret = ret;

    dllist_replace(params, &type->as_proc.params);

    return type;
}

TypeSpecParam* typespec_proc_param(Allocator* allocator, TypeSpec* type)
{
    TypeSpecParam* param = mem_allocate(allocator, sizeof(TypeSpecParam), DEFAULT_ALIGN, true);
    param->type = type;

    return param;
}

TypeSpec* typespec_anon_struct(Allocator* allocator, size_t num_fields, DLList* fields, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_ANON_STRUCT, range);
    type->as_struct.num_fields = num_fields;

    dllist_replace(fields, &type->as_struct.fields);

    return type;
}

TypeSpec* typespec_anon_union(Allocator* allocator, size_t num_fields, DLList* fields, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_ANON_UNION, range);
    type->as_union.num_fields = num_fields;

    dllist_replace(fields, &type->as_union.fields);

    return type;
}

static Expr* expr_alloc(Allocator* allocator, ExprKind kind, ProgRange range)
{
    Expr* expr = mem_allocate(allocator, sizeof(Expr), DEFAULT_ALIGN, true);
    expr->kind = kind;
    expr->range = range;

    return expr;
}
Expr* expr_ternary(Allocator* allocator, Expr* cond, Expr* then_expr, Expr* else_expr)
{
    ProgRange range = {.start = cond->range.start, .end = else_expr->range.end};
    Expr* expr = expr_alloc(allocator, EXPR_TERNARY, range);
    expr->as_ternary.cond = cond;
    expr->as_ternary.then_expr = then_expr;
    expr->as_ternary.else_expr = else_expr;

    return expr;
}

Expr* expr_binary(Allocator* allocator, TokenKind op, Expr* left, Expr* right)
{
    ProgRange range = {.start = left->range.start, .end = right->range.end};
    Expr* expr = expr_alloc(allocator, EXPR_BINARY, range);
    expr->as_binary.op = op;
    expr->as_binary.left = left;
    expr->as_binary.right = right;

    return expr;
}

Expr* expr_unary(Allocator* allocator, TokenKind op, Expr* unary_expr, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_UNARY, range);
    expr->as_unary.op = op;
    expr->as_unary.expr = unary_expr;

    return expr;
}

Expr* expr_field(Allocator* allocator, Expr* object, const char* field, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_FIELD, range);
    expr->as_field.object = object;
    expr->as_field.field = field;

    return expr;
}

Expr* expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_INDEX, range);
    expr->as_index.array = array;
    expr->as_index.index = index;

    return expr;
}

Expr* expr_call(Allocator* allocator, Expr* proc, size_t num_args, DLList* args, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_CALL, range);
    expr->as_call.proc = proc;
    expr->as_call.num_args = num_args;

    dllist_replace(args, &expr->as_call.args);

    return expr;
}

ExprCallArg* expr_call_arg(Allocator* allocator, Expr* expr, const char* name)
{
    ExprCallArg* arg = mem_allocate(allocator, sizeof(ExprCallArg), DEFAULT_ALIGN, true);
    arg->expr = expr;
    arg->name = name;

    return arg;
}

Expr* expr_int(Allocator* allocator, uint64_t value, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_INT, range);
    expr->as_int.value = value;

    return expr;
}

Expr* expr_float(Allocator* allocator, double value, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_FLOAT, range);
    expr->as_float.value = value;

    return expr;
}

Expr* expr_str(Allocator* allocator, const char* value, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_STR, range);
    expr->as_str.value = value;

    return expr;
}

Expr* expr_ident(Allocator* allocator, const char* name, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_IDENT, range);
    expr->as_ident.name = name;

    return expr;
}

Expr* expr_cast(Allocator* allocator, TypeSpec* type, Expr* arg, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_CAST, range);
    expr->as_cast.type = type;
    expr->as_cast.expr = arg;

    return expr;
}

Expr* expr_sizeof(Allocator* allocator, TypeSpec* type, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_SIZEOF, range);
    expr->as_sizeof.type = type;

    return expr;
}

Expr* expr_typeof(Allocator* allocator, Expr* arg, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_TYPEOF, range);
    expr->as_typeof.expr = arg;

    return expr;
}

ExprInitializer* expr_pos_initializer(Allocator* allocator, Expr* init, ProgRange range)
{
    ExprInitializer* expr = mem_allocate(allocator, sizeof(ExprInitializer), DEFAULT_ALIGN, true);
    expr->kind = EXPR_INITIALIZER_POS;
    expr->range = range;
    expr->init = init;

    return expr;
}

ExprInitializer* expr_name_initializer(Allocator* allocator, const char* name, Expr* init, ProgRange range)
{
    ExprInitializer* expr = mem_allocate(allocator, sizeof(ExprInitializer), DEFAULT_ALIGN, true);
    expr->kind = EXPR_INITIALIZER_NAME;
    expr->range = range;
    expr->init = init;
    expr->name = name;

    return expr;
}

ExprInitializer* expr_index_initializer(Allocator* allocator, Expr* index, Expr* init, ProgRange range)
{
    ExprInitializer* expr = mem_allocate(allocator, sizeof(ExprInitializer), DEFAULT_ALIGN, true);
    expr->kind = EXPR_INITIALIZER_INDEX;
    expr->range = range;
    expr->init = init;
    expr->index = index;

    return expr;
}

Expr* expr_compound_lit(Allocator* allocator, TypeSpec* type, size_t num_initzers, DLList* initzers, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_COMPOUND_LIT, range);
    expr->as_compound.type = type;
    expr->as_compound.num_initzers = num_initzers;

    dllist_replace(initzers, &expr->as_compound.initzers);

    return expr;
}

#define decl_alloc(a, k, n, r) (Decl##k*)decl_alloc_((a), sizeof(Decl##k), alignof(Decl##k), DECL_##k, (n), (r))
static Decl* decl_alloc_(Allocator* allocator, size_t size, size_t align, DeclKind kind, const char* name, ProgRange range)
{
    Decl* decl = mem_allocate(allocator, size, align, true);
    decl->kind = kind;
    decl->name = name;
    decl->range = range;

    return (Decl*)decl;
}

Decl* decl_var(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range)
{
    DeclVar* decl = decl_alloc(allocator, Var, name, range);
    decl->type = type;
    decl->init = init;

    return (Decl*)decl;
}

Decl* decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range)
{
    DeclConst* decl = decl_alloc(allocator, Const, name, range);
    decl->type = type;
    decl->init = init;

    return (Decl*)decl;
}

Decl* decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    DeclTypedef* decl = decl_alloc(allocator, Typedef, name, range);
    decl->type = type;

    return (Decl*)decl;
}

Decl* decl_enum(Allocator* allocator, const char* name, TypeSpec* type, size_t num_items, DLList* items,
                ProgRange range)
{
    DeclEnum* decl = decl_alloc(allocator, Enum, name, range);
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
    DeclStruct* decl = decl_alloc(allocator, Struct, name, range);
    decl->num_fields = num_fields;

    dllist_replace(fields, &decl->fields);

    return (Decl*)decl;
}

Decl* decl_union(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range)
{
    DeclUnion* decl = decl_alloc(allocator, Union, name, range);
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
    DeclProc* decl = decl_alloc(allocator, Proc, name, range);
    decl->num_params = num_params;
    decl->ret = ret;
    decl->num_stmts = num_stmts;

    dllist_replace(params, &decl->params);
    dllist_replace(stmts, &decl->stmts);

    return (Decl*)decl;
}

ProcParam* proc_param(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    ProcParam* param = new_type(allocator, ProcParam, true);
    param->name = name;
    param->type = type;
    param->range = range;

    return param;
}

#define stmt_alloc(a, k, r) (Stmt##k*)stmt_alloc_((a), sizeof(Stmt##k), alignof(Stmt##k), STMT_##k, (r))
static Stmt* stmt_alloc_(Allocator* allocator, size_t size, size_t align, StmtKind kind, ProgRange range)
{
    Stmt* stmt = mem_allocate(allocator, size, align, true);
    stmt->kind = kind;
    stmt->range = range;

    return stmt;
}

Stmt* stmt_block(Allocator* allocator, size_t num_stmts, DLList* stmts, ProgRange range)
{
    StmtBlock* stmt = stmt_alloc(allocator, Block, range);
    stmt->num_stmts = num_stmts;

    dllist_replace(stmts, &stmt->stmts);

    return (Stmt*)stmt;
}

Stmt* stmt_decl(Allocator* allocator, Decl* decl)
{
    StmtDecl* stmt = stmt_alloc(allocator, Decl, decl->range);
    stmt->decl = decl;

    return (Stmt*)stmt;
}

Stmt* stmt_expr(Allocator* allocator, Expr* expr, ProgRange range)
{
    StmtExpr* stmt = stmt_alloc(allocator, Expr, range);
    stmt->expr = expr;

    return (Stmt*)stmt;
}

Stmt* stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range)
{
    StmtExprAssign* stmt = stmt_alloc(allocator, ExprAssign, range);
    stmt->left = lexpr;
    stmt->op_assign = op_assign;
    stmt->right = rexpr;

    return (Stmt*)stmt;
}

Stmt* stmt_while(Allocator* allocator, Expr* cond, size_t num_stmts, DLList* stmts, ProgRange range)
{
    StmtWhile* stmt = stmt_alloc(allocator, While, range);
    stmt->cond = cond;
    stmt->num_stmts = num_stmts;

    dllist_replace(stmts, &stmt->stmts);

    return (Stmt*)stmt;
}

Stmt* stmt_do_while(Allocator* allocator, Expr* cond, size_t num_stmts, DLList* stmts, ProgRange range)
{
    StmtDoWhile* stmt = stmt_alloc(allocator, DoWhile, range);
    stmt->cond = cond;
    stmt->num_stmts = num_stmts;

    dllist_replace(stmts, &stmt->stmts);

    return (Stmt*)stmt;
}

Stmt* stmt_if(Allocator* allocator, IfCondBlock* if_blk, size_t num_elif_blks, DLList* elif_blks, ElseBlock* else_blk,
              ProgRange range)
{
    StmtIf* stmt = stmt_alloc(allocator, If, range);

    stmt->if_blk.range = if_blk->range;
    stmt->if_blk.cond = if_blk->cond;
    stmt->if_blk.num_stmts = if_blk->num_stmts;
    dllist_replace(&if_blk->stmts, &stmt->if_blk.stmts);

    stmt->num_elif_blks = num_elif_blks;
    dllist_replace(elif_blks, &stmt->elif_blks);

    stmt->else_blk.range = else_blk->range;
    stmt->else_blk.num_stmts = else_blk->num_stmts;
    dllist_replace(&else_blk->stmts, &stmt->else_blk.stmts);

    return (Stmt*)stmt;
}

ElifBlock* elif_block(Allocator* allocator, Expr* cond, size_t num_stmts, DLList* stmts, ProgRange range)
{
    ElifBlock* elif = new_type(allocator, ElifBlock, true);
    elif->block.range = range;
    elif->block.cond = cond;
    elif->block.num_stmts = num_stmts;

    dllist_replace(stmts, &elif->block.stmts);

    return elif;
}

char* ftprint_typespec(Allocator* allocator, TypeSpec* type)
{
    char* dstr = NULL;

    if (type) {
        switch (type->kind) {
        case TYPE_SPEC_NONE: {
            assert(0);
        } break;
        case TYPE_SPEC_IDENT: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(:ident %s)", type->as_ident.name);
        } break;
        case TYPE_SPEC_PROC: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:proc =>%s", ftprint_typespec(allocator, type->as_proc.ret));

            if (type->as_proc.num_params) {
                ftprint_char_array(&dstr, false, " ");

                DLList* head = &type->as_proc.params;

                for (DLList* it = head->next; it != head; it = it->next) {
                    TypeSpecParam* param = dllist_entry(it, TypeSpecParam, list);

                    ftprint_char_array(&dstr, false, "%s", ftprint_typespec(allocator, param->type));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case TYPE_SPEC_ANON_STRUCT:
        case TYPE_SPEC_ANON_UNION: {
            dstr = array_create(allocator, char, 32);
            bool is_struct = type->kind == TYPE_SPEC_ANON_STRUCT;

            ftprint_char_array(&dstr, false, "(:%s", (is_struct ? "struct" : "union"));

            AggregateBody* aggregate = is_struct ? &type->as_struct : &type->as_union;

            if (aggregate->num_fields) {
                ftprint_char_array(&dstr, false, " ");
                DLList* head = &aggregate->fields;

                for (DLList* it = head->next; it != head; it = it->next) {
                    AggregateField* field = dllist_entry(it, AggregateField, list);

                    ftprint_char_array(&dstr, false, "(%s %s)", field->name, ftprint_typespec(allocator, field->type));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }
            ftprint_char_array(&dstr, false, ")");
        } break;
        case TYPE_SPEC_PTR: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:ptr %s)", ftprint_typespec(allocator, type->as_ptr.base));
        } break;
        case TYPE_SPEC_CONST: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:const %s)", ftprint_typespec(allocator, type->as_const.base));
        } break;
        case TYPE_SPEC_ARRAY: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:arr %s %s)", ftprint_expr(allocator, type->as_array.len),
                               ftprint_typespec(allocator, type->as_array.base));
        } break;
        default: {
            ftprint_err("Unknown typespec kind: %d\n", type->kind);
            assert(0);
        } break;
        }
    } else {
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
        case EXPR_NONE: {
            assert(0);
        } break;
        case EXPR_TERNARY: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(? %s %s %s)", ftprint_expr(allocator, expr->as_ternary.cond),
                               ftprint_expr(allocator, expr->as_ternary.then_expr),
                               ftprint_expr(allocator, expr->as_ternary.else_expr));
        } break;
        case EXPR_BINARY: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(%s %s %s)", token_kind_names[expr->as_binary.op],
                               ftprint_expr(allocator, expr->as_binary.left),
                               ftprint_expr(allocator, expr->as_binary.right));
        } break;
        case EXPR_UNARY: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(%s %s)", token_kind_names[expr->as_unary.op],
                               ftprint_expr(allocator, expr->as_unary.expr));
        } break;
        case EXPR_CALL: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(call %s", ftprint_expr(allocator, expr->as_call.proc));

            if (expr->as_call.num_args) {
                ftprint_char_array(&dstr, false, " ");

                DLList* head = &expr->as_call.args;

                for (DLList* it = head->next; it != head; it = it->next) {
                    ExprCallArg* arg = dllist_entry(it, ExprCallArg, list);

                    if (arg->name) {
                        ftprint_char_array(&dstr, false, "%s=", arg->name);
                    }

                    ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, arg->expr));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }
            ftprint_char_array(&dstr, false, ")");
        } break;
        case EXPR_INDEX: {
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "(arr_index %s %s)", ftprint_expr(allocator, expr->as_index.array),
                               ftprint_expr(allocator, expr->as_index.index));
        } break;
        case EXPR_FIELD: {
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "(obj_field %s .%s)", ftprint_expr(allocator, expr->as_field.object),
                               expr->as_field.field);
        } break;
        case EXPR_INT: {
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "%lu", expr->as_int.value);
        } break;
        case EXPR_FLOAT: {
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "%lf", expr->as_float.value);
        } break;
        case EXPR_STR: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "\"%s\"", expr->as_str.value);
        } break;
        case EXPR_IDENT: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "%s", expr->as_ident.name);
        } break;
        case EXPR_CAST: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(cast %s %s)", ftprint_typespec(allocator, expr->as_cast.type),
                               ftprint_expr(allocator, expr->as_cast.expr));
        } break;
        case EXPR_SIZEOF: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(sizeof %s)", ftprint_typespec(allocator, expr->as_sizeof.type));
        } break;
        case EXPR_TYPEOF: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(typeof %s)", ftprint_expr(allocator, expr->as_typeof.expr));
        } break;
        case EXPR_COMPOUND_LIT: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(compound ");

            if (expr->as_compound.type) {
                ftprint_char_array(&dstr, false, "%s ", ftprint_typespec(allocator, expr->as_compound.type));
            }

            ftprint_char_array(&dstr, false, "{");
            if (expr->as_compound.num_initzers) {
                DLList* head = &expr->as_compound.initzers;

                for (DLList* it = head->next; it != head; it = it->next) {
                    ExprInitializer* init = dllist_entry(it, ExprInitializer, list);

                    if (init->kind == EXPR_INITIALIZER_NAME) {
                        ftprint_char_array(&dstr, false, "%s = ", init->name);
                    } else if (init->kind == EXPR_INITIALIZER_INDEX) {
                        ftprint_char_array(&dstr, false, "[%s] = ", ftprint_expr(allocator, init->index));
                    }

                    ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, init->init));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }
            ftprint_char_array(&dstr, false, "})");
        } break;
        default: {
            ftprint_err("Unknown expr kind: %d\n", expr->kind);
            assert(0);
        } break;
        }
    } else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}

static char* ftprint_stmt_block(Allocator* allocator, size_t num_stmts, DLList* stmts)
{
    char* dstr = array_create(allocator, char, 32);

    ftprint_char_array(&dstr, false, "(stmt-block");

    if (num_stmts) {
        ftprint_char_array(&dstr, false, " ");

        DLList* head = stmts;

        for (DLList* it = head->next; it != head; it = it->next) {
            Stmt* s = dllist_entry(it, Stmt, list);

            ftprint_char_array(&dstr, false, "%s", ftprint_stmt(allocator, s));

            if (it->next != head) {
                ftprint_char_array(&dstr, false, " ");
            }
        }
    }

    ftprint_char_array(&dstr, true, ")");

    return dstr;
}

char* ftprint_stmt(Allocator* allocator, Stmt* stmt)
{
    char* dstr = NULL;

    if (stmt) {
        switch (stmt->kind) {
        case STMT_NONE: {
            assert(0);
        } break;
        case STMT_Block: {
            StmtBlock* s = (StmtBlock*)stmt;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "%s", ftprint_stmt_block(allocator, s->num_stmts, &s->stmts));
        } break;
        case STMT_Decl: {
            StmtDecl* s = (StmtDecl*)stmt;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, s->decl));
        } break;
        case STMT_Expr: {
            StmtExpr* s = (StmtExpr*)stmt;
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, s->expr));
        } break;
        case STMT_ExprAssign: {
            StmtExprAssign* s = (StmtExprAssign*)stmt;
            dstr = array_create(allocator, char, 32);
            const char* op = token_kind_names[s->op_assign];

            ftprint_char_array(&dstr, false, "(%s %s %s)", op, ftprint_expr(allocator, s->left),
                               ftprint_expr(allocator, s->right));
        } break;
        case STMT_While: {
            StmtWhile* s = (StmtWhile*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(while %s %s)", ftprint_expr(allocator, s->cond),
                               ftprint_stmt_block(allocator, s->num_stmts, &s->stmts));
        } break;
        case STMT_DoWhile: {
            StmtDoWhile* s = (StmtDoWhile*)stmt;
            dstr = array_create(allocator, char, 32);

            ftprint_char_array(&dstr, false, "(do-while %s %s)", ftprint_expr(allocator, s->cond),
                               ftprint_stmt_block(allocator, s->num_stmts, &s->stmts));
        } break;
        case STMT_If: {
            StmtIf* s = (StmtIf*)stmt;
            dstr = array_create(allocator, char, 64);

            ftprint_char_array(&dstr, false, "(if %s %s", ftprint_expr(allocator, s->if_blk.cond),
                               ftprint_stmt_block(allocator, s->if_blk.num_stmts, &s->if_blk.stmts));

            if (s->num_elif_blks) {
                ftprint_char_array(&dstr, false, " ");

                DLList* head = &s->elif_blks;

                for (DLList* it = head->next; it != head; it = it->next) {
                    ElifBlock* elif = dllist_entry(it, ElifBlock, list);

                    ftprint_char_array(&dstr, false, "(elif %s %s)", ftprint_expr(allocator, elif->block.cond),
                                       ftprint_stmt_block(allocator, elif->block.num_stmts, &elif->block.stmts));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }

            if (s->else_blk.num_stmts) {
                ftprint_char_array(&dstr, false, " (else %s)",
                                   ftprint_stmt_block(allocator, s->else_blk.num_stmts, &s->else_blk.stmts));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        default: {
            ftprint_err("Unknown stmt kind: %d\n", stmt->kind);
            assert(0);
        } break;
        }

    } else {
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
        case DECL_NONE: {
            assert(0);
        } break;
        case DECL_Var: {
            DeclVar* d = (DeclVar*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(var %s", d->base.name);

            if (d->type) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->type));
            }

            if (d->init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, d->init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case DECL_Const: {
            DeclConst* d = (DeclConst*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(const %s", d->base.name);

            if (d->type) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->type));
            }

            if (d->init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, d->init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case DECL_Typedef: {
            DeclTypedef* d = (DeclTypedef*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(typedef %s %s)", d->base.name,
                               ftprint_typespec(allocator, d->type));
        } break;
        case DECL_Enum: {
            DeclEnum* d = (DeclEnum*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(enum %s", d->base.name);

            if (d->type) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, d->type));
            }

            if (d->num_items) {
                ftprint_char_array(&dstr, false, " ");

                DLList* head = &d->items;

                for (DLList* it = head->next; it != head; it = it->next) {
                    EnumItem* item = dllist_entry(it, EnumItem, list);

                    ftprint_char_array(&dstr, false, "%s", item->name);
                    if (item->value) {
                        ftprint_char_array(&dstr, false, "=%s", ftprint_expr(allocator, item->value));
                    }

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }
            ftprint_char_array(&dstr, false, ")");
        } break;
        case DECL_Struct:
        case DECL_Union: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(%s %s", (decl->kind == DECL_Struct ? "struct" : "union"), decl->name);

            DeclAggregate* d = (DeclAggregate*)decl;

            if (d->num_fields) {
                ftprint_char_array(&dstr, false, " ");
                DLList* head = &d->fields;

                for (DLList* it = head->next; it != head; it = it->next) {
                    AggregateField* field = dllist_entry(it, AggregateField, list);

                    ftprint_char_array(&dstr, false, "(%s %s)", field->name, ftprint_typespec(allocator, field->type));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }
            ftprint_char_array(&dstr, false, ")");
        } break;
        case DECL_Proc: {
            DeclProc* proc = (DeclProc*)decl;
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(proc %s (", decl->name);

            if (proc->num_params) {
                DLList* head = &proc->params;

                for (DLList* it = head->next; it != head; it = it->next) {
                    ProcParam* param = dllist_entry(it, ProcParam, list);

                    ftprint_char_array(&dstr, false, "(%s %s)", param->name, ftprint_typespec(allocator, param->type));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }
            ftprint_char_array(&dstr, false, ") =>%s %s)", ftprint_typespec(allocator, proc->ret),
                               ftprint_stmt_block(allocator, proc->num_stmts, &proc->stmts));
        } break;
        default: {
            ftprint_err("Unknown decl kind: %d\n", decl->kind);
            assert(0);
        } break;
        }
    } else {
        dstr = array_create(allocator, char, 1);
    }

    array_push(dstr, '\0');

    return dstr;
}
