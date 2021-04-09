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

static Decl* decl_alloc(Allocator* allocator, DeclKind kind, const char* name, ProgRange range)
{
    Decl* decl = mem_allocate(allocator, sizeof(Decl), DEFAULT_ALIGN, true);
    decl->kind = kind;
    decl->name = name;
    decl->range = range;

    return decl;
}

Decl* decl_var(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_VAR, name, range);
    decl->as_var.type = type;
    decl->as_var.init = init;

    return decl;
}

Decl* decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_CONST, name, range);
    decl->as_const.type = type;
    decl->as_const.init = init;

    return decl;
}

Decl* decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_TYPEDEF, name, range);
    decl->as_typedef.type = type;

    return decl;
}

Decl* decl_enum(Allocator* allocator, const char* name, TypeSpec* type, size_t num_items, DLList* items,
                ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_ENUM, name, range);
    decl->as_enum.type = type;
    decl->as_enum.num_items = num_items;

    dllist_replace(items, &decl->as_enum.items);

    return decl;
}

DeclEnumItem* decl_enum_item(Allocator* allocator, const char* name, Expr* value)
{
    DeclEnumItem* item = mem_allocate(allocator, sizeof(DeclEnumItem), DEFAULT_ALIGN, true);
    item->name = name;
    item->value = value;

    return item;
}

Decl* decl_struct(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_STRUCT, name, range);
    decl->as_struct.num_fields = num_fields;

    dllist_replace(fields, &decl->as_struct.fields);

    return decl;
}

Decl* decl_union(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_UNION, name, range);
    decl->as_union.num_fields = num_fields;

    dllist_replace(fields, &decl->as_union.fields);

    return decl;
}

AggregateField* aggregate_field(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    AggregateField* field = mem_allocate(allocator, sizeof(AggregateField), DEFAULT_ALIGN, true);
    field->name = name;
    field->type = type;
    field->range = range;

    return field;
}

Decl* decl_proc(Allocator* allocator, const char* name, size_t num_params, DLList* params, TypeSpec* ret,
                StmtBlock* block, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_PROC, name, range);
    decl->as_proc.num_params = num_params;
    decl->as_proc.ret = ret;
    decl->as_proc.block.num_stmts = block->num_stmts;

    dllist_replace(params, &decl->as_proc.params);
    dllist_replace(&block->stmts, &decl->as_proc.block.stmts);

    return decl;
}

DeclProcParam* decl_proc_param(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    DeclProcParam* param = mem_allocate(allocator, sizeof(DeclProcParam), DEFAULT_ALIGN, true);
    param->name = name;
    param->type = type;
    param->range = range;

    return param;
}

static Stmt* stmt_alloc(Allocator* allocator, StmtKind kind, ProgRange range)
{
    Stmt* stmt = mem_allocate(allocator, sizeof(Stmt), DEFAULT_ALIGN, true);
    stmt->kind = kind;
    stmt->range = range;

    return stmt;
}

Stmt* stmt_block(Allocator* allocator, size_t num_stmts, DLList* stmts, ProgRange range)
{
    Stmt* stmt = stmt_alloc(allocator, STMT_BLOCK, range);
    stmt->as_block.num_stmts = num_stmts;

    dllist_replace(stmts, &stmt->as_block.stmts);

    return stmt;
}

Stmt* stmt_decl(Allocator* allocator, Decl* decl)
{
    Stmt* stmt = stmt_alloc(allocator, STMT_DECL, decl->range);
    stmt->as_decl.decl = decl;

    return stmt;
}

Stmt* stmt_expr(Allocator* allocator, Expr* expr, ProgRange range)
{
    Stmt* stmt = stmt_alloc(allocator, STMT_EXPR, range);
    stmt->as_expr.expr = expr;

    return stmt;
}

Stmt* stmt_expr_assign(Allocator* allocator, Expr* lexpr, TokenKind op_assign, Expr* rexpr, ProgRange range)
{
    Stmt* stmt = stmt_alloc(allocator, STMT_EXPR_ASSIGN, range);
    stmt->as_expr_assign.left = lexpr;
    stmt->as_expr_assign.op_assign = op_assign;
    stmt->as_expr_assign.right = rexpr;

    return stmt;
}

Stmt* stmt_while(Allocator* allocator, Expr* cond, StmtBlock* block, ProgRange range)
{
    Stmt* stmt = stmt_alloc(allocator, STMT_WHILE, range);
    stmt->as_while.cond = cond;
    stmt->as_while.block.num_stmts = block->num_stmts;

    dllist_replace(&block->stmts, &stmt->as_while.block.stmts);

    return stmt;
}

Stmt* stmt_do_while(Allocator* allocator, Expr* cond, StmtBlock* block, ProgRange range)
{
    Stmt* stmt = stmt_alloc(allocator, STMT_DO_WHILE, range);
    stmt->as_do_while.cond = cond;
    stmt->as_do_while.block.num_stmts = block->num_stmts;

    dllist_replace(&block->stmts, &stmt->as_do_while.block.stmts);

    return stmt;
}

Stmt* stmt_if(Allocator* allocator, StmtCondBlock* if_blk, size_t num_elif_blks, DLList* elif_blks, StmtBlock* else_blk, 
              ProgRange range)
{
    Stmt* stmt = stmt_alloc(allocator, STMT_IF, range);
    StmtIf* as_if = &stmt->as_if;

    as_if->if_blk.cond = if_blk->cond;
    as_if->if_blk.block.num_stmts = if_blk->block.num_stmts;
    dllist_replace(&if_blk->block.stmts, &as_if->if_blk.block.stmts);

    as_if->num_elif_blks = num_elif_blks;
    dllist_replace(elif_blks, &as_if->elif_blks);

    as_if->else_blk.num_stmts = else_blk->num_stmts;
    dllist_replace(&else_blk->stmts, &as_if->else_blk.stmts);

    return stmt;
}

StmtElifBlock* stmt_elif_block(Allocator* allocator, Expr* cond, StmtBlock* block)
{
    StmtElifBlock* stmt = mem_allocate(allocator, sizeof(StmtElifBlock), DEFAULT_ALIGN, true);
    stmt->cond = cond;
    stmt->block.num_stmts = block->num_stmts;

    dllist_replace(&block->stmts, &stmt->block.stmts);

    return stmt;
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

static char* ftprint_stmt_block(Allocator* allocator, StmtBlock* block)
{
    char* dstr = array_create(allocator, char, 32);

    ftprint_char_array(&dstr, false, "(stmt-block");

    if (block->num_stmts) {
        ftprint_char_array(&dstr, false, " ");

        DLList* head = &block->stmts;

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
        case STMT_BLOCK: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "%s", ftprint_stmt_block(allocator, &stmt->as_block));
        } break;
        case STMT_DECL: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "%s", ftprint_decl(allocator, stmt->as_decl.decl));
        } break;
        case STMT_EXPR: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "%s", ftprint_expr(allocator, stmt->as_expr.expr));
        } break;
        case STMT_EXPR_ASSIGN: {
            dstr = array_create(allocator, char, 32);
            StmtExprAssign* s = &stmt->as_expr_assign;
            const char* op = token_kind_names[s->op_assign];

            ftprint_char_array(&dstr, false, "(%s %s %s)", op, ftprint_expr(allocator, s->left),
                               ftprint_expr(allocator, s->right));
        } break;
        case STMT_WHILE: {
            dstr = array_create(allocator, char, 32);
            StmtCondBlock* w = &stmt->as_while;

            ftprint_char_array(&dstr, false, "(while %s %s)", ftprint_expr(allocator, w->cond),
                               ftprint_stmt_block(allocator, &w->block));
        } break;
        case STMT_DO_WHILE: {
            dstr = array_create(allocator, char, 32);
            StmtCondBlock* w = &stmt->as_do_while;

            ftprint_char_array(&dstr, false, "(do-while %s %s)", ftprint_expr(allocator, w->cond),
                               ftprint_stmt_block(allocator, &w->block));
        } break;
        case STMT_IF: {
            dstr = array_create(allocator, char, 64);
            StmtIf* as_if = &stmt->as_if;

            ftprint_char_array(&dstr, false, "(if %s %s",
                               ftprint_expr(allocator, as_if->if_blk.cond),
                               ftprint_stmt_block(allocator, &as_if->if_blk.block));

            if (as_if->num_elif_blks) {
                ftprint_char_array(&dstr, false, " ");

                DLList* head = &as_if->elif_blks;

                for (DLList* it = head->next; it != head; it = it->next) {
                    StmtElifBlock* elif = dllist_entry(it, StmtElifBlock, list);

                    ftprint_char_array(&dstr, false, "(elif %s %s)", 
                                       ftprint_expr(allocator, elif->cond),
                                       ftprint_stmt_block(allocator, &elif->block));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }

            if (as_if->else_blk.num_stmts) {
                ftprint_char_array(&dstr, false, " (else %s)", ftprint_stmt_block(allocator, &as_if->else_blk));
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
        case DECL_VAR: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(var %s", decl->name);

            if (decl->as_var.type) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, decl->as_var.type));
            }

            if (decl->as_var.init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, decl->as_var.init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case DECL_CONST: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(const %s", decl->name);

            if (decl->as_const.type) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, decl->as_const.type));
            }

            if (decl->as_const.init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, decl->as_const.init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case DECL_TYPEDEF: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(typedef %s %s)", decl->name,
                               ftprint_typespec(allocator, decl->as_typedef.type));
        } break;
        case DECL_ENUM: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(enum ");

            if (decl->as_enum.type) {
                ftprint_char_array(&dstr, false, "%s ", ftprint_typespec(allocator, decl->as_enum.type));
            }

            if (decl->as_enum.num_items) {
                DLList* head = &decl->as_enum.items;

                for (DLList* it = head->next; it != head; it = it->next) {
                    DeclEnumItem* item = dllist_entry(it, DeclEnumItem, list);

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
        case DECL_STRUCT:
        case DECL_UNION: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(%s %s", (decl->kind == DECL_STRUCT ? "struct" : "union"), decl->name);

            AggregateBody* aggregate = decl->kind == DECL_STRUCT ? &decl->as_struct : &decl->as_union;

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
        case DECL_PROC: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(proc %s (", decl->name);

            DeclProc* proc = &decl->as_proc;

            if (proc->num_params) {
                DLList* head = &proc->params;

                for (DLList* it = head->next; it != head; it = it->next) {
                    DeclProcParam* param = dllist_entry(it, DeclProcParam, list);

                    ftprint_char_array(&dstr, false, "(%s %s)", param->name, ftprint_typespec(allocator, param->type));

                    if (it->next != head) {
                        ftprint_char_array(&dstr, false, " ");
                    }
                }
            }
            ftprint_char_array(&dstr, false, ") =>%s %s)", ftprint_typespec(allocator, proc->ret),
                               ftprint_stmt_block(allocator, &proc->block));
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
