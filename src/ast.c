#include "ast.h"
#include "array.h"
#include "cstring.h"

TypeSpec* typespec_alloc(Allocator* allocator, TypeSpecKind kind, ProgRange range)
{
    TypeSpec* type = mem_allocate(allocator, sizeof(TypeSpec), DEFAULT_ALIGN, true);
    type->kind = kind;
    type->range = range;

    return type;
}

TypeSpec* typespec_ident(Allocator* allocator, const char* name, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_IDENT, range);
    type->ident.name = name;

    return type;
}

TypeSpec* typespec_ptr(Allocator* allocator, TypeSpec* base, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_PTR, range);
    type->ptr.base = base;

    return type;
}

TypeSpec* typespec_array(Allocator* allocator, TypeSpec* base, Expr* len, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_ARRAY, range);
    type->array.base = base;
    type->array.len = len;

    return type;
}

TypeSpec* typespec_func(Allocator* allocator, size_t num_params, DLList* params, TypeSpec* ret, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_FUNC, range);
    type->func.num_params = num_params;
    type->func.ret = ret;

    dllist_replace(params, &type->func.params);

    return type;
}

TypeSpecParam* typespec_func_param(Allocator* allocator, TypeSpec* type, const char* name)
{
    TypeSpecParam* param = mem_allocate(allocator, sizeof(TypeSpecParam), DEFAULT_ALIGN, true);
    param->type = type;
    param->name = name;

    return param;
}

Expr* expr_alloc(Allocator* allocator, ExprKind kind, ProgRange range)
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
    expr->eternary.cond = cond;
    expr->eternary.then_expr = then_expr;
    expr->eternary.else_expr = else_expr;

    return expr;
}

Expr* expr_binary(Allocator* allocator, TokenKind op, Expr* left, Expr* right)
{
    ProgRange range = {.start = left->range.start, .end = right->range.end};
    Expr* expr = expr_alloc(allocator, EXPR_BINARY, range);
    expr->ebinary.op = op;
    expr->ebinary.left = left;
    expr->ebinary.right = right;

    return expr;
}

Expr* expr_unary(Allocator* allocator, TokenKind op, Expr* unary_expr, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_UNARY, range);
    expr->eunary.op = op;
    expr->eunary.expr = unary_expr;

    return expr;
}

Expr* expr_field(Allocator* allocator, Expr* object, const char* field, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_FIELD, range);
    expr->efield.object = object;
    expr->efield.field = field;

    return expr;
}

Expr* expr_index(Allocator* allocator, Expr* array, Expr* index, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_INDEX, range);
    expr->eindex.array = array;
    expr->eindex.index = index;

    return expr;
}

Expr* expr_call(Allocator* allocator, Expr* func, size_t num_args, DLList* args, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_CALL, range);
    expr->ecall.func = func;
    expr->ecall.num_args = num_args;

    dllist_replace(args, &expr->ecall.args);

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
    expr->eint.value = value;

    return expr;
}

Expr* expr_float(Allocator* allocator, double value, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_FLOAT, range);
    expr->efloat.value = value;

    return expr;
}

Expr* expr_str(Allocator* allocator, const char* value, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_STR, range);
    expr->estr.value = value;

    return expr;
}

Expr* expr_ident(Allocator* allocator, const char* name, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_IDENT, range);
    expr->eident.name = name;

    return expr;
}

Expr* expr_cast(Allocator* allocator, TypeSpec* type, Expr* unary, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_CAST, range);
    expr->ecast.type = type;
    expr->ecast.expr = unary;

    return expr;
}

Expr* expr_sizeof_type(Allocator* allocator, TypeSpec* type, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_SIZEOF, range);
    expr->esizeof.kind = EXPR_SIZEOF_ARG_TYPE;
    expr->esizeof.type = type;

    return expr;
}

Expr* expr_sizeof_expr(Allocator* allocator, Expr* arg, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_SIZEOF, range);
    expr->esizeof.kind = EXPR_SIZEOF_ARG_EXPR;
    expr->esizeof.expr = arg;

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
    expr->ecompound.type = type;
    expr->ecompound.num_initzers = num_initzers;

    dllist_replace(initzers, &expr->ecompound.initzers);

    return expr;
}

void print_typespec(TypeSpec* type) {
    switch (type->kind) {
    case TYPE_SPEC_NONE: {
        assert(0);
    } break;
    case TYPE_SPEC_IDENT: {

    } break;
    case TYPE_SPEC_FUNC: {
    } break;
    case TYPE_SPEC_PTR: {
    } break;
    case TYPE_SPEC_ARRAY: {
    } break;
    default: {
        assert(0);
    } break;
    }
}

void print_expr(Expr* expr)
{
    switch (expr->kind) {
    case EXPR_NONE: {
        assert(0);
    } break;
    case EXPR_TERNARY: {
        print_out("(? ");
        print_expr(expr->eternary.cond);
        print_out(" ");
        print_expr(expr->eternary.then_expr);
        print_out(" ");
        print_expr(expr->eternary.else_expr);
        print_out(")");
    } break;
    case EXPR_BINARY: {
        print_out("(%s ", token_kind_names[expr->ebinary.op]);
        print_expr(expr->ebinary.left);
        print_out(" ");
        print_expr(expr->ebinary.right);
        print_out(")");
    } break;
    case EXPR_UNARY: {
        print_out("(%s ", token_kind_names[expr->eunary.op]);
        print_expr(expr->eunary.expr);
        print_out(")");
    } break;
    case EXPR_CALL: {
        print_out("(call ");
        print_expr(expr->ecall.func);

        if (expr->ecall.num_args) {
            print_out(" ");

            DLList* head = &expr->ecall.args;

            for (DLList* it = head->next; it != head; it = it->next) {
                ExprCallArg* arg = dllist_entry(it, ExprCallArg, list);
                print_expr(arg->expr);

                if (it->next != head) {
                    print_out(" ");
                }
            }
        }
        print_out(")");
    } break;
    case EXPR_INDEX: {
        print_out("(arr_index ");
        print_expr(expr->eindex.array);
        print_out(" ");
        print_expr(expr->eindex.index);
        print_out(")");
    } break;
    case EXPR_FIELD: {
        print_out("(obj_field ");
        print_expr(expr->efield.object);
        print_out(" .%s)", expr->efield.field);
    } break;
    case EXPR_INT: {
        print_out("%lu", expr->eint.value);
    } break;
    case EXPR_FLOAT: {
        print_out("%lf", expr->efloat.value);
    } break;
    case EXPR_STR: {
        print_out("\"%s\"", expr->estr.value);
    } break;
    case EXPR_IDENT: {
        print_out("%s", expr->eident.name);
    } break;
    case EXPR_CAST: {
        print_out("(cast ");
        print_typespec(expr->ecast.type);
        print_out(" ");
        print_expr(expr->ecast.expr);
        print_out(")");
    } break;
    case EXPR_SIZEOF: {
        if (expr->esizeof.kind == EXPR_SIZEOF_ARG_TYPE) {
            print_out("(sizeof_type ");
            print_typespec(expr->esizeof.type);
            print_out(")");
        } else {
            print_out("(sizeof_expr ");
            print_expr(expr->esizeof.expr);
            print_out(")");
        }
    } break;
    case EXPR_COMPOUND_LIT: {
        print_out("(compound ");
        if (expr->ecompound.type) {
            print_typespec(expr->ecompound.type);
            print_out(" ");
        }

        print_out("{");
        if (expr->ecompound.num_initzers) {
            DLList* head = &expr->ecompound.initzers;

            for (DLList* it = head->next; it != head; it = it->next) {
                ExprInitializer* init = dllist_entry(it, ExprInitializer, list);

                if (init->kind == EXPR_INITIALIZER_NAME) {
                    print_out("%s = ", init->name);
                } else if (init->kind == EXPR_INITIALIZER_INDEX) {
                    print_out("[");
                    print_expr(init->index);
                    print_out("] = ");
                }

                print_expr(init->init);

                if (it->next != head) {
                    print_out(" ");
                }
            }
        }
        print_out("}");

        print_out(")");
    } break;
    default: {
        assert(0);
    } break;
    }
}
