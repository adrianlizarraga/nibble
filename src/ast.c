#include "array.h"
#include "ast.h"
#include "cstring.h"

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

TypeSpec* typespec_func(Allocator* allocator, size_t num_params, DLList* params, TypeSpec* ret, ProgRange range)
{
    TypeSpec* type = typespec_alloc(allocator, TYPE_SPEC_FUNC, range);
    type->as_func.num_params = num_params;
    type->as_func.ret = ret;

    dllist_replace(params, &type->as_func.params);

    return type;
}

TypeSpecParam* typespec_func_param(Allocator* allocator, TypeSpec* type)
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

Expr* expr_cast(Allocator* allocator, TypeSpec* type, Expr* arg, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_CAST, range);
    expr->ecast.type = type;
    expr->ecast.expr = arg;

    return expr;
}

Expr* expr_sizeof(Allocator* allocator, TypeSpec* type, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_SIZEOF, range);
    expr->esizeof.type = type;

    return expr;
}

Expr* expr_typeof(Allocator* allocator, Expr* arg, ProgRange range)
{
    Expr* expr = expr_alloc(allocator, EXPR_TYPEOF, range);
    expr->etypeof.expr = arg;

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
    decl->dvar.type = type;
    decl->dvar.init = init;

    return decl;
}

Decl* decl_const(Allocator* allocator, const char* name, TypeSpec* type, Expr* init, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_CONST, name, range);
    decl->dconst.type = type;
    decl->dconst.init = init;

    return decl;
}

Decl* decl_typedef(Allocator* allocator, const char* name, TypeSpec* type, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_TYPEDEF, name, range);
    decl->dtypedef.type = type;

    return decl;
}

Decl* decl_enum(Allocator* allocator, const char* name, TypeSpec* type, size_t num_items, DLList* items,
                ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_ENUM, name, range);
    decl->denum.type = type;
    decl->denum.num_items = num_items;

    dllist_replace(items, &decl->denum.items);

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
    decl->dstruct.num_fields = num_fields;

    dllist_replace(fields, &decl->dstruct.fields);

    return decl;
}

Decl* decl_union(Allocator* allocator, const char* name, size_t num_fields, DLList* fields, ProgRange range)
{
    Decl* decl = decl_alloc(allocator, DECL_UNION, name, range);
    decl->dunion.num_fields = num_fields;

    dllist_replace(fields, &decl->dunion.fields);

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
        case TYPE_SPEC_FUNC: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(:func =>%s", ftprint_typespec(allocator, type->as_func.ret));

            if (type->as_func.num_params) {
                ftprint_char_array(&dstr, false, " ");

                DLList* head = &type->as_func.params;

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
            ftprint_char_array(&dstr, false, "(? %s %s %s)", ftprint_expr(allocator, expr->eternary.cond),
                               ftprint_expr(allocator, expr->eternary.then_expr),
                               ftprint_expr(allocator, expr->eternary.else_expr));
        } break;
        case EXPR_BINARY: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(%s %s %s)", token_kind_names[expr->ebinary.op],
                               ftprint_expr(allocator, expr->ebinary.left),
                               ftprint_expr(allocator, expr->ebinary.right));
        } break;
        case EXPR_UNARY: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(%s %s)", token_kind_names[expr->eunary.op],
                               ftprint_expr(allocator, expr->eunary.expr));
        } break;
        case EXPR_CALL: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(call %s", ftprint_expr(allocator, expr->ecall.func));

            if (expr->ecall.num_args) {
                ftprint_char_array(&dstr, false, " ");

                DLList* head = &expr->ecall.args;

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
            ftprint_char_array(&dstr, false, "(arr_index %s %s)", ftprint_expr(allocator, expr->eindex.array),
                               ftprint_expr(allocator, expr->eindex.index));
        } break;
        case EXPR_FIELD: {
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "(obj_field %s .%s)", ftprint_expr(allocator, expr->efield.object),
                               expr->efield.field);
        } break;
        case EXPR_INT: {
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "%lu", expr->eint.value);
        } break;
        case EXPR_FLOAT: {
            dstr = array_create(allocator, char, 8);
            ftprint_char_array(&dstr, false, "%lf", expr->efloat.value);
        } break;
        case EXPR_STR: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "\"%s\"", expr->estr.value);
        } break;
        case EXPR_IDENT: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "%s", expr->eident.name);
        } break;
        case EXPR_CAST: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(cast %s %s)", ftprint_typespec(allocator, expr->ecast.type),
                               ftprint_expr(allocator, expr->ecast.expr));
        } break;
        case EXPR_SIZEOF: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(sizeof %s)", ftprint_typespec(allocator, expr->esizeof.type));
        } break;
        case EXPR_TYPEOF: {
            dstr = array_create(allocator, char, 16);
            ftprint_char_array(&dstr, false, "(typeof %s)", ftprint_expr(allocator, expr->etypeof.expr));
        } break;
        case EXPR_COMPOUND_LIT: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(compound ");

            if (expr->ecompound.type) {
                ftprint_char_array(&dstr, false, "%s ", ftprint_typespec(allocator, expr->ecompound.type));
            }

            ftprint_char_array(&dstr, false, "{");
            if (expr->ecompound.num_initzers) {
                DLList* head = &expr->ecompound.initzers;

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

            if (decl->dvar.type) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, decl->dvar.type));
            }

            if (decl->dvar.init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, decl->dvar.init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case DECL_CONST: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(#const %s", decl->name);

            if (decl->dconst.type) {
                ftprint_char_array(&dstr, false, " %s", ftprint_typespec(allocator, decl->dconst.type));
            }

            if (decl->dconst.init) {
                ftprint_char_array(&dstr, false, " %s", ftprint_expr(allocator, decl->dconst.init));
            }

            ftprint_char_array(&dstr, false, ")");
        } break;
        case DECL_TYPEDEF: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(#typedef %s %s)", decl->name,
                               ftprint_typespec(allocator, decl->dtypedef.type));
        } break;
        case DECL_ENUM: {
            dstr = array_create(allocator, char, 32);
            ftprint_char_array(&dstr, false, "(enum ");

            if (decl->denum.type) {
                ftprint_char_array(&dstr, false, "%s ", ftprint_typespec(allocator, decl->denum.type));
            }

            if (decl->denum.num_items) {
                DLList* head = &decl->denum.items;

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

            AggregateBody* aggregate = decl->kind == DECL_STRUCT ? &decl->dstruct : &decl->dunion;

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
