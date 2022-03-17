#include "ast.h"

typedef struct IR_VarBuilder {
    Allocator* arena;
    Allocator* tmp_arena;
    BucketList* str_lits;
    TypeCache* type_cache;
    Module* curr_mod;
} IR_VarBuilder;

static void IR_emit_global_expr(IR_VarBuilder* builder, Expr* expr, ConstExpr* dst);

static void IR_const_expr_from_sym(ConstExpr* dst, Symbol* sym)
{
    if (sym->kind == SYMBOL_VAR) {
        dst->kind = CONST_EXPR_VAR;
        dst->type = sym->type;
        dst->sym = sym;
    }
    else if (sym->kind == SYMBOL_PROC) {
        dst->kind = CONST_EXPR_PROC;
        dst->type = sym->type;
        dst->sym = sym;
    }
    else {
        assert(0);
    }
}

static void IR_emit_global_expr_ident(IR_VarBuilder* builder, ExprIdent* eident, ConstExpr* dst)
{
    Symbol* sym = NULL;

    if (eident->mod_ns) {
        Symbol* sym_modns = lookup_symbol(&builder->curr_mod->scope, eident->mod_ns);
        StmtImport* stmt = (StmtImport*)sym_modns->as_mod.stmt;
        Identifier* sym_name = get_import_sym_name(stmt, eident->name);

        sym = module_get_export_sym(sym_modns->as_mod.mod, sym_name);
    }
    else {
        sym = lookup_symbol(&builder->curr_mod->scope, eident->name);
    }

    assert(sym);

    IR_const_expr_from_sym(dst, sym);
}

static void IR_emit_global_expr_cast(IR_VarBuilder* builder, ExprCast* expr_cast, ConstExpr* dst)
{
    ConstExpr src = {0};
    IR_emit_global_expr(builder, expr_cast->expr, &src);

    dst->kind = CONST_EXPR_MEM_ADDR;
    dst->type = expr_cast->super.type;

    // Only casts from array to pointer should be encountered here. Because global expressions
    // are compile-time constants, any int casts must have been processed in IR_emit_global_expr.
    assert(src.type->kind == TYPE_ARRAY && dst->type->kind == TYPE_PTR);

    if (src.kind == CONST_EXPR_VAR) {
        assert(!src.sym->is_local);

        dst->addr.kind = CONST_ADDR_SYM;
        dst->addr.sym = src.sym;
        dst->addr.disp = 0;
    }
    else if (src.kind == CONST_EXPR_DEREF_ADDR) {
        dst->addr = src.addr;
    }
    else {
        assert(src.kind == CONST_EXPR_STR_LIT);

        dst->addr.kind = CONST_ADDR_STR_LIT;
        dst->addr.str_lit = src.str_lit;
        dst->addr.disp = 0;
    }
}

static void IR_emit_global_expr_unary(IR_VarBuilder* builder, ExprUnary* expr_unary, ConstExpr* dst)
{
    Type* result_type = expr_unary->super.type;

    switch (expr_unary->op) {
    case TKN_CARET: {
        ConstExpr src = {0};

        IR_emit_global_expr(builder, expr_unary->expr, &src);

        dst->kind = CONST_EXPR_MEM_ADDR;
        dst->type = result_type;

        if (src.kind == CONST_EXPR_DEREF_ADDR) {
            dst->addr = src.addr;
        }
        else {
            assert(src.kind == CONST_EXPR_VAR);

            dst->addr.kind = CONST_ADDR_SYM;
            dst->addr.sym = src.sym;
            dst->addr.disp = 0;
        }
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void IR_emit_global_ptr_int_add(ConstExpr* dst, ConstExpr* ptr_op, ConstExpr* int_op, bool add)
{
    assert(int_op->kind == CONST_EXPR_IMM);
    assert(ptr_op->kind == CONST_EXPR_MEM_ADDR);

    u64 base_size = ptr_op->type->as_ptr.base->size;

    if (add) {
        ptr_op->addr.disp += base_size * int_op->imm.as_int._u64;
    }
    else {
        ptr_op->addr.disp -= base_size * int_op->imm.as_int._u64;
    }

    *dst = *ptr_op;
}

static void IR_emit_global_expr_binary(IR_VarBuilder* builder, ExprBinary* expr_binary, ConstExpr* dst)
{
    Type* result_type = expr_binary->super.type;
    ConstExpr left = {0};
    ConstExpr right = {0};

    IR_emit_global_expr(builder, expr_binary->left, &left);
    IR_emit_global_expr(builder, expr_binary->right, &right);

    bool left_is_ptr = left.type->kind == TYPE_PTR;
    bool right_is_ptr = right.type->kind == TYPE_PTR;

    switch (expr_binary->op) {
    case TKN_PLUS: {
        if (left_is_ptr) {
            assert(result_type == left.type);
            IR_emit_global_ptr_int_add(dst, &left, &right, true);
        }
        else {
            assert(result_type == right.type);
            assert(right_is_ptr);
            IR_emit_global_ptr_int_add(dst, &right, &left, true);
        }
        break;
    }
    case TKN_MINUS: {
        assert(left_is_ptr && !right_is_ptr);
        assert(result_type == left.type);
        IR_emit_global_ptr_int_add(dst, &left, &right, false);
        break;
    }
    default:
        assert(0);
        break;
    }
}

static void IR_emit_global_expr_array_lit(IR_VarBuilder* builder, ExprCompoundLit* expr, ConstExpr* dst)
{
    Type* type = expr->super.type;
    assert(type->kind == TYPE_ARRAY);
    assert(!expr->typespec);

    size_t num_initzers = expr->num_initzers;
    ConstArrayMemberInitzer* const_initzers = alloc_array(builder->arena, ConstArrayMemberInitzer, num_initzers, true);

    List* head = &expr->initzers;
    List* it = head->next;
    size_t elem_index = 0;
    size_t initzer_index = 0;

    while (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        ConstArrayMemberInitzer* ir_initzer = const_initzers + initzer_index;

        if (initzer->designator.kind == DESIGNATOR_INDEX) {
            ConstExpr desig_op = {0};
            IR_emit_global_expr(builder, initzer->designator.index, &desig_op);

            assert(desig_op.kind == CONST_EXPR_IMM);
            elem_index = desig_op.imm.as_int._u64;
        }
        else {
            assert(initzer->designator.kind == DESIGNATOR_NONE);
        }

        ir_initzer->index = elem_index;
        IR_emit_global_expr(builder, initzer->init, &ir_initzer->const_expr);

        elem_index += 1;
        initzer_index += 1;
        it = it->next;
    }

    dst->kind = CONST_EXPR_ARRAY_INIT;
    dst->type = type;
    dst->array_initzer.num_initzers = num_initzers;
    dst->array_initzer.initzers = const_initzers;
}

static void IR_emit_global_expr_struct_lit(IR_VarBuilder* builder, ExprCompoundLit* expr, ConstExpr* dst)
{
    Type* type = expr->super.type;
    TypeAggregateBody* type_agg = &type->as_struct.body;
    TypeAggregateField* fields = type_agg->fields;
    size_t num_fields = type_agg->num_fields;

    assert(!expr->typespec);
    assert(type->kind == TYPE_STRUCT);

    ConstExpr** field_exprs = alloc_array(builder->arena, ConstExpr*, num_fields, true);

    List* head = &expr->initzers;
    List* it = head->next;
    size_t field_index = 0;

    while (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        TypeAggregateField* field;

        if (initzer->designator.kind == DESIGNATOR_NAME) {
            field = get_type_struct_field(type, initzer->designator.name);
            assert(field);

            field_index = field->index + 1;
        }
        else {
            assert(initzer->designator.kind == DESIGNATOR_NONE);
            assert(field_index < num_fields);

            field = &fields[field_index++];
        }

        field_exprs[field->index] = alloc_type(builder->arena, ConstExpr, true);
        IR_emit_global_expr(builder, initzer->init, field_exprs[field->index]);

        it = it->next;
    }

    dst->kind = CONST_EXPR_STRUCT_INIT;
    dst->type = type;
    dst->struct_initzer.num_initzers = expr->num_initzers;
    dst->struct_initzer.field_exprs = field_exprs;
}

static void IR_emit_global_expr_union_lit(IR_VarBuilder* builder, ExprCompoundLit* expr, ConstExpr* dst)
{
    Type* type = expr->super.type;

    assert(!expr->typespec);
    assert(type->kind == TYPE_UNION);

    size_t field_index = 0;
    ConstExpr* field_expr = NULL;

    List* head = &expr->initzers;
    List* it = head->next;

    if (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);

        if (initzer->designator.kind == DESIGNATOR_NAME) {
            TypeAggregateField* field = get_type_union_field(type, initzer->designator.name);
            assert(field);

            field_index = field->index;
        }
        else {
            assert(initzer->designator.kind == DESIGNATOR_NONE);
        }

        field_expr = alloc_type(builder->arena, ConstExpr, true);
        IR_emit_global_expr(builder, initzer->init, field_expr);
    }

    dst->kind = CONST_EXPR_UNION_INIT;
    dst->type = type;
    dst->union_initzer.field_expr = field_expr;
    dst->union_initzer.field_index = field_index;
}

static void IR_emit_global_expr_compound_lit(IR_VarBuilder* builder, ExprCompoundLit* expr, ConstExpr* dst)
{
    Type* type = expr->super.type;

    if (type->kind == TYPE_ARRAY) {
        IR_emit_global_expr_array_lit(builder, expr, dst);
    }
    else if (type->kind == TYPE_STRUCT) {
        IR_emit_global_expr_struct_lit(builder, expr, dst);
    }
    else {
        assert(type->kind == TYPE_UNION);
        IR_emit_global_expr_union_lit(builder, expr, dst);
    }
}

static void IR_emit_global_expr(IR_VarBuilder* builder, Expr* expr, ConstExpr* dst)
{
    if (expr->is_constexpr && expr->is_imm) {
        assert(type_is_scalar(expr->type));
        dst->kind = CONST_EXPR_IMM;
        dst->type = expr->type;
        dst->imm = expr->imm;

        return;
    }

    switch (expr->kind) {
    case CST_ExprIdent:
        IR_emit_global_expr_ident(builder, (ExprIdent*)expr, dst);
        break;
    case CST_ExprCast:
        IR_emit_global_expr_cast(builder, (ExprCast*)expr, dst);
        break;
    case CST_ExprBinary:
        IR_emit_global_expr_binary(builder, (ExprBinary*)expr, dst);
        break;
    case CST_ExprUnary:
        IR_emit_global_expr_unary(builder, (ExprUnary*)expr, dst);
        break;
    case CST_ExprCompoundLit:
        IR_emit_global_expr_compound_lit(builder, (ExprCompoundLit*)expr, dst);
        break;
    case CST_ExprStr: {
        ExprStr* expr_str_lit = (ExprStr*)expr;
        StrLit* str_lit = expr_str_lit->str_lit;

        dst->kind = CONST_EXPR_STR_LIT;
        dst->type = expr_str_lit->super.type;
        dst->str_lit = str_lit;

        if (!str_lit->used) {
            str_lit->used = true;
            bucket_list_add_elem(builder->str_lits, str_lit);
        }

        break;
    }
    default:
        ftprint_err("[INTERNAL ERROR]: Unsupported expr kind %d during global var expr generation\n", expr->kind);
        assert(0);
        break;
    }
}
static void IR_build_var(IR_VarBuilder* builder, Symbol* sym)
{
    builder->curr_mod = sym->home;

    DeclVar* decl_var = (DeclVar*)sym->decl;
    ConstExpr const_expr = {.type = sym->type};

    if (decl_var->init) {
        IR_emit_global_expr(builder, decl_var->init, &const_expr);
    }

    sym->as_var.const_expr = const_expr;
}

static void IR_build_vars(Allocator* arena, Allocator* tmp_arena, BucketList* vars, BucketList* str_lits, TypeCache* type_cache)
{
    IR_VarBuilder builder = {.arena = arena, .tmp_arena = tmp_arena, .str_lits = str_lits, .type_cache = type_cache, .curr_mod = NULL};

    AllocatorState tmp_mem_state = allocator_get_state(builder.tmp_arena);

    // Iterate through all global variables and generate operands.
    size_t num_vars = vars->num_elems;

    for (size_t i = 0; i < num_vars; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(vars, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);
        assert(sym->kind == SYMBOL_VAR);

        IR_build_var(&builder, sym);
    }

    allocator_restore_state(tmp_mem_state);
}
