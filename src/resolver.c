#include "resolver.h"
#include "parser.h"

#define OP_FROM_EXPR(e)                                                                                                           \
    {                                                                                                                             \
        .type = (e)->type, .is_constexpr = (e)->is_constexpr, .is_lvalue = (e)->is_lvalue, .is_imm = (e)->is_imm, .imm = (e)->imm \
    }

#define OP_FROM_CONST(t, s)                                                               \
    {                                                                                     \
        .type = (t), .is_constexpr = true, .is_lvalue = false, .is_imm = true, .imm = (s) \
    }

typedef struct ExprOperand {
    Type* type;
    bool is_constexpr;
    bool is_lvalue;
    bool is_imm;
    Scalar imm;
} ExprOperand;

static Symbol* resolve_name(Resolver* resolver, Identifier* name);
static Symbol* resolve_export_name(Resolver* resolver, Identifier* name);
static bool resolve_symbol(Resolver* resolver, Symbol* sym);
static bool resolve_decl_var(Resolver* resolver, Symbol* sym);
static bool resolve_decl_const(Resolver* resolver, Symbol* sym);
static bool resolve_decl_proc(Resolver* resolver, Symbol* sym);
static bool resolve_global_proc_body(Resolver* resolver, Symbol* sym);

typedef struct CastResult {
    bool success;
    bool bad_lvalue;
} CastResult;

static void eop_array_decay(Resolver* resolver, ExprOperand* eop);
static void eop_array_slice_decay(ExprOperand* eop);
static CastResult cast_eop(Resolver* resolver, ExprOperand* eop, Type* type, bool forbid_rvalue_decay);
static CastResult convert_eop(Resolver* resolver, ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay);
static bool eop_is_null_ptr(Resolver* resolver, ExprOperand eop);
static CastResult can_convert_eop(Resolver* resolver, ExprOperand* operand, Type* dst_type, bool forbid_rvalue_decay);
static CastResult can_cast_eop(Resolver* resolver, ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay);
static Expr* try_wrap_cast_expr(Resolver* resolver, ExprOperand* eop, Expr* orig_expr);

static Symbol* lookup_ident(Resolver* resolver, NSIdent* ns_ident);

static bool resolve_expr(Resolver* resolver, Expr* expr, Type* expected_type);
static bool resolve_expr_int(Resolver* resolver, Expr* expr);
static void resolve_binary_eop(Resolver* resolver, TokenKind op, ExprOperand* dst, ExprOperand* left, ExprOperand* right);
static void resolve_unary_eop(Resolver* resolver, TokenKind op, ExprOperand* dst, ExprOperand* src);
static bool resolve_expr_binary(Resolver* resolver, Expr* expr);
static bool resolve_expr_unary(Resolver* resolver, Expr* expr);
static bool resolve_expr_call(Resolver* resolver, Expr* expr);
static bool resolve_expr_ident(Resolver* resolver, Expr* expr);
static bool resolve_cond_expr(Resolver* resolver, Expr* expr, ExprOperand* expr_eop);

static Type* resolve_typespec(Resolver* resolver, TypeSpec* typespec);

enum ResolveStmtRetFlags {
    RESOLVE_STMT_SUCCESS = 0x1,
    RESOLVE_STMT_RETURNS = 0x2,
    RESOLVE_STMT_LOOP_EXITS = 0x4,
};

enum ResolveStmtInFlags {
    RESOLVE_STMT_BREAK_CONTINUE_ALLOWED = 0x1,
};

static unsigned resolve_stmt(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_block(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_block_body(Resolver* resolver, List* stmts, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_do_while(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_while(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_if(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_cond_block(Resolver* resolver, IfCondBlock* cblock, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_expr_assign(Resolver* resolver, Stmt* stmt);

static ModuleState enter_module(Resolver* resolver, Module* mod);
static void exit_module(Resolver* resolver, ModuleState state);

static void set_scope(Resolver* resolver, Scope* scope);
static Scope* push_scope(Resolver* resolver, size_t num_syms);
static void pop_scope(Resolver* resolver);

static ModuleState enter_proc(Resolver* resolver, Symbol* sym);
static void exit_proc(Resolver* resolver, ModuleState state);

static void resolver_on_error(Resolver* resolver, ProgRange range, const char* format, ...)
{
    char buf[MAX_ERROR_LEN];
    size_t size = 0;
    va_list vargs;

    va_start(vargs, format);
    size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
    va_end(vargs);

    error_stream_add(&resolver->ctx->errors, range, buf, size > sizeof(buf) ? sizeof(buf) : size);
}

static void resolver_cast_error(Resolver* resolver, CastResult cast_res, ProgRange range, const char* err_prefix, Type* src_type,
                                Type* dst_type)
{
    assert(!cast_res.success);

    if (cast_res.bad_lvalue) {
        resolver_on_error(resolver, range, "%s: cannot convert a temporary (`%s`) to type `%s`.", err_prefix,
                          type_name(src_type), type_name(dst_type));
    }
    else {
        resolver_on_error(resolver, range, "%s: cannot convert `%s` to type `%s`.", err_prefix,
                          type_name(src_type), type_name(dst_type));
    }
}

static ModuleState enter_module(Resolver* resolver, Module* mod)
{
    ModuleState old_state = resolver->state;

    resolver->state.mod = mod;
    resolver->state.proc = NULL;
    resolver->state.scope = &mod->scope;

    return old_state;
}

static void exit_module(Resolver* resolver, ModuleState state)
{
    resolver->state = state;
}

static void set_scope(Resolver* resolver, Scope* scope)
{
    resolver->state.scope = scope;
}

static Scope* push_scope(Resolver* resolver, size_t num_syms)
{
    Scope* prev_scope = resolver->state.scope;
    Scope* scope = new_scope(&resolver->ctx->ast_mem, num_syms + num_syms);

    scope->parent = prev_scope;

    list_add_last(&prev_scope->children, &scope->lnode);
    set_scope(resolver, scope);

    return scope;
}

static void pop_scope(Resolver* resolver)
{
    resolver->state.scope = resolver->state.scope->parent;
}

static ModuleState enter_proc(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_PROC);
    ModuleState mod_state = enter_module(resolver, sym->home);

    DeclProc* dproc = (DeclProc*)(sym->decl);
    set_scope(resolver, dproc->scope);

    resolver->state.proc = sym;

    return mod_state;
}

static void exit_proc(Resolver* resolver, ModuleState state)
{
    pop_scope(resolver);
    resolver->state.proc = NULL;
    exit_module(resolver, state);
}

#define CASE_INT_CAST(k, o, t, f)                      \
    case k:                                            \
        switch (t) {                                   \
        case INTEGER_U8:                               \
            o->imm.as_int._u8 = (u8)o->imm.as_int.f;   \
            break;                                     \
        case INTEGER_S8:                               \
            o->imm.as_int._s8 = (s8)o->imm.as_int.f;   \
            break;                                     \
        case INTEGER_U16:                              \
            o->imm.as_int._u16 = (u16)o->imm.as_int.f; \
            break;                                     \
        case INTEGER_S16:                              \
            o->imm.as_int._s16 = (s16)o->imm.as_int.f; \
            break;                                     \
        case INTEGER_U32:                              \
            o->imm.as_int._u32 = (u32)o->imm.as_int.f; \
            break;                                     \
        case INTEGER_S32:                              \
            o->imm.as_int._s32 = (s32)o->imm.as_int.f; \
            break;                                     \
        case INTEGER_U64:                              \
            o->imm.as_int._u64 = (u64)o->imm.as_int.f; \
            break;                                     \
        case INTEGER_S64:                              \
            o->imm.as_int._s64 = (s64)o->imm.as_int.f; \
            break;                                     \
        default:                                       \
            o->is_constexpr = false;                   \
            assert(0);                                 \
            break;                                     \
        }                                              \
        break;

static CastResult cast_eop(Resolver* resolver, ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay)
{
    Type* src_type = eop->type;

    CastResult r = can_cast_eop(resolver, eop, dst_type, forbid_rvalue_decay);

    if (!r.success)
        return r;

    // From this point, the following is true:
    // 1) src_type != dst_type
    // 2) types are castable.

    if (eop->is_constexpr && eop->is_imm) {
        if (src_type->kind == TYPE_FLOAT) {
            eop->is_constexpr = dst_type->kind != TYPE_INTEGER;
        }
        else {
            IntegerKind src_int_kind;
            IntegerKind dst_int_kind;

            if (src_type->kind == TYPE_ENUM) {
                src_int_kind = src_type->as_enum.base->as_integer.kind;
            }
            else if (src_type->kind == TYPE_PTR) {
                src_int_kind = INTEGER_U64;
            }
            else {
                src_int_kind = src_type->as_integer.kind;
            }

            if (dst_type->kind == TYPE_ENUM) {
                dst_int_kind = dst_type->as_enum.base->as_integer.kind;
            }
            else if (dst_type->kind == TYPE_PTR) {
                dst_int_kind = INTEGER_U64;
            }
            else {
                dst_int_kind = dst_type->as_integer.kind;
            }


            switch (src_int_kind) {
                CASE_INT_CAST(INTEGER_U8, eop, dst_int_kind, _u8)
                CASE_INT_CAST(INTEGER_S8, eop, dst_int_kind, _s8)
                CASE_INT_CAST(INTEGER_U16, eop, dst_int_kind, _u16)
                CASE_INT_CAST(INTEGER_S16, eop, dst_int_kind, _s16)
                CASE_INT_CAST(INTEGER_U32, eop, dst_int_kind, _u32)
                CASE_INT_CAST(INTEGER_S32, eop, dst_int_kind, _s32)
                CASE_INT_CAST(INTEGER_U64, eop, dst_int_kind, _u64)
                CASE_INT_CAST(INTEGER_S64, eop, dst_int_kind, _s64)
            default:
                eop->is_constexpr = false;
                assert(0);
                break;
            }
        }
    }

    eop->type = dst_type;
    eop->is_lvalue = false;

    return r;
}

static void eop_array_decay(Resolver* resolver, ExprOperand* eop)
{
    assert(eop->type->kind == TYPE_ARRAY);

    eop->type = type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, eop->type->as_array.base);
    eop->is_lvalue = false;
}

static void eop_array_slice_decay(ExprOperand* eop)
{
    assert(type_is_slice(eop->type));

    TypeAggregateField* data_field = get_type_struct_field(eop->type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA]);

    eop->type = data_field->type;
    eop->is_lvalue = false;
}

static CastResult convert_eop(Resolver* resolver, ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay)
{
    CastResult r = can_convert_eop(resolver, eop, dst_type, forbid_rvalue_decay);

    if (!r.success)
        return r;

    cast_eop(resolver, eop, dst_type, forbid_rvalue_decay);

    eop->is_lvalue = false;

    return r;
}

static bool eop_is_null_ptr(Resolver* resolver, ExprOperand eop)
{
    Type* type = eop.type;

    if (eop.is_constexpr && eop.is_imm && (type->kind == TYPE_INTEGER || type->kind == TYPE_PTR)) {
        CastResult r = cast_eop(resolver, &eop, builtin_types[BUILTIN_TYPE_U64].type, false);

        assert(r.success);

        return eop.imm.as_int._u64 == 0;
    }

    return false;
}

static CastResult can_convert_eop(Resolver* resolver, ExprOperand* operand, Type* dst_type, bool forbid_rvalue_decay)
{
    bool convertible = false;
    bool bad_lvalue = false;
    Type* src_type = operand->type;

    // Same types
    if (dst_type == src_type) {
        convertible = true;
    }
    // Can convert between arithmetic types
    else if (type_is_arithmetic(dst_type) && type_is_arithmetic(src_type)) {
        convertible = true;
    }
    // Can convert const NULL (or 0) to a ptr (or proc ptr).
    else if (type_is_ptr_like(dst_type) && eop_is_null_ptr(resolver, *operand)) {
        convertible = true;
    }
    // Can decay an array to a pointer.
    else if ((dst_type->kind == TYPE_PTR) && (src_type->kind == TYPE_ARRAY)) {
        Type* ptr_base = dst_type->as_ptr.base;
        Type* arr_base = src_type->as_array.base;

        convertible = (ptr_base == builtin_types[BUILTIN_TYPE_VOID].type) || (ptr_base == arr_base);
        bad_lvalue = !operand->is_lvalue && forbid_rvalue_decay;
    }
    // Can decay an array slice to a pointer.
    else if ((dst_type->kind == TYPE_PTR) && type_is_slice(src_type)) {
        Type* ptr_base = dst_type->as_ptr.base;
        TypeAggregateField* data_field = get_type_struct_field(src_type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA]);

        convertible = (ptr_base == builtin_types[BUILTIN_TYPE_VOID].type) || (data_field->type == dst_type);
        bad_lvalue = !operand->is_lvalue && forbid_rvalue_decay;
    }
    // Can convert an array into an array slice.
    else if (type_is_slice(dst_type) && (src_type->kind == TYPE_ARRAY)) {
        convertible = slice_and_array_compatible(src_type, dst_type);
        bad_lvalue = !operand->is_lvalue && forbid_rvalue_decay;
    }
    else if ((dst_type->kind == TYPE_PTR) && (src_type->kind == TYPE_PTR)) {
        Type* dst_pointed_type = dst_type->as_ptr.base;
        Type* src_pointed_type = src_type->as_ptr.base;

        // Can convert a "derived" type to a "base" type.
        // A type is "derived" if its first field is of type "base".
        // Ex: struct Base { ... };  struct Derived { Base base;};
        // Derived* d = malloc(...);
        // Base* b = d;
        if (type_is_aggregate(dst_pointed_type) && (src_pointed_type->kind == TYPE_STRUCT) &&
            (dst_pointed_type == src_pointed_type->as_struct.body.fields[0].type)) {
            convertible = true;
        }
        // Can convert if either is a void*
        else if ((dst_pointed_type == builtin_types[BUILTIN_TYPE_VOID].type) ||
                 (src_pointed_type == builtin_types[BUILTIN_TYPE_VOID].type)) {
            convertible = true;
        }
    }

    CastResult r = {.success = convertible && !bad_lvalue, .bad_lvalue = bad_lvalue};

    return r;
}

static CastResult can_cast_eop(Resolver* resolver, ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay)
{
    Type* src_type = eop->type;

    CastResult r = can_convert_eop(resolver, eop, dst_type, forbid_rvalue_decay);

    if (r.success || r.bad_lvalue) {
        return r;
    }

    if (dst_type->kind == TYPE_INTEGER) {
        r.success = type_is_ptr_like(src_type);
    }
    else if (src_type->kind == TYPE_INTEGER) {
        r.success = type_is_ptr_like(dst_type);
    }
    else if (type_is_ptr_like(dst_type) && type_is_ptr_like(src_type)) {
        r.success = true;
    }

    return r;
}

static void promote_int_eops(Resolver* resolver, ExprOperand* eop)
{
    switch (eop->type->kind) {
    case TYPE_INTEGER:
    case TYPE_ENUM:
        if (eop->type->size < builtin_types[BUILTIN_TYPE_S32].type->size)
            cast_eop(resolver, eop, builtin_types[BUILTIN_TYPE_S32].type, false);
        break;
    default:
        break;
    }
}

static void convert_arith_eops(Resolver* resolver, ExprOperand* left, ExprOperand* right)
{
    // If one is an f64, cast the other to f64.
    if (left->type == builtin_types[BUILTIN_TYPE_F64].type) {
        cast_eop(resolver, right, builtin_types[BUILTIN_TYPE_F64].type, false);
    }
    else if (right->type == builtin_types[BUILTIN_TYPE_F64].type) {
        cast_eop(resolver, left, builtin_types[BUILTIN_TYPE_F64].type, false);
    }
    // Else if one is an f32, cast the other to f32.
    else if (left->type == builtin_types[BUILTIN_TYPE_F32].type) {
        cast_eop(resolver, right, builtin_types[BUILTIN_TYPE_F32].type, false);
    }
    else if (right->type == builtin_types[BUILTIN_TYPE_F32].type) {
        cast_eop(resolver, left, builtin_types[BUILTIN_TYPE_F32].type, false);
    }
    // Else, do usual arithmetic conversions.
    else {
        assert(type_is_integer_like(left->type));
        assert(type_is_integer_like(right->type));

        // First, promote both to s32 if smaller than s32.
        // This is a lossless conversion.
        promote_int_eops(resolver, left);
        promote_int_eops(resolver, right);

        // Collapse enum types to their base types.
        if (left->type->kind == TYPE_ENUM) {
            left->type = left->type->as_enum.base;
        }

        if (right->type->kind == TYPE_ENUM) {
            right->type = right->type->as_enum.base;
        }

        if (left->type != right->type) {
            assert(left->type->kind == TYPE_INTEGER && right->type->kind == TYPE_INTEGER);
            TypeInteger* left_as_int = &left->type->as_integer;
            TypeInteger* right_as_int = &right->type->as_integer;

            bool left_signed = left_as_int->is_signed;
            bool right_signed = right_as_int->is_signed;
            int left_rank = type_integer_ranks[left_as_int->kind];
            int right_rank = type_integer_ranks[right_as_int->kind];
            size_t left_size = left->type->size;
            size_t right_size = right->type->size;

            if (left_signed == right_signed) {
                if (left_rank <= right_rank)
                    cast_eop(resolver, left, right->type, false);
                else
                    cast_eop(resolver, right, left->type, false);
            }
            else if (left_signed && (right_rank >= left_rank)) {
                cast_eop(resolver, left, right->type, false);
            }
            else if (right_signed && (left_rank >= right_rank)) {
                cast_eop(resolver, right, left->type, false);
            }
            else if (left_signed && (left_size > right_size)) {
                cast_eop(resolver, right, left->type, false);
            }
            else if (right_signed && (right_size > left_size)) {
                cast_eop(resolver, left, right->type, false);
            }
            else {
                // NOTE: This shouldn't happen for us since we're using fixed-sized types (i.e., u32, s32, u64, etc.)
                // with ranks such that a greater rank implies a larger size.
                //
                // But, I'll leave this here in case I decided to use C's type system later.
                //
                // NOTE: In C, this happens if, for example:
                // 1) short is the same size as int
                // 2) left is int.
                // 3) right is unsigned short.
                //
                // Said another way, this can only occur if a greater rank does not imply a greater size.

                Type* signed_type = left_signed ? left->type : right->type;
                Type* type = type_unsigned_int(signed_type);

                cast_eop(resolver, left, type, false);
                cast_eop(resolver, right, type, false);

                assert(!"We shouldn't reach this code path!!! Usual arithmetic conversions code");
            }
        }
    }

    assert(left->type == right->type);
}

static s64 eval_unary_op_s64(TokenKind op, s64 val)
{
    switch (op) {
    case TKN_PLUS:
        return +val;
    case TKN_MINUS:
        return -val;
    case TKN_NEG:
        return ~val;
    case TKN_NOT:
        return !val;
    default:
        ftprint_err("Unexpected unary op (s64): %d\n", op);
        assert(0);
        break;
    }

    return 0;
}

static u64 eval_unary_op_u64(TokenKind op, u64 val)
{
    switch (op) {
    case TKN_PLUS:
        return +val;
    case TKN_MINUS:
        return -val;
    case TKN_NEG:
        return ~val;
    case TKN_NOT:
        return !val;
    default:
        ftprint_err("Unexpected unary op (s64): %d\n", op);
        assert(0);
        break;
    }

    return 0;
}

static s64 eval_binary_op_s64(TokenKind op, s64 left, s64 right)
{
    switch (op) {
    case TKN_PLUS: // Add
        return left + right;
    case TKN_MINUS: // Subtract
        return left - right;
    case TKN_ASTERISK: // Multiply
        return left * right;
    case TKN_DIV: // Divide
        return right != 0 ? left / right : 0;
    case TKN_MOD: // Modulo (remainder)
        return right != 0 ? left % right : 0;
    case TKN_LSHIFT:
        return left << right;
    case TKN_RSHIFT:
        return left >> right;
    case TKN_AND:
        return left & right;
    case TKN_OR:
        return left | right;
    case TKN_CARET: // xor
        return left ^ right;
    case TKN_LOGIC_AND:
        return left && right;
    case TKN_LOGIC_OR:
        return left || right;
    case TKN_EQ:
        return left == right;
    case TKN_NOTEQ:
        return left != right;
    case TKN_GT:
        return left > right;
    case TKN_GTEQ:
        return left >= right;
    case TKN_LT:
        return left < right;
    case TKN_LTEQ:
        return left <= right;
    default:
        ftprint_err("Unexpected binary op (s64): %d\n", op);
        assert(0);
        break;
    }

    return 0;
}

static u64 eval_binary_op_u64(TokenKind op, u64 left, u64 right)
{
    switch (op) {
    case TKN_PLUS: // Add
        return left + right;
    case TKN_MINUS: // Subtract
        return left - right;
    case TKN_ASTERISK: // Multiply
        return left * right;
    case TKN_DIV: // Divide
        return right != 0 ? left / right : 0;
    case TKN_MOD: // Modulo (remainder)
        return right != 0 ? left % right : 0;
    case TKN_LSHIFT:
        return left << right;
    case TKN_RSHIFT:
        return left >> right;
    case TKN_AND:
        return left & right;
    case TKN_OR:
        return left | right;
    case TKN_CARET: // xor
        return left ^ right;
    case TKN_LOGIC_AND:
        return left && right;
    case TKN_LOGIC_OR:
        return left || right;
    case TKN_EQ:
        return left == right;
    case TKN_NOTEQ:
        return left != right;
    case TKN_GT:
        return left > right;
    case TKN_GTEQ:
        return left >= right;
    case TKN_LT:
        return left < right;
    case TKN_LTEQ:
        return left <= right;
    default:
        ftprint_err("Unexpected binary op (u64): %d\n", op);
        assert(0);
        break;
    }

    return 0;
}

static void eval_const_binary_op(Resolver* resolver, TokenKind op, ExprOperand* dst, Type* type, Scalar left, Scalar right)
{
    if (type_is_integer_like(type)) {
        ExprOperand left_eop = OP_FROM_CONST(type, left);
        ExprOperand right_eop = OP_FROM_CONST(type, right);
        bool is_signed = type_is_signed(type);

        // Compute the operation in the largest type available.
        if (is_signed) {
            cast_eop(resolver, &left_eop, builtin_types[BUILTIN_TYPE_S64].type, false);
            cast_eop(resolver, &right_eop, builtin_types[BUILTIN_TYPE_S64].type, false);

            s64 r = eval_binary_op_s64(op, left_eop.imm.as_int._s64, right_eop.imm.as_int._s64);

            dst->type = builtin_types[BUILTIN_TYPE_S64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._s64 = r;
        }
        else {
            cast_eop(resolver, &left_eop, builtin_types[BUILTIN_TYPE_U64].type, false);
            cast_eop(resolver, &right_eop, builtin_types[BUILTIN_TYPE_U64].type, false);

            u64 r = eval_binary_op_u64(op, left_eop.imm.as_int._u64, right_eop.imm.as_int._u64);

            dst->type = builtin_types[BUILTIN_TYPE_U64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._u64 = r;
        }

        // Cast it back to the original type.
        cast_eop(resolver, dst, type, false);
    }
    else {
        assert(type->kind == TYPE_FLOAT);
    }
}

static void eval_const_unary_op(Resolver* resolver, TokenKind op, ExprOperand* dst, Type* type, Scalar val)
{
    if (type_is_integer_like(type)) {
        ExprOperand val_eop = OP_FROM_CONST(type, val);
        bool is_signed = type_is_signed(type);

        // Compute the operation in the largest type available.
        if (is_signed) {
            cast_eop(resolver, &val_eop, builtin_types[BUILTIN_TYPE_S64].type, false);

            dst->type = builtin_types[BUILTIN_TYPE_S64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._s64 = eval_unary_op_s64(op, val_eop.imm.as_int._s64);
        }
        else {
            cast_eop(resolver, &val_eop, builtin_types[BUILTIN_TYPE_U64].type, false);

            dst->type = builtin_types[BUILTIN_TYPE_U64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._u64 = eval_unary_op_u64(op, val_eop.imm.as_int._u64);
        }

        // Cast it back to the original type.
        cast_eop(resolver, dst, type, false);
    }
    else {
        assert(type->kind == TYPE_FLOAT);
    }
}

static bool try_complete_aggregate_type(Resolver* resolver, Type* type);

typedef struct SeenField {
    Identifier* name;
    ProgRange range;
} SeenField;

// NOTE: Returned object is a stretchy buffer allocated with temporary memory.
// On error, NULL is returned.
static TypeAggregateField* resolve_aggregate_fields(Resolver* resolver, List* in_fields)
{
    TypeAggregateField* out_fields = array_create(&resolver->ctx->tmp_mem, TypeAggregateField, 16);
    HMap seen_fields = hmap(3, &resolver->ctx->tmp_mem); // Used to check for duplicate field names

    List* head = in_fields;

    for (List* it = head->next; it != head; it = it->next) {
        AggregateField* field = list_entry(it, AggregateField, lnode);

        // Check for duplicate field names.
        if (field->name) {
            SeenField* seen_field = hmap_get_obj(&seen_fields, PTR_UINT(field->name));

            if (seen_field) {
                assert(seen_field->name == field->name);
                resolver_on_error(resolver, seen_field->range, "Duplicate member field name `%s`.", field->name->str);
                return NULL;
            }

            seen_field = alloc_type(&resolver->ctx->tmp_mem, SeenField, false);
            seen_field->name = field->name;
            seen_field->range = field->range;

            hmap_put(&seen_fields, PTR_UINT(field->name), PTR_UINT(seen_field));
        }

        // Resolve field's type.
        Type* field_type = resolve_typespec(resolver, field->typespec);

        if (!try_complete_aggregate_type(resolver, field_type)) {
            return NULL;
        }

        if (type_has_incomplete_array(field_type)) {
            resolver_on_error(resolver, field->range, "Member field has an array type of unknown size");
            return NULL;
        }

        if (field_type->size == 0) {
            resolver_on_error(resolver, field->range, "Member field has zero size");
            return NULL;
        }

        TypeAggregateField field_type_info = {.type = field_type, .name = field->name};

        array_push(out_fields, field_type_info);
    }

    return out_fields;
}

static bool complete_aggregate_type(Resolver* resolver, Type* type, DeclAggregate* decl_aggregate)
{
    AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);

    // Resolve each parsed aggregate field into a field type.
    TypeAggregateField* field_types = resolve_aggregate_fields(resolver, &decl_aggregate->fields);

    if (!field_types) {
        return false;
    }

    // Call the appropriate procedure to fill in the aggregate type's size and alignment.
    if (decl_aggregate->super.kind == CST_DeclStruct) {
        complete_struct_type(&resolver->ctx->ast_mem, type, array_len(field_types), field_types);
    }
    else if (decl_aggregate->super.kind == CST_DeclUnion) {
        complete_union_type(&resolver->ctx->ast_mem, type, array_len(field_types), field_types);
    }

    assert(type->kind != TYPE_INCOMPLETE_AGGREGATE);

    allocator_restore_state(mem_state);

    return true;
}

static bool try_complete_aggregate_type(Resolver* resolver, Type* type)
{
    if (type->kind != TYPE_INCOMPLETE_AGGREGATE) {
        return true;
    }

    Symbol* sym = type->as_incomplete.sym;

    if (type->as_incomplete.is_completing) {
        resolver_on_error(resolver, sym->decl->range, "Cannot resolve type `%s` due to cyclic dependency", sym->name->str);
        return false;
    }

    assert(sym->decl->kind == CST_DeclStruct || sym->decl->kind == CST_DeclUnion);

    bool success;
    DeclAggregate* decl_aggregate = (DeclAggregate*)sym->decl;

    ModuleState mod_state = enter_module(resolver, sym->home);
    {
        type->as_incomplete.is_completing = true; // Mark as `completing` to detect dependency cycles.
        success = complete_aggregate_type(resolver, type, decl_aggregate);
    }
    exit_module(resolver, mod_state);

    return success;
}

static Type* get_int_lit_type(u64 value, Type** types, u32 num_types)
{
    assert(num_types);
    Type* type = types[0];

    for (u32 i = 1; i < num_types; i += 1) {
        if (value > type->as_integer.max) {
            type = types[i];
        }
        else {
            return type;
        }
    }

    return (value > type->as_integer.max) ? NULL : type;
}

static bool resolve_expr_int(Resolver* resolver, Expr* expr)
{
    ExprInt* eint = (ExprInt*)expr;
    Type* type = builtin_types[BUILTIN_TYPE_ULLONG].type;

    // Based on integer constant semantics from the C specification (ISO/IEC 9899:TC3)
    u64 value = eint->token.value;
    TokenIntRep rep = eint->token.rep;
    TokenIntSuffix suffix = eint->token.suffix;

    if (rep == TKN_INT_CHAR) {
        type = builtin_types[BUILTIN_TYPE_CHAR].type; // Differs from C spec, where a char literal is an int.
    }
    else if (rep == TKN_INT_DEC) {
        switch (suffix) {
        case TKN_INT_SUFFIX_NONE: {
            Type* types[] = {
                builtin_types[BUILTIN_TYPE_INT].type,
                builtin_types[BUILTIN_TYPE_LONG].type,
                builtin_types[BUILTIN_TYPE_LLONG].type,
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_U: {
            Type* types[] = {builtin_types[BUILTIN_TYPE_UINT].type, builtin_types[BUILTIN_TYPE_ULONG].type,
                             builtin_types[BUILTIN_TYPE_ULLONG].type};

            type = get_int_lit_type(value, types, ARRAY_LEN(types));
            break;
        }
        case TKN_INT_SUFFIX_L: {
            Type* types[] = {
                builtin_types[BUILTIN_TYPE_LONG].type,
                builtin_types[BUILTIN_TYPE_LLONG].type,
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));
            break;
        }
        case TKN_INT_SUFFIX_UL: {
            Type* types[] = {builtin_types[BUILTIN_TYPE_ULONG].type, builtin_types[BUILTIN_TYPE_ULLONG].type};

            type = get_int_lit_type(value, types, ARRAY_LEN(types));
            break;
        }
        case TKN_INT_SUFFIX_LL: {
            type = builtin_types[BUILTIN_TYPE_LLONG].type;

            if (value > type->as_integer.max) {
                type = NULL;
            }
            break;
        }
        case TKN_INT_SUFFIX_ULL: {
            type = builtin_types[BUILTIN_TYPE_ULLONG].type;

            if (value > type->as_integer.max) {
                type = NULL;
            }
            break;
        }
        default:
            assert(0);
            break;
        }
    }
    else {
        // NOTE: An integer literal specified in a different base (e.g., hex).

        switch (suffix) {
        case TKN_INT_SUFFIX_NONE: {
            Type* types[] = {builtin_types[BUILTIN_TYPE_INT].type,   builtin_types[BUILTIN_TYPE_UINT].type,
                             builtin_types[BUILTIN_TYPE_LONG].type,  builtin_types[BUILTIN_TYPE_ULONG].type,
                             builtin_types[BUILTIN_TYPE_LLONG].type, builtin_types[BUILTIN_TYPE_ULLONG].type};

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_U: {
            Type* types[] = {builtin_types[BUILTIN_TYPE_UINT].type, builtin_types[BUILTIN_TYPE_ULONG].type,
                             builtin_types[BUILTIN_TYPE_ULLONG].type};

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_L: {
            Type* types[] = {builtin_types[BUILTIN_TYPE_LONG].type, builtin_types[BUILTIN_TYPE_ULONG].type,
                             builtin_types[BUILTIN_TYPE_LLONG].type, builtin_types[BUILTIN_TYPE_ULLONG].type};

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_UL: {
            Type* types[] = {builtin_types[BUILTIN_TYPE_ULONG].type, builtin_types[BUILTIN_TYPE_ULLONG].type};

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_LL: {
            Type* types[] = {builtin_types[BUILTIN_TYPE_LLONG].type, builtin_types[BUILTIN_TYPE_ULLONG].type};

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_ULL: {
            type = builtin_types[BUILTIN_TYPE_ULLONG].type;

            if (value > type->as_integer.max) {
                type = NULL;
            }
            break;
        }
        default:
            assert(0);
            break;
        }
    }

    // TODO: Can this ever happen with this jacked-up mixing of host and target types???
    if (!type) {
        resolver_on_error(resolver, expr->range, "Integer literal `%llu` is too large", value);
        return false;
    }

    expr->type = type;
    expr->is_constexpr = true;
    expr->is_imm = true;
    expr->is_lvalue = false;
    expr->imm.as_int._u64 = value;

    return true;
}

static bool resolve_expr_sizeof(Resolver* resolver, ExprSizeof* expr)
{
    Type* type = resolve_typespec(resolver, expr->typespec);

    if (!type) {
        return false;
    }

    if (!try_complete_aggregate_type(resolver, type)) {
        return false;
    }

    expr->super.type = builtin_types[BUILTIN_TYPE_USIZE].type;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_int._u64 = type->size;

    return true;
}

static bool resolve_expr_typeid(Resolver* resolver, ExprTypeid* expr)
{
    Type* type = resolve_typespec(resolver, expr->typespec);

    if (!type) {
        return false;
    }

    expr->super.type = builtin_types[BUILTIN_TYPE_USIZE].type;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_int._u64 = type->id;

    return true;
}

static bool resolve_expr_offsetof(Resolver* resolver, ExprOffsetof* expr)
{
    Type* obj_type = resolve_typespec(resolver, expr->obj_ts);

    if (!obj_type) {
        return false;
    }

    if (!try_complete_aggregate_type(resolver, obj_type)) {
        return false;
    }

    if (!type_is_aggregate(obj_type)) {
        resolver_on_error(resolver, expr->obj_ts->range, "First argument of #offsetof must be an aggregate type.");
        return false;
    }

    TypeAggregateField* field = get_type_aggregate_field(obj_type, expr->field_ident);

    if (!field) {
        // TODO: Range on identifier
        resolver_on_error(resolver, expr->super.range, "Type `%s` does not have a field named `%s`.", type_name(obj_type),
                          expr->field_ident->str);
        return false;
    }

    expr->super.type = builtin_types[BUILTIN_TYPE_USIZE].type;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_int._u64 = field->offset;

    return true;
}

static bool resolve_expr_indexof(Resolver* resolver, ExprIndexof* expr)
{
    Type* obj_type = resolve_typespec(resolver, expr->obj_ts);

    if (!obj_type) {
        return false;
    }

    if (!try_complete_aggregate_type(resolver, obj_type)) {
        return false;
    }

    if (!type_is_aggregate(obj_type)) {
        resolver_on_error(resolver, expr->obj_ts->range, "First argument of #indexof must be an aggregate type.");
        return false;
    }

    TypeAggregateField* field = get_type_aggregate_field(obj_type, expr->field_ident);

    if (!field) {
        // TODO: Range on identifier
        resolver_on_error(resolver, expr->super.range, "Type `%s` does not have a field named `%s`.", type_name(obj_type),
                          expr->field_ident->str);
        return false;
    }

    expr->super.type = builtin_types[BUILTIN_TYPE_USIZE].type;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_int._u64 = field->index;

    return true;
}

static bool resolve_expr_length(Resolver* resolver, ExprLength* expr)
{
    if (!resolve_expr(resolver, expr->arg, NULL)) {
        return false;
    }

    Type* type = expr->arg->type;

    if (type->kind != TYPE_ARRAY) {
        resolver_on_error(resolver, expr->arg->range, "The argument of #len must be an array type.");
        return false;
    }

    expr->super.type = builtin_types[BUILTIN_TYPE_USIZE].type;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_int._u64 = type->as_array.len;

    return true;
}

static bool resolve_expr_str(Resolver* resolver, ExprStr* expr)
{
    expr->super.type = type_array(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.arrays, builtin_types[BUILTIN_TYPE_CHAR].type,
                                  expr->str_lit->len + 1);
    expr->super.is_constexpr = true;
    expr->super.is_imm = false;
    expr->super.is_lvalue = true;

    return true;
}

static bool resolve_ptr_int_arith(Resolver* resolver, ExprOperand* dst, ExprOperand* ptr, ExprOperand* int_eop)
{
    // Convert ^void to ^s8
    if (ptr->type->as_ptr.base == builtin_types[BUILTIN_TYPE_VOID].type) {
        ptr->type = type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, builtin_types[BUILTIN_TYPE_S8].type);
    }
    // Ensure base pointer type has non-zero size
    else if (ptr->type->as_ptr.base->size == 0) {
        return false;
    }

    cast_eop(resolver, int_eop, builtin_types[BUILTIN_TYPE_U64].type, false);

    if (ptr->is_constexpr && ptr->is_imm && int_eop->is_constexpr && int_eop->is_imm) {
        dst->is_constexpr = true;
        dst->is_imm = true;
        dst->is_lvalue = false;
        dst->imm.as_int._u64 = ptr->imm.as_int._u64 + (int_eop->imm.as_int._u64 * ptr->type->as_ptr.base->size);
    }
    else {
        dst->is_constexpr = ptr->is_constexpr && int_eop->is_constexpr;
        dst->is_imm = false;
        dst->is_lvalue = false;
    }

    dst->type = ptr->type;

    return true;
}

static void resolve_binary_eop(Resolver* resolver, TokenKind op, ExprOperand* dst, ExprOperand* left, ExprOperand* right)
{
    convert_arith_eops(resolver, left, right);

    if (left->is_constexpr && right->is_constexpr) {
        assert(left->is_imm && right->is_imm);
        eval_const_binary_op(resolver, op, dst, left->type, left->imm, right->imm);
    }
    else {
        dst->type = left->type;
        dst->is_constexpr = false;
        dst->is_imm = false;
        dst->is_lvalue = false;
    }
}

static bool resolve_expr_binary(Resolver* resolver, Expr* expr)
{
    ExprBinary* ebinary = (ExprBinary*)expr;

    if (!resolve_expr(resolver, ebinary->left, NULL))
        return false;

    if (!resolve_expr(resolver, ebinary->right, NULL))
        return false;

    ExprOperand dst_op = {0};
    ExprOperand left_op = OP_FROM_EXPR(ebinary->left);
    ExprOperand right_op = OP_FROM_EXPR(ebinary->right);

    switch (ebinary->op) {
    case TKN_PLUS:
        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(resolver, TKN_PLUS, &dst_op, &left_op, &right_op);
        }
        else if ((left_op.type->kind == TYPE_PTR) && type_is_integer_like(right_op.type)) {
            if (!try_complete_aggregate_type(resolver, left_op.type->as_ptr.base)) {
                return false;
            }

            if (!resolve_ptr_int_arith(resolver, &dst_op, &left_op, &right_op)) {
                resolver_on_error(resolver, ebinary->left->range, "Cannot add to a pointer with a base type (%s) of zero size",
                                  type_name(left_op.type->as_ptr.base));

                return false;
            }
        }
        else if (type_is_integer_like(left_op.type) && (right_op.type->kind == TYPE_PTR)) {
            if (!try_complete_aggregate_type(resolver, right_op.type->as_ptr.base)) {
                return false;
            }

            if (!resolve_ptr_int_arith(resolver, &dst_op, &right_op, &left_op)) {
                resolver_on_error(resolver, ebinary->right->range, "Cannot add to a pointer with a base type (%s) of zero size",
                                  type_name(right_op.type->as_ptr.base));

                return false;
            }
        }
        else {
            resolver_on_error(resolver, expr->range, "Can only add arithmetic and pointer types");
            return false;
        }

        break;
    case TKN_MINUS: {
        bool left_is_ptr = (left_op.type->kind == TYPE_PTR);
        bool right_is_ptr = (right_op.type->kind == TYPE_PTR);

        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(resolver, TKN_MINUS, &dst_op, &left_op, &right_op);
        }
        // ptr - int
        else if (left_is_ptr && type_is_integer_like(right_op.type)) {
            if (!try_complete_aggregate_type(resolver, left_op.type->as_ptr.base)) {
                return false;
            }

            if (!resolve_ptr_int_arith(resolver, &dst_op, &left_op, &right_op)) {
                resolver_on_error(resolver, ebinary->left->range, "Cannot subtract from a pointer with a base type (%s) of zero size",
                                  type_name(left_op.type->as_ptr.base));

                return false;
            }
        }
        // ptr - ptr
        else if (left_is_ptr && right_is_ptr) {
            Type* left_base_type = left_op.type->as_ptr.base;
            Type* right_base_type = right_op.type->as_ptr.base;

            if (!try_complete_aggregate_type(resolver, left_base_type) || !try_complete_aggregate_type(resolver, right_base_type)) {
                return false;
            }

            if ((left_base_type == builtin_types[BUILTIN_TYPE_VOID].type) &&
                (right_base_type == builtin_types[BUILTIN_TYPE_VOID].type)) {
                left_op.type = type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, builtin_types[BUILTIN_TYPE_S8].type);
                right_op.type =
                    type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, builtin_types[BUILTIN_TYPE_S8].type);
            }
            else if ((left_base_type == builtin_types[BUILTIN_TYPE_VOID].type) &&
                     (right_base_type == builtin_types[BUILTIN_TYPE_S8].type)) {
                left_op.type = type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, builtin_types[BUILTIN_TYPE_S8].type);
            }
            else if ((left_base_type == builtin_types[BUILTIN_TYPE_S8].type) &&
                     (right_base_type == builtin_types[BUILTIN_TYPE_VOID].type)) {
                right_op.type =
                    type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, builtin_types[BUILTIN_TYPE_S8].type);
            }

            if (left_op.type != right_op.type) {
                resolver_on_error(resolver, expr->range, "Cannot subtract pointers of different types: `^%s` - `^%s`",
                                  type_name(left_base_type), type_name(right_base_type));
                return false;
            }

            if (left_op.is_constexpr && left_op.is_imm && right_op.is_constexpr && right_op.is_imm) {
                u64 base_size = left_base_type->size;
                u32 base_size_log2 = (u32)clp2(base_size);

                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = (left_op.imm.as_int._u64 - right_op.imm.as_int._u64) >> base_size_log2;
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr && right_op.is_constexpr;
            }

            dst_op.type = builtin_types[BUILTIN_TYPE_S64].type;
        }
        else {
            resolver_on_error(resolver, expr->range, "Can only subtract arithmetic types, pointers, and integers from pointers");
            return false;
        }

        break;
    }
    case TKN_DIV:
    case TKN_MOD:
    case TKN_ASTERISK:
        if (!type_is_arithmetic(left_op.type)) {
            resolver_on_error(resolver, ebinary->left->range,
                              "Left operand of binary operator `%s` must be an arithmetic type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(left_op.type));
            return false;
        }

        if (!type_is_arithmetic(right_op.type)) {
            resolver_on_error(resolver, ebinary->right->range,
                              "Right operand of binary operator `%s` must be an arithmetic type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(right_op.type));
            return false;
        }

        resolve_binary_eop(resolver, ebinary->op, &dst_op, &left_op, &right_op);

        break;
    case TKN_DIVMOD:
        if (!type_is_integer_like(left_op.type)) {
            resolver_on_error(resolver, ebinary->left->range,
                              "Left operand of binary operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(left_op.type));
            return false;
        }

        if (!type_is_integer_like(right_op.type)) {
            resolver_on_error(resolver, ebinary->right->range,
                              "Right operand of binary operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(right_op.type));
            return false;
        }

        convert_arith_eops(resolver, &left_op, &right_op);

        dst_op.type = type_array(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.arrays, left_op.type, 2);
        dst_op.is_constexpr = left_op.is_constexpr && right_op.is_constexpr;
        dst_op.is_imm = false;
        dst_op.is_lvalue = false;

        break;
    case TKN_RSHIFT:
    case TKN_LSHIFT: {
        if (left_op.type->kind != TYPE_INTEGER) {
            resolver_on_error(resolver, ebinary->left->range,
                              "Left operand of binary operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(left_op.type));
            return false;
        }

        if (right_op.type->kind != TYPE_INTEGER) {
            resolver_on_error(resolver, ebinary->right->range,
                              "Right operand of binary operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(right_op.type));
            return false;
        }

        promote_int_eops(resolver, &left_op);
        promote_int_eops(resolver, &right_op);

        if (left_op.is_constexpr && right_op.is_constexpr) {
            assert(left_op.is_imm && right_op.is_imm);
            eval_const_binary_op(resolver, ebinary->op, &dst_op, left_op.type, left_op.imm, right_op.imm);
        }
        else {
            dst_op.type = left_op.type;
        }

        break;
    }
    case TKN_AND:
    case TKN_OR:
    case TKN_CARET: {
        if (!type_is_integer_like(left_op.type)) {
            resolver_on_error(resolver, ebinary->left->range,
                              "Left operand of binary operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(left_op.type));
            return false;
        }

        if (!type_is_integer_like(right_op.type)) {
            resolver_on_error(resolver, ebinary->right->range,
                              "Right operand of binary operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(right_op.type));
            return false;
        }

        resolve_binary_eop(resolver, ebinary->op, &dst_op, &left_op, &right_op);

        break;
    }
    case TKN_EQ:
    case TKN_NOTEQ: {
        bool left_is_ptr = (left_op.type->kind == TYPE_PTR);
        bool right_is_ptr = (right_op.type->kind == TYPE_PTR);

        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(resolver, ebinary->op, &dst_op, &left_op, &right_op);

            // NOTE: resolve_binary_eop will cast to the common type, so cast to s32.
            cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
        }
        else if (left_is_ptr && right_is_ptr) {
            bool same_type = (left_op.type == right_op.type);
            bool one_is_void_ptr = (left_op.type->as_ptr.base == builtin_types[BUILTIN_TYPE_VOID].type) ||
                                   (right_op.type->as_ptr.base == builtin_types[BUILTIN_TYPE_VOID].type);

            if (!same_type && !one_is_void_ptr) {
                // TODO: Better way to print pointer types (recursively print base types).
                resolver_on_error(resolver, expr->range, "Cannot compare pointers of incompatible types");
                return false;
            }

            if (left_op.is_constexpr && left_op.is_imm && right_op.is_constexpr && right_op.is_imm) {
                u64 left_u64 = left_op.imm.as_int._u64;
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, left_u64, right_u64);

                cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr && right_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else if (left_is_ptr && eop_is_null_ptr(resolver, right_op)) {
            if (left_op.is_constexpr && left_op.is_imm) {
                u64 left_u64 = left_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, left_u64, 0);

                cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else if (right_is_ptr && eop_is_null_ptr(resolver, left_op)) {
            if (right_op.is_constexpr && right_op.is_imm) {
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, right_u64, 0);

                cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
            }
            else {
                dst_op.is_constexpr = right_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else {
            resolver_on_error(resolver, expr->range, "Can only compare arithmetic types, or compatible pointer types with `%s`",
                              token_kind_names[ebinary->op]);
            return false;
        }

        break;
    }
    case TKN_GT:
    case TKN_GTEQ:
    case TKN_LT:
    case TKN_LTEQ: {
        bool left_is_ptr = (left_op.type->kind == TYPE_PTR);
        bool right_is_ptr = (right_op.type->kind == TYPE_PTR);

        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(resolver, ebinary->op, &dst_op, &left_op, &right_op);

            // NOTE: resolve_binary_eop will cast to the common type, so cast to s32.
            cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
        }
        else if (left_is_ptr && right_is_ptr) {
            Type* left_base_type = left_op.type->as_ptr.base;
            Type* right_base_type = right_op.type->as_ptr.base;

            if (left_base_type != right_base_type) {
                left_op.type = type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, builtin_types[BUILTIN_TYPE_S8].type);
                right_op.type =
                    type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, builtin_types[BUILTIN_TYPE_S8].type);
            }

            if (left_op.is_constexpr && left_op.is_imm && right_op.is_constexpr && right_op.is_imm) {
                u64 left_u64 = left_op.imm.as_int._u64;
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, left_u64, right_u64);

                cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr && right_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else if (left_is_ptr && eop_is_null_ptr(resolver, right_op)) {
            if (left_op.is_constexpr && left_op.is_imm) {
                u64 left_u64 = left_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, left_u64, 0);

                cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else if (right_is_ptr && eop_is_null_ptr(resolver, left_op)) {
            if (right_op.is_constexpr && right_op.is_imm) {
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, right_u64, 0);

                cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
            }
            else {
                dst_op.is_constexpr = right_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else {
            resolver_on_error(resolver, expr->range, "Can only compare arithmetic types, or compatible pointer types with `%s`",
                              token_kind_names[ebinary->op]);
            return false;
        }

        break;
    }
    case TKN_LOGIC_AND:
    case TKN_LOGIC_OR:
        if (type_is_scalar(left_op.type) && type_is_scalar(right_op.type)) {
            if (left_op.is_constexpr && left_op.is_imm && right_op.is_constexpr && right_op.is_imm) {
                cast_eop(resolver, &left_op, builtin_types[BUILTIN_TYPE_U64].type, false);
                cast_eop(resolver, &right_op, builtin_types[BUILTIN_TYPE_U64].type, false);

                u64 left_u64 = left_op.imm.as_int._u64;
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, left_u64, right_u64);

                cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
            }
            else {
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
                dst_op.is_constexpr = left_op.is_constexpr && right_op.is_constexpr;
            }
        }
        else {
            resolver_on_error(resolver, expr->range, "Can only compare arithmetic types, or compatible pointer types with `%s`",
                              token_kind_names[ebinary->op]);
            return false;
        }

        break;
    default:
        resolver_on_error(resolver, expr->range, "Binary operator `%s` not supported", token_kind_names[ebinary->op]);
        return false;
    }

    ebinary->left = try_wrap_cast_expr(resolver, &left_op, ebinary->left);
    ebinary->right = try_wrap_cast_expr(resolver, &right_op, ebinary->right);

    expr->type = dst_op.type;
    expr->is_lvalue = dst_op.is_lvalue;
    expr->is_constexpr = dst_op.is_constexpr;
    expr->is_imm = dst_op.is_imm;
    expr->imm = dst_op.imm;

    return true;
}

static void resolve_unary_eop(Resolver* resolver, TokenKind op, ExprOperand* dst, ExprOperand* src)
{
    promote_int_eops(resolver, src);

    if (src->is_constexpr && src->is_imm) {
        eval_const_unary_op(resolver, op, dst, src->type, src->imm);
    }
    else {
        dst->type = src->type;
        dst->is_constexpr = false;
        dst->is_lvalue = false;
    }
}

static bool resolve_expr_unary(Resolver* resolver, Expr* expr)
{
    ExprUnary* eunary = (ExprUnary*)expr;

    if (!resolve_expr(resolver, eunary->expr, NULL))
        return false;

    ExprOperand dst_op = {0};
    ExprOperand src_op = OP_FROM_EXPR(eunary->expr);

    switch (eunary->op) {
    case TKN_PLUS:
    case TKN_MINUS:
        if (!type_is_arithmetic(src_op.type)) {
            resolver_on_error(resolver, expr->range, "Can only use unary +/- with arithmetic types");
            return false;
        }

        resolve_unary_eop(resolver, eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_NEG:
        if (!type_is_integer_like(src_op.type)) {
            resolver_on_error(resolver, expr->range, "Can only use unary ~ with integer types");
            return false;
        }

        resolve_unary_eop(resolver, eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_NOT:
        if (!type_is_scalar(src_op.type)) {
            resolver_on_error(resolver, expr->range, "Can only use unary ! with scalar types");
            return false;
        }

        if (src_op.is_constexpr && src_op.is_imm) {
            assert(type_is_integer_like(src_op.type));
            u64 src_u64 = src_op.imm.as_int._u64;

            dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
            dst_op.is_constexpr = true;
            dst_op.is_imm = true;
            dst_op.imm.as_int._u64 = eval_unary_op_u64(eunary->op, src_u64);

            cast_eop(resolver, &dst_op, builtin_types[BUILTIN_TYPE_S32].type, false);
        }
        else {
            dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            dst_op.is_constexpr = src_op.is_constexpr;
        }

        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_CARET: // NOTE: Address-of operator.
        if (!src_op.is_lvalue) {
            resolver_on_error(resolver, expr->range, "Can only take the address of an l-value");
            return false;
        }

        // The following determines whether this expression is a `constexpr`.
        // Ex: The address of a global variable is a constant expression
        //
        // This does not seem like the best way to do this, so this code will probably go away.
        bool is_constexpr = false;

        if (eunary->expr->kind == CST_ExprIdent) {
            ExprIdent* expr_ident = (ExprIdent*)eunary->expr;
            Symbol* sym = lookup_ident(resolver, &expr_ident->ns_ident);

            if (!sym) {
                resolver_on_error(resolver, expr_ident->super.range, "Unknown symbol `%s` in expression",
                                  ftprint_ns_ident(&resolver->ctx->tmp_mem, &expr_ident->ns_ident));
                return false;
            }

            is_constexpr = !sym->is_local;
        }

        dst_op.type = type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, src_op.type);
        dst_op.is_constexpr = is_constexpr;
        break;
    case TKN_ASTERISK: // NOTE: Dereference operator.
        if (src_op.type->kind == TYPE_ARRAY) {
            eop_array_decay(resolver, &src_op);
        }
        else if (type_is_slice(src_op.type)) {
            eop_array_slice_decay(&src_op);
        }

        if (src_op.type->kind != TYPE_PTR) {
            resolver_on_error(resolver, expr->range, "Cannot dereference a non-pointer value.");
            return false;
        }

        dst_op.type = src_op.type->as_ptr.base;
        dst_op.is_lvalue = true;

        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    default:
        resolver_on_error(resolver, expr->range, "Unary operation type `%d` not supported", eunary->op);
        return false;
    }

    expr->type = dst_op.type;
    expr->is_lvalue = dst_op.is_lvalue;
    expr->is_constexpr = dst_op.is_constexpr;
    expr->is_imm = dst_op.is_imm;
    expr->imm = dst_op.imm;

    return true;
}

typedef struct ObjExprResolution {
    Type* type;
    bool is_lvalue;
} ObjExprResolution;

static bool resolve_obj_expr(Resolver* resolver, Expr* obj_expr, ObjExprResolution* result)
{
    // Resolve 'object' expression and make sure it is an aggregate type or a pointer to an aggregate type.

    if (!resolve_expr(resolver, obj_expr, NULL)) {
        return false;
    }

    result->is_lvalue = obj_expr->is_lvalue;
    result->type = obj_expr->type;

    if (result->type->kind == TYPE_PTR) {
        result->type = result->type->as_ptr.base;
        result->is_lvalue = true;
    }

    if (!try_complete_aggregate_type(resolver, result->type)) {
        return false;
    }

    if (!type_is_aggregate(result->type)) {
        resolver_on_error(resolver, obj_expr->range, "Cannot access field on non-aggregate (i.e., struct or union) type `%s`.",
                          type_name(result->type));
        return false;
    }

    return true;
}

static bool resolve_expr_field(Resolver* resolver, ExprField* expr_field)
{
    // Resolve 'object' expression and make sure it is an aggregate type or a pointer to an aggregate type.
    ObjExprResolution obj_info = {0};

    if (!resolve_obj_expr(resolver, expr_field->object, &obj_info)) {
        return false;
    }

    Type* obj_type = obj_info.type;
    bool is_lvalue = obj_info.is_lvalue;

    // Check that the accessed field exists.
    TypeAggregateField* field = get_type_aggregate_field(obj_type, expr_field->field);

    if (!field) {
        // TODO: Range on field identifier
        resolver_on_error(resolver, expr_field->super.range, "Type `%s` does not have a field named `%s`.", type_name(obj_type),
                          expr_field->field->str);
        return false;
    }

    // Set overall expression type and attributes.
    expr_field->super.type = field->type;
    expr_field->super.is_lvalue = is_lvalue;
    expr_field->super.is_constexpr = false;
    expr_field->super.is_imm = false;

    return true;
}

static bool resolve_expr_field_index(Resolver* resolver, ExprFieldIndex* expr)
{
    // Resolve 'object' expression and make sure it is an aggregate type or a pointer to an aggregate type.
    ObjExprResolution obj_info = {0};

    if (!resolve_obj_expr(resolver, expr->object, &obj_info)) {
        return false;
    }

    Type* obj_type = obj_info.type;
    bool is_lvalue = obj_info.is_lvalue;

    // Resolve field index expression.
    if (!resolve_expr(resolver, expr->index, NULL)) {
        return false;
    }

    // Field index must be a constant expression.
    ExprOperand index_op = OP_FROM_EXPR(expr->index);

    if (!(index_op.is_constexpr && index_op.is_imm && type_is_integer_like(index_op.type))) {
        resolver_on_error(resolver, expr->index->range, "Object's field index must be a compile-time constant expression");
        return false;
    }

    // Field index must be within bounds.
    size_t field_index = expr->index->imm.as_int._u64;
    TypeAggregateBody* type_agg = obj_type->kind == TYPE_STRUCT ? &obj_type->as_struct.body : &obj_type->as_union.body;

    if (field_index >= type_agg->num_fields) {
        resolver_on_error(resolver, expr->index->range, "Object field index (%llu) is out of bounds. Type `%s` has %llu fields.",
                          field_index, type_name(obj_type), type_agg->num_fields);
        return false;
    }

    TypeAggregateField* field = type_agg->fields + field_index;

    // Set overall expression type and attributes.
    expr->super.type = field->type;
    expr->super.is_lvalue = is_lvalue;
    expr->super.is_constexpr = false;
    expr->super.is_imm = false;

    return true;
}

static bool resolve_expr_index(Resolver* resolver, Expr* expr)
{
    const char* err_prefix = "Invalid array subscript expression";
    ExprIndex* eindex = (ExprIndex*)expr;

    // Resolve array index expression
    if (!resolve_expr(resolver, eindex->index, NULL)) {
        return false;
    }

    ExprOperand index_op = OP_FROM_EXPR(eindex->index);

    CastResult r = convert_eop(resolver, &index_op, builtin_types[BUILTIN_TYPE_S64].type, false);

    if (!r.success) {
        resolver_cast_error(resolver, r, eindex->index->range, err_prefix, index_op.type, builtin_types[BUILTIN_TYPE_S64].type);
        return false;
    }

    // Resolve array expression
    if (!resolve_expr(resolver, eindex->array, NULL)) {
        return false;
    }

    bool is_lvalue = true;
    ExprOperand array_op = OP_FROM_EXPR(eindex->array);

    if (array_op.type->kind == TYPE_ARRAY) {
        is_lvalue = array_op.is_lvalue;

        // We can do bounds checking if the index is a constant expression.
        if (index_op.is_constexpr) {
            assert(index_op.is_imm);
            size_t idx_val = index_op.imm.as_int._u64;
            size_t arr_len = array_op.type->as_array.len;

            if (idx_val >= arr_len) {
                resolver_on_error(resolver, eindex->index->range, "Array index `%llu` is out of range. Array length is `%llu`.",
                                  idx_val, arr_len);
                return false;
            }
        }

        eop_array_decay(resolver, &array_op);
    }
    else if (type_is_slice(array_op.type)) {
        is_lvalue = array_op.is_lvalue;

        eop_array_slice_decay(&array_op);
    }

    if (array_op.type->kind != TYPE_PTR) {
        resolver_on_error(resolver, eindex->array->range, "Cannot index value of type `%s`",
                          type_name(eindex->array->type));
        return false;
    }

    // Cast array and index expressions if necessary.
    eindex->array = try_wrap_cast_expr(resolver, &array_op, eindex->array);
    eindex->index = try_wrap_cast_expr(resolver, &index_op, eindex->index);

    // Set overall expression type and attributes.
    expr->type = array_op.type->as_ptr.base;
    expr->is_lvalue = is_lvalue;
    expr->is_constexpr = false;
    expr->is_imm = false;

    return true;
}

static Symbol* lookup_ident(Resolver* resolver, NSIdent* ns_ident)
{
    //
    // Tries to lookup a symbol for an identifier in the form <module_namespace>::...::<identifier_name>
    //

    List* head = &ns_ident->idents;
    List* it = head->next;

    IdentNode* inode = list_entry(it, IdentNode, lnode);
    Symbol* sym = resolve_name(resolver, inode->ident);

    it = it->next;

    while (it != head) {
        if (!sym) {
            resolver_on_error(resolver, ns_ident->range, "Unknown namespace `%s`.", inode->ident->str);
            return NULL;
        }

        inode = list_entry(it, IdentNode, lnode);

        if (sym->kind == SYMBOL_MODULE) {
            StmtImport* stmt = (StmtImport*)sym->as_mod.stmt;
            Identifier* sym_name = get_import_sym_name(stmt, inode->ident);

            if (!sym_name) {
                resolver_on_error(resolver, ns_ident->range, "Identifier `%s` is not among the imported symbols in module namespace `%s`",
                                  inode->ident->str, sym->name->str);
                return NULL;
            }

            // Enter the namespace's module, and then try to lookup the identifier with its native name.
            ModuleState mod_state = enter_module(resolver, sym->as_mod.mod);
            sym = resolve_export_name(resolver, sym_name);
            exit_module(resolver, mod_state);
        }
        else if ((sym->kind == SYMBOL_TYPE) && (sym->decl->kind == CST_DeclEnum)) {
            Symbol** enum_items = sym->as_enum.items;
            size_t num_enum_items = sym->as_enum.num_items;
            Symbol* enum_item_sym = NULL;

            // Find enum item symbol.
            for (size_t ii = 0; ii < num_enum_items; ii++) {
                if (enum_items[ii]->name == inode->ident) {
                    enum_item_sym = enum_items[ii];
                    break;
                }
            }

            if (!enum_item_sym) {
                resolver_on_error(resolver, ns_ident->range, "Identifier `%s` is not a valid enum item of `%s`.", inode->ident->str,
                                  type_name(sym->type));
                return NULL;
            }

            sym = enum_item_sym;
        }
        else {
            resolver_on_error(resolver, ns_ident->range, "Symbol `%s` is not a valid namespace.", inode->ident->str);
            return NULL;
        }

        it = it->next;
    }

    return sym;
}

static bool resolve_expr_ident(Resolver* resolver, Expr* expr)
{
    ExprIdent* eident = (ExprIdent*)expr;
    Symbol* sym = lookup_ident(resolver, &eident->ns_ident);

    if (!sym) {
        resolver_on_error(resolver, expr->range, "Unknown symbol `%s` in expression",
                          ftprint_ns_ident(&resolver->ctx->tmp_mem, &eident->ns_ident));
        return false;
    }

    switch (sym->kind) {
    case SYMBOL_VAR:
        expr->type = sym->type;
        expr->is_lvalue = true;
        expr->is_constexpr = false;
        expr->is_imm = false;

        return true;
    case SYMBOL_CONST:
        expr->type = sym->type;
        expr->is_lvalue = false;
        expr->is_constexpr = true;
        expr->is_imm = true;
        expr->imm = sym->as_const.imm;

        return true;
    case SYMBOL_PROC:
        expr->type = sym->type;
        expr->is_lvalue = false;
        expr->is_constexpr = true;
        expr->is_imm = false;

        return true;
    default:
        break;
    }

    resolver_on_error(resolver, expr->range, "Identifier must refer to a var, const, or proc, but `%s` is neither.",
                      ftprint_ns_ident(&resolver->ctx->tmp_mem, &eident->ns_ident));
    return false;
}

static bool resolve_call_arg(Resolver* resolver, ProcCallArg* arg, Type* param_type, bool is_varg)
{
    const char* err_prefix = "Invalid call argument";

    if (!resolve_expr(resolver, arg->expr, NULL)) {
        return false;
    }

    ExprOperand arg_eop = OP_FROM_EXPR(arg->expr);

    bool can_be_any = is_varg && param_type == builtin_types[BUILTIN_TYPE_ANY].type;

    if (type_is_slice(param_type) && (arg_eop.type->kind == TYPE_ARRAY)) {
        CastResult r = cast_eop(resolver, &arg_eop, param_type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, arg->expr->range, err_prefix, arg_eop.type, param_type);
            return false;
        }
    }
    else if (!can_be_any) {
        CastResult r = convert_eop(resolver, &arg_eop, param_type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, arg->expr->range, err_prefix, arg_eop.type, param_type);
            return false;
        }
    }

    arg->expr = try_wrap_cast_expr(resolver, &arg_eop, arg->expr);

    return true;
}

static bool resolve_expr_call(Resolver* resolver, Expr* expr)
{
    ExprCall* ecall = (ExprCall*)expr;

    // Resolve procedure expression.
    if (!resolve_expr(resolver, ecall->proc, NULL))
        return false;

    Type* proc_type = ecall->proc->type;

    // Verifty that we're calling an actual procedure type.
    if (proc_type->kind != TYPE_PROC) {
        resolver_on_error(resolver, ecall->proc->range, "Cannot use procedure call syntax on a value with a non-procedure type");
        return false;
    }

    // Verify that the number of arguments match number of parameters.
    bool is_variadic = proc_type->as_proc.is_variadic;
    size_t num_params = proc_type->as_proc.num_params;
    size_t num_args = ecall->num_args;

    if (is_variadic && (num_args < (num_params - 1))) {
        resolver_on_error(resolver, expr->range,
                          "Incorrect number of procedure call arguments."
                          " Expected at least `%d` arguments, but got `%d`",
                          num_params - 1, num_args);
        return false;
    }

    if (!is_variadic && (num_params != num_args)) {
        resolver_on_error(resolver, expr->range, "Incorrect number of procedure call arguments. Expected `%d` arguments, but got `%d`",
                          num_params, num_args);
        return false;
    }

    size_t n = is_variadic ? num_params - 1 : num_params;
    size_t arg_index = 0;
    Type** params = proc_type->as_proc.params;
    List* head = &ecall->args;
    List* it = head->next;

    // Resolve arguments for non-variadic parameters.
    while (arg_index < n) {
        assert(it != head);
        Type* param_type = params[arg_index];
        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);

        if (!resolve_call_arg(resolver, arg, param_type, false)) {
            return false;
        }

        it = it->next;
        arg_index += 1;
    }

    // Resolve variadic arguments.
    if (is_variadic) {
        Type* variadic_type = params[num_params - 1];
        assert(variadic_type->kind == TYPE_STRUCT);

        TypeAggregateField* data_field = get_type_struct_field(variadic_type, builtin_struct_fields[BUILTIN_STRUCT_FIELD_DATA]);
        assert(data_field);

        Type* ptr_param_type = data_field->type;
        assert(ptr_param_type->kind == TYPE_PTR);

        Type* param_type = ptr_param_type->as_ptr.base;

        while (it != head) {
            ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);

            if (!resolve_call_arg(resolver, arg, param_type, true)) {
                return false;
            }

            it = it->next;
            arg_index += 1;
        }
    }

    expr->type = proc_type->as_proc.ret;
    expr->is_lvalue = false;
    expr->is_constexpr = false;
    expr->is_imm = false;

    return true;
}

static bool resolve_expr_cast(Resolver* resolver, Expr* expr)
{
    ExprCast* ecast = (ExprCast*)expr;

    Type* cast_type = resolve_typespec(resolver, ecast->typespec);

    if (!cast_type)
        return false;

    if (!resolve_expr(resolver, ecast->expr, NULL))
        return false;

    ExprOperand src_eop = OP_FROM_EXPR(ecast->expr);

    CastResult r = cast_eop(resolver, &src_eop, cast_type, false);

    if (!r.success) {
        resolver_cast_error(resolver, r, expr->range, "Invalid explicit cast", src_eop.type, cast_type);
        return false;
    }

    assert(cast_type == src_eop.type);
    assert(!src_eop.is_lvalue);

    expr->type = src_eop.type;
    expr->is_lvalue = src_eop.is_lvalue;
    expr->is_constexpr = src_eop.is_constexpr;
    expr->is_imm = src_eop.is_imm;
    expr->imm = src_eop.imm;

    return true;
}

static bool resolve_expr_array_lit(Resolver* resolver, ExprCompoundLit* expr, Type* type)
{
    assert(type->kind == TYPE_ARRAY);
    Type* elem_type = type->as_array.base;

    if (elem_type == builtin_types[BUILTIN_TYPE_VOID].type) {
        ProgRange r = expr->typespec ? expr->typespec->range : expr->super.range;
        resolver_on_error(resolver, r, "Cannot declare an array of `void` elements");
        return false;
    }

    if (type_is_incomplete_array(elem_type)) {
        ProgRange r = expr->typespec ? expr->typespec->range : expr->super.range;
        resolver_on_error(resolver, r, "Cannot use an incomplete array element type `%s`", type_name(elem_type));
        return false;
    }

    u64 array_len = type->as_array.len;
    u64 elem_index = 0;
    bool infer_len = array_len == 0;
    bool all_initzers_constexpr = true;

    // Iterate through each initializer
    List* head = &expr->initzers;
    List* it = head->next;

    while (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        Designator designator = initzer->designator;

        // Cannot use a name designator.
        if (designator.kind == DESIGNATOR_NAME) {
            // TODO: Range for name designator.
            resolver_on_error(resolver, initzer->range, "Cannot use a name designator (`%s`) for an array compound literal",
                              designator.name->str);
            return false;
        }

        // Resolve array index designator expression.
        if (designator.kind == DESIGNATOR_INDEX) {
            if (!resolve_expr(resolver, designator.index, elem_type))
                return false;

            if (!designator.index->is_constexpr || !designator.index->is_imm) {
                resolver_on_error(resolver, designator.index->range,
                                  "Array index designator must be a compile-time constant expression");
                return false;
            }

            elem_index = designator.index->imm.as_int._u64;
        }

        if (!infer_len && elem_index >= array_len) {
            resolver_on_error(resolver, initzer->range, "Too many elements in array initializer. Expected at most %llu elements.",
                              array_len);
            return false;
        }

        if (!resolve_expr(resolver, initzer->init, elem_type))
            return false;

        ExprOperand init_op = OP_FROM_EXPR(initzer->init);

        // Initializer expression should be convertible to the element type.
        CastResult r = convert_eop(resolver, &init_op, elem_type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, initzer->init->range, "Invalid array initializer", init_op.type, elem_type);
            return false;
        }

        // If initializer expression is convertible to the element type, create a new AST node that makes the conversion
        // explicit.
        initzer->init = try_wrap_cast_expr(resolver, &init_op, initzer->init);

        // Keep track of constness.
        all_initzers_constexpr &= init_op.is_constexpr;

        elem_index += 1;
        it = it->next;
    }

    if (infer_len) {
        type = type_array(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.arrays, elem_type, elem_index);
    }

    expr->super.type = type;
    expr->super.is_lvalue = false;
    expr->super.is_imm = false;
    expr->super.is_constexpr = all_initzers_constexpr;

    return true;
}

static bool resolve_expr_struct_lit(Resolver* resolver, ExprCompoundLit* expr, Type* type)
{
    assert(type->kind == TYPE_STRUCT);
    AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);

    TypeAggregateBody* type_agg = &type->as_struct.body;
    TypeAggregateField* fields = type_agg->fields;
    size_t num_fields = type_agg->num_fields;

    // Keep track of fields with initializers using a bit array.
    BitArray seen_fields = {0};
    bit_arr_init(&seen_fields, &resolver->ctx->tmp_mem, num_fields);

    bool all_initzers_constexpr = true;
    size_t field_index = 0;

    // Iterate through each initializer.
    List* head = &expr->initzers;
    List* it = head->next;

    while (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        Designator* desig = &initzer->designator;

        if (desig->kind == DESIGNATOR_INDEX) { // TODO: Support index for anonymous struct fields
            resolver_on_error(resolver, initzer->range, "Cannot use an index designator for a struct initializer.");
            return false;
        }

        // Determine which field this initializer targets.
        TypeAggregateField* field = NULL;

        if (desig->kind == DESIGNATOR_NAME) {
            field = get_type_struct_field(type, desig->name);

            if (!field) {
                resolver_on_error(resolver, initzer->range, "Initializer targets non-existing struct field: `%s`.", desig->name->str);
                return false;
            }

            field_index = field->index + 1;
        }
        else {
            if (field_index >= num_fields) {
                resolver_on_error(resolver, initzer->range, "Too many initializers, expected at most %llu initializers.", num_fields);
                return false;
            }

            field = &fields[field_index++];
        }

        // Report error if already provided an initializer for this field.
        if (bit_arr_get(&seen_fields, field->index)) {
            const char* name = field->name ? field->name->str : "_anonymous_";
            resolver_on_error(resolver, initzer->range, "Initializer sets field more than once. "
                              "The field's name and index are `%s` and `%llu`.",
                              name, field->index);
            return false;
        }

        bit_arr_set(&seen_fields, field->index, true); // Mark this field as seen.

        // Resolve initializer value.
        if (!resolve_expr(resolver, initzer->init, field->type)) {
            return false;
        }

        ExprOperand init_op = OP_FROM_EXPR(initzer->init);

        // Initializer expression should be convertible to the field type.
        CastResult r = convert_eop(resolver, &init_op, field->type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, initzer->init->range, "Invalid struct initializer", init_op.type, field->type);
            return false;
        }

        // If initializer expression is convertible to the field type, create a new AST node that makes the conversion
        // explicit.
        initzer->init = try_wrap_cast_expr(resolver, &init_op, initzer->init);

        // Keep track of constness.
        all_initzers_constexpr &= init_op.is_constexpr;

        it = it->next;
    }

    expr->super.type = type;
    expr->super.is_lvalue = false;
    expr->super.is_imm = false;
    expr->super.is_constexpr = all_initzers_constexpr;

    allocator_restore_state(mem_state);

    return true;
}

static bool resolve_expr_union_lit(Resolver* resolver, ExprCompoundLit* expr, Type* type)
{
    assert(type->kind == TYPE_UNION);

    if (expr->num_initzers > 1) {
        resolver_on_error(resolver, expr->super.range, "Too many initializers, expected at most 1 initializer for a union type.");
        return false;
    }

    bool initzer_constexpr = true;

    TypeAggregateField* fields = type->as_union.body.fields;
    List* head = &expr->initzers;
    List* it = head->next;

    if (it != head) {
        MemberInitializer* initzer = list_entry(it, MemberInitializer, lnode);
        Designator* desig = &initzer->designator;

        if (desig->kind == DESIGNATOR_INDEX) { // TODO: Support index for anonymous union fields
            resolver_on_error(resolver, initzer->range, "Cannot use an index designator for a union initializer.");
            return false;
        }

        // Determine which field this initializer targets.
        TypeAggregateField* field = NULL;

        if (desig->kind == DESIGNATOR_NAME) {
            field = get_type_union_field(type, desig->name);

            if (!field) {
                resolver_on_error(resolver, initzer->range, "Initializer targets non-existing union field: `%s`.", desig->name->str);
                return false;
            }
        }
        else {
            field = &fields[0];
        }

        // Resolve initializer value.
        if (!resolve_expr(resolver, initzer->init, field->type)) {
            return false;
        }

        ExprOperand init_op = OP_FROM_EXPR(initzer->init);

        // Initializer expression should be convertible to the field type.
        CastResult r = convert_eop(resolver, &init_op, field->type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, initzer->init->range, "Invalid union initializer", init_op.type, field->type);
            return false;
        }

        // If initializer expression is convertible to the field type, create a new AST node that makes the conversion
        // explicit.
        initzer->init = try_wrap_cast_expr(resolver, &init_op, initzer->init);

        // Keep track of constness.
        initzer_constexpr = init_op.is_constexpr;
    }

    expr->super.type = type;
    expr->super.is_lvalue = false;
    expr->super.is_imm = false;
    expr->super.is_constexpr = initzer_constexpr;

    return true;
}

static bool resolve_expr_compound_lit(Resolver* resolver, ExprCompoundLit* expr, Type* expected_type)
{
    Type* lit_type = NULL;

    if (expr->typespec) {
        lit_type = resolve_typespec(resolver, expr->typespec);

        if (!lit_type) {
            return false;
        }
    }

    Type* type = lit_type ? lit_type : expected_type;

    if (!type) {
        resolver_on_error(resolver, expr->super.range, "Unknown type for compound literal");
        return false;
    }

    if (!try_complete_aggregate_type(resolver, type)) {
        return false;
    }

    if (type->kind == TYPE_ARRAY) {
        return resolve_expr_array_lit(resolver, expr, type);
    }
    else if (type->kind == TYPE_STRUCT) {
        return resolve_expr_struct_lit(resolver, expr, type);
    }
    else if (type->kind == TYPE_UNION) {
        return resolve_expr_union_lit(resolver, expr, type);
    }

    resolver_on_error(resolver, expr->super.range, "Invalid compound literal type `%s`.", type_name(type));

    return false;
}

static bool resolve_expr(Resolver* resolver, Expr* expr, Type* expected_type)
{
    switch (expr->kind) {
    case CST_ExprInt:
        return resolve_expr_int(resolver, expr);
    case CST_ExprIdent:
        return resolve_expr_ident(resolver, expr);
    case CST_ExprBinary:
        return resolve_expr_binary(resolver, expr);
    case CST_ExprUnary:
        return resolve_expr_unary(resolver, expr);
    case CST_ExprIndex:
        return resolve_expr_index(resolver, expr);
    case CST_ExprField:
        return resolve_expr_field(resolver, (ExprField*)expr);
    case CST_ExprFieldIndex:
        return resolve_expr_field_index(resolver, (ExprFieldIndex*)expr);
    case CST_ExprCall:
        return resolve_expr_call(resolver, expr);
    case CST_ExprCast:
        return resolve_expr_cast(resolver, expr);
    case CST_ExprCompoundLit:
        return resolve_expr_compound_lit(resolver, (ExprCompoundLit*)expr, expected_type);
    case CST_ExprStr:
        return resolve_expr_str(resolver, (ExprStr*)expr);
    case CST_ExprSizeof:
        return resolve_expr_sizeof(resolver, (ExprSizeof*)expr);
    case CST_ExprTypeid:
        return resolve_expr_typeid(resolver, (ExprTypeid*)expr);
    case CST_ExprOffsetof:
        return resolve_expr_offsetof(resolver, (ExprOffsetof*)expr);
    case CST_ExprIndexof:
        return resolve_expr_indexof(resolver, (ExprIndexof*)expr);
    case CST_ExprLength:
        return resolve_expr_length(resolver, (ExprLength*)expr);
    default:
        ftprint_err("Unsupported expr kind `%d` while resolving\n", expr->kind);
        assert(0);
        break;
    }

    return false;
}

static Type* resolve_typespec(Resolver* resolver, TypeSpec* typespec)
{
    if (!typespec)
        return builtin_types[BUILTIN_TYPE_VOID].type;

    switch (typespec->kind) {
    case CST_TypeSpecIdent: {
        TypeSpecIdent* ts = (TypeSpecIdent*)typespec;
        Symbol* ident_sym = lookup_ident(resolver, &ts->ns_ident);

        if (!ident_sym) {
            resolver_on_error(resolver, typespec->range, "Undefined type `%s`",
                              ftprint_ns_ident(&resolver->ctx->tmp_mem, &ts->ns_ident));
            return NULL;
        }

        if (ident_sym->kind != SYMBOL_TYPE) {
            resolver_on_error(resolver, typespec->range, "Identifier `%s` is not a type",
                              ftprint_ns_ident(&resolver->ctx->tmp_mem, &ts->ns_ident));
            return NULL;
        }

        return ident_sym->type;
    }
    case CST_TypeSpecTypeof: {
        TypeSpecTypeof* ts = (TypeSpecTypeof*)typespec;

        if (!resolve_expr(resolver, ts->expr, NULL))
            return NULL;

        return ts->expr->type;
    }
    case CST_TypeSpecRetType: {
        TypeSpecRetType* ts = (TypeSpecRetType*)typespec;

        Type* proc_type = NULL;

        // Get proc type from expression.
        if (ts->proc_expr) {
            if (!resolve_expr(resolver, ts->proc_expr, NULL)) {
                return NULL;
            }

            proc_type = ts->proc_expr->type;

            if (proc_type->kind != TYPE_PROC) {
                resolver_on_error(resolver, ts->proc_expr->range, "Expected expression of procedure type, but found type `%s`.",
                                  type_name(proc_type));
                return NULL;
            }
        }
        // Get proc type from current procedure
        else {
            Symbol* curr_proc = resolver->state.proc;

            if (!curr_proc) {
                resolver_on_error(resolver, typespec->range, "Cannot use #ret_type (without an argument) outside of a procedure.");
                return NULL;
            }

            assert(curr_proc->kind == SYMBOL_PROC);

            proc_type = curr_proc->type;
        }

        return proc_type->as_proc.ret;
    }
    case CST_TypeSpecPtr: {
        TypeSpecPtr* ts = (TypeSpecPtr*)typespec;
        TypeSpec* base_ts = ts->base;
        Type* base_type = resolve_typespec(resolver, base_ts);

        if (!base_type)
            return NULL;

        return type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, base_type);
    }
    case CST_TypeSpecArray: {
        TypeSpecArray* ts = (TypeSpecArray*)typespec;
        Type* base_type = resolve_typespec(resolver, ts->base);

        if (!base_type)
            return NULL;

        if (base_type->size == 0) {
            resolver_on_error(resolver, ts->base->range, "Array element type must have non-zero size");
            return NULL;
        }

        // Array slice type.
        if (!ts->len && !ts->infer_len) {
            return type_slice(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.slices, &resolver->ctx->type_cache.ptrs, base_type);
        }

        // Actual array type.
        size_t len = 0;

        if (ts->len) {
            assert(!ts->infer_len);

            if (!resolve_expr(resolver, ts->len, NULL))
                return NULL;

            if (!(ts->len->is_constexpr && ts->len->is_imm)) {
                resolver_on_error(resolver, ts->len->range, "Array length must be a compile-time constant");
                return NULL;
            }

            if (ts->len->type->kind != TYPE_INTEGER) {
                resolver_on_error(resolver, ts->len->range, "Array length must be an integer");
                return NULL;
            }

            len = (size_t)(ts->len->imm.as_int._u64);

            if (len == 0) {
                resolver_on_error(resolver, ts->len->range, "Array length must be a positive, non-zero integer");
                return NULL;
            }
        }

        return type_array(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.arrays, base_type, len);
    }
    case CST_TypeSpecProc: {
        TypeSpecProc* ts = (TypeSpecProc*)typespec;

        AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);
        Type** params = array_create(&resolver->ctx->tmp_mem, Type*, ts->num_params);
        List* head = &ts->params;

        for (List* it = head->next; it != head; it = it->next) {
            ProcParam* proc_param = list_entry(it, ProcParam, lnode);
            Type* param = resolve_typespec(resolver, proc_param->typespec);

            if (!param) {
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (type_is_incomplete_array(param)) {
                resolver_on_error(resolver, proc_param->range, "Procedure parameter cannot be an array with an inferred length.");
                return false;
            }

            if (!try_complete_aggregate_type(resolver, param)) {
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (param->size == 0) {
                resolver_on_error(resolver, proc_param->range, "Invalid procedure paramater type `%s` of zero size.",
                                  type_name(param));
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (proc_param->is_variadic) {
                param = type_slice(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.slices, &resolver->ctx->type_cache.ptrs, param);
            }

            array_push(params, param);
        }
        assert(array_len(params) == ts->num_params);

        Type* ret = builtin_types[BUILTIN_TYPE_VOID].type;

        if (ts->ret) {
            ret = resolve_typespec(resolver, ts->ret);

            if (!ret) {
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (type_is_incomplete_array(ret)) {
                assert(ts->ret);
                resolver_on_error(resolver, ts->ret->range, "Procedure return type cannot be an array with an inferred length.");
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (ret->size == 0) {
                resolver_on_error(resolver, ts->ret->range, "Invalid procedure return type `%s` of zero size.", type_name(ret));
                return NULL;
            }
        }

        Type* type =
            type_proc(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.procs, array_len(params), params, ret, ts->is_variadic);
        allocator_restore_state(mem_state);

        return type;
    }
    case CST_TypeSpecStruct: { // Anonymous struct (aka tuples)
        TypeSpecStruct* ts = (TypeSpecStruct*)typespec;

        AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);
        TypeAggregateField* fields = resolve_aggregate_fields(resolver, &ts->fields);

        if (!fields) {
            return NULL;
        }

        Type* type = type_anon_aggregate(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.structs, TYPE_STRUCT,
                                         array_len(fields), fields);

        assert(type->kind == TYPE_STRUCT);
        allocator_restore_state(mem_state);

        return type;
    }
    case CST_TypeSpecUnion: { // Anonymous union
        TypeSpecUnion* ts = (TypeSpecUnion*)typespec;

        AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);
        TypeAggregateField* fields = resolve_aggregate_fields(resolver, &ts->fields);

        if (!fields) {
            return NULL;
        }

        Type* type = type_anon_aggregate(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.unions, TYPE_UNION,
                                         array_len(fields), fields);

        assert(type->kind == TYPE_UNION);
        allocator_restore_state(mem_state);

        return type;
    }
    default:
        ftprint_err("Unsupported typespec kind `%d` in resolution\n", typespec->kind);
        assert(0);
        break;
    }

    return NULL;
}

static bool resolve_decl_var(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_VAR);

    DeclVar* decl = (DeclVar*)sym->decl;
    bool global = !sym->is_local;
    TypeSpec* typespec = decl->typespec;
    Expr* expr = decl->init;
    Type* type = NULL;

    if (typespec) {
        Type* declared_type = resolve_typespec(resolver, typespec);

        if (!declared_type)
            return false;

        if (declared_type == builtin_types[BUILTIN_TYPE_VOID].type) {
            resolver_on_error(resolver, typespec->range, "Cannot declare a variable of type `%s`.", type_name(declared_type));
            return false;
        }

        if (expr) {
            if (!resolve_expr(resolver, expr, declared_type))
                return false;

            assert(!type_has_incomplete_array(expr->type));

            ExprOperand right_eop = OP_FROM_EXPR(expr);

            // If the declared type contains an incomplete array type, try to use the
            // rhs type if the types are compatible.
            if (type_has_incomplete_array(declared_type)) {
                if (!types_are_compatible(declared_type, right_eop.type)) {
                    resolver_on_error(resolver, expr->range,
                                      "Incomplete variable type `%s` is not compatible with expression of type `%s`.",
                                      type_name(declared_type), type_name(right_eop.type));
                    return false;
                }

                declared_type = right_eop.type;
            }
            else {
                CastResult r = convert_eop(resolver, &right_eop, declared_type, true);

                if (!r.success) {
                    resolver_cast_error(resolver, r, sym->decl->range, "Invalid variable declaration", right_eop.type, declared_type);
                    return false;
                }
            }

            if (global && !right_eop.is_constexpr) {
                resolver_on_error(resolver, expr->range, "Global variables must be initialized with a constant expression");
                return false;
            }

            decl->init = try_wrap_cast_expr(resolver, &right_eop, decl->init);
            type = declared_type;
        }
        else {
            if (type_has_incomplete_array(declared_type)) {
                resolver_on_error(resolver, typespec->range, "Cannot infer the number of elements in array type specification");
                return false;
            }

            type = declared_type;
        }
    }
    else {
        assert(expr);

        if (!resolve_expr(resolver, expr, NULL))
            return false;

        if (global && !expr->is_constexpr) {
            resolver_on_error(resolver, expr->range, "Global variables must be initialized with a constant expression");
            return false;
        }

        if (type_has_incomplete_array(expr->type)) {
            resolver_on_error(resolver, expr->range, "Expression type `%s` contains array of unknown size.", type_name(expr->type));
            return false;
        }

        type = expr->type;
    }

    if (!try_complete_aggregate_type(resolver, type)) {
        return false;
    }

    if (type->size == 0) {
        resolver_on_error(resolver, expr->range, "Cannot declare a variable of zero size.");
        return false;
    }

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static bool resolve_decl_typedef(Resolver* resolver, Symbol* sym)
{
    DeclTypedef* decl = (DeclTypedef*)sym->decl;
    Type* type = resolve_typespec(resolver, decl->typespec);

    if (!type) {
        return false;
    }

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static bool resolve_decl_enum(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_TYPE);
    DeclEnum* decl_enum = (DeclEnum*)sym->decl;
    Type* base_type;

    if (decl_enum->typespec) {
        base_type = resolve_typespec(resolver, decl_enum->typespec);

        if (!base_type) {
            return false;
        }
    }
    else {
        base_type = builtin_types[BUILTIN_TYPE_INT].type;
    }

    if (base_type->kind != TYPE_INTEGER) {
        assert(decl_enum->typespec);
        resolver_on_error(resolver, decl_enum->typespec->range, "Enum must be an integer type, but found `%s`.", type_name(base_type));
        return false;
    }

    Type* enum_type = type_enum(&resolver->ctx->ast_mem, base_type, decl_enum);

    // Resolve enum items.
    Symbol** item_syms = alloc_array(&resolver->ctx->ast_mem, Symbol*, decl_enum->num_items, false);
    Scalar prev_enum_val = {0};

    List* head = &decl_enum->items;
    List* it = head->next;
    size_t i = 0;

    while (it != head) {
        DeclEnumItem* enum_item = list_entry(it, DeclEnumItem, lnode);
        Scalar enum_val = {0};

        if (enum_item->value) {
            if (!resolve_expr(resolver, enum_item->value, enum_type)) {
                return false;
            }

            ExprOperand value_eop = OP_FROM_EXPR(enum_item->value);

            if (!value_eop.is_constexpr) {
                resolver_on_error(resolver, enum_item->value->range, "Value for enum item `%s` must be a constant expression",
                                  enum_item->super.name->str);
                return false;
            }

            if (!type_is_integer_like(value_eop.type)) {
                resolver_on_error(resolver, enum_item->value->range, "Enum item's value must be of an integer type");
                return false;
            }

            CastResult r = convert_eop(resolver, &value_eop, enum_type, true);

            if (!r.success) {
                resolver_cast_error(resolver, r, enum_item->super.range, "Invalid enum item declaration", value_eop.type, enum_type);
                return false;
            }

            enum_item->value = try_wrap_cast_expr(resolver, &value_eop, enum_item->value);
            enum_val = value_eop.imm;
        }
        else if (i > 0) { // Has a previous value
            Scalar one_imm = {.as_int._u64 = 1};
            ExprOperand item_op = {0};

            eval_const_binary_op(resolver, TKN_PLUS, &item_op, enum_type, prev_enum_val, one_imm);
            enum_val = item_op.imm;
        }

        Symbol* enum_sym = new_symbol_decl(&resolver->ctx->ast_mem, (Decl*)enum_item, sym->home);
        enum_sym->type = enum_type;
        enum_sym->status = SYMBOL_STATUS_RESOLVED;
        enum_sym->as_const.imm = enum_val;
        item_syms[i] = enum_sym;

        prev_enum_val = enum_val;
        it = it->next;
        i += 1;
    }

    sym->type = enum_type;
    sym->status = SYMBOL_STATUS_RESOLVED;
    sym->as_enum.items = item_syms;
    sym->as_enum.num_items = decl_enum->num_items;

    return true;
}

static bool resolve_decl_const(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_CONST);

    DeclConst* decl = (DeclConst*)sym->decl;
    TypeSpec* typespec = decl->typespec;
    Expr* init = decl->init;

    if (!resolve_expr(resolver, init, NULL))
        return false;

    if (!init->is_constexpr) {
        resolver_on_error(resolver, init->range, "Value for const decl `%s` must be a constant expression", decl->super.name->str);
        return false;
    }

    if (!type_is_scalar(init->type)) {
        resolver_on_error(resolver, init->range, "Constant expression must be of a scalar type");
        return false;
    }

    Type* type = NULL;

    if (typespec) {
        Type* declared_type = resolve_typespec(resolver, typespec);

        if (!declared_type)
            return false;

        ExprOperand init_eop = OP_FROM_EXPR(init);

        CastResult r = convert_eop(resolver, &init_eop, declared_type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, typespec->range, "Invalid const declaration", init_eop.type, declared_type);
            return false;
        }

        decl->init = try_wrap_cast_expr(resolver, &init_eop, decl->init);

        type = declared_type;
    }
    else {
        type = init->type;
    }

    assert(type);

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;
    sym->as_const.imm = decl->init->imm;

    return true;
}

static bool resolve_proc_param(Resolver* resolver, Symbol* sym)
{
    DeclVar* decl = (DeclVar*)sym->decl;
    TypeSpec* typespec = decl->typespec;

    assert(typespec);

    Type* type = resolve_typespec(resolver, typespec);

    if (!type) {
        return false;
    }

    if (type_is_incomplete_array(type)) {
        resolver_on_error(resolver, decl->super.range, "Procedure parameter cannot be an array with an inferred length.");
        return false;
    }

    if (!try_complete_aggregate_type(resolver, type)) {
        return false;
    }

    if (type->size == 0) {
        resolver_on_error(resolver, decl->super.range, "Cannot declare a parameter of zero size.");
        return false;
    }

    if (decl->flags & DECL_VAR_IS_VARIADIC) {
        type = type_slice(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.slices, &resolver->ctx->type_cache.ptrs, type);
    }

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static bool resolve_decl_proc(Resolver* resolver, Symbol* sym)
{
    DeclProc* decl = (DeclProc*)sym->decl;

    bool is_variadic = decl->is_variadic;
    bool is_incomplete = decl->is_incomplete;
    bool is_foreign = decl->super.flags & DECL_IS_FOREIGN;
    bool is_intrinsic = decl->super.name->kind == IDENTIFIER_INTRINSIC;

    if (is_foreign && !is_incomplete) {
        // TODO: Need a ProgRange for just the procedure header.
        resolver_on_error(resolver, decl->super.range, "Foreign declaration cannot have a body");
        return false;
    }

    if (is_incomplete && !(is_foreign || is_intrinsic)) {
        resolver_on_error(resolver, decl->super.range, "Procedure `%s` must have a body", decl->super.name->str);
        return false;
    }

    decl->scope = push_scope(resolver, decl->num_params + decl->num_decls);

    AllocatorState mem_state = allocator_get_state(&resolver->ctx->tmp_mem);
    Type** params = array_create(&resolver->ctx->tmp_mem, Type*, 16);
    List* head = &decl->params;

    for (List* it = head->next; it != head; it = it->next) {
        Decl* proc_param = list_entry(it, Decl, lnode);
        Symbol* param_sym = add_unresolved_symbol(&resolver->ctx->ast_mem, decl->scope, sym->home, proc_param);

        assert(param_sym);

        if (!resolve_proc_param(resolver, param_sym)) {
            allocator_restore_state(mem_state);
            return false;
        }

        array_push(params, param_sym->type);
    }

    pop_scope(resolver);
    assert(array_len(params) == decl->num_params);

    Type* ret_type = builtin_types[BUILTIN_TYPE_VOID].type;

    if (decl->ret) {
        ret_type = resolve_typespec(resolver, decl->ret);

        if (!ret_type) {
            return false;
        }

        if (!try_complete_aggregate_type(resolver, ret_type)) {
            return false;
        }

        if (type_is_incomplete_array(ret_type)) {
            resolver_on_error(resolver, decl->ret->range,
                              "Procedure return type cannot be an array with an inferred length.");
            return false;
        }

        if (ret_type->size == 0) {
            resolver_on_error(resolver, decl->super.range, "Invalid procedure return type `%s` of zero size.", type_name(ret_type));
            return false;
        }
    }

    sym->type = type_proc(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.procs, array_len(params), params, ret_type, is_variadic);
    sym->status = SYMBOL_STATUS_RESOLVED;

    allocator_restore_state(mem_state);
    return true;
}

static bool resolve_global_proc_body(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_PROC);
    DeclProc* dproc = (DeclProc*)(sym->decl);

    if (dproc->is_incomplete) {
        return true;
    }

    ModuleState mod_state = enter_proc(resolver, sym);

    Type* ret_type = sym->type->as_proc.ret;
    unsigned r = resolve_stmt_block_body(resolver, &dproc->stmts, ret_type, 0);
    bool returns = r & RESOLVE_STMT_RETURNS;
    bool success = r & RESOLVE_STMT_SUCCESS;

    assert(!success || !(r & RESOLVE_STMT_LOOP_EXITS));

    dproc->returns = returns;

    if ((ret_type != builtin_types[BUILTIN_TYPE_VOID].type) && !returns && success) {
        resolver_on_error(resolver, dproc->super.range, "Not all code paths in procedure `%s` return a value", dproc->super.name->str);
        return false;
    }

    exit_proc(resolver, mod_state);

    return success;
}

static unsigned resolve_stmt_block_body(Resolver* resolver, List* stmts, Type* ret_type, unsigned flags)
{
    unsigned ret_success = RESOLVE_STMT_SUCCESS;
    List* head = stmts;

    for (List* it = head->next; it != head; it = it->next) {
        Stmt* child_stmt = list_entry(it, Stmt, lnode);

        // Check for statement after return.
        if (ret_success & RESOLVE_STMT_RETURNS) {
            resolver_on_error(resolver, child_stmt->range, "Statement will never execute because all previous control paths return");

            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }

        // Check for statement after break/continue
        if (ret_success & RESOLVE_STMT_LOOP_EXITS) {
            resolver_on_error(resolver, child_stmt->range,
                              "Statement will never execute because all previous control paths break or continue the loop");

            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }

        unsigned r = resolve_stmt(resolver, child_stmt, ret_type, flags);

        // NOTE: Track whether any statement in the block returns from the parent procedure.
        ret_success = (r & RESOLVE_STMT_RETURNS) | (r & RESOLVE_STMT_LOOP_EXITS) | ret_success;

        if (!(r & RESOLVE_STMT_SUCCESS)) {
            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }
    }

    return ret_success;
}

static unsigned resolve_stmt_block(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtBlock* sblock = (StmtBlock*)stmt;
    sblock->scope = push_scope(resolver, sblock->num_decls);

    unsigned ret_success = resolve_stmt_block_body(resolver, &sblock->stmts, ret_type, flags);

    pop_scope(resolver);

    return ret_success;
}

static bool resolve_cond_expr(Resolver* resolver, Expr* expr, ExprOperand* expr_eop)
{
    if (!resolve_expr(resolver, expr, NULL))
        return false;

    // TODO: THIS IS ERROR-PRONE. Will be buggy when add new fields.
    expr_eop->type = expr->type;
    expr_eop->is_constexpr = expr->is_constexpr;
    expr_eop->is_imm = expr->is_imm;
    expr_eop->is_lvalue = expr->is_lvalue;
    expr_eop->imm = expr->imm;

    if (!type_is_scalar(expr_eop->type)) {
        resolver_on_error(resolver, expr->range, "Conditional expression must resolve to a scalar type, have type `%s`",
                          type_name(expr_eop->type));
        return false;
    }

    return true;
}

static unsigned resolve_cond_block(Resolver* resolver, IfCondBlock* cblock, Type* ret_type, unsigned flags)
{
    ExprOperand cond_eop = {0};

    if (!resolve_cond_expr(resolver, cblock->cond, &cond_eop))
        return 0;

    cblock->cond = try_wrap_cast_expr(resolver, &cond_eop, cblock->cond);

    return resolve_stmt(resolver, cblock->body, ret_type, flags);
}

static unsigned resolve_stmt_if(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtIf* sif = (StmtIf*)stmt;

    // Resolve if block.
    unsigned ret = resolve_cond_block(resolver, &sif->if_blk, ret_type, flags);

    if (!(ret & RESOLVE_STMT_SUCCESS))
        return 0;

    // Resolve else block.
    if (sif->else_blk.body) {
        ret &= resolve_stmt(resolver, sif->else_blk.body, ret_type, flags);
    }
    else {
        ret &= ~RESOLVE_STMT_RETURNS;
        ret &= ~RESOLVE_STMT_LOOP_EXITS;
    }

    return ret;
}

static unsigned resolve_stmt_for(Resolver* resolver, StmtFor* stmt_for, Type* ret_type, unsigned flags)
{
    stmt_for->scope = push_scope(resolver, 2); // At most 1 variable declaration in for-loop's init statement.

    unsigned flags_no_break = flags;
    flags_no_break &= ~RESOLVE_STMT_BREAK_CONTINUE_ALLOWED;

    unsigned ret = RESOLVE_STMT_SUCCESS;

    // Init statement.
    if (stmt_for->init) {
        ret &= resolve_stmt(resolver, stmt_for->init, ret_type, flags_no_break);

        if (!(ret & RESOLVE_STMT_SUCCESS)) {
            return 0;
        }

        // Throw an error if the init statement returns.
        if (ret & RESOLVE_STMT_RETURNS) {
            resolver_on_error(resolver, stmt_for->init->range, "For-loop body will never execute");
            return 0;
        }
    }

    // Condition expression.
    if (stmt_for->cond) {
        ExprOperand cond_eop = {0};

        if (!resolve_cond_expr(resolver, stmt_for->cond, &cond_eop)) {
            return 0;
        }

        stmt_for->cond = try_wrap_cast_expr(resolver, &cond_eop, stmt_for->cond);
    }

    // Loop body.
    ret &= resolve_stmt(resolver, stmt_for->body, ret_type, flags | RESOLVE_STMT_BREAK_CONTINUE_ALLOWED);

    if (!(ret & RESOLVE_STMT_SUCCESS)) {
        return 0;
    }

    // Next iteration statement.
    if (stmt_for->next) {
        ret &= resolve_stmt(resolver, stmt_for->next, ret_type, flags_no_break);
    }

    // NOTE: Because for loops don't have an "else" path, we can't say that all control paths return.
    // TODO: Add else to for-loop!!
    ret &= ~RESOLVE_STMT_RETURNS;
    ret &= ~RESOLVE_STMT_LOOP_EXITS; // Break/continue do not propagate out from loops.

    pop_scope(resolver);

    return ret;
}

static unsigned resolve_stmt_while(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtWhile* swhile = (StmtWhile*)stmt;
    ExprOperand cond_eop = {0};

    // Resolve condition expression.
    if (!resolve_cond_expr(resolver, swhile->cond, &cond_eop))
        return 0;

    swhile->cond = try_wrap_cast_expr(resolver, &cond_eop, swhile->cond);

    // Resolve loop body.
    unsigned ret = resolve_stmt(resolver, swhile->body, ret_type, flags | RESOLVE_STMT_BREAK_CONTINUE_ALLOWED);

    // NOTE: Because while loops don't have an "else" path, we can't say that all control paths return.
    // TODO: Add else to while loop!!
    ret &= ~RESOLVE_STMT_RETURNS;
    ret &= ~RESOLVE_STMT_LOOP_EXITS; // Break/continue do not propagate out from loops.

    return ret;
}

static unsigned resolve_stmt_do_while(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtDoWhile* swhile = (StmtDoWhile*)stmt;
    ExprOperand cond_eop = {0};

    // Resolve condition expression.
    if (!resolve_cond_expr(resolver, swhile->cond, &cond_eop))
        return 0;

    swhile->cond = try_wrap_cast_expr(resolver, &cond_eop, swhile->cond);

    // Resolve loop body.
    unsigned ret = resolve_stmt(resolver, swhile->body, ret_type, flags | RESOLVE_STMT_BREAK_CONTINUE_ALLOWED);

    // Report an error if the do-while loop always returns before the condition check.
    if (ret & RESOLVE_STMT_RETURNS) {
        resolver_on_error(resolver, swhile->cond->range, "All paths in do-while loop's body return before condition check.");
        ret &= ~RESOLVE_STMT_SUCCESS;
    }

    // Report an error if the do-while loop always breaks out before condition check.
    // TODO: Continue should be ok?
    if (ret & RESOLVE_STMT_LOOP_EXITS) {
        resolver_on_error(resolver, swhile->cond->range,
                          "All paths in do-while loop's body break or continue before condition check.");
        ret &= ~RESOLVE_STMT_SUCCESS;
    }

    return ret;
}

static Expr* try_wrap_cast_expr(Resolver* resolver, ExprOperand* eop, Expr* orig_expr)
{
    Expr* expr = orig_expr;

    if (orig_expr->type != eop->type) {
        if (expr->is_constexpr && expr->is_imm) {
            assert(type_is_scalar(expr->type));
            assert(eop->is_constexpr);
            expr->imm = eop->imm;
        }
        else {
            expr = new_expr_cast(&resolver->ctx->ast_mem, NULL, orig_expr, true, orig_expr->range);
        }

        expr->type = eop->type;
    }

    expr->is_lvalue = eop->is_lvalue;
    expr->is_constexpr = eop->is_constexpr;
    expr->is_imm = eop->is_imm;

    return expr;
}

static unsigned resolve_stmt_expr_assign(Resolver* resolver, Stmt* stmt)
{
    StmtExprAssign* sassign = (StmtExprAssign*)stmt;
    Expr* lhs_expr = sassign->left;
    Expr* rhs_expr = sassign->right;

    if (!resolve_expr(resolver, lhs_expr, NULL))
        return 0;

    if (!resolve_expr(resolver, rhs_expr, NULL))
        return 0;

    if (!lhs_expr->is_lvalue) {
        resolver_on_error(resolver, lhs_expr->range, "Left side of assignment statement must be an l-value");
        return 0;
    }

    TokenKind op_assign = sassign->op_assign;

    switch (op_assign) {
    case TKN_ASSIGN: {
        ExprOperand rhs_eop = OP_FROM_EXPR(rhs_expr);
        CastResult r = convert_eop(resolver, &rhs_eop, lhs_expr->type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, rhs_expr->range, "Invalid assignment statement", rhs_eop.type, lhs_expr->type);
            return 0;
        }

        sassign->right = try_wrap_cast_expr(resolver, &rhs_eop, sassign->right);
        break;
    }
    case TKN_ADD_ASSIGN:
    case TKN_SUB_ASSIGN: {
        // NOTE: Side-effects of lhs must occur only once.
        //
        // EX: Assume foo() returns a monotonically increasing integer every time it is called.
        //
        // var arr : [3]int = ...;
        // arr[foo()] += 1.0;
        //
        // Should become =>
        //
        // var _ptr : ^int = ^arr[foo()];
        // *_ptr = *_ptr + 1.0
        //

        ExprOperand left_op = OP_FROM_EXPR(lhs_expr);
        ExprOperand right_op = OP_FROM_EXPR(rhs_expr);
        ExprOperand binary_op = {0};

        // Initialize strings used for error messages.
        const char* op_name;
        const char* prep_str;
        if (op_assign == TKN_ADD_ASSIGN) { op_name = "add"; prep_str = "to"; } else { op_name = "subtract"; prep_str = "from"; }

        // Resolve left and right operands of a binary expression.
        // NOTE: ptr arithmetic is only allowed if left is a pointer (unlike normal binary expression).
        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(resolver, TKN_PLUS, &binary_op, &left_op, &right_op);
        }
        else if ((left_op.type->kind == TYPE_PTR) && type_is_integer_like(right_op.type)) {
            if (!try_complete_aggregate_type(resolver, left_op.type->as_ptr.base)) {
                return 0;
            }

            if (!resolve_ptr_int_arith(resolver, &binary_op, &left_op, &right_op)) {
                resolver_on_error(resolver, lhs_expr->range, "Cannot %s %s a pointer with a base type (%s) of zero size", op_name,
                                  prep_str, type_name(left_op.type->as_ptr.base));

                return 0;
            }
        }
        else {
            resolver_on_error(resolver, stmt->range, "Cannot %s a value of type `%s` %s a `%s` in a compound assignment statement.",
                              op_name, type_name(right_op.type), prep_str, type_name(left_op.type));
            return 0;
        }

        // Ensure that binary operation's result can be implicitly converted to lhs's type.
        CastResult r = convert_eop(resolver, &binary_op, lhs_expr->type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, stmt->range, "Invalid compound assignment statement", binary_op.type, lhs_expr->type);
            return 0;
        }

        // Only cast right subexpression. Bytecode generator will manually cast lhs to the same type.
        sassign->right = try_wrap_cast_expr(resolver, &right_op, rhs_expr);
        break;
    }
    default:
        resolver_on_error(resolver, stmt->range, "Sorry! Only the `=` assignment operator is currently supported. Soon!");
        return 0;
    }

    return RESOLVE_STMT_SUCCESS;
}

static bool resolve_static_assert(Resolver* resolver, StmtStaticAssert* sassert)
{
    if (!resolve_expr(resolver, sassert->cond, NULL)) {
        return false;
    }

    if (!(sassert->cond->is_constexpr && sassert->cond->is_imm)) {
        resolver_on_error(resolver, sassert->cond->range, "#static_assert condition must be a compile-time constant expression");
        return false;
    }

    if (sassert->cond->imm.as_int._u32 == 0) {
        const char* msg_pre = "static assertion failed";

        if (sassert->msg)
            resolver_on_error(resolver, sassert->super.range, "%s: %s", msg_pre, sassert->msg->str);
        else
            resolver_on_error(resolver, sassert->super.range, "%s", msg_pre);

        return false;
    }

    return true;
}

static bool resolve_global_stmt(Resolver* resolver, Stmt* stmt)
{
    switch (stmt->kind) {
    case CST_StmtStaticAssert: {
        StmtStaticAssert* sassert = (StmtStaticAssert*)stmt;

        return resolve_static_assert(resolver, sassert);
    }
    default:
        assert(0);
        break;
    }

    return false;
}

static unsigned resolve_stmt(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags)
{
    unsigned ret = 0;
    bool break_continue_allowed = flags & RESOLVE_STMT_BREAK_CONTINUE_ALLOWED;

    switch (stmt->kind) {
    case CST_StmtNoOp: {
        ret = RESOLVE_STMT_SUCCESS;
        break;
    }
    case CST_StmtStaticAssert: {
        StmtStaticAssert* sassert = (StmtStaticAssert*)stmt;

        ret = resolve_static_assert(resolver, sassert) ? RESOLVE_STMT_SUCCESS : 0;
        break;
    }
    case CST_StmtReturn: {
        ret = RESOLVE_STMT_RETURNS;
        StmtReturn* sret = (StmtReturn*)stmt;
        Type* type_void = builtin_types[BUILTIN_TYPE_VOID].type;

        if (!sret->expr && (ret_type != type_void)) {
            resolver_on_error(resolver, stmt->range, "Return statement is missing a return value of type `%s`", type_name(ret_type));
            break;
        }

        if (sret->expr && (ret_type == type_void)) {
            resolver_on_error(resolver, stmt->range, "Procedure with a `void` return type cannot return a value");
            break;
        }

        if (sret->expr) {
            if (!resolve_expr(resolver, sret->expr, ret_type))
                break;

            ExprOperand ret_eop = OP_FROM_EXPR(sret->expr);

            CastResult r = convert_eop(resolver, &ret_eop, ret_type, true);

            if (!r.success) {
                resolver_cast_error(resolver, r, sret->expr->range, "Invalid return type", ret_eop.type, ret_type);
                break;
            }

            sret->expr = try_wrap_cast_expr(resolver, &ret_eop, sret->expr);
        }

        ret |= RESOLVE_STMT_SUCCESS;
        break;
    }
    case CST_StmtBreak: {
        if (break_continue_allowed)
            ret = RESOLVE_STMT_SUCCESS | RESOLVE_STMT_LOOP_EXITS;
        else
            resolver_on_error(resolver, stmt->range, "Illegal break statement");

        break;
    }
    case CST_StmtContinue: {
        if (break_continue_allowed)
            ret = RESOLVE_STMT_SUCCESS | RESOLVE_STMT_LOOP_EXITS;
        else
            resolver_on_error(resolver, stmt->range, "Illegal continue statement");

        break;
    }
    case CST_StmtIf: {
        ret = resolve_stmt_if(resolver, stmt, ret_type, flags);
        break;
    }
    case CST_StmtFor: {
        ret = resolve_stmt_for(resolver, (StmtFor*)stmt, ret_type, flags);
        break;
    }
    case CST_StmtWhile: {
        ret = resolve_stmt_while(resolver, stmt, ret_type, flags);
        break;
    }
    case CST_StmtDoWhile: {
        ret = resolve_stmt_do_while(resolver, stmt, ret_type, flags);
        break;
    }
    case CST_StmtExpr: {
        StmtExpr* sexpr = (StmtExpr*)stmt;

        if (resolve_expr(resolver, sexpr->expr, NULL))
            ret = RESOLVE_STMT_SUCCESS;

        break;
    }
    case CST_StmtExprAssign: {
        ret = resolve_stmt_expr_assign(resolver, stmt);
        break;
    }
    case CST_StmtDecl: {
        StmtDecl* sdecl = (StmtDecl*)stmt;
        Decl* decl = sdecl->decl;
        Scope* scope = resolver->state.scope;

        if (decl->kind == CST_DeclVar || decl->kind == CST_DeclConst) {
            Symbol* sym = add_unresolved_symbol(&resolver->ctx->ast_mem, scope, resolver->state.mod, decl);

            if (!sym) {
                resolver_on_error(resolver, stmt->range, "Identifier `%s` shadows a previous local declaration", decl->name->str);
            }
            else if ((decl->kind == CST_DeclVar) && resolve_decl_var(resolver, sym)) {
                ret = RESOLVE_STMT_SUCCESS;
            }
            else if ((decl->kind == CST_DeclConst) && resolve_decl_const(resolver, sym)) {
                ret = RESOLVE_STMT_SUCCESS;
            }
        }
        else {
            // TODO: Support other declaration kinds.
            resolver_on_error(resolver, stmt->range, "Only variable and type declarations are supported inside procedures");
        }

        break;
    }
    case CST_StmtBlock: {
        ret = resolve_stmt_block(resolver, stmt, ret_type, flags);
        break;
    }
    default:
        assert(0);
        break;
    }

    return ret;
}

static bool resolve_symbol(Resolver* resolver, Symbol* sym)
{
    if (sym->status == SYMBOL_STATUS_RESOLVED)
        return true;

    if (sym->status == SYMBOL_STATUS_RESOLVING) {
        assert(sym->decl);
        resolver_on_error(resolver, sym->decl->range, "Cannot resolve symbol `%s` due to cyclic dependency", sym->name->str);
        return false;
    }

    assert(sym->status == SYMBOL_STATUS_UNRESOLVED);

    bool success = false;
    bool is_global = !sym->is_local;

    ModuleState mod_state = enter_module(resolver, sym->home);

    sym->status = SYMBOL_STATUS_RESOLVING;

    switch (sym->kind) {
    case SYMBOL_VAR:
        success = resolve_decl_var(resolver, sym);

        if (is_global) {
            bucket_list_add_elem(&resolver->ctx->vars, sym);
        }
        break;
    case SYMBOL_CONST:
        assert(sym->decl->kind == CST_DeclConst);
        success = resolve_decl_const(resolver, sym);
        break;
    case SYMBOL_PROC:
        success = resolve_decl_proc(resolver, sym);

        if (is_global) {
            bucket_list_add_elem(&resolver->ctx->procs, sym);
        }
        break;
    case SYMBOL_TYPE: {
        assert(sym->decl);
        Decl* decl = sym->decl;

        if (decl->kind == CST_DeclTypedef) {
            success = resolve_decl_typedef(resolver, sym);
        }
        else if (decl->kind == CST_DeclEnum) {
            success = resolve_decl_enum(resolver, sym);
        }
        else {
            assert(decl->kind == CST_DeclStruct || decl->kind == CST_DeclUnion);

            sym->type = type_incomplete_aggregate(&resolver->ctx->ast_mem, sym);
            sym->status = SYMBOL_STATUS_RESOLVED;

            if (is_global) {
                bucket_list_add_elem(&resolver->ctx->aggregate_types, sym);
            }

            success = true;
        }

        break;
    }
    default:
        ftprint_err("Unhandled symbol kind `%d`\n", sym->kind);
        assert(0);
        break;
    }

    exit_module(resolver, mod_state);

    return success;
}

static Symbol* resolve_name(Resolver* resolver, Identifier* name)
{
    Symbol* sym = lookup_symbol(resolver->state.scope, name);

    if (!sym) {
        return NULL;
    }

    if (!resolve_symbol(resolver, sym)) {
        return NULL;
    }

    return sym;
}

static Symbol* resolve_export_name(Resolver* resolver, Identifier* name)
{
    Symbol* sym = module_get_export_sym(resolver->state.mod, name);

    if (!sym) {
        return NULL;
    }

    if (!resolve_symbol(resolver, sym)) {
        return NULL;
    }

    return sym;
}

bool resolve_module(Resolver* resolver, Module* mod)
{
    ModuleState mod_state = enter_module(resolver, mod);

    // Resolve declaration "headers". Will not resolve procedure bodies or complete aggregate types.
    List* sym_head = &mod->scope.sym_list;

    for (List* it = sym_head->next; it != sym_head; it = it->next) {
        Symbol* sym = list_entry(it, Symbol, lnode);

        assert(sym->home == mod);

        if (!resolve_symbol(resolver, sym))
            return false;
    }

    // Resolve global statements (e.g., #static_assert)
    List* head = &mod->stmts;

    for (List* it = head->next; it != head; it = it->next) {
        Stmt* stmt = list_entry(it, Stmt, lnode);

        if (stmt->kind != CST_StmtDecl) {
            if (!resolve_global_stmt(resolver, stmt))
                return false;
        }
    }

    exit_module(resolver, mod_state);

    return true;
}

bool resolve_reachable_sym_defs(Resolver* resolver)
{
    BucketList* procs = &resolver->ctx->procs;
    BucketList* aggregate_types = &resolver->ctx->aggregate_types;

    // NOTE: The procs bucket-list may grow during iteration if new proc symbols are encountered
    // while resolving proc/struct/union bodies.
    //
    // Therefore, _DO NOT CACHE_ procs->num_elems into a local variable.
    for (size_t i = 0; i < procs->num_elems; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(procs, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);
        assert(sym->kind == SYMBOL_PROC);

        if (!resolve_global_proc_body(resolver, sym)) {
            return false;
        }
    }

    for (size_t i = 0; i < aggregate_types->num_elems; i += 1) {
        void** sym_ptr = bucket_list_get_elem_packed(aggregate_types, i);
        assert(sym_ptr);
        Symbol* sym = (Symbol*)(*sym_ptr);
        assert(sym->kind == SYMBOL_TYPE);

        if (!try_complete_aggregate_type(resolver, sym->type)) {
            return false;
        }
    }

    return true;
}
