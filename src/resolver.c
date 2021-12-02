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
static bool resolve_proc_stmts(Resolver* resolver, Symbol* sym);

static bool cast_eop(ExprOperand* eop, Type* type);
static bool convert_eop(ExprOperand* eop, Type* dst_type);
static bool eop_is_null_ptr(ExprOperand eop);
static bool can_convert_eop(ExprOperand* operand, Type* dst_type);
static bool can_cast_eop(ExprOperand* eop, Type* dst_type);
static bool eop_decay(Resolver* resolver, ExprOperand* eop, ProgRange range);
static Expr* try_wrap_cast_expr(Resolver* resolver, ExprOperand* eop, Expr* orig_expr);

static Symbol* lookup_ident(Resolver* resolver, ExprIdent* expr);

static bool resolve_expr(Resolver* resolver, Expr* expr, Type* expected_type);
static bool resolve_expr_int(Resolver* resolver, Expr* expr);
static void resolve_binary_eop(TokenKind op, ExprOperand* dst, ExprOperand* left, ExprOperand* right);
static void resolve_unary_eop(TokenKind op, ExprOperand* dst, ExprOperand* src);
static bool resolve_expr_binary(Resolver* resolver, Expr* expr);
static bool resolve_expr_unary(Resolver* resolver, Expr* expr);
static bool resolve_expr_call(Resolver* resolver, Expr* expr);
static bool resolve_expr_ident(Resolver* resolver, Expr* expr);
static bool resolve_cond_expr(Resolver* resolver, Expr* expr, ExprOperand* expr_eop);

static Type* resolve_typespec(Resolver* resolver, TypeSpec* typespec);

enum ResolveStmtRetFlags {
    RESOLVE_STMT_SUCCESS = 0x1,
    RESOLVE_STMT_RETURNS = 0x2,
};

enum ResolveStmtInFlags {
    RESOLVE_STMT_BREAK_ALLOWED = 0x1,
    RESOLVE_STMT_CONTINUE_ALLOWED = 0x2,
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

static ModuleState enter_module(Resolver* resolver, Module* mod)
{
    ModuleState old_state = resolver->state;

    resolver->state.mod = mod;
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

static bool cast_eop(ExprOperand* eop, Type* dst_type)
{
    Type* src_type = eop->type;

    if (src_type == dst_type) {
        eop->type = dst_type;
        return true;
    }

    if (!can_cast_eop(eop, dst_type))
        return false;

    // From this point, the following is true:
    // 1) src_type != dst_type
    // 2) types are castable.

    if (eop->is_constexpr && eop->is_imm) {
        if (src_type->kind == TYPE_FLOAT) {
            eop->is_constexpr = dst_type->kind != TYPE_INTEGER;
        }
        else {
            if (src_type->kind == TYPE_ENUM)
                src_type = src_type->as_enum.base;

            if (dst_type->kind == TYPE_ENUM)
                dst_type = dst_type->as_enum.base;

            IntegerKind src_int_kind = src_type->as_integer.kind;
            IntegerKind dst_int_kind = dst_type->as_integer.kind;

            if (src_type->kind == TYPE_PTR)
                src_int_kind = INTEGER_U64;
            if (dst_type->kind == TYPE_PTR)
                dst_int_kind = INTEGER_U64;

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

    return true;
}

static bool convert_eop(ExprOperand* eop, Type* dst_type)
{
    if (!can_convert_eop(eop, dst_type))
        return false;

    cast_eop(eop, dst_type);

    eop->is_lvalue = false;

    return true;
}

static bool eop_is_null_ptr(ExprOperand eop)
{
    Type* type = eop.type;

    if (eop.is_constexpr && eop.is_imm && (type->kind == TYPE_INTEGER || type->kind == TYPE_PTR)) {
        cast_eop(&eop, builtin_types[BUILTIN_TYPE_U64].type);

        return eop.imm.as_int._u64 == 0;
    }

    return false;
}

static bool can_convert_eop(ExprOperand* operand, Type* dst_type)
{
    bool convertible = false;
    Type* src_type = operand->type;

    // Same types
    if (dst_type == src_type) {
        convertible = true;
    }
    // Can convert anything to void
    else if (dst_type == builtin_types[BUILTIN_TYPE_VOID].type) {
        convertible = true;
    }
    // Can convert between arithmetic types
    else if (type_is_arithmetic(dst_type) && type_is_arithmetic(src_type)) {
        convertible = true;
    }
    // Can convert const NULL (or 0) to a ptr (or proc ptr).
    else if (type_is_ptr_like(dst_type) && eop_is_null_ptr(*operand)) {
        convertible = true;
    }
    else if ((dst_type->kind == TYPE_PTR) && (src_type->kind == TYPE_PTR)) {
        Type* dst_pointed_type = dst_type->as_ptr.base;
        Type* src_pointed_type = src_type->as_ptr.base;

        // Can convert a "derived" type to a "base" type.
        // A type is "derived" if its first field is of type "base".
        // Ex: struct Base { ... };  struct Derived { Base base;};
        // Derived* d = malloc(...);
        // Base* b = d;
        if (type_is_aggregate(dst_pointed_type) && type_is_aggregate(src_pointed_type) &&
            (dst_pointed_type == src_pointed_type->as_aggregate.fields[0].type)) {
            convertible = true;
        }
        // Can convert if either is a void*
        else if ((dst_pointed_type == builtin_types[BUILTIN_TYPE_VOID].type) ||
                 (src_pointed_type == builtin_types[BUILTIN_TYPE_VOID].type)) {
            convertible = true;
        }
    }

    return convertible;
}

static bool can_cast_eop(ExprOperand* eop, Type* dst_type)
{
    bool castable = false;
    Type* src_type = eop->type;

    if (can_convert_eop(eop, dst_type))
        castable = true;
    else if (dst_type->kind == TYPE_INTEGER)
        castable = type_is_ptr_like(src_type);
    else if (src_type->kind == TYPE_INTEGER)
        castable = type_is_ptr_like(dst_type);
    else if (type_is_ptr_like(dst_type) && type_is_ptr_like(src_type))
        castable = true;

    return castable;
}

static void promote_int_eops(ExprOperand* eop)
{
    switch (eop->type->kind) {
    case TYPE_INTEGER:
    case TYPE_ENUM:
        if (eop->type->size < builtin_types[BUILTIN_TYPE_S32].type->size)
            cast_eop(eop, builtin_types[BUILTIN_TYPE_S32].type);
        break;
    default:
        break;
    }
}

static void convert_arith_eops(ExprOperand* left, ExprOperand* right)
{
    // If one is an f64, cast the other to f64.
    if (left->type == builtin_types[BUILTIN_TYPE_F64].type) {
        cast_eop(right, builtin_types[BUILTIN_TYPE_F64].type);
    }
    else if (right->type == builtin_types[BUILTIN_TYPE_F64].type) {
        cast_eop(left, builtin_types[BUILTIN_TYPE_F64].type);
    }
    // Else if one is an f32, cast the other to f32.
    else if (left->type == builtin_types[BUILTIN_TYPE_F32].type) {
        cast_eop(right, builtin_types[BUILTIN_TYPE_F32].type);
    }
    else if (right->type == builtin_types[BUILTIN_TYPE_F32].type) {
        cast_eop(left, builtin_types[BUILTIN_TYPE_F32].type);
    }
    // Else, do usual arithmetic conversions.
    else {
        assert(type_is_integer_like(left->type));
        assert(type_is_integer_like(right->type));

        // First, promote both to s32 if smaller than s32.
        // This is a lossless conversion.
        promote_int_eops(left);
        promote_int_eops(right);

        if (left->type != right->type) {
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
                    cast_eop(left, right->type);
                else
                    cast_eop(right, left->type);
            }
            else if (left_signed && (right_rank >= left_rank)) {
                cast_eop(left, right->type);
            }
            else if (right_signed && (left_rank >= right_rank)) {
                cast_eop(right, left->type);
            }
            else if (left_signed && (left_size > right_size)) {
                cast_eop(right, left->type);
            }
            else if (right_signed && (right_size > left_size)) {
                cast_eop(left, right->type);
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

                cast_eop(left, type);
                cast_eop(right, type);

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

static void eval_const_binary_op(TokenKind op, ExprOperand* dst, Type* type, Scalar left, Scalar right)
{
    if (type_is_integer_like(type)) {
        ExprOperand left_eop = OP_FROM_CONST(type, left);
        ExprOperand right_eop = OP_FROM_CONST(type, right);
        bool is_signed = false;

        if (type->kind == TYPE_ENUM) {
            is_signed = type->as_enum.base->as_integer.is_signed;
        }
        else if (type->kind == TYPE_INTEGER) {
            is_signed = type->as_integer.is_signed;
        }

        // Compute the operation in the largest type available.
        if (is_signed) {
            cast_eop(&left_eop, builtin_types[BUILTIN_TYPE_S64].type);
            cast_eop(&right_eop, builtin_types[BUILTIN_TYPE_S64].type);

            s64 r = eval_binary_op_s64(op, left_eop.imm.as_int._s64, right_eop.imm.as_int._s64);

            dst->type = builtin_types[BUILTIN_TYPE_S64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._s64 = r;
        }
        else {
            cast_eop(&left_eop, builtin_types[BUILTIN_TYPE_U64].type);
            cast_eop(&right_eop, builtin_types[BUILTIN_TYPE_U64].type);

            u64 r = eval_binary_op_u64(op, left_eop.imm.as_int._u64, right_eop.imm.as_int._u64);

            dst->type = builtin_types[BUILTIN_TYPE_U64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._u64 = r;
        }

        // Cast it back to the original type.
        cast_eop(dst, type);
    }
    else {
        assert(type->kind == TYPE_FLOAT);
    }
}

static void eval_const_unary_op(TokenKind op, ExprOperand* dst, Type* type, Scalar val)
{
    if (type_is_integer_like(type)) {
        ExprOperand val_eop = OP_FROM_CONST(type, val);
        bool is_signed = false;

        if (type->kind == TYPE_ENUM) {
            is_signed = type->as_enum.base->as_integer.is_signed;
        }
        else if (type->kind == TYPE_INTEGER) {
            is_signed = type->as_integer.is_signed;
        }

        // Compute the operation in the largest type available.
        if (is_signed) {
            cast_eop(&val_eop, builtin_types[BUILTIN_TYPE_S64].type);

            dst->type = builtin_types[BUILTIN_TYPE_S64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._s64 = eval_unary_op_s64(op, val_eop.imm.as_int._s64);
        }
        else {
            cast_eop(&val_eop, builtin_types[BUILTIN_TYPE_U64].type);

            dst->type = builtin_types[BUILTIN_TYPE_U64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._u64 = eval_unary_op_u64(op, val_eop.imm.as_int._u64);
        }

        // Cast it back to the original type.
        cast_eop(dst, type);
    }
    else {
        assert(type->kind == TYPE_FLOAT);
    }
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
        type = builtin_types[BUILTIN_TYPE_INT].type;
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

    expr->super.type = builtin_types[BUILTIN_TYPE_USIZE].type;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_int._u64 = type->size;

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

static void resolve_binary_eop(TokenKind op, ExprOperand* dst, ExprOperand* left, ExprOperand* right)
{
    convert_arith_eops(left, right);

    if (left->is_constexpr && right->is_constexpr) {
        assert(left->is_imm && right->is_imm);
        eval_const_binary_op(op, dst, left->type, left->imm, right->imm);
    }
    else {
        dst->type = left->type;
        dst->is_constexpr = false;
        dst->is_imm = false;
        dst->is_lvalue = false;
    }
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

    cast_eop(int_eop, builtin_types[BUILTIN_TYPE_U64].type);

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

    if (!eop_decay(resolver, &left_op, ebinary->left->range))
        return false;

    if (!eop_decay(resolver, &right_op, ebinary->right->range))
        return false;

    switch (ebinary->op) {
    case TKN_PLUS:
        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(TKN_PLUS, &dst_op, &left_op, &right_op);
        }
        else if ((left_op.type->kind == TYPE_PTR) && type_is_integer_like(right_op.type)) {
            if (!resolve_ptr_int_arith(resolver, &dst_op, &left_op, &right_op)) {
                resolver_on_error(resolver, ebinary->left->range, "Cannot add to a pointer with a base type (%s) of zero size",
                                  type_name(left_op.type->as_ptr.base));

                return false;
            }
        }
        else if (type_is_integer_like(left_op.type) && (right_op.type->kind == TYPE_PTR)) {
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
            resolve_binary_eop(TKN_MINUS, &dst_op, &left_op, &right_op);
        }
        // ptr - int
        else if (left_is_ptr && type_is_integer_like(right_op.type)) {
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

        resolve_binary_eop(ebinary->op, &dst_op, &left_op, &right_op);

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

        promote_int_eops(&left_op);
        promote_int_eops(&right_op);

        if (left_op.is_constexpr && right_op.is_constexpr) {
            assert(left_op.is_imm && right_op.is_imm);
            eval_const_binary_op(ebinary->op, &dst_op, left_op.type, left_op.imm, right_op.imm);
        }
        else {
            dst_op.type = left_op.type;
        }

        break;
    }
    case TKN_EQ:
    case TKN_NOTEQ: {
        bool left_is_ptr = (left_op.type->kind == TYPE_PTR);
        bool right_is_ptr = (right_op.type->kind == TYPE_PTR);

        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(ebinary->op, &dst_op, &left_op, &right_op);

            // NOTE: resolve_binary_eop will cast to the common type, so cast to s32.
            cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
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

                cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr && right_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else if (left_is_ptr && eop_is_null_ptr(right_op)) {
            if (left_op.is_constexpr && left_op.is_imm) {
                u64 left_u64 = left_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, left_u64, 0);

                cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else if (right_is_ptr && eop_is_null_ptr(left_op)) {
            if (right_op.is_constexpr && right_op.is_imm) {
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, right_u64, 0);

                cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
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
            resolve_binary_eop(ebinary->op, &dst_op, &left_op, &right_op);

            // NOTE: resolve_binary_eop will cast to the common type, so cast to s32.
            cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
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

                cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr && right_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else if (left_is_ptr && eop_is_null_ptr(right_op)) {
            if (left_op.is_constexpr && left_op.is_imm) {
                u64 left_u64 = left_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, left_u64, 0);

                cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
            }
            else {
                dst_op.is_constexpr = left_op.is_constexpr;
                dst_op.type = builtin_types[BUILTIN_TYPE_S32].type;
            }
        }
        else if (right_is_ptr && eop_is_null_ptr(left_op)) {
            if (right_op.is_constexpr && right_op.is_imm) {
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, right_u64, 0);

                cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
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
                cast_eop(&left_op, builtin_types[BUILTIN_TYPE_U64].type);
                cast_eop(&right_op, builtin_types[BUILTIN_TYPE_U64].type);

                u64 left_u64 = left_op.imm.as_int._u64;
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_U64].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._u64 = eval_binary_op_u64(ebinary->op, left_u64, right_u64);

                cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
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
    expr->imm = dst_op.imm;

    return true;
}

static void resolve_unary_eop(TokenKind op, ExprOperand* dst, ExprOperand* src)
{
    promote_int_eops(src);

    if (src->is_constexpr && src->is_imm) {
        eval_const_unary_op(op, dst, src->type, src->imm);
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
        if (!eop_decay(resolver, &src_op, expr->range))
            return false;

        if (!type_is_arithmetic(src_op.type)) {
            resolver_on_error(resolver, expr->range, "Can only use unary +/- with arithmetic types");
            return false;
        }

        resolve_unary_eop(eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_NEG:
        if (!eop_decay(resolver, &src_op, expr->range))
            return false;

        if (!type_is_integer_like(src_op.type)) {
            resolver_on_error(resolver, expr->range, "Can only use unary ~ with integer types");
            return false;
        }

        resolve_unary_eop(eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_NOT:
        if (!eop_decay(resolver, &src_op, expr->range))
            return false;

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

            cast_eop(&dst_op, builtin_types[BUILTIN_TYPE_S32].type);
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
            Symbol* sym = lookup_ident(resolver, expr_ident);

            assert(sym);

            is_constexpr = !sym->is_local;
        }

        dst_op.type = type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, src_op.type);
        dst_op.is_constexpr = is_constexpr;
        break;
    case TKN_ASTERISK: // NOTE: Dereference operator.
        if (!eop_decay(resolver, &src_op, expr->range))
            return false;

        if (src_op.type->kind != TYPE_PTR) {
            resolver_on_error(resolver, expr->range, "Cannot dereference a non-pointer value.");
            return false;
        }

        dst_op.type = src_op.type->as_ptr.base;
        dst_op.is_lvalue = true;
        break;
    default:
        resolver_on_error(resolver, expr->range, "Unary operation type `%d` not supported", eunary->op);
        return false;
    }

    expr->type = dst_op.type;
    expr->is_lvalue = dst_op.is_lvalue;
    expr->is_constexpr = dst_op.is_constexpr;
    expr->imm = dst_op.imm;

    return true;
}

static bool resolve_expr_index(Resolver* resolver, Expr* expr)
{
    ExprIndex* eindex = (ExprIndex*)expr;

    // Resolve array expression
    if (!resolve_expr(resolver, eindex->array, NULL))
        return false;

    ExprOperand array_op = OP_FROM_EXPR(eindex->array);

    if (!eop_decay(resolver, &array_op, eindex->array->range))
        return false;

    if (array_op.type->kind != TYPE_PTR) {
        resolver_on_error(resolver, eindex->array->range, "Cannot index non-pointer or non-array type `%s`",
                          type_name(eindex->array->type));
        return false;
    }

    // Resolve array index expression
    if (!resolve_expr(resolver, eindex->index, NULL))
        return false;

    ExprOperand index_op = OP_FROM_EXPR(eindex->index);

    if (!convert_eop(&index_op, builtin_types[BUILTIN_TYPE_S64].type)) {
        resolver_on_error(resolver, eindex->index->range, "Array index of type `%s` cannot be converted to an integer",
                          type_name(eindex->index->type));
        return false;
    }

    // Cast array and index expressions if necessary.
    eindex->array = try_wrap_cast_expr(resolver, &array_op, eindex->array);
    eindex->index = try_wrap_cast_expr(resolver, &index_op, eindex->index);

    // Set overall expression type and attributes.
    expr->type = array_op.type->as_ptr.base;
    expr->is_lvalue = true;
    expr->is_constexpr = false;
    expr->is_imm = false;

    return true;
}

static Symbol* lookup_ident(Resolver* resolver, ExprIdent* expr)
{
    //
    // Tries to lookup a symbol for an identifier in the form <module_namespace>::<identifier_name>
    //

    Symbol* sym = NULL;

    if (expr->mod_ns) {
        // Lookup namespace symbol.
        Symbol* sym_modns = resolve_name(resolver, expr->mod_ns);

        if (!sym_modns || (sym_modns->kind != SYMBOL_MODULE)) {
            resolver_on_error(resolver, expr->super.range, "Unknown module namespace `%s::` in expression", expr->mod_ns->str);
            return NULL;
        }

        StmtImport* stmt = (StmtImport*)sym_modns->as_mod.stmt;
        Identifier* sym_name = get_import_sym_name(stmt, expr->name);

        if (!sym_name) {
            resolver_on_error(resolver, expr->super.range,
                              "Identifier `%s` is not among the imported symbols in module namespace `%s`", expr->name->str,
                              sym_modns->name->str);
            return NULL;
        }

        // Enter the namespace's module, and then try to lookup the identifier with its native name.
        ModuleState mod_state = enter_module(resolver, sym_modns->as_mod.mod);
        sym = resolve_export_name(resolver, sym_name);
        exit_module(resolver, mod_state);
    }
    else {
        sym = resolve_name(resolver, expr->name);
    }

    if (!sym) {
        // TODO: Print full identifier name (with module namespace).
        resolver_on_error(resolver, expr->super.range, "Unknown symbol `%s` in expression", expr->name->str);
        return NULL;
    }

    return sym;
}

static bool resolve_expr_ident(Resolver* resolver, Expr* expr)
{
    ExprIdent* eident = (ExprIdent*)expr;
    Symbol* sym = lookup_ident(resolver, eident);

    if (!sym) {
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
        expr->imm = ((DeclConst*)(sym->decl))->init->imm;

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

    resolver_on_error(resolver, expr->range, "Expression identifier `%s` must refer to a var, const, or proc declaration",
                      eident->name->str);
    return false;
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
    if (proc_type->as_proc.num_params != ecall->num_args) {
        resolver_on_error(resolver, expr->range, "Incorrect number of procedure call arguments. Expected `%d` arguments, but got `%d`",
                          proc_type->as_proc.num_params, ecall->num_args);
        return false;
    }

    // Resolve argument expressions and verify that argument types match parameter types.
    List* head = &ecall->args;
    List* it = head->next;
    Type** params = proc_type->as_proc.params;
    size_t i = 0;

    while (it != head) {
        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);

        if (!resolve_expr(resolver, arg->expr, NULL))
            return false;

        Type* param_type = params[i];
        ExprOperand arg_eop = OP_FROM_EXPR(arg->expr);

        if (!eop_decay(resolver, &arg_eop, arg->range))
            return false;

        if (!convert_eop(&arg_eop, param_type)) {
            resolver_on_error(resolver, arg->range,
                              "Incorrect type for argument %d of procedure call. Expected type `%s`, but got `%s`", (i + 1),
                              type_name(params[i]), type_name(arg->expr->type));
            return false;
        }

        arg->expr = try_wrap_cast_expr(resolver, &arg_eop, arg->expr);

        it = it->next;
        i += 1;
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

    if (!eop_decay(resolver, &src_eop, ecast->expr->range))
        return false;

    if (!cast_eop(&src_eop, cast_type)) {
        resolver_on_error(resolver, expr->range, "Cannot cast from type `%s` to type `%s`", type_name(ecast->expr->type),
                          type_name(cast_type));
        return false;
    }

    assert(cast_type == src_eop.type);

    expr->type = src_eop.type;
    expr->is_lvalue = src_eop.is_lvalue;
    expr->is_constexpr = src_eop.is_constexpr;
    expr->is_imm = src_eop.is_imm;
    expr->imm = src_eop.imm;

    return true;
}

static bool resolve_expr_array_compound_lit(Resolver* resolver, ExprCompoundLit* expr, Type* type)
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
    bool is_compound_lit = expr->typespec != NULL; // Otherwise, it is an initializer
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

        if ((elem_type->kind == TYPE_PTR) && !eop_decay(resolver, &init_op, initzer->init->range)) {
            return false;
        }

        bool is_valid_array_init = (initzer->init->kind == CST_ExprCompoundLit) || (initzer->init->kind == CST_ExprStr);

        if ((elem_type->kind == TYPE_ARRAY) && !is_valid_array_init) {
            resolver_on_error(resolver, initzer->init->range, "Invalid array initializer");
            return false;
        }

        // Initializer expression should be convertible to the element type.
        if (!convert_eop(&init_op, elem_type)) {
            resolver_on_error(resolver, initzer->init->range, "Array initializer of type `%s` cannot be converted to `%s`",
                              type_name(initzer->init->type), type_name(elem_type));
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

    // TODO: HMMMMMM.... there's a difference between an array initializer (not lvalue) and a compound literal (lvalue),
    // but the syntax is ambiguous.
    expr->super.type = type;
    expr->super.is_lvalue = is_compound_lit;
    expr->super.is_imm = false;
    expr->super.is_constexpr =
        !is_compound_lit &&
        all_initzers_constexpr; // || (is_compound_lit && all_initzers_constexpr && type_is_const(type->as_array.base))

    return true;
}

static bool resolve_expr_compound_lit(Resolver* resolver, ExprCompoundLit* expr, Type* expected_type)
{
    Type* type = expected_type;

    if (expr->typespec) {
        type = resolve_typespec(resolver, expr->typespec);

        if (expected_type && type != expected_type) {
            resolver_on_error(resolver, expr->typespec->range, "Compound literal type `%s` does not match expected type `%s`",
                              type_name(type), type_name(expected_type));
            return false;
        }
    }

    if (!type) {
        resolver_on_error(resolver, expr->super.range, "Unknown type for compound literal");
        return false;
    }

    // For now, only allow array types.
    // TODO: Support struct types
    if (type->kind == TYPE_ARRAY) {
        return resolve_expr_array_compound_lit(resolver, expr, type);
    }

    resolver_on_error(resolver, expr->super.range, "Invalid compound literal type `%s`", type_name(type));

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

        // TODO: Support module path

        Identifier* ident_name = ts->name;
        Symbol* ident_sym = resolve_name(resolver, ident_name);

        if (!ident_sym) {
            resolver_on_error(resolver, typespec->range, "Undefined type `%s`", ident_name->str);
            return NULL;
        }

        if (ident_sym->kind != SYMBOL_TYPE) {
            resolver_on_error(resolver, typespec->range, "Identifier `%s` is not a type", ident_name->str);
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

        size_t len = 0;

        if (ts->len) {
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

            param = type_incomplete_decay(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, param);

            if (param->kind == TYPE_ARRAY) {
                resolver_on_error(resolver, proc_param->range,
                                  "Procedure parameter cannot be an array type. Use a pointer or an incomplete array type.");
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (param->size == 0) {
                resolver_on_error(resolver, proc_param->range, "Invalid procedure paramater type `%s` of zero size.",
                                  type_name(param));
                allocator_restore_state(mem_state);
                return NULL;
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

            ret = type_incomplete_decay(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, ret);

            if (ret->kind == TYPE_ARRAY) {
                assert(ts->ret);
                resolver_on_error(resolver, ts->ret->range, "Procedure return type cannot be an array, but found `%s`.",
                                  type_name(ret));
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (ret->size == 0) {
                resolver_on_error(resolver, ts->ret->range, "Invalid procedure return type `%s` of zero size.", type_name(ret));
                return NULL;
            }
        }

        Type* type = type_proc(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.procs, array_len(params), params, ret);
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

            if (type_has_incomplete_array(expr->type)) {
                resolver_on_error(resolver, expr->range, "Expression type `%s` contains array of unknown size.",
                                  type_name(expr->type));
                return false;
            }

            ExprOperand right_eop = OP_FROM_EXPR(expr);
            bool is_valid_array_init = (expr->kind == CST_ExprCompoundLit) || (expr->kind == CST_ExprStr);

            // If assigning an array to a pointer, try to decay the right-hand-side expression into a pointer.
            // Ex: var p : ^char = bytes_array;
            if ((declared_type->kind == TYPE_PTR) && !eop_decay(resolver, &right_eop, expr->range)) {
                return false;
            }

            // If the declared type is an array, rhs must be a vaild array initializer.
            // Ex: var buf : [4]char = {0,1,2,3};
            // Ex: var buf : []char = "Hello";
            if ((declared_type->kind == TYPE_ARRAY) && !is_valid_array_init) {
                resolver_on_error(resolver, expr->range, "Invalid array initializer");
                return false;
            }

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
            else if (!convert_eop(&right_eop, declared_type)) {
                resolver_on_error(resolver, sym->decl->range, "Incompatible types. Cannot convert `%s` to `%s`",
                                  type_name(right_eop.type), type_name(declared_type));
                return false;
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

    // TODO: Complete incomplete aggregate type

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

    // TODO: Complete incomplete aggregate type
    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static bool resolve_decl_enum(Resolver* resolver, Symbol* sym)
{
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

    sym->type = type_enum(&resolver->ctx->ast_mem, base_type, decl_enum);
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static bool resolve_decl_const(Resolver* resolver, Symbol* sym)
{
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

        if (!convert_eop(&init_eop, declared_type)) {
            resolver_on_error(resolver, typespec->range, "Incompatible types. Cannot convert expression of type `%s` to `%s`",
                              type_name(init->type), type_name(declared_type));
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

    type = type_incomplete_decay(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, type);

    if (type->kind == TYPE_ARRAY) {
        resolver_on_error(resolver, typespec->range,
                          "Procedure parameter cannot be an array type. Use a pointer or an incomplete array type.");
        return false;
    }

    if (type->size == 0) {
        resolver_on_error(resolver, decl->super.range, "Cannot declare a parameter of zero size.");
        return false;
    }

    sym->type = type;
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static bool resolve_decl_proc(Resolver* resolver, Symbol* sym)
{
    DeclProc* decl = (DeclProc*)sym->decl;

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

        // TODO: complete incomplete param type (struct, union)

        array_push(params, param_sym->type);
    }

    pop_scope(resolver);
    assert(array_len(params) == decl->num_params);
    allocator_restore_state(mem_state);

    Type* ret_type = builtin_types[BUILTIN_TYPE_VOID].type;

    if (decl->ret) {
        ret_type = resolve_typespec(resolver, decl->ret);

        if (!ret_type) {
            return false;
        }

        ret_type = type_incomplete_decay(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, ret_type);

        if (ret_type->kind == TYPE_ARRAY) {
            resolver_on_error(resolver, decl->ret->range,
                              "Procedure return type cannot be an array type. Use a pointer or an incomplete array type.");
            return false;
        }

        if (ret_type->size == 0) {
            resolver_on_error(resolver, decl->super.range, "Invalid procedure return type `%s` of zero size.", type_name(ret_type));
            return false;
        }
    }

    sym->type = type_proc(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.procs, array_len(params), params, ret_type);
    sym->status = SYMBOL_STATUS_RESOLVED;

    return true;
}

static bool resolve_proc_stmts(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_PROC);

    DeclProc* dproc = (DeclProc*)(sym->decl);

    set_scope(resolver, dproc->scope);

    Type* ret_type = sym->type->as_proc.ret;
    unsigned r = resolve_stmt_block_body(resolver, &dproc->stmts, ret_type, 0);
    bool returns = r & RESOLVE_STMT_RETURNS;
    bool success = r & RESOLVE_STMT_SUCCESS;

    pop_scope(resolver);

    dproc->returns = returns;

    if ((ret_type != builtin_types[BUILTIN_TYPE_VOID].type) && !returns && success) {
        resolver_on_error(resolver, dproc->super.range, "Not all code paths in procedure `%s` return a value", dproc->super.name->str);
        return false;
    }

    return success;
}

static bool resolve_global_proc_body(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_PROC);
    DeclProc* dproc = (DeclProc*)(sym->decl);

    if (dproc->is_incomplete) {
        return true;
    }

    ModuleState mod_state = enter_module(resolver, sym->home);
    bool success = resolve_proc_stmts(resolver, sym);

    exit_module(resolver, mod_state);

    return success;

    // TODO: Support local struct/union/enum declarations inside procedures.
    /*
    while (array_len(resolver->incomplete_syms))
    {
        Symbol* sym = array_pop(resolver->incomplete_syms);

        if (sym->kind == SYMBOL_PROC)
        {
            push_ir_proc(resolver, sym);

            if (!resolve_proc_stmts(resolver, sym))
                return false;
        }
    }
    */

    return true;
}

static unsigned resolve_stmt_block_body(Resolver* resolver, List* stmts, Type* ret_type, unsigned flags)
{
    unsigned ret_success = RESOLVE_STMT_SUCCESS;
    List* head = stmts;

    for (List* it = head->next; it != head; it = it->next) {
        Stmt* child_stmt = list_entry(it, Stmt, lnode);

        if (ret_success & RESOLVE_STMT_RETURNS) {
            resolver_on_error(resolver, child_stmt->range, "Statement will never be executed; all previous control paths return");

            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }

        unsigned r = resolve_stmt(resolver, child_stmt, ret_type, flags);

        // NOTE: Track whether any statement in the block returns from the parent procedure.
        ret_success = (r & RESOLVE_STMT_RETURNS) | ret_success;

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

    if (!eop_decay(resolver, expr_eop, expr->range))
        return false;

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
    if (sif->else_blk.body)
        ret &= resolve_stmt(resolver, sif->else_blk.body, ret_type, flags);
    else
        ret &= ~RESOLVE_STMT_RETURNS;

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
    unsigned ret = resolve_stmt(resolver, swhile->body, ret_type, flags | RESOLVE_STMT_BREAK_ALLOWED | RESOLVE_STMT_CONTINUE_ALLOWED);

    // NOTE: Because while loops don't have an "else" path, we can't say that all control paths return.
    // TODO: Add else to while loop!!
    ret &= ~RESOLVE_STMT_RETURNS;

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
    unsigned ret = resolve_stmt(resolver, swhile->body, ret_type, flags | RESOLVE_STMT_BREAK_ALLOWED | RESOLVE_STMT_CONTINUE_ALLOWED);

    // Report an error if the do-while loop always returns before the condition check.
    if (ret & RESOLVE_STMT_RETURNS) {
        resolver_on_error(resolver, swhile->cond->range, "All paths in do-while loop's body return before condition check.");
        ret &= ~RESOLVE_STMT_SUCCESS;
    }

    return ret;
}

static bool eop_decay(Resolver* resolver, ExprOperand* eop, ProgRange range)
{
    if (eop->type->kind != TYPE_ARRAY) {
        return true;
    }

    if (!eop->is_lvalue) {
        resolver_on_error(resolver, range, "An array rvalue (e.g., initializer) cannot be converted to a pointer");
        return false;
    }

    eop->type = type_ptr(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.ptrs, eop->type->as_array.base);
    eop->is_lvalue = false;

    return true;
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
    Expr* left_expr = sassign->left;
    Expr* right_expr = sassign->right;

    if (!resolve_expr(resolver, left_expr, NULL))
        return 0;

    if (!resolve_expr(resolver, right_expr, NULL))
        return 0;

    if (!left_expr->is_lvalue) {
        resolver_on_error(resolver, left_expr->range, "Left side of assignment statement must be an l-value");
        return 0;
    }

    if (left_expr->type->kind == TYPE_ARRAY) {
        resolver_on_error(resolver, left_expr->range, "Left side of assignment statement cannot be an array");
        return 0;
    }

    // TODO: Support other assignment operators.
    if (sassign->op_assign != TKN_ASSIGN) {
        resolver_on_error(resolver, stmt->range, "Sorry! Only the `=` assignment operator is currently supported. Soon!");
        return 0;
    }

    ExprOperand right_eop = OP_FROM_EXPR(right_expr);

    if ((left_expr->type->kind == TYPE_PTR) && !eop_decay(resolver, &right_eop, right_expr->range))
        return false;

    if (!convert_eop(&right_eop, left_expr->type)) {
        resolver_on_error(resolver, right_expr->range, "Type mismatch in assignment statement: expected type `%s`, but got `%s`",
                          type_name(left_expr->type), type_name(right_eop.type));
        return 0;
    }

    sassign->right = try_wrap_cast_expr(resolver, &right_eop, sassign->right);

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
    bool break_allowed = flags & RESOLVE_STMT_BREAK_ALLOWED;
    bool continue_allowed = flags & RESOLVE_STMT_CONTINUE_ALLOWED;

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

        if (!sret->expr && (ret_type != builtin_types[BUILTIN_TYPE_VOID].type)) {
            resolver_on_error(resolver, stmt->range, "Return statement is missing a return value of type `%s`", type_name(ret_type));
            break;
        }

        if (sret->expr) {
            if (!resolve_expr(resolver, sret->expr, ret_type))
                break;

            ExprOperand ret_eop = OP_FROM_EXPR(sret->expr);

            if (!eop_decay(resolver, &ret_eop, sret->expr->range))
                return false;

            if (!convert_eop(&ret_eop, ret_type)) {
                resolver_on_error(resolver, sret->expr->range, "Invalid return type. Wanted `%s`, but got `%s`", type_name(ret_type),
                                  type_name(ret_eop.type));
                break;
            }

            sret->expr = try_wrap_cast_expr(resolver, &ret_eop, sret->expr);
        }

        ret |= RESOLVE_STMT_SUCCESS;
        break;
    }
    case CST_StmtBreak: {
        if (break_allowed)
            ret = RESOLVE_STMT_SUCCESS;
        else
            resolver_on_error(resolver, stmt->range, "Illegal break statement");

        break;
    }
    case CST_StmtContinue: {
        if (continue_allowed)
            ret = RESOLVE_STMT_SUCCESS;
        else
            resolver_on_error(resolver, stmt->range, "Illegal continue statement");

        break;
    }
    case CST_StmtIf: {
        ret = resolve_stmt_if(resolver, stmt, ret_type, flags);
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

            if (!sym)
                resolver_on_error(resolver, stmt->range, "Identifier `%s` shadows a previous local declaration", decl->name->str);
            else if (resolve_decl_var(resolver, sym))
                ret = RESOLVE_STMT_SUCCESS;
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

    stmt->returns = ret & RESOLVE_STMT_RETURNS;

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
            resolver_on_error(resolver, decl->range, "Aggregate type declarations are not supported _yet_.");
            success = false;
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

    return true;
}
