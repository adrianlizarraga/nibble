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

typedef struct CastResult {
    bool success;
    bool bad_lvalue;
} CastResult;

static void resolver_cast_error(Resolver* resolver, CastResult cast_res, ProgRange range, const char* err_prefix, Type* src_type,
                                Type* dst_type)
{
    assert(!cast_res.success);

    if (cast_res.bad_lvalue) {
        resolver_on_error(resolver, range, "%s: cannot convert a temporary (`%s`) to type `%s`.", err_prefix, type_name(src_type),
                          type_name(dst_type));
    }
    else {
        resolver_on_error(resolver, range, "%s: cannot convert `%s` to type `%s`.", err_prefix, type_name(src_type),
                          type_name(dst_type));
    }
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

static CastResult can_convert_eop(ExprOperand* operand, Type* dst_type, bool forbid_rvalue_decay)
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
    // Can convert a pointer to a bool.
    else if (type_is_bool(dst_type)) {
        convertible = type_is_ptr_like(src_type);
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
        // Can convert a "derived" type to a "base" type.
        // A type is "derived" if its first field is of type "base".
        // Ex: struct Base { ... };  struct Derived { Base base;};
        // Derived* d = malloc(...);
        // Base* b = d;
        if (ptr_types_are_derived(dst_type, src_type)) {
            convertible = true;
        }
        // Can convert if either is a ^void
        else if ((dst_type == type_ptr_void) || (src_type == type_ptr_void)) {
            convertible = true;
        }
        // Can convert if pointer base types are compatible (e.g., ^u8 is compatible with ^U8EnumType)
        else if (types_are_compatible(dst_type->as_ptr.base, src_type->as_ptr.base)) {
            convertible = true;
        }
    }

    CastResult r = {.success = convertible && !bad_lvalue, .bad_lvalue = bad_lvalue};

    return r;
}

static CastResult can_cast_eop(ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay)
{
    Type* src_type = eop->type;

    CastResult r = can_convert_eop(eop, dst_type, forbid_rvalue_decay);

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

#define CASE_INT_CAST(k, o, t, f)                       \
    case k:                                             \
        switch (t) {                                    \
        case INTEGER_BOOL:                              \
            o->imm.as_int._bool = o->imm.as_int.f != 0; \
            break;                                      \
        case INTEGER_U8:                                \
            o->imm.as_int._u8 = (u8)o->imm.as_int.f;    \
            break;                                      \
        case INTEGER_S8:                                \
            o->imm.as_int._s8 = (s8)o->imm.as_int.f;    \
            break;                                      \
        case INTEGER_U16:                               \
            o->imm.as_int._u16 = (u16)o->imm.as_int.f;  \
            break;                                      \
        case INTEGER_S16:                               \
            o->imm.as_int._s16 = (s16)o->imm.as_int.f;  \
            break;                                      \
        case INTEGER_U32:                               \
            o->imm.as_int._u32 = (u32)o->imm.as_int.f;  \
            break;                                      \
        case INTEGER_S32:                               \
            o->imm.as_int._s32 = (s32)o->imm.as_int.f;  \
            break;                                      \
        case INTEGER_U64:                               \
            o->imm.as_int._u64 = (u64)o->imm.as_int.f;  \
            break;                                      \
        case INTEGER_S64:                               \
            o->imm.as_int._s64 = (s64)o->imm.as_int.f;  \
            break;                                      \
        default:                                        \
            o->is_constexpr = false;                    \
            assert(0);                                  \
            break;                                      \
        }                                               \
        break;

#define CASE_INT_FLOAT_CAST(k, o, t, f)                  \
    case k:                                              \
        switch (t) {                                     \
        case FLOAT_F64:                                  \
            o->imm.as_float._f64 = (f64)o->imm.as_int.f; \
            break;                                       \
        case FLOAT_F32:                                  \
            o->imm.as_float._f32 = (f32)o->imm.as_int.f; \
            break;                                       \
        default:                                         \
            o->is_constexpr = false;                     \
            assert(0);                                   \
            break;                                       \
        }                                                \
        break;

#define CASE_FLOAT_INT_CAST(k, o, t, f)                     \
    case k:                                                 \
        switch (t) {                                        \
        case INTEGER_BOOL:                                  \
            o->imm.as_int._bool = o->imm.as_float.f != 0.0; \
            break;                                          \
        case INTEGER_U8:                                    \
            o->imm.as_int._u8 = (u8)o->imm.as_float.f;      \
            break;                                          \
        case INTEGER_S8:                                    \
            o->imm.as_int._s8 = (s8)o->imm.as_float.f;      \
            break;                                          \
        case INTEGER_U16:                                   \
            o->imm.as_int._u16 = (u16)o->imm.as_float.f;    \
            break;                                          \
        case INTEGER_S16:                                   \
            o->imm.as_int._s16 = (s16)o->imm.as_float.f;    \
            break;                                          \
        case INTEGER_U32:                                   \
            o->imm.as_int._u32 = (u32)o->imm.as_float.f;    \
            break;                                          \
        case INTEGER_S32:                                   \
            o->imm.as_int._s32 = (s32)o->imm.as_float.f;    \
            break;                                          \
        case INTEGER_U64:                                   \
            o->imm.as_int._u64 = (u64)o->imm.as_float.f;    \
            break;                                          \
        case INTEGER_S64:                                   \
            o->imm.as_int._s64 = (s64)o->imm.as_float.f;    \
            break;                                          \
        default:                                            \
            assert(0);                                      \
            break;                                          \
        }                                                   \
        break;

static CastResult cast_eop(ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay)
{
    Type* src_type = eop->type;

    CastResult r = can_cast_eop(eop, dst_type, forbid_rvalue_decay);

    if (!r.success)
        return r;

    // From this point, the following is true:
    // 1) src_type != dst_type
    // 2) types are castable.

    if (eop->is_constexpr && eop->is_imm) {
        if (src_type->kind == TYPE_FLOAT && dst_type->kind == TYPE_FLOAT) {
            FloatKind src_float_kind = src_type->as_float.kind;
            FloatKind dst_float_kind = dst_type->as_float.kind;

            switch (src_float_kind) {
            case FLOAT_F32: {
                if (dst_float_kind == FLOAT_F64) {
                    eop->imm.as_float._f64 = (f64)eop->imm.as_float._f32;
                }
                break;
            }
            case FLOAT_F64: {
                if (dst_float_kind == FLOAT_F32) {
                    eop->imm.as_float._f32 = (f32)eop->imm.as_float._f64;
                }
                break;
            }
            default:
                assert(0);
                break;
            }
        }
        else if (src_type->kind == TYPE_FLOAT && type_is_integer_like(dst_type)) {
            FloatKind src_float_kind = src_type->as_float.kind;
            IntegerKind dst_int_kind =
                dst_type->kind == TYPE_ENUM ? dst_type->as_enum.base->as_integer.kind : dst_type->as_integer.kind;

            switch (src_float_kind) {
                CASE_FLOAT_INT_CAST(FLOAT_F32, eop, dst_int_kind, _f32)
                CASE_FLOAT_INT_CAST(FLOAT_F64, eop, dst_int_kind, _f64)
            default:
                assert(0);
                break;
            }
        }
        else if (type_is_integer_like(src_type) && dst_type->kind == TYPE_FLOAT) {
            FloatKind dst_float_kind = dst_type->as_float.kind;
            IntegerKind src_int_kind =
                src_type->kind == TYPE_ENUM ? src_type->as_enum.base->as_integer.kind : src_type->as_integer.kind;

            switch (src_int_kind) {
                CASE_INT_FLOAT_CAST(INTEGER_BOOL, eop, dst_float_kind, _bool)
                CASE_INT_FLOAT_CAST(INTEGER_U8, eop, dst_float_kind, _u8)
                CASE_INT_FLOAT_CAST(INTEGER_S8, eop, dst_float_kind, _s8)
                CASE_INT_FLOAT_CAST(INTEGER_U16, eop, dst_float_kind, _u16)
                CASE_INT_FLOAT_CAST(INTEGER_S16, eop, dst_float_kind, _s16)
                CASE_INT_FLOAT_CAST(INTEGER_U32, eop, dst_float_kind, _u32)
                CASE_INT_FLOAT_CAST(INTEGER_S32, eop, dst_float_kind, _s32)
                CASE_INT_FLOAT_CAST(INTEGER_U64, eop, dst_float_kind, _u64)
                CASE_INT_FLOAT_CAST(INTEGER_S64, eop, dst_float_kind, _s64)
            default:
                assert(0);
                break;
            }
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
                assert(src_type->kind == TYPE_INTEGER);
                src_int_kind = src_type->as_integer.kind;
            }

            if (dst_type->kind == TYPE_ENUM) {
                dst_int_kind = dst_type->as_enum.base->as_integer.kind;
            }
            else if (dst_type->kind == TYPE_PTR) {
                dst_int_kind = INTEGER_U64;
            }
            else {
                assert(dst_type->kind == TYPE_INTEGER);
                dst_int_kind = dst_type->as_integer.kind;
            }

            switch (src_int_kind) {
                CASE_INT_CAST(INTEGER_BOOL, eop, dst_int_kind, _bool)
                CASE_INT_CAST(INTEGER_U8, eop, dst_int_kind, _u8)
                CASE_INT_CAST(INTEGER_S8, eop, dst_int_kind, _s8)
                CASE_INT_CAST(INTEGER_U16, eop, dst_int_kind, _u16)
                CASE_INT_CAST(INTEGER_S16, eop, dst_int_kind, _s16)
                CASE_INT_CAST(INTEGER_U32, eop, dst_int_kind, _u32)
                CASE_INT_CAST(INTEGER_S32, eop, dst_int_kind, _s32)
                CASE_INT_CAST(INTEGER_U64, eop, dst_int_kind, _u64)
                CASE_INT_CAST(INTEGER_S64, eop, dst_int_kind, _s64)
            default:
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

static CastResult convert_eop(ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay)
{
    CastResult r = can_convert_eop(eop, dst_type, forbid_rvalue_decay);

    if (!r.success)
        return r;

    cast_eop(eop, dst_type, forbid_rvalue_decay);

    eop->is_lvalue = false;

    return r;
}

static void promote_int_eops(ExprOperand* eop)
{
    switch (eop->type->kind) {
    case TYPE_INTEGER:
    case TYPE_ENUM:
        if (eop->type->size < builtin_types[BUILTIN_TYPE_S32].type->size)
            cast_eop(eop, builtin_types[BUILTIN_TYPE_S32].type, false);
        break;
    default:
        break;
    }
}

static Type* convert_ptr_eops(ExprOperand* a_op, ExprOperand* b_op)
{
    Type* common_type = common_ptr_type(a_op->type, b_op->type);

    if (common_type) {
        a_op->type = common_type;
        b_op->type = common_type;
    }

    return common_type;
}

static Type* convert_arith_eops(ExprOperand* left, ExprOperand* right)
{
    // If one is an f64, cast the other to f64.
    if (left->type == builtin_types[BUILTIN_TYPE_F64].type) {
        cast_eop(right, builtin_types[BUILTIN_TYPE_F64].type, false);
    }
    else if (right->type == builtin_types[BUILTIN_TYPE_F64].type) {
        cast_eop(left, builtin_types[BUILTIN_TYPE_F64].type, false);
    }
    // Else if one is an f32, cast the other to f32.
    else if (left->type == builtin_types[BUILTIN_TYPE_F32].type) {
        cast_eop(right, builtin_types[BUILTIN_TYPE_F32].type, false);
    }
    else if (right->type == builtin_types[BUILTIN_TYPE_F32].type) {
        cast_eop(left, builtin_types[BUILTIN_TYPE_F32].type, false);
    }
    // Else, do usual arithmetic conversions.
    else {
        assert(type_is_integer_like(left->type));
        assert(type_is_integer_like(right->type));

        // First, promote both to s32 if smaller than s32.
        // This is a lossless conversion.
        promote_int_eops(left);
        promote_int_eops(right);

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

            bool left_signed = int_kind_signed[left_as_int->kind];
            bool right_signed = int_kind_signed[right_as_int->kind];
            int left_rank = type_integer_ranks[left_as_int->kind];
            int right_rank = type_integer_ranks[right_as_int->kind];
            size_t left_size = left->type->size;
            size_t right_size = right->type->size;

            if (left_signed == right_signed) {
                if (left_rank <= right_rank)
                    cast_eop(left, right->type, false);
                else
                    cast_eop(right, left->type, false);
            }
            else if (left_signed && (right_rank >= left_rank)) {
                cast_eop(left, right->type, false);
            }
            else if (right_signed && (left_rank >= right_rank)) {
                cast_eop(right, left->type, false);
            }
            else if (left_signed && (left_size > right_size)) {
                cast_eop(right, left->type, false);
            }
            else if (right_signed && (right_size > left_size)) {
                cast_eop(left, right->type, false);
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

                cast_eop(left, type, false);
                cast_eop(right, type, false);

                assert(!"We shouldn't reach this code path!!! Usual arithmetic conversions code");
            }
        }
    }

    assert(left->type == right->type);

    return left->type;
}

#define DEF_EVAL_UNARY_OP_INT_FUNC(T)                                 \
    static T eval_unary_op_##T(TokenKind op, T val)                   \
    {                                                                 \
        switch (op) {                                                 \
        case TKN_PLUS:                                                \
            return +val;                                              \
        case TKN_MINUS:                                               \
            return -val;                                              \
        case TKN_NEG:                                                 \
            return ~val;                                              \
        case TKN_NOT:                                                 \
            return !val;                                              \
        default:                                                      \
            NIBBLE_FATAL_EXIT("Unexpected unary op (##T): %d\n", op); \
            return 0;                                                 \
        }                                                             \
    }

DEF_EVAL_UNARY_OP_INT_FUNC(s64)
DEF_EVAL_UNARY_OP_INT_FUNC(u64)

#define DEF_EVAL_UNARY_OP_FLOAT_FUNC(T)                               \
    static T eval_unary_op_##T(TokenKind op, T val)                   \
    {                                                                 \
        switch (op) {                                                 \
        case TKN_PLUS:                                                \
            return +val;                                              \
        case TKN_MINUS:                                               \
            return -val;                                              \
        default:                                                      \
            NIBBLE_FATAL_EXIT("Unexpected unary op (##T): %d\n", op); \
            return 0.0;                                               \
        }                                                             \
    }

DEF_EVAL_UNARY_OP_FLOAT_FUNC(f64)
DEF_EVAL_UNARY_OP_FLOAT_FUNC(f32)

#define DEF_EVAL_BINARY_OP_FLOAT_FUNC(T)                               \
    static T eval_binary_op_##T(TokenKind op, T left, T right)         \
    {                                                                  \
        switch (op) {                                                  \
        case TKN_PLUS:                                                 \
            return left + right;                                       \
        case TKN_MINUS:                                                \
            return left - right;                                       \
        case TKN_ASTERISK:                                             \
            return left * right;                                       \
        case TKN_DIV:                                                  \
            return left / right;                                       \
        default:                                                       \
            NIBBLE_FATAL_EXIT("Unexpected binary op (##T): %d\n", op); \
            return 0.0;                                                \
        }                                                              \
    }

DEF_EVAL_BINARY_OP_FLOAT_FUNC(f64)
DEF_EVAL_BINARY_OP_FLOAT_FUNC(f32)

#define DEF_EVAL_BINARY_LOGICAL_OP_FUNC(T)                                     \
    static bool eval_binary_logical_op_##T(TokenKind op, T left, T right)      \
    {                                                                          \
        switch (op) {                                                          \
        case TKN_LOGIC_AND:                                                    \
            return left && right;                                              \
        case TKN_LOGIC_OR:                                                     \
            return left || right;                                              \
        case TKN_EQ:                                                           \
            return left == right;                                              \
        case TKN_NOTEQ:                                                        \
            return left != right;                                              \
        case TKN_GT:                                                           \
            return left > right;                                               \
        case TKN_GTEQ:                                                         \
            return left >= right;                                              \
        case TKN_LT:                                                           \
            return left < right;                                               \
        case TKN_LTEQ:                                                         \
            return left <= right;                                              \
        default:                                                               \
            NIBBLE_FATAL_EXIT("Unexpected binary logical op (##T): %d\n", op); \
            return false;                                                      \
        }                                                                      \
    }

DEF_EVAL_BINARY_LOGICAL_OP_FUNC(f64)
DEF_EVAL_BINARY_LOGICAL_OP_FUNC(f32)
DEF_EVAL_BINARY_LOGICAL_OP_FUNC(s64)
DEF_EVAL_BINARY_LOGICAL_OP_FUNC(u64)

#define DEF_EVAL_BINARY_OP_INT_FUNC(T)                                 \
    static T eval_binary_op_##T(TokenKind op, T left, T right)         \
    {                                                                  \
        switch (op) {                                                  \
        case TKN_PLUS:                                                 \
            return left + right;                                       \
        case TKN_MINUS:                                                \
            return left - right;                                       \
        case TKN_ASTERISK:                                             \
            return left * right;                                       \
        case TKN_DIV:                                                  \
            return right != 0 ? left / right : 0;                      \
        case TKN_MOD:                                                  \
            return right != 0 ? left % right : 0;                      \
        case TKN_LSHIFT:                                               \
            return left << right;                                      \
        case TKN_RSHIFT:                                               \
            return left >> right;                                      \
        case TKN_AND:                                                  \
            return left & right;                                       \
        case TKN_OR:                                                   \
            return left | right;                                       \
        case TKN_CARET:                                                \
            return left ^ right;                                       \
        default:                                                       \
            NIBBLE_FATAL_EXIT("Unexpected binary op (##T): %d\n", op); \
            return 0;                                                  \
        }                                                              \
    }

DEF_EVAL_BINARY_OP_INT_FUNC(s64)
DEF_EVAL_BINARY_OP_INT_FUNC(u64)

static void eval_binary_op(TokenKind op, ExprOperand* dst, Type* type, Scalar left, Scalar right)
{
    if (type_is_integer_like(type)) {
        ExprOperand left_eop = OP_FROM_CONST(type, left);
        ExprOperand right_eop = OP_FROM_CONST(type, right);
        bool is_signed = type_is_signed(type);

        // Compute the operation in the largest type available.
        if (is_signed) {
            cast_eop(&left_eop, builtin_types[BUILTIN_TYPE_S64].type, false);
            cast_eop(&right_eop, builtin_types[BUILTIN_TYPE_S64].type, false);

            s64 r = eval_binary_op_s64(op, left_eop.imm.as_int._s64, right_eop.imm.as_int._s64);

            dst->type = builtin_types[BUILTIN_TYPE_S64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._s64 = r;
        }
        else {
            cast_eop(&left_eop, builtin_types[BUILTIN_TYPE_U64].type, false);
            cast_eop(&right_eop, builtin_types[BUILTIN_TYPE_U64].type, false);

            u64 r = eval_binary_op_u64(op, left_eop.imm.as_int._u64, right_eop.imm.as_int._u64);

            dst->type = builtin_types[BUILTIN_TYPE_U64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._u64 = r;
        }

        // Cast it back to the original type.
        cast_eop(dst, type, false);
    }
    else {
        assert(type->kind == TYPE_FLOAT);
        FloatKind fkind = type->as_float.kind;

        dst->type = type;
        dst->is_constexpr = true;
        dst->is_imm = true;
        dst->is_lvalue = false;

        if (fkind == FLOAT_F64) {
            dst->imm.as_float._f64 = eval_binary_op_f64(op, left.as_float._f64, right.as_float._f64);
        }
        else {
            assert(fkind == FLOAT_F32);
            dst->imm.as_float._f32 = eval_binary_op_f32(op, left.as_float._f32, right.as_float._f32);
        }
    }
}

static void eval_binary_logical_op(TokenKind op, ExprOperand* dst, Type* type, Scalar left, Scalar right)
{
    bool result = false;

    if (type_is_integer_like(type)) {
        ExprOperand left_eop = OP_FROM_CONST(type, left);
        ExprOperand right_eop = OP_FROM_CONST(type, right);
        bool is_signed = type_is_signed(type);

        // Compute the operation in the largest type available.
        if (is_signed) {
            cast_eop(&left_eop, builtin_types[BUILTIN_TYPE_S64].type, false);
            cast_eop(&right_eop, builtin_types[BUILTIN_TYPE_S64].type, false);

            result = eval_binary_logical_op_s64(op, left_eop.imm.as_int._s64, right_eop.imm.as_int._s64);
        }
        else {
            cast_eop(&left_eop, builtin_types[BUILTIN_TYPE_U64].type, false);
            cast_eop(&right_eop, builtin_types[BUILTIN_TYPE_U64].type, false);

            result = eval_binary_logical_op_u64(op, left_eop.imm.as_int._u64, right_eop.imm.as_int._u64);
        }
    }
    else {
        assert(type->kind == TYPE_FLOAT);
        FloatKind fkind = type->as_float.kind;

        if (fkind == FLOAT_F64) {
            result = eval_binary_logical_op_f64(op, left.as_float._f64, right.as_float._f64);
        }
        else {
            assert(fkind == FLOAT_F32);
            result = eval_binary_logical_op_f32(op, left.as_float._f32, right.as_float._f32);
        }
    }

    dst->type = builtin_types[BUILTIN_TYPE_BOOL].type;
    dst->is_constexpr = true;
    dst->is_imm = true;
    dst->is_lvalue = false;
    dst->imm.as_int._bool = result;
}

static void eval_unary_op(TokenKind op, ExprOperand* dst, Type* type, Scalar val)
{
    if (type_is_integer_like(type)) {
        ExprOperand val_eop = OP_FROM_CONST(type, val);
        bool is_signed = type_is_signed(type);

        // Compute the operation in the largest type available.
        if (is_signed) {
            cast_eop(&val_eop, builtin_types[BUILTIN_TYPE_S64].type, false);

            dst->type = builtin_types[BUILTIN_TYPE_S64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._s64 = eval_unary_op_s64(op, val_eop.imm.as_int._s64);
        }
        else {
            cast_eop(&val_eop, builtin_types[BUILTIN_TYPE_U64].type, false);

            dst->type = builtin_types[BUILTIN_TYPE_U64].type;
            dst->is_constexpr = true;
            dst->is_imm = true;
            dst->is_lvalue = false;
            dst->imm.as_int._u64 = eval_unary_op_u64(op, val_eop.imm.as_int._u64);
        }

        // Cast it back to the original type.
        cast_eop(dst, type, false);
    }
    else {
        assert(type->kind == TYPE_FLOAT);
        dst->type = type;
        dst->is_constexpr = true;
        dst->is_imm = true;
        dst->is_lvalue = false;

        if (type->as_float.kind == FLOAT_F64) {
            dst->imm.as_float._f64 = eval_unary_op_f64(op, val.as_float._f64);
        }
        else {
            assert(type->as_float.kind == FLOAT_F32);
            dst->imm.as_float._f32 = eval_unary_op_f32(op, val.as_float._f32);
        }
    }
}

static void eval_unary_not(ExprOperand* dst, Type* type, Scalar val)
{
    bool result = false;

    if (type_is_integer_like(type)) {
        ExprOperand val_eop = OP_FROM_CONST(type, val);
        bool is_signed = type_is_signed(type);

        // Compute the operation in the largest type available.
        if (is_signed) {
            cast_eop(&val_eop, builtin_types[BUILTIN_TYPE_S64].type, false);
            result = !val_eop.imm.as_int._s64;
        }
        else {
            cast_eop(&val_eop, builtin_types[BUILTIN_TYPE_U64].type, false);
            result = !val_eop.imm.as_int._u64;
        }
    }
    else {
        assert(type->kind == TYPE_FLOAT);
        FloatKind fkind = type->as_float.kind;

        if (fkind == FLOAT_F64) {
            result = !val.as_float._f64;
        }
        else {
            assert(fkind == FLOAT_F32);
            result = !val.as_float._f32;
        }
    }

    dst->type = builtin_types[BUILTIN_TYPE_BOOL].type;
    dst->is_constexpr = true;
    dst->is_imm = true;
    dst->is_lvalue = false;
    dst->imm.as_int._bool = result;
}

static Type* get_int_lit_type(u64 value, Type** types, u32 num_types)
{
    assert(num_types);
    Type* type = types[0];

    for (u32 i = 1; i < num_types; i += 1) {
        assert(type->kind == TYPE_INTEGER);
        u64 max = int_kind_max[type->as_integer.kind];

        if (value > max) {
            type = types[i];
        }
        else {
            return type;
        }
    }

    return (value > int_kind_max[type->as_integer.kind]) ? NULL : type;
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

            if (value > int_kind_max[type->as_integer.kind]) {
                type = NULL;
            }
            break;
        }
        case TKN_INT_SUFFIX_ULL: {
            type = builtin_types[BUILTIN_TYPE_ULLONG].type;

            if (value > int_kind_max[type->as_integer.kind]) {
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

            if (value > int_kind_max[type->as_integer.kind]) {
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

static bool resolve_expr_float(Resolver* resolver, ExprFloat* expr)
{
    (void)resolver;

    expr->super.type = (expr->fkind == FLOAT_F64) ? builtin_types[BUILTIN_TYPE_F64].type : builtin_types[BUILTIN_TYPE_F32].type;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_float = expr->value;

    return true;
}

static bool resolve_expr_bool_lit(Resolver* resolver, ExprBoolLit* expr)
{
    (void)resolver;

    expr->super.type = builtin_types[BUILTIN_TYPE_BOOL].type;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_int._bool = expr->val;

    return true;
}

static bool resolve_expr_null_lit(Resolver* resolver, ExprNullLit* expr)
{
    (void)resolver;

    expr->super.type = type_ptr_void;
    expr->super.is_constexpr = true;
    expr->super.is_imm = true;
    expr->super.is_lvalue = false;
    expr->super.imm.as_int._u64 = 0;

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

    cast_eop(int_eop, builtin_types[BUILTIN_TYPE_U64].type, false);

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

static void resolve_non_const_binary_eop(ExprOperand* dst, ExprOperand* left, ExprOperand* right)
{
    dst->type = convert_arith_eops(left, right);
    dst->is_constexpr = false;
    dst->is_imm = false;
    dst->is_lvalue = false;
}

static void resolve_binary_eop(TokenKind op, ExprOperand* dst, ExprOperand* left, ExprOperand* right)
{
    Type* type = convert_arith_eops(left, right);

    if (left->is_constexpr && right->is_constexpr) {
        assert(left->is_imm && right->is_imm);
        eval_binary_op(op, dst, type, left->imm, right->imm);
    }
    else {
        dst->type = type;
        dst->is_constexpr = false;
        dst->is_imm = false;
        dst->is_lvalue = false;
    }
}

static void resolve_binary_logical_eop(TokenKind op, ExprOperand* dst, ExprOperand* left, ExprOperand* right)
{
    Type* common_type = convert_arith_eops(left, right);

    if (left->is_constexpr && right->is_constexpr) {
        assert(left->is_imm && right->is_imm);
        eval_binary_logical_op(op, dst, common_type, left->imm, right->imm);
        assert(dst->type == builtin_types[BUILTIN_TYPE_BOOL].type);
    }
    else {
        dst->type = builtin_types[BUILTIN_TYPE_BOOL].type;
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
            resolve_binary_eop(TKN_PLUS, &dst_op, &left_op, &right_op);
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
            resolve_binary_eop(TKN_MINUS, &dst_op, &left_op, &right_op);
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
    case TKN_MOD:
        if (!type_is_integer_like(left_op.type)) {
            resolver_on_error(resolver, ebinary->left->range, "Left operand of operator `%%` must be an integer type, not type `%s`.",
                              type_name(left_op.type));
            return false;
        }

        if (!type_is_integer_like(right_op.type)) {
            resolver_on_error(resolver, ebinary->right->range,
                              "Right operand of operator `%%` must be an integer type, not type `%s`.", type_name(right_op.type));
            return false;
        }

        resolve_binary_eop(ebinary->op, &dst_op, &left_op, &right_op);

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

        Type* op_type = convert_arith_eops(&left_op, &right_op);

        dst_op.type = type_array(&resolver->ctx->ast_mem, &resolver->ctx->type_cache.arrays, op_type, 2);
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

        if (left_op.is_constexpr && right_op.is_constexpr) {
            assert(left_op.is_imm && right_op.is_imm);
            eval_binary_op(ebinary->op, &dst_op, left_op.type, left_op.imm, right_op.imm);
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

        resolve_binary_eop(ebinary->op, &dst_op, &left_op, &right_op);

        break;
    }
    case TKN_EQ:
    case TKN_NOTEQ: {
        bool left_is_ptr = (left_op.type->kind == TYPE_PTR);
        bool right_is_ptr = (right_op.type->kind == TYPE_PTR);

        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_logical_eop(ebinary->op, &dst_op, &left_op, &right_op);
        }
        else if (left_is_ptr && right_is_ptr) {
            Type* common_type = convert_ptr_eops(&left_op, &right_op);

            if (!common_type) {
                resolver_on_error(resolver, expr->range, "Cannot compare pointers of incompatible types");
                return false;
            }

            if (left_op.is_constexpr && left_op.is_imm && right_op.is_constexpr && right_op.is_imm) {
                u64 left_u64 = left_op.imm.as_int._u64;
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_BOOL].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._bool = eval_binary_logical_op_u64(ebinary->op, left_u64, right_u64);
            }
            else {
                // NOTE: The only constexpr ptr that is NOT an immediate, is the addresses of a global variable.
                // Comparison of global addresses is not a compile-time constexpr because we won't know the address values
                // until after code generation.
                dst_op.is_constexpr = false;
                dst_op.type = builtin_types[BUILTIN_TYPE_BOOL].type;
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
            resolve_binary_logical_eop(ebinary->op, &dst_op, &left_op, &right_op);
        }
        else if (left_is_ptr && right_is_ptr) {
            Type* common_type = convert_ptr_eops(&left_op, &right_op);

            if (!common_type) {
                resolver_on_error(resolver, expr->range, "Cannot compare pointers of incompatible types");
                return false;
            }

            if (left_op.is_constexpr && left_op.is_imm && right_op.is_constexpr && right_op.is_imm) {
                u64 left_u64 = left_op.imm.as_int._u64;
                u64 right_u64 = right_op.imm.as_int._u64;

                dst_op.type = builtin_types[BUILTIN_TYPE_BOOL].type;
                dst_op.is_constexpr = true;
                dst_op.is_imm = true;
                dst_op.imm.as_int._bool = eval_binary_logical_op_u64(ebinary->op, left_u64, right_u64);
            }
            else {
                // NOTE: The only constexpr ptr that is NOT an immediate, is the addresses of a global variable.
                // Comparison of global addresses is not a compile-time constexpr because we won't know the address values
                // until after code generation.
                dst_op.is_constexpr = false;
                dst_op.type = builtin_types[BUILTIN_TYPE_BOOL].type;
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
    case TKN_LOGIC_OR: {
        CastResult r = convert_eop(&left_op, builtin_types[BUILTIN_TYPE_BOOL].type, false);

        if (!r.success) {
            resolver_on_error(resolver, ebinary->left->range, "Left operand of logical operator `%s` must be convertible to `bool`",
                              token_kind_names[ebinary->op]);
            return false;
        }

        r = convert_eop(&right_op, builtin_types[BUILTIN_TYPE_BOOL].type, false);

        if (!r.success) {
            resolver_on_error(resolver, ebinary->right->range, "Right operand of logical operator `%s` must be convertible to `bool`",
                              token_kind_names[ebinary->op]);
            return false;
        }

        if (left_op.is_constexpr && left_op.is_imm && right_op.is_constexpr && right_op.is_imm) {
            u64 left_bool = left_op.imm.as_int._bool;
            u64 right_bool = right_op.imm.as_int._bool;

            dst_op.type = builtin_types[BUILTIN_TYPE_BOOL].type;
            dst_op.is_constexpr = true;
            dst_op.is_imm = true;
            dst_op.imm.as_int._bool = eval_binary_logical_op_u64(ebinary->op, (u64)left_bool, (u64)right_bool);
        }
        else if (left_op.is_constexpr && (left_op.type->kind == TYPE_PTR) && right_op.is_constexpr &&
                 (right_op.type->kind == TYPE_PTR)) {
            // If left and right operands are addresses of global variables, return true.
            dst_op.type = builtin_types[BUILTIN_TYPE_BOOL].type;
            dst_op.is_constexpr = true;
            dst_op.is_imm = true;
            dst_op.imm.as_int._bool = true;
        }
        else {
            dst_op.type = builtin_types[BUILTIN_TYPE_BOOL].type;
            dst_op.is_constexpr = false;
        }

        break;
    }
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

static bool resolve_expr_ternary(Resolver* resolver, ExprTernary* expr)
{
    // Resolve the condition expression.
    if (!resolve_expr(resolver, expr->cond, NULL)) {
        return false;
    }

    // Ensure condition is convertible to a boolean.
    ExprOperand cond_op = OP_FROM_EXPR(expr->cond);
    CastResult r = convert_eop(&cond_op, builtin_types[BUILTIN_TYPE_BOOL].type, false);

    if (!r.success) {
        resolver_on_error(resolver, expr->cond->range, "Condition expression in tenary operator must be convertible to `bool`.");
        return false;
    }

    // Resolve then and else expressions.
    if (!resolve_expr(resolver, expr->then_expr, NULL)) {
        return false;
    }

    if (!resolve_expr(resolver, expr->else_expr, NULL)) {
        return false;
    }

    ExprOperand then_op = OP_FROM_EXPR(expr->then_expr);
    ExprOperand else_op = OP_FROM_EXPR(expr->else_expr);
    ExprOperand dst_op = {0}; // Not an lvalue.

    if (type_is_arithmetic(then_op.type) && type_is_arithmetic(else_op.type)) {
        dst_op.type = convert_arith_eops(&then_op, &else_op);
    }
    else if (then_op.type->kind == TYPE_PTR && else_op.type->kind == TYPE_PTR) {
        Type* common_type = convert_ptr_eops(&then_op, &else_op);

        if (!common_type) {
            resolver_on_error(resolver, merge_ranges(expr->then_expr->range, expr->else_expr->range),
                              "'then' and 'else' expressions in ternary operator are of incompatible pointer types (%s and %s).",
                              type_name(then_op.type), type_name(else_op.type));
            return false;
        }

        dst_op.type = common_type;
    }
    else if (then_op.type == else_op.type) {
        dst_op.type = then_op.type;
    }
    else {
        resolver_on_error(resolver, merge_ranges(expr->then_expr->range, expr->else_expr->range),
                          "'then' and 'else' expressions in ternary operator are of incompatible types (%s and %s).",
                          type_name(then_op.type), type_name(else_op.type));
        return false;
    }

    // NOTE: Must check `.is_imm` because the address of a global variable is a constant expression that does not yet
    // have an immediate value.
    if (cond_op.is_constexpr && then_op.is_constexpr && then_op.is_imm && else_op.is_constexpr && else_op.is_imm) {
        Scalar result_imm = (!cond_op.is_imm || cond_op.imm.as_int._bool) ? then_op.imm : else_op.imm;

        dst_op.is_constexpr = true;
        dst_op.is_imm = true;
        dst_op.imm = result_imm;
    }
    else {
        dst_op.is_constexpr = cond_op.is_constexpr && then_op.is_constexpr && else_op.is_constexpr;
    }

    expr->cond = try_wrap_cast_expr(resolver, &cond_op, expr->cond);
    expr->then_expr = try_wrap_cast_expr(resolver, &then_op, expr->then_expr);
    expr->else_expr = try_wrap_cast_expr(resolver, &else_op, expr->else_expr);

    expr->super.type = dst_op.type;
    expr->super.is_lvalue = dst_op.is_lvalue;
    expr->super.is_constexpr = dst_op.is_constexpr;
    expr->super.is_imm = dst_op.is_imm;
    expr->super.imm = dst_op.imm;

    return true;
}

static void resolve_unary_eop(TokenKind op, ExprOperand* dst, ExprOperand* src)
{
    promote_int_eops(src);

    if (src->is_constexpr && src->is_imm) {
        eval_unary_op(op, dst, src->type, src->imm);
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

        resolve_unary_eop(eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_NEG:
        if (!type_is_integer_like(src_op.type)) {
            resolver_on_error(resolver, expr->range, "Can only use unary ~ with integer types");
            return false;
        }

        resolve_unary_eop(eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_NOT:
        if (!type_is_scalar(src_op.type)) {
            resolver_on_error(resolver, expr->range, "Can only use unary ! with scalar types");
            return false;
        }

        if (src_op.is_constexpr && src_op.is_imm) {
            eval_unary_not(&dst_op, src_op.type, src_op.imm);
        }
        else {
            dst_op.type = builtin_types[BUILTIN_TYPE_BOOL].type;
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

    CastResult r = convert_eop(&index_op, builtin_types[BUILTIN_TYPE_S64].type, false);

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
        resolver_on_error(resolver, eindex->array->range, "Cannot index value of type `%s`", type_name(eindex->array->type));
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
        CastResult r = cast_eop(&arg_eop, param_type, true);

        if (!r.success) {
            resolver_cast_error(resolver, r, arg->expr->range, err_prefix, arg_eop.type, param_type);
            return false;
        }
    }
    else if (!can_be_any) {
        CastResult r = convert_eop(&arg_eop, param_type, true);

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

    CastResult r = cast_eop(&src_eop, cast_type, false);

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

static bool resolve_expr_bit_cast(Resolver* resolver, ExprBitCast* expr)
{
    Type* cast_type = resolve_typespec(resolver, expr->typespec);

    if (!cast_type)
        return false;

    if (!resolve_expr(resolver, expr->expr, NULL))
        return false;

    ExprOperand src_eop = OP_FROM_EXPR(expr->expr);

    // Size and alignment of expr's type must be equal to the size of the destination bit_cast type.
    // If the source is not an lvalue, then the alignment can differ.
    if ((src_eop.type->size != cast_type->size) ||
        (src_eop.is_lvalue && (src_eop.type->align != cast_type->align))) {
        resolver_on_error(resolver, expr->super.range,
                          "Cannot bit_cast an expression (of type `%s`) to a type (`%s`) of a different size "
                          "or alignment requirement.", type_name(src_eop.type), type_name(cast_type));
        return false;
    }

    // TODO: Handle bit casting an immediate (e.g., 0xF3AA) to a non-immediate object (e.g., struct{char; char;}).
    // Can convert this expression to a compound literal.
    if (src_eop.is_imm && type_is_obj_like(cast_type)) {
        resolver_on_error(resolver, expr->super.range, "Cannot cast a compile-time constant expression "
                          "to a struct/union/array type. This will be allowed in the future!");
        return false;
    }

    expr->super.type = cast_type;
    expr->super.is_lvalue = src_eop.is_lvalue;
    expr->super.is_constexpr = src_eop.is_constexpr;
    expr->super.is_imm = src_eop.is_imm;
    expr->super.imm = src_eop.imm;

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
        CastResult r = convert_eop(&init_op, elem_type, true);

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
            resolver_on_error(resolver, initzer->range,
                              "Initializer sets field more than once. "
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
        CastResult r = convert_eop(&init_op, field->type, true);

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
        CastResult r = convert_eop(&init_op, field->type, true);

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
    case CST_ExprFloat:
        return resolve_expr_float(resolver, (ExprFloat*)expr);
    case CST_ExprBoolLit:
        return resolve_expr_bool_lit(resolver, (ExprBoolLit*)expr);
    case CST_ExprNullLit:
        return resolve_expr_null_lit(resolver, (ExprNullLit*)expr);
    case CST_ExprIdent:
        return resolve_expr_ident(resolver, expr);
    case CST_ExprUnary:
        return resolve_expr_unary(resolver, expr);
    case CST_ExprBinary:
        return resolve_expr_binary(resolver, expr);
    case CST_ExprTernary:
        return resolve_expr_ternary(resolver, (ExprTernary*)expr);
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
    case CST_ExprBitCast:
        return resolve_expr_bit_cast(resolver, (ExprBitCast*)expr);
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
        NIBBLE_FATAL_EXIT("Unsupported expr kind `%d` while resolving\n", expr->kind);
        break;
    }

    return false;
}

