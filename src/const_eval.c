#include "const_eval.h"

#define DEF_EVAL_UNARY_OP_INT_FUNC(T)                                 \
    T eval_unary_op_##T(TokenKind op, T val)                          \
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
    T eval_unary_op_##T(TokenKind op, T val)                          \
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
    T eval_binary_op_##T(TokenKind op, T left, T right)                \
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
    bool eval_binary_logical_op_##T(TokenKind op, T left, T right)             \
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
    T eval_binary_op_##T(TokenKind op, T left, T right)                \
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

Scalar eval_binary_op_64bit(TokenKind op, bool is_signed, Scalar left, Scalar right)
{
    Scalar result;

    if (is_signed) {
        result.as_int._s64 = eval_binary_op_s64(op, left.as_int._s64, right.as_int._s64);
    }
    else {
        result.as_int._u64 = eval_binary_op_u64(op, left.as_int._u64, right.as_int._u64);
    }

    return result;
}

bool eval_binary_logical_op_64bit(TokenKind op, bool is_signed, Scalar left, Scalar right)
{
    bool result;

    if (is_signed) {
        result = eval_binary_logical_op_s64(op, left.as_int._s64, right.as_int._s64);
    }
    else {
        result = eval_binary_logical_op_u64(op, left.as_int._u64, right.as_int._u64);
    }

    return result;
}
