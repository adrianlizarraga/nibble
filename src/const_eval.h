#pragma once
#include "basics.h"
#include "lexer/module.h"

f64 eval_binary_op_f64(TokenKind op, f64 left, f64 right);
f32 eval_binary_op_f32(TokenKind op, f32 left, f32 right);
s64 eval_binary_op_s64(TokenKind op, s64 left, s64 right);
u64 eval_binary_op_u64(TokenKind op, u64 left, u64 right);
Scalar eval_binary_op_64bit(TokenKind op, bool is_signed, Scalar left, Scalar right);

bool eval_binary_logical_op_f64(TokenKind op, f64 left, f64 right);
bool eval_binary_logical_op_f32(TokenKind op, f32 left, f32 right);
bool eval_binary_logical_op_s64(TokenKind op, s64 left, s64 right);
bool eval_binary_logical_op_u64(TokenKind op, u64 left, u64 right);
bool eval_binary_logical_op_64bit(TokenKind op, bool is_signed, Scalar left, Scalar right);

f64 eval_unary_op_f64(TokenKind op, f64 val);
f32 eval_unary_op_f32(TokenKind op, f32 val);
s64 eval_unary_op_s64(TokenKind op, s64 val);
u64 eval_unary_op_u64(TokenKind op, u64 val);
