#ifndef NIBBLE_RESOLVER_INTERNAL_H
#define NIBBLE_RESOLVER_INTERNAL_H
#include "resolver/module.h"

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

CastResult convert_eop(ExprOperand* eop, Type* dst_type, bool forbid_rvalue_decay);
Expr* try_wrap_cast_expr(Resolver* resolver, ExprOperand* eop, Expr* orig_expr);
void eval_binary_op(TokenKind op, ExprOperand* dst, Type* type, Scalar left, Scalar right);

bool try_complete_aggregate_type(Resolver* resolver, Type* type);

bool resolve_decl_var(Resolver* resolver, Symbol* sym);
bool resolve_decl_const(Resolver* resolver, Symbol* sym);
bool resolve_decl_proc(Resolver* resolver, Symbol* sym);
bool resolve_decl_enum(Resolver* resolver, Symbol* sym);
bool resolve_decl_typedef(Resolver* resolver, Symbol* sym);
bool resolve_global_proc_body(Resolver* resolver, Symbol* sym);
bool resolve_expr(Resolver* resolver, Expr* expr, Type* expected_type);
void resolve_non_const_binary_eop(ExprOperand* dst, ExprOperand* left, ExprOperand* right);
bool resolve_ptr_int_arith(Resolver* resolver, ExprOperand* dst, ExprOperand* ptr, ExprOperand* int_eop);
Type* resolve_typespec(Resolver* resolver, TypeSpec* typespec);
unsigned resolve_stmt(Resolver* resolver, Stmt* stmt, Type* ret_type, unsigned flags);
bool resolve_global_stmt(Resolver* resolver, Stmt* stmt);

void resolver_on_error(Resolver* resolver, ProgRange range, const char* format, ...);
void resolver_cast_error(Resolver* resolver, CastResult cast_res, ProgRange range, const char* err_prefix, Type* src_type,
                         Type* dst_type);
ModuleState enter_module(Resolver* resolver, Module* mod);
void exit_module(Resolver* resolver, ModuleState state);
void set_scope(Resolver* resolver, Scope* scope);
Scope* push_scope(Resolver* resolver, size_t num_syms);
void pop_scope(Resolver* resolver);
ModuleState enter_proc(Resolver* resolver, Symbol* sym);
void exit_proc(Resolver* resolver, ModuleState state);
bool resolve_symbol(Resolver* resolver, Symbol* sym);
Symbol* resolve_name(Resolver* resolver, Identifier* name);
Symbol* resolve_export_name(Resolver* resolver, Identifier* name);
Symbol* lookup_ident(Resolver* resolver, NSIdent* ns_ident);
#endif
