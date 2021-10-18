#include "resolver.h"
#include "parser.h"

#define OP_FROM_EXPR(e)                                                                                                \
    {                                                                                                                  \
        .type = (e)->type, .is_constexpr = (e)->is_constexpr, .is_lvalue = (e)->is_lvalue, .const_val = (e)->const_val \
    }

#define OP_FROM_CONST(t, s)                                                     \
    {                                                                           \
        .type = (t), .is_constexpr = true, .is_lvalue = false, .const_val = (s) \
    }

typedef struct ExprOperand {
    Type* type;
    bool is_constexpr;
    bool is_lvalue;
    Scalar const_val;
} ExprOperand;

static Symbol* resolve_name(Resolver* resolver, const char* name);
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
static bool eop_decay(Resolver* resolver, ExprOperand* eop);
static Expr* try_wrap_cast_expr(Resolver* resolver, ExprOperand* eop, Expr* orig_expr);

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

static void set_scope(Resolver* resolver, Scope* scope);
static Scope* push_scope(Resolver* resolver, size_t num_syms);
static void pop_scope(Resolver* resolver);
static void add_scope_symbol(Scope* scope, Symbol* sym);

static void init_builtin_syms(Resolver* resolver);
static bool add_global_type_symbol(Resolver* resolver, const char* name, Type* type);
static Symbol* add_unresolved_symbol(Resolver* resolver, Scope* scope, SymbolKind kind, const char* name, Decl* decl);

static void resolver_on_error(Resolver* resolver, const char* format, ...)
{
    char buf[MAX_ERROR_LEN];
    size_t size = 0;
    va_list vargs;

    va_start(vargs, format);
    size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
    va_end(vargs);

    add_byte_stream_chunk(resolver->errors, buf, size > sizeof(buf) ? sizeof(buf) : size);
}

static void add_scope_symbol(Scope* scope, Symbol* sym)
{
    hmap_put(&scope->sym_table, PTR_UINT(sym->name), PTR_UINT(sym));
    list_add_last(&scope->sym_list, &sym->lnode);

    scope->sym_kind_counts[sym->kind] += 1;
}

static void fill_decl_symbol_info(Decl* decl, SymbolKind* kind, const char** name)
{
    // TODO: This is ugly.

    switch (decl->kind) {
    case CST_DeclVar: {
        DeclVar* dvar = (DeclVar*)decl;

        *kind = SYMBOL_VAR;
        *name = dvar->name; // TODO: Multiple variables per cst decl
        break;
    }
    case CST_DeclConst: {
        DeclConst* dconst = (DeclConst*)decl;

        *kind = SYMBOL_CONST;
        *name = dconst->name;
        break;
    }
    case CST_DeclProc: {
        DeclProc* dproc = (DeclProc*)decl;

        *kind = SYMBOL_PROC;
        *name = dproc->name;
        break;
    }
    case CST_DeclEnum: {
        DeclEnum* denum = (DeclEnum*)decl;

        *kind = SYMBOL_TYPE;
        *name = denum->name;
        break;
    }
    case CST_DeclUnion: {
        DeclUnion* dunion = (DeclUnion*)decl;

        *kind = SYMBOL_TYPE;
        *name = dunion->name;
        break;
    }
    case CST_DeclStruct: {
        DeclStruct* dstruct = (DeclStruct*)decl;

        *kind = SYMBOL_TYPE;
        *name = dstruct->name;
        break;
    }
    case CST_DeclTypedef: {
        DeclTypedef* dtypedef = (DeclTypedef*)decl;

        *kind = SYMBOL_TYPE;
        *name = dtypedef->name;
        break;
    }
    default:
        *kind = SYMBOL_NONE;
        *name = NULL;
        ftprint_err("Not handling Decl kind %d in file %s, line %d\n", decl->kind, __FILE__, __LINE__);
        assert(0);
        break;
    }
}

static bool add_global_type_symbol(Resolver* resolver, const char* name, Type* type)
{
    const char* sym_name = intern_ident(name, cstr_len(name), NULL, NULL);

    if (lookup_scope_symbol(resolver->global_scope, sym_name)) {
        resolver_on_error(resolver, "Duplicate definition of `%s`", sym_name);
        return false;
    }

    Symbol* sym = new_symbol_builtin_type(resolver->ast_mem, sym_name, type);

    add_scope_symbol(resolver->global_scope, sym);

    return true;
}

static Symbol* add_unresolved_symbol(Resolver* resolver, Scope* scope, SymbolKind kind, const char* name, Decl* decl)
{
    if (lookup_symbol(scope, name))
        return NULL; // Shadows a symbol in the current scope or a parent scope.

    Symbol* sym = new_symbol_decl(resolver->ast_mem, kind, name, decl);
    sym->status = SYMBOL_STATUS_UNRESOLVED;
    sym->is_local = (scope != resolver->global_scope);

    add_scope_symbol(scope, sym);

    return sym;
}

static void set_scope(Resolver* resolver, Scope* scope)
{
    resolver->curr_scope = scope;
}

static Scope* push_scope(Resolver* resolver, size_t num_syms)
{
    Scope* prev_scope = resolver->curr_scope;
    Scope* scope = new_scope(resolver->ast_mem, num_syms + num_syms);

    scope->parent = prev_scope;

    list_add_last(&prev_scope->children, &scope->lnode);
    set_scope(resolver, scope);

    return scope;
}

static void pop_scope(Resolver* resolver)
{
    resolver->curr_scope = resolver->curr_scope->parent;
}

static void init_builtin_syms(Resolver* resolver)
{
    add_global_type_symbol(resolver, "void", type_void);
    add_global_type_symbol(resolver, "u8", type_u8);
    add_global_type_symbol(resolver, "s8", type_s8);
    add_global_type_symbol(resolver, "u16", type_u16);
    add_global_type_symbol(resolver, "s16", type_s16);
    add_global_type_symbol(resolver, "u32", type_u32);
    add_global_type_symbol(resolver, "s32", type_s32);
    add_global_type_symbol(resolver, "u64", type_u64);
    add_global_type_symbol(resolver, "s64", type_s64);
    add_global_type_symbol(resolver, "f32", type_f32);
    add_global_type_symbol(resolver, "f64", type_f64);

    // Aliased types
    add_global_type_symbol(resolver, "bool", type_bool);
    add_global_type_symbol(resolver, "char", type_char);
    add_global_type_symbol(resolver, "schar", type_schar);
    add_global_type_symbol(resolver, "uchar", type_uchar);
    add_global_type_symbol(resolver, "short", type_short);
    add_global_type_symbol(resolver, "ushort", type_ushort);
    add_global_type_symbol(resolver, "int", type_int);
    add_global_type_symbol(resolver, "uint", type_uint);
    add_global_type_symbol(resolver, "long", type_long);
    add_global_type_symbol(resolver, "ulong", type_ulong);
    add_global_type_symbol(resolver, "llong", type_llong);
    add_global_type_symbol(resolver, "ullong", type_ullong);
    add_global_type_symbol(resolver, "ssize", type_ssize);
    add_global_type_symbol(resolver, "usize", type_usize);
}

#define CASE_INT_CAST(k, o, t, f)                                  \
    case k:                                                        \
        switch (t) {                                               \
        case INTEGER_U8:                                           \
            o->const_val.as_int._u8 = (u8)o->const_val.as_int.f;   \
            break;                                                 \
        case INTEGER_S8:                                           \
            o->const_val.as_int._s8 = (s8)o->const_val.as_int.f;   \
            break;                                                 \
        case INTEGER_U16:                                          \
            o->const_val.as_int._u16 = (u16)o->const_val.as_int.f; \
            break;                                                 \
        case INTEGER_S16:                                          \
            o->const_val.as_int._s16 = (s16)o->const_val.as_int.f; \
            break;                                                 \
        case INTEGER_U32:                                          \
            o->const_val.as_int._u32 = (u32)o->const_val.as_int.f; \
            break;                                                 \
        case INTEGER_S32:                                          \
            o->const_val.as_int._s32 = (s32)o->const_val.as_int.f; \
            break;                                                 \
        case INTEGER_U64:                                          \
            o->const_val.as_int._u64 = (u64)o->const_val.as_int.f; \
            break;                                                 \
        case INTEGER_S64:                                          \
            o->const_val.as_int._s64 = (s64)o->const_val.as_int.f; \
            break;                                                 \
        default:                                                   \
            o->is_constexpr = false;                               \
            break;                                                 \
        }                                                          \
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

    if (eop->is_constexpr) {
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

    if (eop.is_constexpr && (type->kind == TYPE_INTEGER || type->kind == TYPE_PTR)) {
        cast_eop(&eop, type_u64);

        return eop.const_val.as_int._u64 == 0;
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
    else if (dst_type == type_void) {
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
        else if ((dst_pointed_type == type_void) || (src_pointed_type == type_void)) {
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
        if (eop->type->size < type_s32->size)
            cast_eop(eop, type_s32);
        break;
    default:
        break;
    }
}

static void convert_arith_eops(ExprOperand* left, ExprOperand* right)
{
    // If one is an f64, cast the other to f64.
    if (left->type == type_f64) {
        cast_eop(right, type_f64);
    }
    else if (right->type == type_f64) {
        cast_eop(left, type_f64);
    }
    // Else if one is an f32, cast the other to f32.
    else if (left->type == type_f32) {
        cast_eop(right, type_f32);
    }
    else if (right->type == type_f32) {
        cast_eop(left, type_f32);
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

        // Compute the operation in the largest type available.
        if (type->as_integer.is_signed) {
            cast_eop(&left_eop, type_s64);
            cast_eop(&right_eop, type_s64);

            s64 r = eval_binary_op_s64(op, left_eop.const_val.as_int._s64, right_eop.const_val.as_int._s64);

            dst->type = type_s64;
            dst->is_constexpr = true;
            dst->is_lvalue = false;
            dst->const_val.as_int._s64 = r;
        }
        else {
            cast_eop(&left_eop, type_u64);
            cast_eop(&right_eop, type_u64);

            u64 r = eval_binary_op_u64(op, left_eop.const_val.as_int._u64, right_eop.const_val.as_int._u64);

            dst->type = type_u64;
            dst->is_constexpr = true;
            dst->is_lvalue = false;
            dst->const_val.as_int._u64 = r;
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

        // Compute the operation in the largest type available.
        if (type->as_integer.is_signed) {
            cast_eop(&val_eop, type_s64);

            dst->type = type_s64;
            dst->is_constexpr = true;
            dst->is_lvalue = false;
            dst->const_val.as_int._s64 = eval_unary_op_s64(op, val_eop.const_val.as_int._s64);
        }
        else {
            cast_eop(&val_eop, type_u64);

            dst->type = type_u64;
            dst->is_constexpr = true;
            dst->is_lvalue = false;
            dst->const_val.as_int._u64 = eval_unary_op_u64(op, val_eop.const_val.as_int._u64);
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
    Type* type = type_ullong;

    // Based on integer constant semantics from the C specification (ISO/IEC 9899:TC3)
    u64 value = eint->token.value;
    TokenIntRep rep = eint->token.rep;
    TokenIntSuffix suffix = eint->token.suffix;

    if (rep == TKN_INT_CHAR) {
        type = type_int;
    }
    else if (rep == TKN_INT_DEC) {
        switch (suffix) {
        case TKN_INT_SUFFIX_NONE: {
            Type* types[] = {
                type_int,
                type_long,
                type_llong,
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_U: {
            Type* types[] = {
                type_uint,
                type_ulong,
                type_ullong
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));
            break;
        }
        case TKN_INT_SUFFIX_L: {
            Type* types[] = {
                type_long,
                type_llong,
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));
            break;
        }
        case TKN_INT_SUFFIX_UL: {
            Type* types[] = {
                type_ulong,
                type_ullong
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));
            break;
        }
        case TKN_INT_SUFFIX_LL: {
            type = type_llong;

            if (value > type->as_integer.max) {
                type = NULL;
            }
            break;
        }
        case TKN_INT_SUFFIX_ULL: {
            type = type_ullong;

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
            Type* types[] = {
                type_int,
                type_uint,
                type_long,
                type_ulong,
                type_llong,
                type_ullong
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_U: {
            Type* types[] = {
                type_uint,
                type_ulong,
                type_ullong
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_L: {
            Type* types[] = {
                type_long,
                type_ulong,
                type_llong,
                type_ullong
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_UL: {
            Type* types[] = {
                type_ulong,
                type_ullong
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_LL: {
            Type* types[] = {
                type_llong,
                type_ullong
            };

            type = get_int_lit_type(value, types, ARRAY_LEN(types));

            break;
        }
        case TKN_INT_SUFFIX_ULL: {
            type = type_ullong;

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
        resolver_on_error(resolver, "Integer literal `%llu` is too large", value);
        return false;
    }

    expr->type = type;
    expr->is_constexpr = true;
    expr->is_lvalue = false;
    expr->const_val.as_int._u64 = value;

    return true;
}

static bool resolve_expr_str(Resolver* resolver, ExprStr* expr)
{
    expr->super.type = type_array(resolver->ast_mem, &resolver->type_cache->arrays, type_char, expr->str_lit->len + 1);
    expr->super.is_constexpr = true;
    expr->super.is_lvalue = true;

    return true;
}

static void resolve_binary_eop(TokenKind op, ExprOperand* dst, ExprOperand* left, ExprOperand* right)
{
    convert_arith_eops(left, right);

    if (left->is_constexpr && right->is_constexpr) {
        eval_const_binary_op(op, dst, left->type, left->const_val, right->const_val);
    }
    else {
        dst->type = left->type;
        dst->is_constexpr = false;
        dst->is_lvalue = false;
    }
}

static bool resolve_ptr_int_arith(Resolver* resolver, ExprOperand* dst, ExprOperand* ptr, ExprOperand* int_eop)
{
    // Convert ^void to ^s8
    if (ptr->type->as_ptr.base == type_void) {
        ptr->type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, type_s8);
    }
    // Ensure base pointer type has non-zero size
    else if (ptr->type->as_ptr.base->size == 0) {
        return false;
    }

    cast_eop(int_eop, type_u64);

    dst->type = ptr->type;
    dst->is_constexpr = false;
    dst->is_lvalue = false;

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

    if (!eop_decay(resolver, &left_op))
        return false;

    if (!eop_decay(resolver, &right_op))
        return false;

    switch (ebinary->op) {
    case TKN_PLUS:
        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(TKN_PLUS, &dst_op, &left_op, &right_op);
        }
        else if ((left_op.type->kind == TYPE_PTR) && type_is_integer_like(right_op.type)) {
            if (!resolve_ptr_int_arith(resolver, &dst_op, &left_op, &right_op)) {
                resolver_on_error(resolver, "Cannot add to a pointer with a base type (%s) of zero size",
                                  type_name(left_op.type->as_ptr.base));

                return false;
            }
        }
        else if (type_is_integer_like(left_op.type) && (right_op.type->kind == TYPE_PTR)) {
            if (!resolve_ptr_int_arith(resolver, &dst_op, &right_op, &left_op)) {
                resolver_on_error(resolver, "Cannot add to a pointer with a base type (%s) of zero size",
                                  type_name(right_op.type->as_ptr.base));

                return false;
            }
        }
        else {
            resolver_on_error(resolver, "Can only add arithmetic and pointer types");
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
                resolver_on_error(resolver, "Cannot subtract from a pointer with a base type (%s) of zero size",
                                  type_name(left_op.type->as_ptr.base));

                return false;
            }
        }
        // ptr - ptr
        else if (left_is_ptr && right_is_ptr) {
            Type* left_base_type = left_op.type->as_ptr.base;
            Type* right_base_type = right_op.type->as_ptr.base;

            if ((left_base_type == type_void) && (right_base_type == type_void)) {
                left_op.type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, type_s8);
                right_op.type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, type_s8);
            }
            else if ((left_base_type == type_void) && (right_base_type == type_s8)) {
                left_op.type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, type_s8);
            }
            else if ((left_base_type == type_s8) && (right_base_type == type_void)) {
                right_op.type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, type_s8);
            }

            if (left_op.type != right_op.type) {
                resolver_on_error(resolver, "Cannot subtract pointers of different types: `^%s` - `^%s`",
                                  type_name(left_base_type), type_name(right_base_type));
                return false;
            }

            dst_op.type = type_s64;
            dst_op.is_constexpr = false;
            dst_op.is_lvalue = false;
        }
        else {
            resolver_on_error(resolver, "Can only subtract arithmetic types, pointers, and integers from pointers");
            return false;
        }

        break;
    }
    case TKN_DIV:
    case TKN_ASTERISK:
        if (!type_is_arithmetic(left_op.type)) {
            resolver_on_error(resolver,
                              "Left operand of binary operator `%s` must be an arithmetic type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(left_op.type));
            return false;
        }

        if (!type_is_arithmetic(right_op.type)) {
            resolver_on_error(resolver,
                              "Right operand of binary operator `%s` must be an arithmetic type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(right_op.type));
            return false;
        }

        resolve_binary_eop(ebinary->op, &dst_op, &left_op, &right_op);

        break;
    case TKN_RSHIFT:
    case TKN_LSHIFT: {
        if (left_op.type->kind != TYPE_INTEGER) {
            resolver_on_error(resolver, "Left operand of binary operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(left_op.type));
            return false;
        }

        if (right_op.type->kind != TYPE_INTEGER) {
            resolver_on_error(resolver, "Right operand of binary operator `%s` must be an integer type, not type `%s`",
                              token_kind_names[ebinary->op], type_name(right_op.type));
            return false;
        }

        promote_int_eops(&left_op);
        promote_int_eops(&right_op);

        if (left_op.is_constexpr && right_op.is_constexpr) {
            eval_const_binary_op(ebinary->op, &dst_op, left_op.type, left_op.const_val, right_op.const_val);
        }
        else {
            dst_op.type = left_op.type;
            dst_op.is_constexpr = false;
            dst_op.is_lvalue = false;
        }

        break;
    }
    case TKN_EQ:
    case TKN_NOTEQ: {
        bool left_is_ptr = (left_op.type->kind == TYPE_PTR);
        bool right_is_ptr = (right_op.type->kind == TYPE_PTR);

        if (type_is_arithmetic(left_op.type) && type_is_arithmetic(right_op.type)) {
            resolve_binary_eop(ebinary->op, &dst_op, &left_op, &right_op);
            cast_eop(&dst_op, type_s32); // NOTE: resolve_binary_eop will cast to the common type, so cast to s32.
        }
        else if (left_is_ptr && right_is_ptr) {
            bool same_type = (left_op.type == right_op.type);
            bool one_is_void_ptr =
                (left_op.type->as_ptr.base == type_void) || (right_op.type->as_ptr.base == type_void);

            if (!same_type && !one_is_void_ptr) {
                // TODO: Better way to print pointer types (recursively print base types).
                resolver_on_error(resolver, "Cannot compare pointers of incompatible types");
                return false;
            }

            dst_op.type = type_s32;
            dst_op.is_constexpr = false;
            dst_op.is_lvalue = false;
        }
        else if ((left_is_ptr && eop_is_null_ptr(right_op)) || (right_is_ptr && eop_is_null_ptr(left_op))) {
            dst_op.type = type_s32;
            dst_op.is_constexpr = false;
            dst_op.is_lvalue = false;
        }
        else {
            resolver_on_error(resolver, "Can only compare arithmetic types, or compatible pointer types with `%s`",
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
            cast_eop(&dst_op, type_s32); // NOTE: resolve_binary_eop will cast to the common type, so cast to s32.
        }
        else if (left_is_ptr && right_is_ptr) {
            Type* left_base_type = left_op.type->as_ptr.base;
            Type* right_base_type = right_op.type->as_ptr.base;

            if (left_base_type != right_base_type) {
                left_op.type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, type_s8);
                right_op.type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, type_s8);
            }

            dst_op.type = left_op.type;
            dst_op.is_constexpr = false;
            dst_op.is_lvalue = false;
        }
        else if ((left_is_ptr && eop_is_null_ptr(right_op)) || (right_is_ptr && eop_is_null_ptr(left_op))) {
            dst_op.type = type_s32;
            dst_op.is_constexpr = false;
            dst_op.is_lvalue = false;
        }
        else {
            resolver_on_error(resolver, "Can only compare arithmetic types, or compatible pointer types with `%s`",
                              token_kind_names[ebinary->op]);
            return false;
        }

        break;
    }
    case TKN_LOGIC_AND:
    case TKN_LOGIC_OR:
        if (type_is_scalar(left_op.type) && type_is_scalar(right_op.type)) {
            if (left_op.is_constexpr && right_op.is_constexpr) {
                // NOTE: Only cast const operands to s32 here for convenience.
                // The generated code does not need the operands to be same size; they only
                // need to be comparable to 0.
                //
                // TODO: THIS IS WRONG. Need to properly cast to bool (i.e., true if != 0), and not just truncate.
                // Otherwise, ptr values with, for example, only the top bit set would evaluate to 0!!!!!
                cast_eop(&left_op, type_u64);
                cast_eop(&right_op, type_u64);

                dst_op.type = type_s32;
                dst_op.is_constexpr = true;
                dst_op.is_lvalue = false;

                if (ebinary->op == TKN_LOGIC_AND)
                    dst_op.const_val.as_int._s32 = (left_op.const_val.as_int._u64 && right_op.const_val.as_int._u64);
                else
                    dst_op.const_val.as_int._s32 = (left_op.const_val.as_int._u64 || right_op.const_val.as_int._u64);
            }
            else {
                dst_op.type = type_s32;
                dst_op.is_constexpr = false;
                dst_op.is_lvalue = false;
            }
        }
        else {
            resolver_on_error(resolver, "Can only compare arithmetic types, or compatible pointer types with `%s`",
                              token_kind_names[ebinary->op]);
            return false;
        }

        break;
    default:
        resolver_on_error(resolver, "Binary operator `%s` not supported", token_kind_names[ebinary->op]);
        return false;
    }

    ebinary->left = try_wrap_cast_expr(resolver, &left_op, ebinary->left);
    ebinary->right = try_wrap_cast_expr(resolver, &right_op, ebinary->right);

    expr->type = dst_op.type;
    expr->is_lvalue = dst_op.is_lvalue;
    expr->is_constexpr = dst_op.is_constexpr;
    expr->const_val = dst_op.const_val;

    return true;
}

static void resolve_unary_eop(TokenKind op, ExprOperand* dst, ExprOperand* src)
{
    promote_int_eops(src);

    if (src->is_constexpr) {
        eval_const_unary_op(op, dst, src->type, src->const_val);
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
        if (!eop_decay(resolver, &src_op))
            return false;

        if (!type_is_arithmetic(src_op.type)) {
            resolver_on_error(resolver, "Can only use unary +/- with arithmetic types");
            return false;
        }

        resolve_unary_eop(eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_NEG:
        if (!eop_decay(resolver, &src_op))
            return false;

        if (!type_is_integer_like(src_op.type)) {
            resolver_on_error(resolver, "Can only use unary ~ with integer types");
            return false;
        }

        resolve_unary_eop(eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_NOT:
        if (!eop_decay(resolver, &src_op))
            return false;

        if (!type_is_scalar(src_op.type)) {
            resolver_on_error(resolver, "Can only use unary ! with scalar types");
            return false;
        }

        resolve_unary_eop(eunary->op, &dst_op, &src_op);
        eunary->expr = try_wrap_cast_expr(resolver, &src_op, eunary->expr);
        break;
    case TKN_CARET: // NOTE: Address-of operator.
        if (!src_op.is_lvalue) {
            resolver_on_error(resolver, "Can only take the address of an l-value");
            return false;
        }

        dst_op.type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, src_op.type);
        dst_op.is_lvalue = false;
        dst_op.is_constexpr = false;
        break;
    case TKN_ASTERISK: // NOTE: Dereference operator.
        if (!eop_decay(resolver, &src_op))
            return false;

        if (src_op.type->kind != TYPE_PTR) {
            resolver_on_error(resolver, "Cannot dereference a non-pointer value.");
            return false;
        }

        dst_op.type = src_op.type->as_ptr.base;
        dst_op.is_lvalue = true;
        dst_op.is_constexpr = false;
        break;
    default:
        resolver_on_error(resolver, "Unary operation type `%d` not supported", eunary->op);
        return false;
    }

    expr->type = dst_op.type;
    expr->is_lvalue = dst_op.is_lvalue;
    expr->is_constexpr = dst_op.is_constexpr;
    expr->const_val = dst_op.const_val;

    return true;
}

static bool resolve_expr_index(Resolver* resolver, Expr* expr)
{
    ExprIndex* eindex = (ExprIndex*)expr;

    // Resolve array expression
    if (!resolve_expr(resolver, eindex->array, NULL))
        return false;

    ExprOperand array_op = OP_FROM_EXPR(eindex->array);

    if (!eop_decay(resolver, &array_op))
        return false;

    if (array_op.type->kind != TYPE_PTR) {
        resolver_on_error(resolver, "Cannot index non-pointer or non-array type `%s`", type_name(eindex->array->type));
        return false;
    }

    // Resolve array index expression
    if (!resolve_expr(resolver, eindex->index, NULL))
        return false;

    ExprOperand index_op = OP_FROM_EXPR(eindex->index);

    if (!convert_eop(&index_op, type_s64)) {
        resolver_on_error(resolver, "Array index of type `%s` cannot be converted to an integer",
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

    return true;
}

static bool resolve_expr_ident(Resolver* resolver, Expr* expr)
{
    ExprIdent* eident = (ExprIdent*)expr;
    Symbol* sym = resolve_name(resolver, eident->name);

    if (!sym) {
        resolver_on_error(resolver, "Unknown symbol `%s` in expression", eident->name);
        return false;
    }

    switch (sym->kind) {
    case SYMBOL_VAR:
        expr->type = sym->type;
        expr->is_lvalue = true;
        expr->is_constexpr = false;

        return true;
    case SYMBOL_CONST:
        expr->type = sym->type;
        expr->is_lvalue = false;
        expr->is_constexpr = true;
        expr->const_val = ((DeclConst*)(sym->decl))->init->const_val;

        return true;
    case SYMBOL_PROC:
        expr->type = sym->type;
        expr->is_lvalue = false;
        expr->is_constexpr = false;

        return true;
    default:
        break;
    }

    resolver_on_error(resolver, "Expression identifier `%s` must refer to a var, const, or proc declaration",
                      eident->name);
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
        resolver_on_error(resolver, "Cannot use procedure call syntax on a value with a non-procedure type");
        return false;
    }

    // Verify that the number of arguments match number of parameters.
    if (proc_type->as_proc.num_params != ecall->num_args) {
        resolver_on_error(resolver,
                          "Incorrect number of procedure call arguments. Expected `%d` arguments, but got `%d`",
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

        Type* param_type = type_decay(resolver->ast_mem, &resolver->type_cache->ptrs, params[i]); // TODO: Cast at site?
        ExprOperand arg_eop = OP_FROM_EXPR(arg->expr);

        if (!eop_decay(resolver, &arg_eop))
            return false;

        if (!convert_eop(&arg_eop, param_type)) {
            resolver_on_error(resolver,
                              "Incorrect type for argument %d of procedure call. Expected type `%s`, but got `%s`",
                              (i + 1), type_name(params[i]), type_name(arg->expr->type));
            return false;
        }

        arg->expr = try_wrap_cast_expr(resolver, &arg_eop, arg->expr);

        it = it->next;
        i += 1;
    }

    expr->type = proc_type->as_proc.ret;
    expr->is_lvalue = false;
    expr->is_constexpr = false;

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

    if (!eop_decay(resolver, &src_eop))
        return false;

    if (!cast_eop(&src_eop, cast_type)) {
        resolver_on_error(resolver, "Cannot cast from type `%s` to type `%s`", type_name(ecast->expr->type),
                          type_name(cast_type));
        return false;
    }

    assert(cast_type == src_eop.type);

    expr->type = src_eop.type;
    expr->is_lvalue = src_eop.is_lvalue;
    expr->is_constexpr = src_eop.is_constexpr;
    expr->const_val = src_eop.const_val;

    return true;
}

static bool resolve_expr_array_compound_lit(Resolver* resolver, ExprCompoundLit* expr, Type* type)
{
    assert(type->kind == TYPE_ARRAY);

    if (type->as_array.base == type_void) {
        resolver_on_error(resolver, "Cannot declare an array of `void` elements");
        return false;
    }

    Type* elem_type = type->as_array.base;
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
            resolver_on_error(resolver, "Cannot use a name designator (`%s`) for an array compound literal",
                              designator.name);
            return false;
        }

        // Resolve array index designator expression.
        if (designator.kind == DESIGNATOR_INDEX) {
            if (!resolve_expr(resolver, designator.index, elem_type))
                return false;

            if (!designator.index->is_constexpr) {
                resolver_on_error(resolver, "Array index designator must be a compile-time constant expression");
                return false;
            }

            elem_index = designator.index->const_val.as_int._u64;
        }

        if (!infer_len && elem_index >= array_len) {
            resolver_on_error(resolver, "Array index designator `%llu` is not within the expected array bounds (`%llu`)",
                              elem_index, array_len);
            return false;
        }

        if (!resolve_expr(resolver, initzer->init, NULL))
            return false;

        ExprOperand init_op = OP_FROM_EXPR(initzer->init);

        // Initializer expression should be convertible to the element type.
        if (!convert_eop(&init_op, elem_type)) {
            resolver_on_error(resolver, "Array initializer of type `%s` cannot be converted to `%s`",
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
        type = type_array(resolver->ast_mem, &resolver->type_cache->arrays, elem_type, elem_index);
        ftprint_out("Inferred length %llu\n", elem_index);
    }

    // TODO: HMMMMMM.... there's a difference between an array initializer (not lvalue) and a compound literal (lvalue),
    // but the syntax is ambiguous.
    expr->super.type = type;
    expr->super.is_lvalue = is_compound_lit;
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
            resolver_on_error(resolver, "Compound literal type `%s` does not match expected type `%s`", type_name(type),
                              type_name(expected_type));
            return false;
        }
    }

    if (!type) {
        resolver_on_error(resolver, "Unknown type for compound literal");
        return false;
    }

    // For now, only allow array types.
    // TODO: Support struct types
    if (type->kind == TYPE_ARRAY) {
        return resolve_expr_array_compound_lit(resolver, expr, type);
    }

    resolver_on_error(resolver, "Invalid compound literal type `%s`", type_name(type));

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
        return type_void;

    switch (typespec->kind) {
    case CST_TypeSpecIdent: {
        TypeSpecIdent* ts = (TypeSpecIdent*)typespec;

        // TODO: Support module path

        const char* ident_name = ts->name;
        Symbol* ident_sym = resolve_name(resolver, ident_name);

        if (!ident_sym) {
            resolver_on_error(resolver, "Undefined type `%s`", ident_name);
            return NULL;
        }

        if (ident_sym->kind != SYMBOL_TYPE) {
            resolver_on_error(resolver, "Symbol `%s` is not a type", ident_name);
            return NULL;
        }

        return ident_sym->type;
    }
    case CST_TypeSpecPtr: {
        TypeSpecPtr* ts = (TypeSpecPtr*)typespec;
        TypeSpec* base_ts = ts->base;
        Type* base_type = resolve_typespec(resolver, base_ts);

        if (!base_type)
            return NULL;

        return type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, base_type);
    }
    case CST_TypeSpecArray: {
        TypeSpecArray* ts = (TypeSpecArray*)typespec;
        Type* base_type = resolve_typespec(resolver, ts->base);

        if (!base_type)
            return NULL;

        if (base_type->size == 0) {
            resolver_on_error(resolver, "Array element type must have non-zero size");
            return NULL;
        }

        size_t len = 0;

        if (ts->len) {
            if (!resolve_expr(resolver, ts->len, NULL))
                return NULL;

            if (!ts->len->is_constexpr) {
                resolver_on_error(resolver, "Array length must be a compile-time constant");
                return NULL;
            }

            if (ts->len->type->kind != TYPE_INTEGER) {
                resolver_on_error(resolver, "Array length must be an integer");
                return NULL;
            }

            len = (size_t)(ts->len->const_val.as_int._u64);

            if (len == 0) {
                resolver_on_error(resolver, "Array length must be a positive, non-zero integer");
                return NULL;
            }
        }

        return type_array(resolver->ast_mem, &resolver->type_cache->arrays, base_type, len);
    }
    case CST_TypeSpecProc: {
        TypeSpecProc* ts = (TypeSpecProc*)typespec;

        AllocatorState mem_state = allocator_get_state(resolver->tmp_mem);
        Type** params = array_create(resolver->tmp_mem, Type*, ts->num_params);
        List* head = &ts->params;

        for (List* it = head->next; it != head; it = it->next) {
            ProcParam* proc_param = list_entry(it, ProcParam, lnode);
            Type* param = resolve_typespec(resolver, proc_param->typespec);

            if (!param) {
                allocator_restore_state(mem_state);
                return NULL;
            }

            if (param == type_void) {
                resolver_on_error(resolver, "Procedure parameter cannot be void");
                allocator_restore_state(mem_state);
                return NULL;
            }

            array_push(params, param);
        }
        assert(array_len(params) == ts->num_params);

        Type* ret = type_void;

        if (ts->ret) {
            ret = resolve_typespec(resolver, ts->ret);

            if (!ret) {
                allocator_restore_state(mem_state);
                return NULL;
            }
        }

        Type* type = type_proc(resolver->ast_mem, &resolver->type_cache->procs, array_len(params), params, ret);
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

        if (expr) {
            if (!resolve_expr(resolver, expr, declared_type))
                return false;

            ExprOperand right_eop = OP_FROM_EXPR(expr);

            // If assigning an array to a pointer, try to decay the right-hand-side expression.
            if ((declared_type->kind == TYPE_PTR) && !eop_decay(resolver, &right_eop))
                return false;

            if ((declared_type->kind == TYPE_ARRAY) && !declared_type->as_array.len) {
                declared_type = right_eop.type; // Get complete array type (with length) from right-hand-side expression.
            }
            else if (!convert_eop(&right_eop, declared_type)) {
                resolver_on_error(resolver, "Incompatible types. Cannot convert `%s` to `%s`",
                                  type_name(right_eop.type), type_name(declared_type));
                return false;
            }

            bool is_valid_array_init = (expr->kind == CST_ExprCompoundLit) || (expr->kind == CST_ExprStr);

            if ((declared_type->kind == TYPE_ARRAY) && !is_valid_array_init) {
                resolver_on_error(resolver, "Invalid array initializer");
                return false;
            }

            if (global && !right_eop.is_constexpr) {
                resolver_on_error(resolver, "Global variables must be initialized with a constant value");
                return false;
            }

            decl->init = try_wrap_cast_expr(resolver, &right_eop, decl->init);
            type = declared_type;
        }
        else {
            if ((declared_type->kind == TYPE_ARRAY) && !declared_type->as_array.len) {
                resolver_on_error(resolver, "Cannot infer the number of elements in array type specification");
                return false;
            }

            type = declared_type;
        }
    }
    else {
        assert(expr); // NOTE: Parser should catch this.

        if (!resolve_expr(resolver, expr, NULL))
            return false;

        if (global && !expr->is_constexpr) {
            resolver_on_error(resolver, "Global variables must be initialized with a constant value");
            return false;
        }

        type = expr->type;
    }

    assert(type);

    // TODO: Complete incomplete aggregate type
    sym->type = type;
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
        resolver_on_error(resolver, "Value for const decl `%s` must be a constant expression", decl->name);
        return false;
    }

    if (!type_is_scalar(init->type)) {
        resolver_on_error(resolver, "Constant expression must be of a scalar type");
        return false;
    }

    Type* type = NULL;

    if (typespec) {
        Type* declared_type = resolve_typespec(resolver, typespec);

        ExprOperand init_eop = OP_FROM_EXPR(init);

        if ((declared_type->kind == TYPE_PTR) && !eop_decay(resolver, &init_eop))
            return false;

        if (!convert_eop(&init_eop, declared_type)) {
            resolver_on_error(resolver, "Incompatible types. Cannot convert expression of type `%s` to `%s`",
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

static bool resolve_decl_proc_annotations(Resolver* resolver, DeclProc* decl)
{
    bool intrinsic = false;
    bool foreign = false;

    List* head = &decl->super.annotations;
    List* it = head->next;

    while (it != head) {
        Annotation* a = list_entry(it, Annotation, lnode);
        const char* name = a->name;

        if (name == annotation_names[ANNOTATION_INTRINSIC]) {
            if (intrinsic) {
                resolver_on_error(resolver, "Duplicate @intrinsic annotations");
                return false;
            }

            intrinsic = true;
        }
        else if (name == annotation_names[ANNOTATION_FOREIGN]) {
            if (foreign) {
                resolver_on_error(resolver, "Duplicate @foreign annotations");
                return false;
            }

            foreign = true;
        }

        it = it->next;
    }

    if (intrinsic && !decl->is_incomplete) {
        resolver_on_error(resolver, "Intrinsic procedure cannot have a body");
        return false;
    }

    // TODO: Check against known intrinsic procedures.

    if (foreign && !decl->is_incomplete) {
        resolver_on_error(resolver, "Foreign procedure cannot have a body");
        return false;
    }

    return true;
}

static bool resolve_decl_proc(Resolver* resolver, Symbol* sym)
{
    DeclProc* decl = (DeclProc*)sym->decl;

    if (!resolve_decl_proc_annotations(resolver, decl)) {
        return false;
    }

    decl->scope = push_scope(resolver, decl->num_params + decl->num_decls);

    AllocatorState mem_state = allocator_get_state(resolver->tmp_mem);
    Type** params = array_create(resolver->tmp_mem, Type*, 16);
    List* head = &decl->params;

    for (List* it = head->next; it != head; it = it->next) {
        DeclVar* proc_param = (DeclVar*)list_entry(it, Decl, lnode);
        Symbol* param_sym =
            add_unresolved_symbol(resolver, decl->scope, SYMBOL_VAR, proc_param->name, (Decl*)proc_param);

        assert(param_sym);

        if (!resolve_decl_var(resolver, param_sym)) {
            allocator_restore_state(mem_state);
            return false;
        }

        // TODO: recursive ptr decay on param type
        // TODO: complete incomplete param type (struct, union)

        if (param_sym->type == type_void) {
            resolver_on_error(resolver, "Procedure parameter cannot be void");
            allocator_restore_state(mem_state);
            return false;
        }

        array_push(params, param_sym->type);
    }

    pop_scope(resolver);
    assert(array_len(params) == decl->num_params);
    allocator_restore_state(mem_state);

    Type* ret_type = type_void;

    if (decl->ret) {
        ret_type = resolve_typespec(resolver, decl->ret);

        if (!ret_type)
            return false;
    }

    sym->type = type_proc(resolver->ast_mem, &resolver->type_cache->procs, array_len(params), params, ret_type);
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

    if ((ret_type != type_void) && !returns && success) {
        resolver_on_error(resolver, "Not all code paths in procedure `%s` return a value", dproc->name);
        return false;
    }

    return success;
}

static bool resolve_global_proc_body(Resolver* resolver, Symbol* sym)
{
    assert(sym->kind == SYMBOL_PROC);

    if (((DeclProc*)(sym->decl))->is_incomplete)
        return true;

    if (!resolve_proc_stmts(resolver, sym))
        return false;

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
        if (ret_success & RESOLVE_STMT_RETURNS) {
            resolver_on_error(resolver, "Statement will never be executed; all previous control paths return");

            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }

        Stmt* child_stmt = list_entry(it, Stmt, lnode);
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
    expr_eop->is_lvalue = expr->is_lvalue;
    expr_eop->const_val = expr->const_val;

    if (!eop_decay(resolver, expr_eop))
        return false;

    if (!type_is_scalar(expr_eop->type)) {
        resolver_on_error(resolver, "Conditional expression must resolve to a scalar type, have type `%s`",
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
    unsigned ret = resolve_stmt(resolver, swhile->body, ret_type,
                                flags | RESOLVE_STMT_BREAK_ALLOWED | RESOLVE_STMT_CONTINUE_ALLOWED);

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
    unsigned ret = resolve_stmt(resolver, swhile->body, ret_type,
                                flags | RESOLVE_STMT_BREAK_ALLOWED | RESOLVE_STMT_CONTINUE_ALLOWED);

    // Report an error if the do-while loop always returns before the condition check.
    if (ret & RESOLVE_STMT_RETURNS) {
        resolver_on_error(resolver, "All paths in do-while loop's body return");
        ret &= ~RESOLVE_STMT_SUCCESS;
    }

    return ret;
}

static bool eop_decay(Resolver* resolver, ExprOperand* eop)
{
    if (eop->type->kind != TYPE_ARRAY) {
        return true;
    }

    if (!eop->is_lvalue) {
        resolver_on_error(resolver, "An array rvalue (e.g., initializer) cannot be converted to a pointer");
        return false;
    }

    eop->type = type_ptr(resolver->ast_mem, &resolver->type_cache->ptrs, eop->type->as_array.base);
    eop->is_lvalue = false;

    return true;
}

static Expr* try_wrap_cast_expr(Resolver* resolver, ExprOperand* eop, Expr* orig_expr)
{
    Expr* expr = orig_expr;

    if (orig_expr->type != eop->type) {
        if (expr->is_constexpr && type_is_scalar(expr->type)) {
            assert(eop->is_constexpr);
            expr->const_val = eop->const_val;
        }
        else {
            expr = new_expr_cast(resolver->ast_mem, NULL, orig_expr, true, orig_expr->range);
        }

        expr->type = eop->type;
    }

    expr->is_lvalue = eop->is_lvalue;

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
        resolver_on_error(resolver, "Left side of assignment statement must be an l-value");
        return 0;
    }

    if (left_expr->type->kind == TYPE_ARRAY) {
        resolver_on_error(resolver, "Left side of assignment statement cannot be an array");
        return 0;
    }

    // TODO: Support other assignment operators.
    if (sassign->op_assign != TKN_ASSIGN) {
        resolver_on_error(resolver, "Sorry! Only the `=` assignment operator is currently supported. Soon!");
        return 0;
    }

    ExprOperand right_eop = OP_FROM_EXPR(right_expr);

    if ((left_expr->type->kind == TYPE_PTR) && !eop_decay(resolver, &right_eop))
        return false;

    if (!convert_eop(&right_eop, left_expr->type)) {
        resolver_on_error(resolver, "Type mismatch in assignment statement: expected type `%s`, but got `%s`",
                          type_name(left_expr->type), type_name(right_eop.type));
        return 0;
    }

    sassign->right = try_wrap_cast_expr(resolver, &right_eop, sassign->right);

    return RESOLVE_STMT_SUCCESS;
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
    case CST_StmtReturn: {
        ret = RESOLVE_STMT_RETURNS;
        StmtReturn* sret = (StmtReturn*)stmt;

        if (!sret->expr && (ret_type != type_void)) {
            resolver_on_error(resolver, "Return statement is missing a return value of type `%s`", type_name(ret_type));
            break;
        }

        if (sret->expr) {
            if (!resolve_expr(resolver, sret->expr, ret_type))
                break;

            ExprOperand ret_eop = OP_FROM_EXPR(sret->expr);

            if (!eop_decay(resolver, &ret_eop))
                return false;

            if (!convert_eop(&ret_eop, ret_type)) {
                resolver_on_error(resolver, "Invalid return type. Wanted `%s`, but got `%s`", type_name(ret_type),
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
            resolver_on_error(resolver, "Illegal break statement");

        break;
    }
    case CST_StmtContinue: {
        if (continue_allowed)
            ret = RESOLVE_STMT_SUCCESS;
        else
            resolver_on_error(resolver, "Illegal continue statement");

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
        Scope* scope = resolver->curr_scope;

        if (decl->kind == CST_DeclVar) {
            DeclVar* dvar = (DeclVar*)decl;
            Symbol* sym = add_unresolved_symbol(resolver, scope, SYMBOL_VAR, dvar->name, decl);

            if (!sym)
                resolver_on_error(resolver, "Variable `%s` shadows a previous local declaration", dvar->name);
            else if (resolve_decl_var(resolver, sym))
                ret = RESOLVE_STMT_SUCCESS;
        }
        else {
            // TODO: Support other declaration kinds.
            resolver_on_error(resolver, "Only variable and type declarations are supported inside procedures");
        }

        break;
    }
    case CST_StmtBlock: {
        ret = resolve_stmt_block(resolver, stmt, ret_type, flags);
        break;
    }
    default:
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
        resolver_on_error(resolver, "Cannot resolve symbol `%s` due to cyclic dependency", sym->name);
        return false;
    }

    assert(sym->status == SYMBOL_STATUS_UNRESOLVED);

    sym->status = SYMBOL_STATUS_RESOLVING;

    switch (sym->kind) {
    case SYMBOL_VAR:
        return resolve_decl_var(resolver, sym);
    case SYMBOL_CONST:
        return resolve_decl_const(resolver, sym);
    case SYMBOL_PROC:
        return resolve_decl_proc(resolver, sym);
    default:
        ftprint_err("Unhandled symbol kind `%d`\n", sym->kind);
        assert(0);
        break;
    }

    return false;
}

static Symbol* resolve_name(Resolver* resolver, const char* name)
{
    Symbol* sym = lookup_symbol(resolver->curr_scope, name);

    if (!sym)
        return NULL;

    if (!resolve_symbol(resolver, sym))
        return NULL;

    return sym;
}

bool resolve_global_decls(Resolver* resolver, List* decls)
{
    size_t num_decls = 0;
    List* head = decls;

    // Install decls in global symbol table.
    for (List* it = head->next; it != head; it = it->next) {
        Decl* decl = list_entry(it, Decl, lnode);
        SymbolKind kind = SYMBOL_NONE;
        const char* name = NULL;

        fill_decl_symbol_info(decl, &kind, &name);
        add_unresolved_symbol(resolver, resolver->global_scope, kind, name, decl);

        num_decls++;
    }

    // Resolve declaration "headers". Will not resolve procedure bodies or complete aggregate types.
    List* sym_head = &resolver->global_scope->sym_list;

    for (List* it = sym_head->next; it != sym_head; it = it->next) {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (!resolve_symbol(resolver, sym))
            return false;
    }

    // Resolve declaration "bodies".
    for (List* it = sym_head->next; it != sym_head; it = it->next) {
        Symbol* sym = list_entry(it, Symbol, lnode);

        if (sym->kind == SYMBOL_PROC) {
            if (!resolve_global_proc_body(resolver, sym))
                return false;
        }
    }

    return true;
}

void init_resolver(Resolver* resolver, Allocator* ast_mem, Allocator* tmp_mem, ByteStream* errors,
                   TypeCache* type_cache, Scope* global_scope)
{
    resolver->ast_mem = ast_mem;
    resolver->tmp_mem = tmp_mem;
    resolver->errors = errors;
    resolver->type_cache = type_cache;
    resolver->global_scope = global_scope;

    set_scope(resolver, global_scope);
    init_builtin_syms(resolver);
}
