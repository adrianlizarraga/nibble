#include "resolver.h"
#include "parser.h"
#define NIBBLE_PRINT_DECLS

static bool resolve_decls(Program* prog);
static Symbol* resolve_name(Program* prog, const char* name);
static bool resolve_symbol(Program* prog, Symbol* sym);
static bool resolve_decl(Program* prog, Decl* decl);
static bool resolve_decl_var(Program* prog, Decl* decl);
static bool resolve_decl_const(Program* prog, Decl* decl);
static bool resolve_decl_proc(Program* prog, Decl* decl);
static bool resolve_decl_proc_body(Program* prog, Decl* decl);
static bool resolve_expr(Program* prog, Expr* expr, Type* expected_type);
static bool resolve_expr_int(Program* prog, Expr* expr);
static bool resolve_expr_binary(Program* prog, Expr* expr);
static bool resolve_expr_call(Program* prog, Expr* expr);
static bool resolve_cond_expr(Program* prog, Expr* expr);
static Type* resolve_typespec(Program* prog, TypeSpec* typespec);

enum ResolveStmtRetFlags {
    RESOLVE_STMT_SUCCESS = 0x1,
    RESOLVE_STMT_RETURNS = 0x2,
};

enum ResolveStmtInFlags {
    RESOLVE_STMT_BREAK_ALLOWED = 0x1,
    RESOLVE_STMT_CONTINUE_ALLOWED = 0x2,
};

static unsigned resolve_stmt(Program* prog, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_block(Program* prog, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_while(Program* prog, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_if(Program* prog, Stmt* stmt, Type* ret_type, unsigned flags);
static unsigned resolve_cond_block(Program* prog, IfCondBlock* cblock, Type* ret_type, unsigned flags);
static unsigned resolve_stmt_expr_assign(Program* prog, Stmt* stmt);

static Symbol* lookup_symbol(Program* prog, const char* name);
static Symbol* lookup_local_symbol(Program* prog, const char* name);
static Symbol* enter_scope(Program* prog);
static void exit_scope(Program* prog, Symbol* scope_begin);
static bool push_local_var(Program* prog, Decl* decl);

static bool parse_code(Program* prog, const char* code);
static void init_builtin_syms(Program* prog);
static bool add_global_type_symbol(Program* prog, const char* name, Type* type);
static bool add_global_decl_symbol(Program* prog, Decl* decl);

static char* slurp_file(Allocator* allocator, const char* filename)
{
    FILE* fd = fopen(filename, "r");
    if (!fd)
    {
        NIBBLE_FATAL_EXIT("Failed to open file %s", filename);
        return NULL;
    }

    if (fseek(fd, 0, SEEK_END) < 0)
    {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    long int size = ftell(fd);
    if (size < 0)
    {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    char* buf = mem_allocate(allocator, size + 1, DEFAULT_ALIGN, false);
    if (!buf)
    {
        NIBBLE_FATAL_EXIT("Out of memory: %s:%d", __FILE__, __LINE__);
        return NULL;
    }

    if (fseek(fd, 0, SEEK_SET) < 0)
    {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    size_t n = fread(buf, 1, (size_t)size, fd);
    if (ferror(fd))
    {
        NIBBLE_FATAL_EXIT("Failed to read file %s", filename);
        return NULL;
    }

    fclose(fd);

    buf[n] = '\0';

    return buf;
}

static void program_on_error(Program* prog, const char* format, ...)
{
    char buf[MAX_ERROR_LEN];
    size_t size = 0;
    va_list vargs;

    va_start(vargs, format);
    size = vsnprintf(buf, MAX_ERROR_LEN, format, vargs) + 1;
    va_end(vargs);

    add_byte_stream_chunk(&prog->errors, buf, size > sizeof(buf) ? sizeof(buf) : size);
}

static bool add_global_decl_symbol(Program* prog, Decl* decl)
{
    const char* sym_name = decl->name;

    if (hmap_get(&prog->global_syms, PTR_UINT(sym_name)))
    {
        program_on_error(prog, "Duplicate definition of `%s`", sym_name);
        return false;
    }

    Symbol* sym = new_symbol_decl(&prog->ast_mem, decl);

    list_add_last(&prog->decls, &decl->lnode);
    hmap_put(&prog->global_syms, PTR_UINT(sym_name), PTR_UINT(sym));

    return true;
}

static bool add_global_type_symbol(Program* prog, const char* name, Type* type)
{
    const char* sym_name = intern_ident(name, cstr_len(name), NULL, NULL);

    if (hmap_get(&prog->global_syms, PTR_UINT(sym_name)))
    {
        program_on_error(prog, "Duplicate definition of `%s`", sym_name);
        return false;
    }

    Symbol* sym = new_symbol_type(&prog->ast_mem, sym_name, type);

    hmap_put(&prog->global_syms, PTR_UINT(sym_name), PTR_UINT(sym));

    return true;
}

static void init_builtin_syms(Program* prog)
{
    add_global_type_symbol(prog, "void", type_void);
    add_global_type_symbol(prog, "bool", type_bool);
    add_global_type_symbol(prog, "char", type_char);
    add_global_type_symbol(prog, "schar", type_schar);
    add_global_type_symbol(prog, "uchar", type_uchar);
    add_global_type_symbol(prog, "short", type_short);
    add_global_type_symbol(prog, "ushort", type_ushort);
    add_global_type_symbol(prog, "int", type_int);
    add_global_type_symbol(prog, "uint", type_uint);
    add_global_type_symbol(prog, "long", type_long);
    add_global_type_symbol(prog, "ulong", type_ulong);
    add_global_type_symbol(prog, "llong", type_llong);
    add_global_type_symbol(prog, "ullong", type_ullong);
    add_global_type_symbol(prog, "ssize", type_ssize);
    add_global_type_symbol(prog, "usize", type_usize);
    add_global_type_symbol(prog, "float32", type_f32);
    add_global_type_symbol(prog, "float64", type_f64);
}

static bool parse_code(Program* prog, const char* code)
{
    Parser parser = {0};

    parser_init(&parser, &prog->ast_mem, &prog->tmp_mem, code, 0, &prog->errors);
    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF))
    {
        Decl* decl = parse_decl(&parser);

        if (!decl)
            return false;

        add_global_decl_symbol(prog, decl);

#ifdef NIBBLE_PRINT_DECLS
        ftprint_out("%s\n", ftprint_decl(&prog->gen_mem, decl));
#endif
    }

    return true;
}

static Symbol* enter_scope(Program* prog)
{
    return prog->local_syms_at;
}

static void exit_scope(Program* prog, Symbol* scope_begin)
{
    prog->local_syms_at = scope_begin;
}

static bool push_local_var(Program* prog, Decl* decl)
{
    // TODO: Only look up to the beginning of the current scope to allow shadowing
    // variables in parent scopes. This currently prohibits all local variable shadowing;
    // global variable shadowing is currently allowed.
    if (lookup_local_symbol(prog, decl->name))
        return false;

    if (prog->local_syms_at == prog->local_syms + MAX_LOCAL_SYMS)
    {
        NIBBLE_FATAL_EXIT("INTERNAL ERROR: Pushed too many local symbols");
        return false;
    }

    Symbol* sym = prog->local_syms_at;
    sym->kind = SYMBOL_DECL;
    sym->status = SYMBOL_STATUS_RESOLVED;
    sym->name = decl->name;
    sym->decl = decl;

    prog->local_syms_at += 1;

    return true;
}

static Symbol* lookup_local_symbol(Program* prog, const char* name)
{
    for (Symbol* it = prog->local_syms_at; it != prog->local_syms; it -= 1)
    {
        Symbol* sym = it - 1;

        if (sym->name == name)
            return sym;
    }

    return NULL;
}

static Symbol* lookup_symbol(Program* prog, const char* name)
{
    // Lookup local symbols first.
    Symbol* sym = lookup_local_symbol(prog, name);

    // Lookup global syms.
    if (!sym)
    {
        uint64_t* pval = hmap_get(&prog->global_syms, PTR_UINT(name));
        sym = pval ? (void*)*pval : NULL;
    }

    return sym;
}

static bool resolve_expr_int(Program* prog, Expr* expr)
{
    ExprInt* eint = (ExprInt*)expr;

    // TODO: Take into account literal suffix (e.g., u, ul, etc.)
    expr->type = type_int;
    expr->is_const = true;
    expr->is_lvalue = false;
    expr->const_val.kind = SCALAR_INTEGER;
    expr->const_val.as_int.kind = INTEGER_INT; // TODO: Redundant
    expr->const_val.as_int.i = (int)eint->value;

    return true;
}

static bool resolve_expr_binary(Program* prog, Expr* expr)
{
    ExprBinary* ebinary = (ExprBinary*)expr;
    Expr* left = ebinary->left;
    Expr* right = ebinary->right;

    if (!resolve_expr(prog, left, NULL))
        return false;

    if (!resolve_expr(prog, right, NULL))
        return false;

    switch (ebinary->op)
    {
        case TKN_PLUS:
            if (type_is_arithmetic(left->type) && type_is_arithmetic(right->type))
            {
                if (left->type == right->type)
                {
                    if (left->is_const && right->is_const)
                    {
                        assert(left->type == type_int); // TODO: Support other types
                        expr->type = left->type;
                        expr->is_const = true;
                        expr->is_lvalue = false;
                        expr->const_val.kind = SCALAR_INTEGER;
                        expr->const_val.as_int.kind = INTEGER_INT; // TODO: Redundant
                        expr->const_val.as_int.i = (int)left->const_val.as_int.i + (int)right->const_val.as_int.i;
                    }
                    else
                    {
                        expr->type = left->type;
                        expr->is_const = false;
                        expr->is_lvalue = false;
                    }

                    return true;
                }
                else
                {
                    // TODO: Support type conversion
                    program_on_error(prog, "Cannot add operands of different types");
                    return false;
                }
            }
            else
            {
                // TODO: Support pointer arithmetic.
                program_on_error(prog, "Can only add arithmetic types");
                return false;
            }
            break;
        default:
            program_on_error(prog, "Operation type `%d` not supported", ebinary->op);
            break;
    }

    return false;
}

static bool resolve_expr_ident(Program* prog, Expr* expr)
{
    ExprIdent* eident = (ExprIdent*)expr;
    Symbol* sym = resolve_name(prog, eident->name);

    if (!sym)
    {
        program_on_error(prog, "Unknown symbol `%s` in expression", eident->name);
        return false;
    }

    switch (sym->kind)
    {
        case SYMBOL_DECL:
            switch (sym->decl->kind)
            {
                case AST_DeclVar:
                    expr->type = type_decay(&prog->ast_mem, &prog->type_ptr_cache, sym->decl->type);
                    expr->is_lvalue = true;
                    expr->is_const = false;
                    eident->sym = sym;

                    return true;
                case AST_DeclConst:
                    expr->type = sym->decl->type;
                    expr->is_lvalue = false;
                    expr->is_const = true;
                    expr->const_val = ((DeclConst*)(sym->decl))->init->const_val;
                    eident->sym = sym;

                    return true;
                case AST_DeclProc:
                    expr->type = sym->decl->type;
                    expr->is_lvalue = false;
                    expr->is_const = false;
                    eident->sym = sym;

                    return true;
                default:
                    break;
            }
            break;
        default:
            break;
    }

    program_on_error(prog, "Expression identifier `%s` must refer to a var, const, or proc declaration", eident->name);
    return false;
}

static bool resolve_expr_call(Program* prog, Expr* expr)
{
    ExprCall* ecall = (ExprCall*)expr;

    // Resolve procedure expression.
    if (!resolve_expr(prog, ecall->proc, NULL))
        return false;

    Type* proc_type = ecall->proc->type;

    // Verifty that we're calling an actual procedure type.
    if (proc_type->kind != TYPE_PROC)
    {
        program_on_error(prog, "Cannot use procedure call syntax on a value with a non-procedure type");
        return false;
    }

    // Verify that the number of arguments match number of parameters.
    if (proc_type->as_proc.num_params != ecall->num_args)
    {
        program_on_error(prog, "Incorrect number of procedure call arguments. Expected `%d` arguments, but got `%d`",
                         proc_type->as_proc.num_params, ecall->num_args);
        return false;
    }

    // Resolve argument expressions and verify that argument types match parameter types.
    List* head = &ecall->args;
    List* it = head->next;
    Type** params = proc_type->as_proc.params;
    size_t i = 0;

    while (it != head)
    {
        ProcCallArg* arg = list_entry(it, ProcCallArg, lnode);

        if (!resolve_expr(prog, arg->expr, NULL))
            return false;

        Type* arg_type = type_decay(&prog->ast_mem, &prog->type_ptr_cache, arg->expr->type);
        Type* param_type = type_decay(&prog->ast_mem, &prog->type_ptr_cache, params[i]);

        // TODO: Support type conversion
        if (arg->expr->type != param_type)
        {
            program_on_error(prog, "Incorrect type for argument %d of procedure call. Expected type `%s`, but got `%s`",
                             (i + 1), type_name(params[i]), type_name(arg_type));
            return false;
        }

        it = it->next;
        i += 1;
    }

    expr->type = proc_type->as_proc.ret;
    expr->is_lvalue = false;
    expr->is_const = false;

    return true;
}

static bool resolve_expr(Program* prog, Expr* expr, Type* expected_type)
{
    switch (expr->kind)
    {
        case AST_ExprInt:
            return resolve_expr_int(prog, expr);
        case AST_ExprBinary:
            return resolve_expr_binary(prog, expr);
        case AST_ExprIdent:
            return resolve_expr_ident(prog, expr);
        case AST_ExprCall:
            return resolve_expr_call(prog, expr);
        default:
            ftprint_err("Unsupported expr kind `%d` while resolving\n", expr->kind);
            assert(0);
            break;
    }

    return false;
}

static Type* resolve_typespec(Program* prog, TypeSpec* typespec)
{
    if (!typespec)
        return type_void;

    Type* type = NULL;

    switch (typespec->kind)
    {
        case AST_TypeSpecIdent:
        {
            TypeSpecIdent* ts = (TypeSpecIdent*)typespec;

            // TODO: Support module path

            const char* ident_name = ts->name;
            Symbol* ident_sym = lookup_symbol(prog, ident_name);

            if (!ident_sym)
            {
                program_on_error(prog, "Unresolved type `%s`", ident_name);
                return NULL;
            }

            if (!symbol_is_type(ident_sym))
            {
                program_on_error(prog, "Symbol `%s` is not a type", ident_name);
                return NULL;
            }

            resolve_symbol(prog, ident_sym);

            return ident_sym->kind == SYMBOL_TYPE ? ident_sym->type : ident_sym->decl->type;
        }
        case AST_TypeSpecPtr:
        {
            TypeSpecPtr* ts = (TypeSpecPtr*)typespec;
            TypeSpec* base_ts = ts->base;
            Type* base_type = resolve_typespec(prog, base_ts);

            if (!base_type)
                return NULL;

            return type_ptr(&prog->ast_mem, &prog->type_ptr_cache, base_type);
        }
        case AST_TypeSpecProc:
        {
            TypeSpecProc* ts = (TypeSpecProc*)typespec;

            AllocatorState mem_state = allocator_get_state(&prog->tmp_mem);
            Type** params = array_create(&prog->tmp_mem, Type*, 16);
            List* head = &ts->params;

            for (List* it = head->next; it != head; it = it->next)
            {
                ProcParam* proc_param = list_entry(it, ProcParam, lnode);
                Type* param = resolve_typespec(prog, proc_param->typespec);

                if (!param)
                {
                    allocator_restore_state(mem_state);
                    return NULL;
                }

                if (param == type_void)
                {
                    program_on_error(prog, "Procedure parameter cannot be void");
                    allocator_restore_state(mem_state);
                    return NULL;
                }

                array_push(params, param);
            }

            assert(array_len(params) == ts->num_params);
            allocator_restore_state(mem_state);

            Type* ret = type_void;

            if (ts->ret)
            {
                ret = resolve_typespec(prog, ts->ret);

                if (!ret)
                    return NULL;
            }

            return type_proc(&prog->ast_mem, &prog->type_proc_cache, array_len(params), params, ret);
        }
        default:
            ftprint_err("Unsupported typespec kind `%d` in resolution\n", typespec->kind);
            assert(0);
            break;
    }

    return type;
}

static bool resolve_decl_var(Program* prog, Decl* decl)
{
    DeclVar* decl_var = (DeclVar*)decl;
    TypeSpec* typespec = decl_var->typespec;
    Expr* expr = decl_var->init;
    Type* type = NULL;

    if (typespec)
    {
        Type* declared_type = resolve_typespec(prog, typespec);

        if (!declared_type)
            return NULL;

        if (expr)
        {
            if (resolve_expr(prog, expr, declared_type))
            {
                Type* inferred_type = expr->type;

                // TODO: Check if can convert type.
                if (inferred_type != declared_type)
                {
                    program_on_error(prog, "Incompatible types. Cannot convert `%s` to `%s`", type_name(inferred_type),
                                     type_name(declared_type));
                }
                else
                {
                    type = declared_type;
                }
            }
        }
        else
        {
            type = declared_type;
        }
    }
    else
    {
        assert(expr); // NOTE: Parser should catch this.

        if (resolve_expr(prog, expr, NULL))
            type = expr->type;
    }

    // TODO: Complete incomplete aggregate type

    decl->type = type;

    return type != NULL;
}

static bool resolve_decl_const(Program* prog, Decl* decl)
{
    DeclConst* dconst = (DeclConst*)decl;
    TypeSpec* typespec = dconst->typespec;
    Expr* init = dconst->init;

    if (!resolve_expr(prog, init, NULL))
        return false;

    if (!init->is_const)
    {
        program_on_error(prog, "Value for const decl `%s` must be a constant expression", decl->name);
        return false;
    }

    if (!type_is_scalar(init->type))
    {
        program_on_error(prog, "Constant expression must be of a scalar type");
        return false;
    }

    if (typespec)
    {
        Type* declared_type = resolve_typespec(prog, typespec);

        if (declared_type != init->type)
        {
            // TODO: Support type conversions
            program_on_error(prog, "Incompatible types. Cannot convert expression of type `%s` to `%s`",
                             type_name(init->type), type_name(declared_type));
            return false;
        }

        decl->type = declared_type;
    }
    else
    {
        decl->type = init->type;
    }

    return true;
}

static bool resolve_decl_proc(Program* prog, Decl* decl)
{
    DeclProc* dproc = (DeclProc*)decl;
    AllocatorState mem_state = allocator_get_state(&prog->tmp_mem);
    Type** params = array_create(&prog->tmp_mem, Type*, 16);
    List* head = &dproc->params;

    for (List* it = head->next; it != head; it = it->next)
    {
        Decl* proc_param = list_entry(it, Decl, lnode);

        if (!resolve_decl_var(prog, proc_param))
        {
            allocator_restore_state(mem_state);
            return false;
        }

        // TODO: recursive ptr decay on param type
        // TODO: complete incomplete param type (struct, union)

        if (proc_param->type == type_void)
        {
            program_on_error(prog, "Procedure parameter cannot be void");
            allocator_restore_state(mem_state);
            return false;
        }

        array_push(params, proc_param->type);
    }

    assert(array_len(params) == dproc->num_params);
    allocator_restore_state(mem_state);

    Type* ret = type_void;

    if (dproc->ret)
    {
        ret = resolve_typespec(prog, dproc->ret);

        if (!ret)
            return false;
    }

    decl->type = type_proc(&prog->ast_mem, &prog->type_proc_cache, array_len(params), params, ret);

    return true;
}

static bool resolve_decl_proc_body(Program* prog, Decl* decl)
{
    assert(decl->kind == AST_DeclProc);

    DeclProc* dproc = (DeclProc*)decl;
    Symbol* scope_begin = enter_scope(prog);
    List* head = &dproc->params;

    for (List* it = head->next; it != head; it = it->next)
    {
        Decl* proc_param = list_entry(it, Decl, lnode);

        push_local_var(prog, proc_param);
    }

    Type* ret_type = decl->type->as_proc.ret;
    unsigned r = resolve_stmt(prog, dproc->body, ret_type, 0);
    bool returns = r & RESOLVE_STMT_RETURNS;
    bool success = r & RESOLVE_STMT_SUCCESS;

    exit_scope(prog, scope_begin);

    if ((ret_type != type_void) && !returns && success)
    {
        program_on_error(prog, "Not all code paths in procedure `%s` return a value", decl->name);
        return false;
    }

    return success;
}

static bool resolve_decl(Program* prog, Decl* decl)
{
    switch (decl->kind)
    {
        case AST_DeclVar:
            return resolve_decl_var(prog, decl);
        case AST_DeclConst:
            return resolve_decl_const(prog, decl);
        case AST_DeclEnum:
        case AST_DeclUnion:
        case AST_DeclStruct:
        case AST_DeclTypedef:
            ftprint_err("Decl kind `%d` not YET supported in resolution\n", decl->kind);
            break;
        case AST_DeclProc:
            return resolve_decl_proc(prog, decl);
        default:
            ftprint_err("Unknown decl kind `%d` while resolving symbol\n", decl->kind);
            assert(0);
            break;
    }

    return false;
}

static unsigned resolve_stmt_block(Program* prog, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtBlock* sblock = (StmtBlock*)stmt;
    Symbol* scope_begin = enter_scope(prog);

    unsigned ret_success = RESOLVE_STMT_SUCCESS;
    List* head = &sblock->stmts;

    for (List* it = head->next; it != head; it = it->next)
    {
        if (ret_success & RESOLVE_STMT_RETURNS)
        {
            program_on_error(prog, "Statement will never be executed; all previous control paths return");

            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }

        Stmt* child_stmt = list_entry(it, Stmt, lnode);
        unsigned r = resolve_stmt(prog, child_stmt, ret_type, flags);

        // NOTE: Track whether any statement in the block returns from the parent procedure.
        ret_success = (r & RESOLVE_STMT_RETURNS) | ret_success;

        if (!(r & RESOLVE_STMT_SUCCESS))
        {
            ret_success &= ~RESOLVE_STMT_SUCCESS;
            break;
        }
    }

    exit_scope(prog, scope_begin);

    return ret_success;
}

static bool resolve_cond_expr(Program* prog, Expr* expr)
{
    // Resolve condition expression.
    if (!resolve_expr(prog, expr, NULL))
        return false;

    // Ensure that condition express is a scalar type.
    Type* cond_type = type_decay(&prog->ast_mem, &prog->type_ptr_cache, expr->type);

    if (!type_is_scalar(cond_type))
    {
        program_on_error(prog, "Conditional expression must resolve to a scalar type, have type `%s`",
                         type_name(cond_type));
        return false;
    }

    return true;
}

static unsigned resolve_cond_block(Program* prog, IfCondBlock* cblock, Type* ret_type, unsigned flags)
{
    if (!resolve_cond_expr(prog, cblock->cond))
        return 0;

    return resolve_stmt(prog, cblock->body, ret_type, flags);
}

static unsigned resolve_stmt_if(Program* prog, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtIf* sif = (StmtIf*)stmt;

    // Resolve if block.
    unsigned ret = resolve_cond_block(prog, &sif->if_blk, ret_type, flags);

    if (!(ret & RESOLVE_STMT_SUCCESS))
        return 0;

    // Resolve elif blocks.
    List* head = &sif->elif_blks;
    List* it = head->next;

    while (it != head)
    {
        IfCondBlock* elif_blk = list_entry(it, IfCondBlock, lnode);

        unsigned elif_ret = resolve_cond_block(prog, elif_blk, ret_type, flags);

        if (!(elif_ret & RESOLVE_STMT_SUCCESS))
            return 0;

        ret &= elif_ret; // NOTE: All blocks have to return in order to say that all control paths return.

        it = it->next;
    }

    // TODO: Ensure conditions are mutually exclusive (condition ANDed with each previous condition == false)
    // Can probably only do this for successive condition expressions that evaluate to compile-time constants.

    // Resolve else block.
    if (sif->else_blk.body)
        ret &= resolve_stmt(prog, sif->else_blk.body, ret_type, flags);
    else
        ret &= ~RESOLVE_STMT_RETURNS;

    return ret;
}

static unsigned resolve_stmt_while(Program* prog, Stmt* stmt, Type* ret_type, unsigned flags)
{
    StmtWhile* swhile = (StmtWhile*)stmt;

    // Resolve condition expression.
    if (!resolve_cond_expr(prog, swhile->cond))
        return 0;

    // Resolve loop body.
    unsigned ret =
        resolve_stmt(prog, swhile->body, ret_type, flags | RESOLVE_STMT_BREAK_ALLOWED | RESOLVE_STMT_CONTINUE_ALLOWED);

    // NOTE: Because while loops don't have an "else" path, we can't say that all control paths return.
    // TODO: Add else to while loop!!
    ret &= ~RESOLVE_STMT_RETURNS;

    return ret;
}

static unsigned resolve_stmt_expr_assign(Program* prog, Stmt* stmt)
{
    StmtExprAssign* sassign = (StmtExprAssign*)stmt;
    Expr* left_expr = sassign->left;
    Expr* right_expr = sassign->right;

    if (!resolve_expr(prog, left_expr, NULL))
        return 0;

    if (!resolve_expr(prog, right_expr, NULL))
        return 0;

    if (!left_expr->is_lvalue)
    {
        program_on_error(prog, "Left side of assignment statement must be an l-value");
        return 0;
    }

    if (left_expr->type->kind == TYPE_ARRAY)
    {
        program_on_error(prog, "Left side of assignment statement cannot be an array");
        return 0;
    }

    // TODO: Support other assignment operators.
    if (sassign->op_assign != TKN_ASSIGN)
    {
        program_on_error(prog, "Sorry! Only the `=` assignment operator is currently supported. Soon!");
        return 0;
    }

    Type* left_type = left_expr->type;
    Type* right_type = type_decay(&prog->ast_mem, &prog->type_ptr_cache, right_expr->type);

    // TODO: Support type conversion.
    if (left_type != right_type)
    {
        program_on_error(prog, "Type mismatch in assignment statement: expected type `%s`, but got `%s`",
                         type_name(left_type), type_name(right_type));
        return 0;
    }

    return RESOLVE_STMT_SUCCESS;
}

static unsigned resolve_stmt(Program* prog, Stmt* stmt, Type* ret_type, unsigned flags)
{
    bool break_allowed = flags & RESOLVE_STMT_BREAK_ALLOWED;
    bool continue_allowed = flags & RESOLVE_STMT_CONTINUE_ALLOWED;

    switch (stmt->kind)
    {
        case AST_StmtNoOp:
            return RESOLVE_STMT_SUCCESS;
        case AST_StmtReturn:
        {
            StmtReturn* sret = (StmtReturn*)stmt;

            if (!sret->expr && (ret_type != type_void))
            {
                program_on_error(prog, "Return statement is missing a return value of type `%s`", type_name(ret_type));
                return RESOLVE_STMT_RETURNS;
            }

            if (sret->expr)
            {
                if (!resolve_expr(prog, sret->expr, ret_type))
                    return RESOLVE_STMT_RETURNS;

                // TODO: Support type conversions
                if (sret->expr->type != ret_type)
                {
                    program_on_error(prog, "Invalid return type. Wanted `%s`, but got `%s`", type_name(ret_type),
                                     type_name(sret->expr->type));
                    return RESOLVE_STMT_RETURNS;
                }
            }

            return RESOLVE_STMT_SUCCESS | RESOLVE_STMT_RETURNS;
        }
        case AST_StmtBreak:
            if (!break_allowed)
            {
                program_on_error(prog, "Illegal break statement");
                return 0;
            }

            return RESOLVE_STMT_SUCCESS;
        case AST_StmtContinue:
            if (!continue_allowed)
            {
                program_on_error(prog, "Illegal continue statement");
                return 0;
            }

            return RESOLVE_STMT_SUCCESS;
        case AST_StmtIf:
            return resolve_stmt_if(prog, stmt, ret_type, flags);
        case AST_StmtWhile:
        case AST_StmtDoWhile:
            return resolve_stmt_while(prog, stmt, ret_type, flags);
        case AST_StmtExpr:
        {
            StmtExpr* sexpr = (StmtExpr*)stmt;

            if (!resolve_expr(prog, sexpr->expr, NULL))
                return 0;

            return RESOLVE_STMT_SUCCESS;
        }
        case AST_StmtExprAssign:
            return resolve_stmt_expr_assign(prog, stmt);
        case AST_StmtDecl:
        {
            StmtDecl* sdecl = (StmtDecl*)stmt;
            Decl* decl = sdecl->decl;

            if (decl->kind != AST_DeclVar)
            {
                // TODO: Support other declaration kinds.
                program_on_error(prog, "Only variable declarations are supported inside procedures");
                return 0;
            }

            if (!resolve_decl(prog, decl))
                return 0;

            if (!push_local_var(prog, decl))
            {
                program_on_error(prog, "Variable `%s` shadows a previous local declaration", decl->name);
                return 0;
            }

            return RESOLVE_STMT_SUCCESS;
        }
        case AST_StmtBlock:
            return resolve_stmt_block(prog, stmt, ret_type, flags);
        default:
            break;
    }

    return 0;
}

static bool resolve_symbol(Program* prog, Symbol* sym)
{
    if (sym->status == SYMBOL_STATUS_RESOLVED)
        return true;

    if (sym->status == SYMBOL_STATUS_RESOLVING)
    {
        program_on_error(prog, "Cannot resolve symbol `%s` due to cyclic dependency", sym->name);
        return false;
    }

    assert(sym->status == SYMBOL_STATUS_UNRESOLVED);

    sym->status = SYMBOL_STATUS_RESOLVING;

    bool resolved = true;

    switch (sym->kind)
    {
        case SYMBOL_TYPE:
            break;
        case SYMBOL_DECL:
            resolved = resolve_decl(prog, sym->decl);
            break;
        default:
            ftprint_err("Unknown symbol kind `%d`\n", sym->kind);
            assert(0);
            break;
    }

    if (resolved)
        sym->status = SYMBOL_STATUS_RESOLVED;
    else
        ftprint_err("Failed to resolve `%s`\n", sym->name);

    return resolved;
}

static Symbol* resolve_name(Program* prog, const char* name)
{
    Symbol* sym = lookup_symbol(prog, name);

    if (!sym)
        return NULL;

    if (!resolve_symbol(prog, sym))
        return NULL;

    return sym;
}

static bool resolve_decls(Program* prog)
{
    List* head = &prog->decls;

    for (List* it = head->next; it != head; it = it->next)
    {
        Decl* decl = list_entry(it, Decl, lnode);
        Symbol* sym = resolve_name(prog, decl->name);

        if (!sym)
            return NULL;
    }

    return true;
}

static bool resolve_decls_body(Program* prog)
{
    List* head = &prog->decls;

    for (List* it = head->next; it != head; it = it->next)
    {
        Decl* decl = list_entry(it, Decl, lnode);

        if (decl->kind == AST_DeclProc)
        {
            if (!resolve_decl_proc_body(prog, decl))
                return false;
        }
    }

    return true;
}

static void print_errors(ByteStream* errors)
{
    if (errors->count > 0)
    {
        ftprint_out("\nErrors: %lu\n", errors->count);

        ByteStreamChunk* chunk = errors->first;

        while (chunk)
        {
            ftprint_out("%s\n", chunk->buf);

            chunk = chunk->next;
        }
    }
}

Program* compile_program(const char* path)
{
    Allocator boot_mem = allocator_create(65536);
    Program* prog = alloc_type(&boot_mem, Program, true);

    prog->gen_mem = boot_mem;
    prog->ast_mem = allocator_create(4096);
    prog->tmp_mem = allocator_create(256);
    prog->errors = byte_stream_create(&prog->ast_mem);
    prog->path = intern_str_lit(path, cstr_len(path));
    prog->code = slurp_file(&prog->gen_mem, prog->path);
    prog->global_syms = hmap(8, NULL);
    prog->local_syms_at = prog->local_syms;
    prog->type_ptr_cache = hmap(6, NULL);
    prog->type_proc_cache = hmap(6, NULL);

    list_head_init(&prog->decls);
    init_builtin_syms(prog);

    if (parse_code(prog, prog->code))
    {
        if (resolve_decls(prog))
        {
            if (resolve_decls_body(prog))
            {
                ftprint_out("FINISHED RESOLVING\n");
            }
        }
    }

    print_errors(&prog->errors);

    return prog;
}

void free_program(Program* prog)
{
    hmap_destroy(&prog->global_syms);
    hmap_destroy(&prog->type_ptr_cache);
    hmap_destroy(&prog->type_proc_cache);

    // Clean up memory arenas
    Allocator bootstrap = prog->gen_mem;

#ifndef NDEBUG
    print_allocator_stats(&prog->ast_mem, "Prog AST mem stats");
    print_allocator_stats(&prog->tmp_mem, "Prog tmp mem stats");
    print_allocator_stats(&prog->gen_mem, "Prog gen mem stats");
#endif

    allocator_destroy(&prog->ast_mem);
    allocator_destroy(&prog->tmp_mem);
    allocator_destroy(&bootstrap);
}
