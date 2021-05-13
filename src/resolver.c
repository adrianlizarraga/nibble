#include "resolver.h"
#include "parser.h"
#define NIBBLE_PRINT_DECLS

static bool resolve_program(Program* prog);
static Symbol* resolve_name(Program* prog, const char* name);
static bool resolve_symbol(Program* prog, Symbol* sym);
static bool resolve_decl(Program* prog, Decl* decl);
static bool resolve_decl_var(Program* prog, Decl* decl);
static Type* resolve_typespec(Program* prog, TypeSpec* typespec);
static bool resolve_expr(Program* prog, Expr* expr, Type* expected_type);
static bool resolve_expr_int(Program* prog, Expr* expr);
static Symbol* lookup_symbol(Program* prog, const char* name);

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

static Symbol* lookup_symbol(Program* prog, const char* name)
{
    // Lookup local symbols first.
    for (Symbol* it = prog->local_syms; it != prog->local_syms_at; it += 1)
    {
        if (it->name == name)
            return it;
    }

    // Lookup global syms.
    uint64_t* pval = hmap_get(&prog->global_syms, PTR_UINT(name));
    Symbol* sym = pval ? (void*)*pval : NULL;

    return sym;
}

static bool resolve_expr_int(Program* prog, Expr* expr)
{
    ExprInt* eint = (ExprInt*)expr;

    // TODO: Take into account literal suffix (e.g., u, ul, etc.)
    expr->type = type_int;
    expr->is_const = true;
    expr->const_val.kind = SCALAR_INTEGER;
    expr->const_val.as_int.kind = INTEGER_INT; // TODO: Redundant
    expr->const_val.as_int.i = (int)eint->value;

    return true;
}

static bool resolve_expr(Program* prog, Expr* expr, Type* expected_type)
{
    switch (expr->kind)
    {
        case AST_ExprInt: {
            return resolve_expr_int(prog, expr);
        } break;
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
            size_t num_params = 0;

            for (List* it = head->next; it != head; it = it->next)
            {
                ProcParam* proc_param = list_entry(it, ProcParam, lnode);
                Type* param = resolve_typespec(prog, proc_param->typespec);

                if (!param)
                {
                    allocator_restore_state(mem_state);
                    return NULL;
                }

                params[num_params] = param;
                num_params += 1;
            }

            assert(num_params == ts->num_params);
            allocator_restore_state(mem_state);

            Type* ret = resolve_typespec(prog, ts->ret);
            if (!ret)
                return NULL;

            return type_proc(&prog->ast_mem, &prog->type_proc_cache, num_params, params, ret);
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

        if (expr && declared_type)
        {
            if (resolve_expr(prog, expr, declared_type))
            {
                Type* inferred_type = expr->type;

                // TODO: Check if can convert type.
                if (inferred_type != declared_type)
                {
                    program_on_error(prog, "Incompatible types. Cannot convert `%s` to `%s`", 
                                     type_name(inferred_type),
                                     type_name(declared_type));
                }
                else
                {
                    type = declared_type;
                }
            }
        }
    }
    else
    {
        assert(expr); // NOTE: Parser should catch this.

        if (resolve_expr(prog, expr, NULL))
            type = expr->type;
    }

    decl->type = type;

    return type != NULL;
}

static bool resolve_decl(Program* prog, Decl* decl)
{
    switch (decl->kind)
    {
        case AST_DeclVar:
            return resolve_decl_var(prog, decl);
        case AST_DeclConst:
        case AST_DeclEnum:
        case AST_DeclUnion:
        case AST_DeclStruct:
        case AST_DeclTypedef:
        case AST_DeclProc:
            ftprint_err("Decl kind `%d` not YET supported in resolution\n", decl->kind);
            break;
        default:
            ftprint_err("Unknown decl kind `%d` while resolving symbol\n", decl->kind);
            assert(0);
            break;
    }

    return false;
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

static bool resolve_program(Program* prog)
{
    List* head = &prog->decls;

    for (List* it = head->next; it != head; it = it->next)
    {
        Decl* decl = list_entry(it, Decl, lnode);
        Symbol* sym = resolve_name(prog, decl->name);

        if (!sym)
            return NULL;

        ftprint_out("Resolved symbol `%s` type: `%s`\n", sym->name, type_name(decl->type));
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
        resolve_program(prog);
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
