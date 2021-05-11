#include "resolver.h"
#include "parser.h"
#define NIBBLE_PRINT_DECLS

static void resolve_symbol(Program* prog, Symbol* sym);
static Type* resolve_decl_var(Program* prog, DeclVar* decl);
static Type* resolve_typespec(Program* prog, TypeSpec* typespec);
static ResolvedExpr resolve_expr(Program* prog, Expr* expr, Type* expected_type);
static ResolvedExpr resolve_expr_int(Program* prog, ExprInt* expr);
static Symbol* lookup_symbol(Program* prog, const char* name);

static void init_scope(Scope* scope, Scope* parent);
static void free_scope(Scope* scope);

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

static Symbol* sym_alloc(Allocator* allocator, SymbolKind kind, const char* name)
{
    Symbol* sym = alloc_type(allocator, Symbol, true);

    sym->kind = kind;
    sym->name = name;

    return sym;
}

static Symbol* sym_decl(Allocator* allocator, Decl* decl)
{
    Symbol* sym = NULL;

    switch (decl->kind)
    {
        case AST_DeclVar:
            sym = sym_alloc(allocator, SYMBOL_VAR, decl->name);
            sym->as_var.decl = decl;
            break;
        case AST_DeclConst:
            sym = sym_alloc(allocator, SYMBOL_CONST, decl->name);
            sym->as_const.decl = decl;
            break;
        case AST_DeclProc:
            sym = sym_alloc(allocator, SYMBOL_PROC, decl->name);
            sym->as_proc.decl = decl;
            break;
        case AST_DeclStruct:
            sym = sym_alloc(allocator, SYMBOL_TYPE, decl->name);
            sym->as_type.decl = decl;
            sym->as_type.kind = SYMBOL_TYPE_STRUCT;
            break;
        case AST_DeclUnion:
            sym = sym_alloc(allocator, SYMBOL_TYPE, decl->name);
            sym->as_type.decl = decl;
            sym->as_type.kind = SYMBOL_TYPE_UNION;
            break;
        case AST_DeclEnum:
            sym = sym_alloc(allocator, SYMBOL_TYPE, decl->name);
            sym->as_type.decl = decl;
            sym->as_type.kind = SYMBOL_TYPE_ENUM;
            break;
        case AST_DeclTypedef:
            sym = sym_alloc(allocator, SYMBOL_TYPE, decl->name);
            sym->as_type.decl = decl;
            sym->as_type.kind = SYMBOL_TYPE_TYPEDEF;
            break;
        default:
            ftprint_err("Cannot create symbol from declaration kind %d\n", decl->kind);
            assert(0);
            break;
    }

    return sym;
}

static bool add_decl_sym(Program* prog, Scope* scope, Decl* decl)
{
    const char* sym_name = decl->name;

    if (hmap_get(&scope->syms_map, PTR_UINT(sym_name)))
    {
        program_on_error(prog, "Duplicate definition of `%s`", sym_name);
        return false;
    }

    Symbol* sym = sym_decl(&prog->ast_mem, decl);

    list_add_last(&scope->decls, &decl->lnode);
    hmap_put(&scope->syms_map, PTR_UINT(sym_name), PTR_UINT(sym));

    return true;
}

static bool add_basic_type_sym(Program* prog, const char* name, Type* type)
{
    const char* sym_name = intern_ident(name, cstr_len(name), NULL, NULL);
    Allocator* allocator = &prog->ast_mem;
    Scope* scope = &prog->global_scope;

    if (hmap_get(&scope->syms_map, PTR_UINT(sym_name)))
    {
        program_on_error(prog, "Duplicate definition of `%s`", sym_name);
        return false;
    }

    Symbol* sym = sym_alloc(allocator, SYMBOL_TYPE, sym_name);

    sym->status = SYMBOL_STATUS_RESOLVED;
    sym->flags = SYMBOL_IS_BUILTIN;
    sym->as_type.kind = SYMBOL_TYPE_BASIC;
    sym->as_type.type = type;

    hmap_put(&scope->syms_map, PTR_UINT(sym_name), PTR_UINT(sym));

    return true;
}

static void init_builtin_syms(Program* prog)
{
    add_basic_type_sym(prog, "void", type_void);
    add_basic_type_sym(prog, "bool", type_bool);
    add_basic_type_sym(prog, "char", type_char);
    add_basic_type_sym(prog, "schar", type_schar);
    add_basic_type_sym(prog, "uchar", type_uchar);
    add_basic_type_sym(prog, "short", type_short);
    add_basic_type_sym(prog, "ushort", type_ushort);
    add_basic_type_sym(prog, "int", type_int);
    add_basic_type_sym(prog, "uint", type_uint);
    add_basic_type_sym(prog, "long", type_long);
    add_basic_type_sym(prog, "ulong", type_ulong);
    add_basic_type_sym(prog, "llong", type_llong);
    add_basic_type_sym(prog, "ullong", type_ullong);
    add_basic_type_sym(prog, "ssize", type_ssize);
    add_basic_type_sym(prog, "usize", type_usize);
    add_basic_type_sym(prog, "float32", type_float32);
    add_basic_type_sym(prog, "float64", type_float64);
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

        add_decl_sym(prog, &prog->global_scope, decl);

#ifdef NIBBLE_PRINT_DECLS
        ftprint_out("%s\n", ftprint_decl(&prog->gen_mem, decl));
#endif
    }

    return true;
}

static Symbol* lookup_symbol(Program* prog, const char* name)
{
    for (Scope* scope = prog->curr_scope; scope != NULL; scope = scope->parent)
    {
        uint64_t* pval = hmap_get(&scope->syms_map, PTR_UINT(name));

        if (pval)
            return (Symbol*)*pval;
    }

    return NULL;
}

static ResolvedExpr resolve_expr_int(Program* prog, ExprInt* expr)
{
    // TODO: Take into account literal suffix (e.g., u, ul, etc.)
    ResolvedExpr operand = {
        .type = type_int,
        .value.kind = SCALAR_INTEGER,
        .value.as_int.i = (int)expr->value,
        .is_const = true
    };

    return operand;
}

static ResolvedExpr resolve_expr(Program* prog, Expr* expr, Type* expected_type)
{
    ResolvedExpr operand = {0};

    switch (expr->kind)
    {
        case AST_ExprInt: {
            ExprInt* eint = (ExprInt*)expr;
            operand = resolve_expr_int(prog, eint);
        } break;
        default:
            ftprint_err("Unsupported expr kind `%d` while resolving\n", expr->kind);
            assert(0);
            break;
    }

    return operand;
}

static Type* resolve_typespec(Program* prog, TypeSpec* typespec)
{
    if (!typespec)
        return type_void;

    Type* type = NULL;

    switch (typespec->kind)
    {
        case AST_TypeSpecIdent: {
            TypeSpecIdent* ts = (TypeSpecIdent*)typespec;

            // TODO: Support module path

            const char* ident_name = ts->name;
            Symbol* ident_sym = lookup_symbol(prog, ident_name);

            if (!ident_sym)
            {
                program_on_error(prog, "Unresolved type `%s`", ident_name);
                return NULL;
            }

            if (ident_sym->kind != SYMBOL_TYPE)
            {
                program_on_error(prog, "Symbol `%s` is not a type", ident_name);
                return NULL;
            }

            resolve_symbol(prog, ident_sym);

            return ident_sym->as_type.type;
        }
        case AST_TypeSpecPtr: {
            TypeSpecPtr* ts = (TypeSpecPtr*)typespec;
            TypeSpec* base_ts = ts->base;
            Type* base_type = resolve_typespec(prog, base_ts);

            if (!base_type)
                return NULL;

            return type_ptr(&prog->ast_mem, base_type); // TODO: Hash consing!
        }
        default:
            ftprint_err("Unsupported typespec kind `%d` in resolution\n", typespec->kind);
            assert(0);
            break;
    }

    return type;
}

static Type* resolve_decl_var(Program* prog, DeclVar* decl)
{
    TypeSpec* typespec = decl->typespec;
    Expr* expr = decl->init;
    Type* type = NULL;

    if (typespec)
    {
        Type* declared_type = resolve_typespec(prog, typespec);

        if (expr && declared_type)
        {
            ResolvedExpr operand = resolve_expr(prog, expr, declared_type);
            Type* inferred_type = operand.type;

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
    else
    {
        assert(expr); // NOTE: Parser should catch this.

        ResolvedExpr operand = resolve_expr(prog, expr, NULL);
        type = operand.type;
    }

    return type;
}

static void resolve_symbol(Program* prog, Symbol* sym)
{
    if (sym->status == SYMBOL_STATUS_RESOLVED)
        return;

    if (sym->status == SYMBOL_STATUS_RESOLVING)
    {
        program_on_error(prog, "Cannot resolve symbol `%s` due to cyclic dependency", sym->name);
        return;
    }

    assert(sym->status == SYMBOL_STATUS_UNRESOLVED);

    sym->status = SYMBOL_STATUS_RESOLVING;

    switch (sym->kind)
    {
        case SYMBOL_VAR:
            sym->as_var.type = resolve_decl_var(prog, (DeclVar*)sym->as_var.decl);
            sym->status = SYMBOL_STATUS_RESOLVED;
            break;
        case SYMBOL_CONST:
            break;
        case SYMBOL_PROC:
            break;
        case SYMBOL_TYPE:
            break;
        default:
            ftprint_err("Unknown symbol kind `%d`\n", sym->kind);
            assert(0);
            break;
    }
}

static Symbol* resolve_name(Program* prog, const char* name)
{
    Symbol* sym = lookup_symbol(prog, name);

    if (sym)
        resolve_symbol(prog, sym);

    return sym;
}

static void enter_scope(Program* prog, Scope* scope)
{
    assert(scope);
    prog->curr_scope = scope;
}

static void exit_scope(Program* prog)
{
    prog->curr_scope = prog->curr_scope->parent;
}

static void resolve_scope(Program* prog, Scope* scope)
{
    enter_scope(prog, scope);

    List* head = &scope->decls;

    for (List* it = head->next; it != head; it = it->next)
    {
        Decl* decl = list_entry(it, Decl, lnode);
        Symbol* sym = resolve_name(prog, decl->name);

        //ftprint_out("Resolved `%s`. type is `%s`\n", sym->name, type_name(sym->t.type));
        ftprint_out("Resolved symbol `%s`\n", sym->name);
    }

    exit_scope(prog);
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

    init_scope(&prog->global_scope, NULL);
    init_builtin_syms(prog);

    if (parse_code(prog, prog->code))
    {
        resolve_scope(prog, &prog->global_scope);
    }

    print_errors(&prog->errors);

    return prog;
}

static void init_scope(Scope* scope, Scope* parent)
{
    scope->syms_map = hmap(8, NULL);
    scope->parent = parent;

    list_head_init(&scope->decls);
    list_head_init(&scope->children);
}

static void free_scope(Scope* scope)
{
    hmap_destroy(&scope->syms_map);

    List* head = &scope->children;

    for (List* it = head->next; it != head; it = it->next)
    {
        Scope* child = list_entry(it, Scope, lnode);

        free_scope(child);
    }
}

void free_program(Program* prog)
{
    free_scope(&prog->global_scope);

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
