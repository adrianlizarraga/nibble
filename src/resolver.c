#include "resolver.h"
#include "parser.h"
#define NIBBLE_PRINT_DECLS

static void resolve_symbol(Program* prog, Symbol* sym);
static Type* resolve_decl_var(Program* prog, DeclVar* decl);
static Type* resolve_typespec(Program* prog, TypeSpec* typespec);
static ResolvedExpr resolve_expr(Program* prog, Expr* expr, Type* expected_type);
static ResolvedExpr resolve_expr_int(Program* prog, ExprInt* expr);
static Symbol* lookup_symbol(Program* prog, const char* name);

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

static Module* enter_module(Program* prog, Module* module)
{
    Module* old = prog->curr_module;

    prog->curr_module = module;

    return old;
}

static void restore_module(Program* prog, Module* module)
{
    prog->curr_module = module;
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

static Symbol* sym_alloc(Allocator* allocator, SymbolKind kind, const char* name, Decl* decl, Module* module)
{
    Symbol* sym = alloc_type(allocator, Symbol, true);

    if (!sym)
    {
        NIBBLE_FATAL_EXIT("Out of memory: %s:%d", __FILE__, __LINE__);
        return NULL;
    }

    sym->kind = kind;
    sym->module = module;
    sym->name = name;
    sym->decl = decl;

    return sym;
}

static Symbol* sym_decl(Allocator* allocator, Decl* decl, Module* module)
{
    SymbolKind kind = SYMBOL_NONE;

    switch (decl->kind)
    {
        case AST_DeclVar:
            kind = SYMBOL_VAR;
            break;
        case AST_DeclConst:
            kind = SYMBOL_CONST;
            break;
        case AST_DeclProc:
            kind = SYMBOL_PROC;
            break;
        case AST_DeclStruct:
        case AST_DeclUnion:
        case AST_DeclEnum:
        case AST_DeclTypedef:
            kind = SYMBOL_TYPE;
            break;
        default:
            NIBBLE_FATAL_EXIT("Cannot create symbol from declaration kind %d\n", decl->kind);
            break;
    }

    return sym_alloc(allocator, kind, decl->name, decl, module);
}

static bool add_global_decl_sym(Program* prog, Module* module, Decl* decl)
{
    const char* sym_name = decl->name;

    if (hmap_get(&module->syms, PTR_UINT(sym_name)))
    {
        program_on_error(prog, "Duplicate definition of `%s`", sym_name);
        return false;
    }

    Allocator* allocator = &prog->ast_mem;
    Symbol* sym = sym_decl(allocator, decl, module);

    hmap_put(&module->syms, PTR_UINT(sym_name), PTR_UINT(sym));

    return true;
}

static bool add_global_type_sym(Program* prog, Module* module, const char* name, Type* type)
{
    const char* sym_name = intern_ident(name, cstr_len(name), NULL, NULL);

    if (hmap_get(&module->syms, PTR_UINT(sym_name)))
    {
        program_on_error(prog, "Duplicate definition of `%s`", sym_name);
        return false;
    }

    Allocator* allocator = &prog->ast_mem;
    Symbol* sym = sym_alloc(allocator, SYMBOL_TYPE, sym_name, NULL, NULL);
    sym->status = SYMBOL_STATUS_RESOLVED;
    sym->t.type = type;

    hmap_put(&module->syms, PTR_UINT(sym_name), PTR_UINT(sym));

    return true;
}

static void add_module(Program* prog, Module* module)
{
    uint64_t* pval = hmap_get(&prog->modules, PTR_UINT(module->path));
    Module* m = pval ? (void*)*pval : NULL;

    if (m != module)
    {
        assert(!m);
        hmap_put(&prog->modules, PTR_UINT(module->path), PTR_UINT(module));
    }
}

static void import_builtin_syms(Program* prog, Module* module)
{
    add_global_type_sym(prog, module, "void", type_void);
    add_global_type_sym(prog, module, "bool", type_bool);
    add_global_type_sym(prog, module, "char", type_char);
    add_global_type_sym(prog, module, "schar", type_schar);
    add_global_type_sym(prog, module, "uchar", type_uchar);
    add_global_type_sym(prog, module, "short", type_short);
    add_global_type_sym(prog, module, "ushort", type_ushort);
    add_global_type_sym(prog, module, "int", type_int);
    add_global_type_sym(prog, module, "uint", type_uint);
    add_global_type_sym(prog, module, "long", type_long);
    add_global_type_sym(prog, module, "ulong", type_ulong);
    add_global_type_sym(prog, module, "llong", type_llong);
    add_global_type_sym(prog, module, "ullong", type_ullong);
    add_global_type_sym(prog, module, "ssize", type_ssize);
    add_global_type_sym(prog, module, "usize", type_usize);
    add_global_type_sym(prog, module, "float32", type_float32);
    add_global_type_sym(prog, module, "float64", type_float64);
}

static void parse_module(Program* prog, Module* module)
{
    AllocatorState mem_state = allocator_get_state(&prog->gen_mem);
    const char* code = slurp_file(&prog->gen_mem, module->path);

    Parser parser = {0};
    Decl** decls = array_create(&prog->gen_mem, Decl*, 32);

    parser_init(&parser, &prog->ast_mem, &prog->tmp_mem, code, module->range.start, &prog->errors);
    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF))
    {
        Decl* decl = parse_decl(&parser);

        if (!decl)
            break;

        array_push(decls, decl);
    }

    module->range.end = module->range.start + ((parser.lexer.at - parser.lexer.str) + 1);
    module->num_decls = array_len(decls);
    module->decls = mem_dup_array(&prog->ast_mem, Decl*, decls, array_len(decls));

#ifdef NIBBLE_PRINT_DECLS
    ftprint_out("%s\n", ftprint_decls(&prog->gen_mem, module->num_decls, module->decls));
#endif

    allocator_restore_state(mem_state);
    import_builtin_syms(prog, module);

    for (size_t i = 0; i < module->num_decls; i += 1)
    {
        Decl* decl = module->decls[i];

        add_global_decl_sym(prog, module, decl);
    }
}

static Module* import_module(Program* prog, const char* module_path)
{
    uint64_t* pval = hmap_get(&prog->modules, PTR_UINT(module_path));
    Module* module = pval ? (void*)*pval : NULL;

    if (!module)
    {
        module = alloc_type(&prog->ast_mem, Module, true);
        module->path = module_path;
        module->syms = hmap(8, NULL);
        module->range.start = prog->curr_pos;

        add_module(prog, module);
        parse_module(prog, module);

        prog->curr_pos = module->range.end;
    }

    return module;
}

static void free_module(Module* module)
{
    hmap_destroy(&module->syms);
}

static Symbol* lookup_symbol(Program* prog, const char* name)
{
    // TODO: Lookup local scopes first.

    uint64_t* pval = hmap_get(&prog->curr_module->syms, PTR_UINT(name));

    return pval ? (Symbol*)*pval : NULL;
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

            return ident_sym->t.type;
        }
        case AST_TypeSpecPtr: {
            TypeSpecPtr* ts = (TypeSpecPtr*)typespec;
            TypeSpec* base_ts = ts->base;
            Type* base_type = resolve_typespec(prog, base_ts);

            if (!base_type)
                return NULL;

            return type_ptr(&prog->ast_mem, base_type);
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

    Module* old_module = enter_module(prog, sym->module);

    switch (sym->kind)
    {
        case SYMBOL_VAR:
            sym->t.type = resolve_decl_var(prog, (DeclVar*)sym->decl);
            sym->status = SYMBOL_STATUS_RESOLVED;
            break;
        case SYMBOL_CONST:
            break;
        case SYMBOL_PROC:
            break;
        case SYMBOL_TYPE:
            break;
        case SYMBOL_MODULE:
            break;
        default:
            ftprint_err("Unknown symbol kind `%d`\n", sym->kind);
            assert(0);
            break;
    }

    restore_module(prog, old_module);
}

static Symbol* resolve_name(Program* prog, const char* name)
{
    Symbol* sym = lookup_symbol(prog, name);

    if (sym)
        resolve_symbol(prog, sym);

    return sym;
}

static void resolve_module(Program* prog, Module* module)
{
    Module* old_module = enter_module(prog, module);

    for (size_t i = 0; i < module->num_decls; i += 1)
    {
        const char* name = module->decls[i]->name;
        Symbol* sym = resolve_name(prog, name);

        ftprint_out("Resolved `%s`. type is `%s`\n", name, type_name(sym->t.type));
    }

    restore_module(prog, old_module);
}

Program* compile_program(const char* path)
{
    const char* main_module_path = intern_str_lit(path, cstr_len(path));
    Allocator boot_mem = allocator_create(65536);
    Program* prog = alloc_type(&boot_mem, Program, true);

    prog->gen_mem = boot_mem;
    prog->ast_mem = allocator_create(4096);
    prog->tmp_mem = allocator_create(256);
    prog->errors = byte_stream_create(&prog->ast_mem);
    prog->modules = hmap(8, NULL);

    list_head_init(&prog->local_syms);

    // 1. Import module (parses, install decl syms, install builtins, import imports (not yet))
    Module* module = import_module(prog, main_module_path);

    // 2. Resolve all syms
    //resolve_module(prog, module);

    if (prog->errors.count > 0)
    {
        ftprint_out("\nErrors: %lu\n", prog->errors.count);

        ByteStreamChunk* chunk = prog->errors.first;

        while (chunk)
        {
            ftprint_out("%s\n", chunk->buf);

            chunk = chunk->next;
        }
    }

    return prog;
}

void free_program(Program* prog)
{
    // Clean up modules
    for (size_t i = 0; i < prog->modules.cap; i += 1)
    {
        HMapEntry* entry = prog->modules.entries + i;

        if (entry->key)
        {
            Module* module = UINT_PTR(entry->value, Module);
            free_module(module);
        }
    }

    hmap_destroy(&prog->modules);

    // TODO: Clean up scopes

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
