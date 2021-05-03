#include "resolver.h"
#include "parser.h"


static char* slurp_file(Allocator* allocator, const char* filename)
{
    FILE* fd = fopen(filename, "r");
    if (!fd)
        return NULL;

    if (fseek(fd, 0, SEEK_END) < 0)
        return NULL;

    long int size = ftell(fd);
    if (size < 0)
        return NULL;

    char* buf = mem_allocate(allocator, size + 1, DEFAULT_ALIGN, false);
    if (!buf)
        return NULL;

    if (fseek(fd, 0, SEEK_SET) < 0)
        return NULL;

    size_t n = fread(buf, 1, (size_t)size, fd);
    if (ferror(fd))
        return NULL;

    fclose(fd);

    buf[n] = '\0';

    return buf;
}

static void resolver_destroy(Resolver* resolver)
{
}

Symbol* sym_alloc(Allocator* allocator, SymbolKind kind, const char* name, Decl* decl, Module* module)
{
    Symbol* sym = new_type(allocator, Symbol, true);

    sym->kind = kind;
    sym->module = module;
    sym->name = name;
    sym->decl = decl;

    return sym;
}

Symbol* sym_decl(Allocator* allocator, Decl* decl, Module* module)
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
            assert(!"Cannot create symbol from declaration kind %d\n", decl->kind);
            break;
    }

    return sym_alloc(allocator, kind, decl->name, decl, module);
}

void add_global_decl_sym(Resolver* resolver, Decl* decl)
{
    Allocator* allocator = &resolver->curr_module->ast_arena;
    Symbol* sym = sym_decl(allocator, decl, resolver->curr_module);

    if (decl->kind == AST_DeclEnum)
    {
        // TODO: Add each enum as a separate const decl symbol.
    }

    add_global_sym(resolver, sym);

    return sym;
}

Program* compile_program(const char* path)
{
    const char* name = intern_str_lit(path, cstr_len(path));
    Allocator boot_mem = allocator_create(4096);
    Program* prog = new_type(&boot_mem, Program, true);

    prog->gen_mem = boot_mem;
    prog->ast_mem = allocator_create(4096);
    prog->errors = byte_stream_create(&module->ast_mem);
    prog->local_syms = hash_map(8, allocator);

    const char* code = slurp_file(&module->gen_mem, filename);

    if (!code)
    {
        free_program(prog);
        return NULL;
    }

    // 1. Import module (parses, install decl syms, import builtins, import imports (not yet))
    // 2. Resolve all syms

    Module* module = parse_module(&prog->gen_mem, &prog->ast_mem, &prog->errors, code, 0); 

    prog->curr_module = module;

    for (size_t i = 0; i < module->num_decls; i += 1)
    {
        Decl* decl = module->decls[i];

        add_global_decl_sym(&resolver, decl);
    }

    resolver_destroy(&resolver);

    return module;
}

static Ast parse_code(Allocator* tmp_mem, Allocator* ast_mem, ByteStream* errors, const char* code, ProgPos pos)
{
    Ast ast = {0};
    Parser parser = {0};
    AllocatorState mem_state = allocator_get_state(tmp_mem);
    Decl** decls = array_create(tmp_mem, Decl*, 32);

    parser_init(&parser, ast_mem, code, pos, errors);
    next_token(&parser);

    while (!is_token_kind(&parser, TKN_EOF))
    {
        Decl* decl = parse_decl(&parser);
        if (!decl)
            break;

        array_push(decls, decl);
    }

    ast.num_decls = array_len(decls);
    ast.decls = mem_dup_array(ast_mem, Decl*, decls, array_len(decls));

#ifdef NIBBLE_PRINT_DECLS
    ftprint_out("%s\n", ftprint_decls(tmp_mem, ast.num_decls, ast.decls));
#endif


    if (errors->num_chunks > 0)
    {
        ftprint_out("\nParsing errors: %lu\n", errors->num_chunks);

        ByteStreamChunk* chunk = errors->first;

        while (chunk)
        {
            ftprint_out("%s\n", chunk->buf);
            chunk = chunk->next;
        }
    }

    parser_destroy(&parser);
    allocator_restore_state(mem_state);

    return ast;
}

void free_module(Module* module)
{
    hash_map_destroy(&module->syms);
}

void free_program(Program* prog)
{
    Allocator bootstrap = prog->gen_mem;

#ifndef NDEBUG
    print_allocator_stats(&prog->ast_mem, "Prog AST mem stats");
    print_allocator_stats(&prog->gen_mem, "Prog gen mem stats");
#endif

    hash_map_destroy(&prog->local_syms);
    allocator_destroy(&prog->ast_mem);
    allocator_destroy(&bootstrap);
}
