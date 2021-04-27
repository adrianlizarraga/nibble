#include "resolver.h"
#include "parser.h"

//#define NIBBLE_PRINT_DECLS 

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

Module* compile_module(const char* filename, ProgPos pos)
{
    Allocator bootstrap = allocator_create(4096);
    Module* module = new_type(&bootstrap, Module, true);
    module->allocator = bootstrap;
    module->errors = byte_stream_create(&module->allocator);
    module->ast_arena = allocator_create(4096);
    module->syms_map = hash_map(8, NULL);

    const char* str = slurp_file(&module->allocator, filename);
    if (!str)
        return NULL;

    /////////////////////////////////////
    //  Parse top-level declarations
    /////////////////////////////////////

    Parser parser = {0};
    parser_init(&parser, &module->ast_arena, str, pos, &module->errors);
    next_token(&parser);

    AllocatorState mem_state = allocator_get_state(&module->allocator);
    Decl** decls = array_create(&module->allocator, Decl*, 32);

    while (!is_token_kind(&parser, TKN_EOF))
    {
        Decl* decl = parse_decl(&parser);
        if (!decl)
            break;

        array_push(decls, decl);
    }

    module->num_decls = array_len(decls);
    module->decls = mem_dup_array(&module->ast_arena, Decl*, decls, array_len(decls));

#ifdef NIBBLE_PRINT_DECLS
    ftprint_out("%s\n", ftprint_decls(&module->allocator, module->num_decls, module->decls));
#endif

    allocator_restore_state(mem_state);

    ///////////////////////////////////
    //  Print errors
    ///////////////////////////////////

    if (module->errors.num_chunks > 0)
    {
        ftprint_out("\nParsing errors: %lu\n", module->errors.num_chunks);
        ByteStreamChunk* chunk = module->errors.first;

        while (chunk)
        {
            ftprint_out("%s\n", chunk->buf);
            chunk = chunk->next;
        }
    }

    parser_destroy(&parser);

    return module;
}

void free_module(Module* module)
{
    Allocator bootstrap = module->allocator;

#ifndef NDEBUG
    print_allocator_stats(&module->ast_arena, "AST mem stats");
    print_allocator_stats(&module->allocator, "Module mem stats");
#endif

    hash_map_destroy(&module->syms_map);
    allocator_destroy(&module->ast_arena);
    allocator_destroy(&bootstrap);
}
